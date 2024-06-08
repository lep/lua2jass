{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Compiler.Lua where

import qualified Jass.Ast as Jass

import Prelude hiding (head, LT, GT, EQ)
import qualified Prelude

import Data.Text (Text)
import qualified Data.Text as Text

import Language.Lua

import System.Environment

import Control.Arrow

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import Compiler.Bytecode (Bytecode)
import qualified Compiler.Bytecode as B

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy as BL

import qualified Jass.Printer as Jass
import Data.ByteString.Builder
import System.IO

import Options.Applicative

type Asm = [Bytecode]
type S = (Int, Map Text Asm, [B.Label])

type CompileM = StateT S (Writer Asm)

runCompiler :: CompileM a -> Map Text Asm
runCompiler c = --snd . fst . runWriter . flip execStateT (0, Map.empty, [])
    let (cnt, asm, l) = fst . runWriter $ execStateT c (0, Map.empty, [])
    in asm

runCompiler' :: S -> CompileM a -> (S, Asm, a)
runCompiler' initialState m = 
    let x = runStateT m initialState
        ((a,s), w) = runWriter x
    in (s, w, a)

emit :: Bytecode -> CompileM ()
emit = tell . pure

-- pred is important here as varargs and/or normal args get messed up
-- becasuse they share the same space. in the future it would probably be
-- wise to check this out again and make it grow towars positive infinity
-- instead
fresh :: CompileM Int
fresh = do
    (cnt, m, l) <- get
    let cnt' = pred cnt
    put (cnt', m, l)
    pure cnt'
    --modify (first pred) >> gets fst

pushBreakLabel :: B.Label -> CompileM ()
pushBreakLabel lbl = do
    (cnt, m, l) <- get
    let l' = lbl:l
    put (cnt, m, l')

popBreakLabel :: CompileM ()
popBreakLabel = do
    (cnt, m, l) <- get
    put (cnt, m, tail l)

getCurrentBreakLabel :: CompileM B.Label
getCurrentBreakLabel = do
    (_, _, l) <- get
    pure $ Prelude.head l

addFunction :: Text -> Asm -> CompileM ()
addFunction fn asm = do
    (s, m, l) <- get
    let m' = Map.insert fn asm m
    put (s, m', l)

local :: CompileM a -> CompileM (a, Asm)
local x = do
    s <- get
    let (s', w, a) = runCompiler' s x
    put s'
    pure (a, w)


compileScript :: Block -> CompileM ()
compileScript block = do
    (_, asm) <- local $ do
        emit $ B.Fun 0 "$main"
        compileBlock' False block
        emit B.Ret
    addFunction "$main" asm

compileReturn :: B.Register -> [Exp] -> CompileM ()
compileReturn reg_ret = go 1
  where
    go _ [] = pure ()
    go idx [PrefixExp (PEFunCall fc)] = do
        reg_res <- compileFunCall fc
        emit $ B.Append idx reg_ret reg_res
    go idx [Vararg] = do
        reg_res <- compileExp compilePrefixExp Vararg
        emit $ B.Append idx reg_ret reg_res
    go idx (Vararg:es) = do
        reg_tbl <- fresh
        reg_one <- fresh
        reg_res <- fresh
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show idx
        emit $ B.LitInt reg_one "1"
        emit $ B.GetLit reg_tbl "..."
        emit $ B.GetTable reg_res reg_tbl reg_one
        emit $ B.SetTable reg_ret reg_idx reg_res
        go (succ idx) es
    go idx (e:es) = do
        reg_res <- compileExp compilePrefixExp' e
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show idx
        emit $ B.SetTable reg_ret reg_idx reg_res
        go (succ idx) es

    
compileFn :: Block -> CompileM ()
compileFn (Block stmts ret) = do
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just es -> do
            reg_ret <- fresh
            emit $ B.GetLit reg_ret "$ret"
            compileReturn reg_ret es

    emit B.Ret

compileBlock :: Block -> CompileM ()
compileBlock block = do
    emit B.Enter
    compileBlock' True block

compileBlock' :: Bool -> Block -> CompileM ()
compileBlock' emitLeave (Block stmts ret) = do
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just es -> do
            reg_ret <- fresh
            emit $ B.GetLit reg_ret "$ret"
            compileReturn reg_ret es
            when emitLeave $
                emit B.Leave
            emit B.Ret

compileVarAssign :: (Text -> CompileM a) -> Var -> CompileM (B.Register -> CompileM ())
compileVarAssign define = \case
    VarName (Name name) -> do
        pure $ \value -> do
            define name
            emit $ B.SetLit name value
    Select prefixExp exp -> do
        tbl <- compilePrefixExp' prefixExp
        idx <- compileExp compilePrefixExp' exp
        pure $ \value -> emit $ B.SetTable tbl idx value
    SelectName tbl (Name name) -> do
        tbl' <- compilePrefixExp' tbl
        idx <- fresh
        emit $ B.LitString idx name
        pure $ \value -> emit $ B.SetTable tbl' idx value
        

compileAssign :: [Var] -> [Exp] -> CompileM ()
compileAssign = compileAssign' (const $ pure ())

compileAssign' :: (Text -> CompileM a) -> [Var] -> [Exp] -> CompileM ()
compileAssign' define vs es = do
    -- compile all lhs first, then compile all rhs, then match up the registers
    -- with special handling of varargs stuff
    fns <- mapM (compileVarAssign define) vs
    regs_es <- mapM (compileExp compilePrefixExp) es
    go fns regs_es es
  where
    go [] _ _ = pure ()
    go fs _ [] = mapM_ `flip` fs $ \_ -> do
        reg_nil <- fresh
        emit $ B.LitNil reg_nil
        mapM_ (\f -> f reg_nil) fs

    go fs [reg_ret] [PrefixExp (PEFunCall _)] = do
        forM_ (zip [1..] fs) $ \(idx, f) -> do
            reg_idx <- fresh
            reg_tmp1 <- fresh
            emit $ B.LitInt reg_idx $ Text.pack $ show idx
            emit $ B.GetTable reg_tmp1 reg_ret reg_idx
            f reg_tmp1
    go fs [reg_varargs] [Vararg] = do
        forM_ (zip [1..] fs) $ \(idx, f) -> do
            reg_idx <- fresh
            emit $ B.LitInt reg_idx $ Text.pack $ show idx
            
            reg_val <- fresh
            emit $ B.GetTable reg_val reg_varargs reg_idx
            f reg_val

    go (f:fs) (reg_fc:regs) ((PrefixExp (PEFunCall _)):es) = do
        reg_one <- fresh
        emit $ B.LitInt reg_one "1"
        reg_val <- fresh
        emit $ B.GetTable reg_val reg_fc reg_one
        f reg_val
        go fs regs es
        
    go (f:fs) (reg_varargs:regs) (Vararg:es) = do
        reg_one <- fresh
        emit $ B.LitInt reg_one "1"
        reg_val <- fresh
        emit $ B.GetTable reg_val reg_varargs reg_one
        f reg_val

        go  fs regs es


    go (f:fs) (reg_e:regs) (_:es) = do
        f reg_e
        go fs regs es

        
compileStat :: Stat -> CompileM ()
compileStat = \case
    EmptyStat -> pure ()
    FunCall fc -> void $ compileFunCall fc

    Assign vs es -> compileAssign vs es


    LocalAssign names Nothing ->
        forM_ names $ \(Name name) ->
            emit $ B.Local name

    LocalAssign names (Just exps) ->
        void $ compileAssign' (emit . B.Local) (map VarName names) exps

    Do block -> do
        emit B.Enter
        compileBlock' False block
        emit B.Leave

    Repeat block expr -> do

        lbl_while_start <- fresh
        lbl_break <- fresh

        pushBreakLabel lbl_break

        emit $ B.Label lbl_while_start
        emit B.Enter

        compileBlock' False block

        reg_cond <- compileExp compilePrefixExp' expr

        emit $ B.JumpT lbl_break reg_cond
        emit B.Leave
        emit $ B.Jump lbl_while_start

        emit $ B.Label lbl_break
        emit B.Leave

        popBreakLabel

    While cond block -> do
        lbl_start <- fresh
        lbl_end <- fresh
        lbl_break <- fresh
        pushBreakLabel lbl_break

        emit $ B.Label lbl_start
        c <- compileExp compilePrefixExp' cond
        c' <- fresh
        emit $ B.Not c' c
        emit $ B.JumpT lbl_end c'

        emit B.Enter
        compileBlock' False block
        emit B.Leave

        emit $ B.Jump lbl_start

        emit $ B.Label lbl_break
        emit B.Leave

        emit $ B.Label lbl_end
        popBreakLabel

    ForIn names@(Name var1:_) exps block -> do
        let varname_f = VarName $ Name "$f"
            varname_s = VarName $ Name "$s"
            varname_var = VarName $ Name "$var"

        lbl_while_start <- fresh
        lbl_break <- fresh
        pushBreakLabel lbl_break

        -- do
        emit B.Enter

        -- local f, s, var = exps
        emit $ B.Local "$f"
        emit $ B.Local "$s"
        emit $ B.Local "$var"


        compileAssign [varname_f, varname_s, varname_var] exps

        -- while true
        emit $ B.Label lbl_while_start
        emit B.Enter


        -- local var_1, ..., var_n = f(s, var)
        forM_ names $ \(Name name) ->
            emit $ B.Local name

        compileAssign (map VarName names) [
            PrefixExp $ PEFunCall $ NormalFunCall (PEVar varname_f) $ Args [PrefixExp $ PEVar varname_s, PrefixExp $ PEVar varname_var]
            ]

        -- if var_1 == nil then break end
        reg_nil <- fresh
        emit $ B.LitNil reg_nil

        reg_var1 <- fresh
        emit $ B.GetLit reg_var1 var1

        reg_cmp <- fresh
        emit $ B.EQ reg_cmp reg_var1 reg_nil
        emit $ B.JumpT lbl_break reg_cmp
        emit $ B.SetLit "$var" reg_var1

        -- <block>
        compileBlock' False block

        -- end
        emit B.Leave
        emit $ B.Jump lbl_while_start

        emit $ B.Label lbl_break
        emit B.Leave

        -- end
        emit B.Leave

        popBreakLabel
        

    ForRange (Name var) start end step block -> do

        lbl_while_start <- fresh
        lbl_break <- fresh

        pushBreakLabel lbl_break

        -- do
        emit B.Enter

        -- local var, limit, step = ...
        emit $ B.Local "$var"
        emit $ B.Local "$limit"
        emit $ B.Local "$step"

        reg_start <- compileExp compilePrefixExp' start
        --emit $ B.SetLit "$var" reg_start -- we set $var to $var - $step directly

        reg_limit <- compileExp compilePrefixExp' end
        emit $ B.SetLit "$limit" reg_limit

        reg_step <- case step of
            Nothing -> do
                one <- fresh
                emit $ B.LitInt one "1"
                pure one
            Just e -> compileExp compilePrefixExp' e
        emit $ B.SetLit "$step" reg_step

        -- var = var - step
        emit $ B.Sub reg_start reg_start reg_step
        emit $ B.SetLit "$var" reg_start

        -- while true
        emit $ B.Label lbl_while_start
        emit B.Enter

        -- var = var + step
        reg_var <- fresh
        reg_limit <- fresh
        reg_step <- fresh
        reg_t1 <- fresh

        emit $ B.GetLit reg_t1 "$var"
        emit $ B.GetLit reg_step "$step"
        emit $ B.Add reg_var reg_t1 reg_step
        emit $ B.SetLit "$var" reg_var

        emit $ B.GetLit reg_limit "$limit"

        -- if (step >= 0 and var > limit) or (step < 0 and var < limit )
        reg_zero <- fresh
        emit $ B.LitInt reg_zero "0"
        reg_step_gte_0 <- fresh
        reg_step_lt_0 <- fresh
        reg_var_gt_limit <- fresh
        reg_var_lt_limit <- fresh
        reg_and1 <- fresh
        reg_and2 <- fresh

        lbl_or1 <- fresh
        lbl_or2 <- fresh
        -- step >= 0, but invert it for the jump
        emit $ B.LT reg_step_gte_0 reg_step reg_zero
        -- var > limit, but invert it for the jump
        emit $ B.LTE reg_var_gt_limit reg_var reg_limit

        -- and
        emit $ B.JumpT lbl_or1 reg_step_gte_0
        emit $ B.JumpT lbl_or1 reg_var_gt_limit 
        -- break
        emit $ B.JumpT lbl_break reg_and1 

        emit $ B.Label lbl_or1
        -- step < 0, but invert it for the jump
        emit $ B.Not reg_step_lt_0 reg_step_gte_0
        -- var < limit, but invert for the jump
        emit $ B.GTE reg_var_lt_limit reg_var reg_limit
        -- and
        emit $ B.JumpT lbl_or2 reg_step_lt_0 
        emit $ B.JumpT lbl_or2 reg_var_lt_limit 

        -- break
        emit $ B.JumpT lbl_break reg_and1 


        emit $ B.Label lbl_or2
        --local v = var
        emit $ B.Local var
        emit $ B.SetLit var reg_var

        -- <block>
        compileBlock' False block


        --end
        emit B.Leave
        emit $ B.Jump lbl_while_start


        -- we're missing one leave here
        emit $ B.Label lbl_break
        emit B.Leave


        -- end
        emit B.Leave

        popBreakLabel

    LocalFunAssign (Name name) funBody -> do
        x <- fresh
        let internalName = Text.pack $ "$" <> Text.unpack name <> show x
        compileFunBody x internalName funBody
        emit $ B.Local name
        emit $ B.Lambda x internalName
        emit $ B.SetLit name x

    FunAssign (FunName (Name fnname) [] Nothing) funBody -> do
        let internalName = "$" <> fnname
        x <- fresh
        compileFunBody x internalName funBody
        emit $ B.Lambda x internalName
        emit $ B.SetLit fnname x

    FunAssign (FunName (Name t) ns Nothing) funBody -> do
        r0 <- fresh
        emit $ B.GetLit r0 t
        let Name fnname = last ns

        (u, internalName) <- foldM `flip` (r0, t) `flip` init ns $ \(r0, name) (Name n) -> do
            rn <- fresh
            ln <- fresh
            emit $ B.LitString ln n
            emit $ B.GetTable rn r0 ln
            pure (rn, name <>"."<> n)

        let internalName' = "$" <> internalName <> "." <> fnname

        x <- fresh
        compileFunBody x internalName' funBody
        emit $ B.Lambda x internalName'
        l1 <- fresh
        emit $ B.LitString l1 fnname
        emit $ B.SetTable u l1 x


    FunAssign (FunName (Name fnname) [] (Just (Name obj))) (FunBody args isVararg body) -> do
        -- function x:f() end --> function x.f(self) end
        let internalName = "$" <> fnname <> "_" <> obj
            funBody = FunBody (Name "self":args) isVararg body
        x <- fresh
        compileFunBody x internalName funBody
        emit $ B.Lambda x internalName
        y <- fresh
        z <- fresh
        emit $ B.LitString y obj
        emit $ B.GetLit z fnname
        emit $ B.SetTable z y x 

    FunAssign (FunName (Name t) ns (Just (Name method))) (FunBody args isVararg body) -> do
        r0 <- fresh
        emit $ B.GetLit r0 t
        let fnname = method

        (u, internalName) <- foldM `flip` (r0, t) `flip` ns $ \(r0, name) (Name n) -> do
            rn <- fresh
            ln <- fresh
            emit $ B.LitString ln n
            emit $ B.GetTable rn r0 ln
            pure (rn, name <>"."<> n)

        let internalName' = "$" <> internalName <> ":" <> fnname
        let funBody = FunBody (Name "self":args) isVararg body
        x <- fresh
        compileFunBody x internalName' funBody
        emit $ B.Lambda x internalName'

        l1 <- fresh
        emit $ B.LitString l1 fnname
        emit $ B.SetTable u l1 x


    If ifs elseBlock -> do
        lbl_end <- fresh

        mapM_ (compileIfBlock lbl_end) ifs
        
        traverse (compileElseBlock lbl_end) elseBlock
        emit $ B.Label lbl_end
    Break -> do
        lbl <- getCurrentBreakLabel
        emit $ B.Jump lbl

    x -> error $ unwords ["unhandled", show x ]
        
  where
    compileIfBlock lbl_end (cond, block) = do
        lbl_not <- fresh
        not_cond <- fresh
        c <- compileExp compilePrefixExp' cond
        emit $ B.Not not_cond c
        emit $ B.JumpT lbl_not not_cond
        compileBlock block
        emit $ B.Jump lbl_end
        emit $ B.Label lbl_not

compileElseBlock :: B.Label -> Block -> CompileM ()
compileElseBlock lbl_end body = do
    compileBlock body
    emit $ B.Jump lbl_end

compileFunCallArgs :: B.Register -> [Exp] -> Int -> CompileM ()
compileFunCallArgs reg_params args cnt = go cnt args
  where
    go _ [] = pure ()
    go cnt [PrefixExp (PEFunCall fc)] = do
        reg_res <- compileFunCall fc
        emit $ B.Append cnt reg_params reg_res
    go cnt [Vararg] = do
        reg_vararg <- fresh
        emit $ B.GetLit reg_vararg "..."
        emit $ B.Append cnt reg_params reg_vararg
    go cnt (Vararg:xs) = do
        reg_vararg <- fresh
        emit $ B.GetLit reg_vararg "..."
        reg_one <- fresh
        emit $ B.LitInt reg_one "1"
        reg_idx <- fresh
        reg_res <- fresh
        emit $ B.GetTable reg_res reg_vararg reg_one
        emit $ B.LitInt reg_idx $ Text.pack $ show cnt
        emit $ B.SetTable reg_params reg_idx reg_res
        go (succ cnt) xs
    go cnt (x:xs) = do
        reg_res <- compileExp compilePrefixExp' x
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show cnt
        emit $ B.SetTable reg_params reg_idx reg_res
        go (succ cnt) xs

compileFunArgs :: Int -> Int -> FunArg -> CompileM ()
compileFunArgs cnt reg_table = \case
    Args args -> compileFunCallArgs reg_table args cnt
    StringArg txt -> do
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show cnt

        reg_litstr <- compileExp compilePrefixExp' $ String txt

        emit $ B.SetTable reg_table reg_idx reg_litstr
    TableArg tbl_fields -> do
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show cnt

        reg_tbl <- fresh
        emit $ B.Table reg_tbl
        compileTableConst reg_tbl tbl_fields

        emit $ B.SetTable reg_table reg_idx reg_tbl

compileFunArgs' :: B.Register -> FunArg -> CompileM ()
compileFunArgs' = compileFunArgs 1

compileFunCall :: FunCall -> CompileM B.Register
compileFunCall = \case
    NormalFunCall fn funArg -> do
        fn <- compilePrefixExp' fn

        reg_zero <- fresh
        reg_ret <- fresh
        reg_params <- fresh
        emit $ B.Table reg_params
        emit $ B.Table reg_ret
        emit $ B.LitInt reg_zero "0"

        emit $ B.SetTable reg_params reg_zero reg_ret
        compileFunArgs' reg_params funArg
        emit $ B.Call fn reg_params
        pure reg_ret

    MethodCall prefixExp (Name name) funArg -> do
        reg_zero <- fresh
        reg_ret <- fresh
        reg_obj <- compilePrefixExp' prefixExp

        reg_name <- fresh
        emit $ B.LitString reg_name name

        reg_fn <- fresh
        emit $ B.GetTable reg_fn reg_obj reg_name

        reg_params <- fresh
        emit $ B.Table reg_params
        emit $ B.Table reg_ret
        emit $ B.LitInt reg_zero "0"

        reg_one <- fresh
        emit $ B.LitInt reg_one "1"

        emit $ B.SetTable reg_params reg_zero reg_ret
        emit $ B.SetTable reg_params reg_one reg_obj
        compileFunArgs 2 reg_params funArg

        emit $ B.Call reg_fn reg_params
        pure reg_ret

{--
    In "normal" expressions we only want the first return value of a function,
    but in some special cases we need all return values.

    This one selects only the first one.
-}
compilePrefixExp' :: PrefixExp -> CompileM B.Register
compilePrefixExp' = \case
    PEVar var -> compileVar var
    PEFunCall funcall -> do
        reg_res <- compileFunCall funcall
        reg_one <- fresh
        reg_ret <- fresh
        emit $ B.LitInt reg_one "1"
        emit $ B.GetTable reg_ret reg_res reg_one
        pure reg_ret
    Paren e -> compileExp compilePrefixExp' e

{-
    this one returns all the values
-}
compilePrefixExp :: PrefixExp -> CompileM B.Register
compilePrefixExp = \case
    PEVar var -> compileVar var
    PEFunCall funcall -> compileFunCall funcall
    Paren e -> compileExp compilePrefixExp' e

toByteCodeBinop :: Binop -> B.Register -> B.Register -> B.Register -> Bytecode
toByteCodeBinop = \case
    GTE -> B.GTE
    EQ -> B.EQ
    GT -> B.GT
    LT -> B.LT
    LTE -> B.LTE
    Sub -> B.Sub
    Mul -> B.Mul
    Add -> B.Add

    Div -> B.Div
    Exp -> B.Exp
    Mod -> B.Mod
    Concat -> B.Concat
    NEQ -> B.NEQ
    IDiv -> B.IDiv
    ShiftL -> B.ShiftL
    ShiftR -> B.ShiftR
    BAnd -> B.BAnd
    BOr -> B.BOr
    BXor -> B.BXor


toByteCodeUnop :: Unop -> B.Register -> B.Register -> Bytecode
toByteCodeUnop = \case
    Neg -> B.Neg
    Not -> B.Not
    Len -> B.Len
    Complement -> B.Complement

compileFunBody :: B.Label -> Text -> FunBody -> CompileM ()
compileFunBody lbl internalName (FunBody args isVararg body) = do
    let normal_args = succ $ length args
    (_, asm) <- local $ do
        emit $ B.Fun lbl internalName
        emit $ B.Local "$ret"
        emit $ B.SetLit "$ret" 0
        forM_ (zip [1..] args) $ \(param, Name arg) -> do
            emit $ B.Local arg
            emit $ B.SetLit arg param
        when isVararg $ do
            reg_varargs <- fresh
            reg_internal_params <- fresh
            emit $ B.GetLit reg_internal_params "$params"
            emit $ B.Local "..."
            emit $ B.Table reg_varargs
            emit $ B.GetList normal_args reg_varargs reg_internal_params
            emit $ B.SetLit "..." reg_varargs
        compileFn body
    addFunction internalName asm

compileExp :: (PrefixExp -> CompileM B.Register) -> Exp -> CompileM B.Register
compileExp pec = \case
    EFunDef funBody -> do
        x <- fresh
        let internalName = "$lambda" <> Text.pack (show x)
        compileFunBody x internalName funBody
        emit $ B.Lambda x internalName
        pure x
    String x -> do
        reg <- fresh
        emit $ B.LitString reg $ B.bla x
        pure reg
    Bool b -> do
        reg <- fresh
        emit $ B.LitBool reg b
        pure reg
    Nil -> do
        reg <- fresh
        emit $ B.LitNil reg
        pure reg
    Number IntNum x -> do
        reg <- fresh
        emit $ B.LitInt reg x
        pure reg
    Number FloatNum x -> do
        reg <- fresh
        emit $ B.LitFloat reg x
        pure reg

    Vararg -> do
        reg_ret <- fresh
        emit $ B.GetLit reg_ret "..."
        pure reg_ret

    PrefixExp e -> pec e
        
    Unop op e -> do
        reg <- fresh
        x <- compileExp compilePrefixExp' e
        emit $ toByteCodeUnop op reg x
        pure reg

    Binop And a b -> do
        reg_ret <- fresh
        reg_not <- fresh
        lbl_end <- fresh
        reg_a <- compileExp compilePrefixExp' a
        emit $ B.Set reg_ret reg_a
        emit $ B.Not reg_not reg_a
        emit $ B.JumpT lbl_end reg_not
        reg_b <- compileExp compilePrefixExp' b
        emit $ B.Set reg_ret reg_b
        emit $ B.Label lbl_end
        pure reg_ret

    Binop Or a b -> do
        lbl_end <- fresh
        reg_ret <- fresh
        reg_a <- compileExp compilePrefixExp' a
        emit $ B.Set reg_ret reg_a
        emit $ B.JumpT lbl_end reg_a
        reg_b <- compileExp compilePrefixExp' b
        emit $ B.Set reg_ret reg_b
        emit $ B.Label lbl_end
        pure reg_ret

    Binop binop a b -> do
        reg <- fresh
        a' <- compileExp compilePrefixExp' a
        b' <- compileExp compilePrefixExp' b
        emit $ toByteCodeBinop binop reg a' b'
        pure reg

    TableConst tableArgs -> do
        tbl <- fresh
        emit $ B.Table tbl
        compileTableConst tbl tableArgs
        pure tbl

compileTableConst :: Int -> [TableField] -> CompileM ()
compileTableConst reg_tbl = go 1
  where
    go _ [] = pure ()
    go cnt [Field (PrefixExp (PEFunCall fc)) ] = do
        reg_res <- compileFunCall fc
        emit $ B.Append cnt reg_tbl reg_res
    go cnt [Field Vararg] = do
        reg_vararg <- fresh
        emit $ B.GetLit reg_vararg "..."
        emit $ B.Append cnt reg_tbl reg_vararg

    go cnt (x:xs) =
      case x of
        ExpField idx val -> do
            reg_idx <- compileExp compilePrefixExp' idx
            reg_val <- compileExp compilePrefixExp' val
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go cnt xs
        NamedField (Name idx) val -> do
            reg_idx <- fresh
            emit $ B.LitString reg_idx idx
            reg_val <- compileExp compilePrefixExp' val
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go cnt xs
        Field exp -> do
            reg_idx <- fresh
            emit $ B.LitInt reg_idx $ Text.pack $ show cnt
            reg_val <- compileExp compilePrefixExp' exp
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go (succ cnt) xs

compileVar :: Var -> CompileM B.Register
compileVar = \case
    VarName (Name name) -> do
        tmp <- fresh
        emit $ B.GetLit tmp name
        pure tmp
    Select prefixExp exp -> do
        x <- fresh
        table <- compilePrefixExp' prefixExp
        index <- compileExp compilePrefixExp' exp
        emit $ B.GetTable x table index
        pure x

    SelectName prefixExp (Name name) -> do
        x <- fresh
        table <- compilePrefixExp' prefixExp
        index <- fresh
        emit $ B.LitString index name
        emit $ B.GetTable x table index
        pure x



compile :: Block -> ((String, Set String), Jass.Ast String Jass.Programm)
compile ast =
    let asm = concatMap snd . Map.toList . runCompiler $ compileScript ast
        scope = "Auto"
        deps = Set.singleton "Ins"
    in ((scope, deps), B.toJassFunction asm)
