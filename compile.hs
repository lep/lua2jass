{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Debug.Trace

import Prelude hiding (head, LT, GT, EQ)

import Data.Aeson (encode)

import Data.Text (Text)
import qualified Data.Text as Text

import Language.Lua hiding (Label)
import Language.Lua.Syntax hiding (Label)

import System.Environment

import Control.Arrow

import Control.Monad.Writer
import Control.Monad.Writer.Class

import Control.Monad.State
import Control.Monad.State.Class

import Bytecode (Bytecode)
import qualified Bytecode as B

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.ByteString.Lazy as BL

type Asm = [Bytecode]
type S = (Int, Map Text Asm)

type CompileM = StateT S (Writer Asm)
--type CompileM = WriterT Asm (State S)

runCompiler :: CompileM a -> Map Text Asm
runCompiler = snd . fst . runWriter . flip execStateT (0, Map.empty)

runCompiler' :: S -> CompileM a -> (S, Asm, a)
runCompiler' initialState m = 
    let x = runStateT m initialState
        ((a,s), w) = runWriter x
    in (s, w, a)

emit :: Bytecode -> CompileM ()
emit = tell . pure

fresh :: CompileM Int
fresh = modify (first pred) >> gets fst

addFunction :: Text -> Asm -> CompileM ()
addFunction fn asm = do
    (s, m) <- get
    let m' = Map.insert fn asm m
    put (s, m')

local :: CompileM a -> CompileM (a, Asm)
local x = do
    s <- get
    let (s', w, a) = runCompiler' s x
    put s'
    pure (a, w)


compileScript :: Block -> CompileM ()
compileScript block = do
    (_, asm) <- local $ do
        emit $ B.Fun "$_main"
        compileBlock block
        emit B.Ret
    addFunction "$_main" asm

compileReturn reg_ret = go 1
  where
    go _ [] = pure ()
    go idx [PrefixExp (PEFunCall fc)] = do
        reg_res <- compileFunCall fc
        emit $ B.Append idx reg_ret reg_res
    go idx (e:es) = do
        reg_res <- compileExp e
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show idx
        emit $ B.SetTable reg_ret reg_idx reg_res
        go (succ idx) es

    
compileFn :: Block -> CompileM ()
compileFn (Block stmts ret) = do
    emit $ B.Local "$_ret"
    ret' <- fresh
    emit $ B.Table ret'
    emit $ B.SetLit "$_ret" ret'
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just es -> compileReturn ret' es
    emit B.Ret

compileBlock :: Block -> CompileM ()
compileBlock block = do
    emit B.Enter
    compileBlock' block
    emit B.Leave

compileBlock' :: Block -> CompileM ()
compileBlock' (Block stmts ret) = do
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just es -> do
            reg_ret <- fresh
            emit $ B.GetLit reg_ret "$_ret"
            compileReturn reg_ret es
            emit $ B.Ret

--compileVarAssign :: B.Register -> Var -> CompileM ()
compileVarAssign = \case
    VarName (Name name) -> do
        pure $ \value -> B.SetLit name value
    Select prefixExp exp -> do
        tbl <- compilePrefixExp prefixExp
        idx <- compileExp exp
        pure $ \value -> B.SetTable tbl idx value
    SelectName tbl (Name name) -> do
        tbl' <- compilePrefixExp tbl
        idx <- fresh
        emit $ B.LitString idx name
        pure $ \value -> B.SetTable tbl' idx value
        

compileAssign vs es = do
    fns <- mapM compileVarAssign vs
    go fns es
  where
    go [] es = mapM_ compileExp es
    go fs [] = mapM_ `flip` fs $ \v -> do
        reg_nil <- fresh
        emit $ B.LitNil reg_nil
        mapM_ (\f -> emit $ f reg_nil) fs

    go fs [PrefixExp (PEFunCall fc)] = do
        reg_ret <- compileFunCall fc
        reg_table <- fresh
        forM_ (zip [1..] fs) $ \(idx, f) -> do
            reg_idx <- fresh
            reg_tmp1 <- fresh
            emit $ B.LitInt reg_idx $ Text.pack $ show idx
            emit $ B.GetTable reg_tmp1 reg_ret reg_idx
            emit $ f reg_tmp1


    go (f:fs) (e:es) = do
        reg_e <- compileExp e
        emit $ f reg_e
        go fs es
        
compileStat :: Stat -> CompileM ()
compileStat = \case
    EmptyStat -> pure ()
    FunCall fc -> void $ compileFunCall fc

    Assign vs es -> compileAssign vs es

    ---- test
    --Assign [ v1, v2 ] [ e1, e2 ] -> do
    --    a1 <- compileVarAssign v1
    --    a2 <- compileVarAssign v2
    --    reg_e1 <- compileExp e1
    --    reg_e2 <- compileExp e2
    --    emit $ a1 reg_e1
    --    emit $ a2 reg_e2
    --Assign [VarName (Name v1)] [e1] -> do
    --    y <- compileExp e1
    --    emit $ B.SetLit v1 y
    --Assign [Select prefixExp exp] [e1] -> do
    --    tbl <- compilePrefixExp prefixExp
    --    idx <- compileExp exp
    --    val <- compileExp e1
    --    emit $ B.SetTable tbl idx val
    --Assign [SelectName tbl (Name name)] [e1] -> do
    --    tbl' <- compilePrefixExp tbl
    --    idx <- fresh
    --    emit $ B.LitString idx name
    --    e <- compileExp e1
    --    emit $ B.SetTable tbl' idx e
        

    LocalAssign [Name v1] (Just [e1]) -> do
        x <- compileExp e1
        emit $ B.Local v1
        emit $ B.SetLit v1 x
    --    error "TODO"
    Do block -> compileBlock block

    Repeat block expr -> do
        lbl_start <- fresh
        reg_transfer <- fresh
        let local_name = "$" <> Text.pack (show reg_transfer)
        emit $ B.Local local_name

        emit $ B.Label lbl_start
        emit $ B.Enter

        compileBlock' block

        reg_cond <- compileExp expr
        reg_tmp1 <- fresh
        emit $ B.Not reg_tmp1 reg_cond
        emit $ B.SetLit local_name reg_tmp1
        emit $ B.Leave
        
        reg_tmp2 <- fresh
        emit $ B.GetLit reg_tmp2 local_name
        emit $ B.JumpT lbl_start reg_tmp2
        

    While cond block -> do
        lbl_start <- fresh
        lbl_end <- fresh

        emit $ B.Label lbl_start
        c <- compileExp cond
        c' <- fresh
        emit $ B.Not c' c
        emit $ B.JumpT lbl_end c'

        compileBlock block


        emit $ B.Jump lbl_start
        emit $ B.Label lbl_end

    -- TODO: this will need refactoring
    ForIn [(Name n)] [e1] block -> do
        reg_iterator <- compileExp e1
        reg_nil <- fresh
        lbl_start <- fresh
        lbl_end <- fresh

        emit $ B.LitNil reg_nil

        -- initial value of n
        reg_tmp1 <- fresh
        reg_params <- fresh
        emit $ B.Table reg_params
        emit $ B.Call reg_tmp1 reg_iterator reg_params
        emit $ B.SetLit n reg_tmp1


        emit $ B.Label lbl_start
        -- compare with nil
        reg_tmp2 <- fresh
        reg_nilCheck <- fresh
        emit $ B.GetLit reg_tmp2 n
        emit $ B.EQ reg_nilCheck reg_nil reg_tmp2
        emit $ B.JumpT lbl_end reg_nilCheck

        compileBlock block
        emit $ B.Jump lbl_start
        emit $ B.Label lbl_end
        

    ForRange (Name var) start end step block -> do
        lbl_start <- fresh
        lbl_end <- fresh

        zero <- fresh
        emit $ B.LitInt zero "0"

        reg_start <- compileExp start
        emit $ B.Local var
        emit $ B.SetLit var reg_start

        reg_end <- compileExp end

        reg_step <- case step of
            Nothing -> do
                one <- fresh
                emit $ B.LitInt one "1"
                pure one
            Just e -> compileExp e

        -- initial compare to see if we have to skip the loop alltogether
        -- adapted from lvm.c:forlimit
        -- original `step > 0 ? start > end : start < end`
        -- becomes `(step < 0) != (start >= end)
        x <- fresh
        y <- fresh
        skip_loop <- fresh
        emit $ B.LT x reg_step zero
        emit $ B.GTE y reg_start reg_end
        emit $ B.NEQ skip_loop x y
        emit $ B.JumpT lbl_end skip_loop


        -- loop body
        emit $ B.Label lbl_start
        compileBlock block
    

        -- equality check
        reg_var <- fresh
        reg_tmp1 <- fresh
        emit $ B.GetLit reg_var var
        emit $ B.EQ reg_tmp1 reg_var reg_end
        emit $ B.JumpT lbl_end reg_tmp1

        -- var += step
        reg_tmp2 <- fresh
        emit $ B.Add reg_tmp2 reg_var reg_step
        emit $ B.SetLit var reg_tmp2

        emit $ B.Jump lbl_start
        emit $ B.Label lbl_end

    LocalFunAssign (Name name) funBody -> do
        x <- fresh
        let internalName = Text.pack $ "$_" <> Text.unpack name <> show x
        compileFunBody internalName funBody
        emit $ B.Local name
        emit $ B.Lambda x internalName
        emit $ B.SetLit name x

    FunAssign (FunName (Name fnname) [] Nothing) funBody -> do
        let internalName = "$_" <> fnname
        x <- fresh
        compileFunBody internalName funBody
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

        let internalName' = "$_" <> internalName <> "." <> fnname

        x <- fresh
        compileFunBody internalName' funBody
        emit $ B.Lambda x internalName'
        l1 <- fresh
        emit $ B.LitString l1 fnname
        emit $ B.SetTable u l1 x


    FunAssign (FunName (Name fnname) [] (Just (Name obj))) (FunBody args isVararg body) -> do
        -- function x:f() end --> function x.f(self) end
        let internalName = "$_" <> fnname <> "_" <> obj
            funBody = FunBody (Name "self":args) isVararg body
        x <- fresh
        compileFunBody internalName funBody
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

        let internalName' = "$_" <> internalName <> ":" <> fnname
        let funBody = FunBody (Name "self":args) isVararg body
        x <- fresh
        compileFunBody internalName' funBody
        emit $ B.Lambda x internalName'

        l1 <- fresh
        emit $ B.LitString l1 fnname
        emit $ B.SetTable u l1 x


    If ifs elseBlock -> do
        lbl_end <- fresh

        mapM_ (compileIfBlock lbl_end) ifs
        
        traverse (compileElseBlock lbl_end) elseBlock
        emit $ B.Label lbl_end
        
    x -> error $ show x
  where
    compileIfBlock lbl_end (cond, block) = do
        lbl_not <- fresh
        not_cond <- fresh
        c <- compileExp cond
        emit $ B.Not not_cond c
        emit $ B.JumpT lbl_not not_cond
        compileBlock block
        emit $ B.Jump lbl_end
        emit $ B.Label lbl_not

    compileElseBlock lbl_end body = do
        compileBlock body
        emit $ B.Jump lbl_end

compileFunCallArgs' reg_params args cnt = go cnt args
  where
    go cnt [] = pure ()
    go cnt [PrefixExp (PEFunCall fc)] = do
        reg_res <- compileFunCall fc
        emit $ B.Append cnt reg_params reg_res
    go cnt (x:xs) = do
        reg_res <- compileExp x
        reg_idx <- fresh
        emit $ B.LitInt reg_idx $ Text.pack $ show cnt
        emit $ B.SetTable reg_params reg_idx reg_res
        go (succ cnt) xs

compileFunCallArgs reg_params args = compileFunCallArgs' reg_params args 1

compileFunCall = \case
    NormalFunCall fn (Args args) -> do
        ret <- fresh
        fn <- compilePrefixExp fn
        reg_params <- fresh
        emit $ B.Table reg_params
        compileFunCallArgs reg_params args
        emit $ B.Call ret fn reg_params
        pure ret

    MethodCall prefixExp (Name name) (Args args) -> do
        reg_ret <- fresh
        reg_obj <- compilePrefixExp prefixExp

        reg_name <- fresh
        emit $ B.LitString reg_name name

        reg_fn <- fresh
        emit $ B.GetTable reg_fn reg_obj reg_name

        reg_params <- fresh
        emit $ B.Table reg_params

        reg_one <- fresh
        emit $ B.LitInt reg_one "1"

        emit $ B.SetTable reg_params reg_one reg_obj
        compileFunCallArgs' reg_params args 2

        emit $ B.Call reg_ret reg_fn reg_params
        pure reg_ret


compileFunArg = \case
   Args args -> mapM compileExp args


compilePrefixExp = \case
    PEVar var -> compileVar var
    PEFunCall funcall -> do
        reg_res <- compileFunCall funcall
        reg_one <- fresh
        reg_ret <- fresh
        emit $ B.LitInt reg_one "1"
        emit $ B.GetTable reg_ret reg_res reg_one
        pure reg_ret
    Paren e -> compileExp e

toByteCodeBinop = \case
    GTE -> B.GTE
    EQ -> B.EQ
    GT -> B.GT
    LT -> B.LT
    LTE -> B.LTE
    Sub -> B.Sub
    Mul -> B.Mul
    Add -> B.Add

toByteCodeUnop = \case
    Neg -> B.Neg
    Not -> B.Not
    Len -> B.Len
    Complement -> B.Complement

compileFunBody internalName (FunBody args isVararg body) = do
    (_, asm) <- local $ do
        emit $ B.Fun internalName
        forM_ (zip [1..] args) $ \(param, Name arg) -> do
            emit $ B.Local arg
            emit $ B.SetLit arg param
        compileFn body
    addFunction internalName asm

compileExp = \case
    EFunDef funBody -> do
        x <- fresh
        let internalName = "$_lambda" <> Text.pack (show x)
        compileFunBody internalName funBody
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
    Unop op e -> do
        reg <- fresh
        x <- compileExp e
        emit $ toByteCodeUnop op reg x
        pure reg
    Binop binop a b -> do
        reg <- fresh
        a' <- compileExp a
        b' <- compileExp b
        emit $ (toByteCodeBinop binop) reg a' b'
        pure reg
    PrefixExp e -> compilePrefixExp e

    TableConst tableArgs -> do
        tbl <- fresh
        emit $ B.Table tbl
        compileTableConst tbl tableArgs
        pure tbl

    x -> error $ show x

compileTableConst reg_tbl = go 1
  where
    go cnt [] = pure ()
    go cnt [Field (PrefixExp (PEFunCall fc)) ] = do
        reg_res <- compileFunCall fc
        emit $ B.Append cnt reg_tbl reg_res

    go cnt (x:xs) =
      case x of
        ExpField idx val -> do
            reg_idx <- compileExp idx
            reg_val <- compileExp val
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go cnt xs
        NamedField (Name idx) val -> do
            reg_idx <- fresh
            emit $ B.LitString reg_idx idx
            reg_val <- compileExp val
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go cnt xs
        Field exp -> do
            reg_idx <- fresh
            emit $ B.LitInt reg_idx $ Text.pack $ show cnt
            reg_val <- compileExp exp
            emit $ B.SetTable reg_tbl reg_idx reg_val
            go (succ cnt) xs

compileVar = \case
    VarName (Name name) -> do
        tmp <- fresh
        emit $ B.GetLit tmp name
        pure tmp
    Select prefixExp exp -> do
        x <- fresh
        table <- compilePrefixExp prefixExp
        index <- compileExp exp
        emit $ B.GetTable x table index
        pure x

    SelectName prefixExp (Name name) -> do
        x <- fresh
        table <- compilePrefixExp prefixExp
        index <- fresh
        emit $ B.LitString index name
        emit $ B.GetTable x table index
        pure x
        


main = do
    [args] <- getArgs
    Right ast <- parseFile args
    --hPrint stderr ast

    let asm = concatMap snd . Map.toList . runCompiler $ compileScript ast

    BL.putStr $ encode asm
