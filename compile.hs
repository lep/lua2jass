{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Debug.Trace

import Prelude hiding (head, LT, GT)

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

    
compileFn :: Block -> CompileM ()
compileFn (Block stmts ret) = do
    emit $ B.Local "$_ret"
    ret' <- fresh
    emit $ B.Table ret'
    emit $ B.SetLit "$_ret" ret'
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just [e] -> do -- TODO: multiple returns
            x <- compileExp e
            zero <- fresh
            emit $ B.LitInt zero "0"
            --emit $ B.SetLit "$_ret" x
            emit $ B.SetTable ret' zero x
            emit B.Ret
    --emit B.Leave
    emit B.Ret

compileBlock :: Block -> CompileM ()
compileBlock (Block stmts ret) = do
    emit B.Enter
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just [e] -> do -- TODO: multiple returns
            x <- compileExp e

            ret' <- fresh
            emit $ B.GetLit ret' "$_ret"
            zero <- fresh
            emit $ B.LitInt zero "0"
            --emit $ B.SetLit "$_ret" x
            emit $ B.SetTable ret' zero x
            emit B.Ret

    emit B.Leave
    --emit B.Ret

compileStat :: Stat -> CompileM ()
compileStat = \case
    EmptyStat -> pure ()
    FunCall fc -> void $ compileFunCall fc
    Assign [VarName (Name v1)] [e1] -> do
        y <- compileExp e1
        emit $ B.SetLit v1 y
    Assign [Select prefixExp exp] [e1] -> do
        tbl <- compilePrefixExp prefixExp
        idx <- compileExp exp
        val <- compileExp e1
        emit $ B.SetTable tbl idx val
    Assign [SelectName tbl (Name name)] [e1] -> do
        tbl' <- compilePrefixExp tbl
        idx <- fresh
        emit $ B.LitString idx name
        e <- compileExp e1
        emit $ B.SetTable tbl' idx e
        

    LocalAssign [Name v1] (Just [e1]) -> do
        x <- compileExp e1
        emit $ B.Local v1
        emit $ B.SetLit v1 x
    --    error "TODO"
    Do block -> compileBlock block

    Repeat block expr -> do
        lbl_start <- fresh
        emit $ B.Label lbl_start
        compileBlock block
        c <- compileExp expr
        c' <- fresh
        emit $ B.Not c' c
        emit $ B.JumpT lbl_start c'
        

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

        -- initial compare to see if we have to skip the loop alltogehter
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


compileFunCall = \case
    NormalFunCall fn args -> do
        ret <- fresh
        fn <- compilePrefixExp fn
        args <- compileFunArg args
        forM_ (zip args [1..]) $ \(arg, idx) ->
            emit $ B.Bind idx arg
        emit $ B.Call ret fn
        pure ret

    MethodCall prefixExp (Name name) funarg -> do
        ret <- fresh
        obj <- compilePrefixExp prefixExp
        args <- compileFunArg funarg
        name' <- fresh
        emit $ B.LitString name' name
        fn <- fresh
        emit $ B.GetTable fn obj name'
        emit $ B.Bind 1 obj
        forM_ (zip args [2..]) $ \(arg, idx) ->
            emit $ B.Bind idx arg
        emit $ B.Call ret fn
        pure ret


compileFunArg = \case
   Args args -> mapM compileExp args


compilePrefixExp = \case
    PEVar var -> compileVar var
    PEFunCall funcall -> do
        ret <- compileFunCall funcall
        zero <- fresh
        emit $ B.LitInt zero "0"
        emit $ B.GetTable ret ret zero
        pure ret

toByteCodeBinop = \case
    GTE -> B.GTE
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
        foldM `flip` 1 `flip` tableArgs $ \cnt arg ->
          case arg of
            ExpField idx val -> do
                idx' <- compileExp idx
                val' <- compileExp val
                emit $ B.SetTable tbl idx' val'
                pure cnt
            NamedField (Name idx) val -> do
                idx' <- fresh
                emit $ B.LitString idx' idx
                val' <- compileExp val
                emit $ B.SetTable tbl idx' val'
                pure cnt
            Field exp -> do
                idx <- fresh
                emit $ B.LitInt idx $ Text.pack $ show cnt
                val' <- compileExp exp
                emit $ B.SetTable tbl idx val'
                pure $ succ cnt

            
        pure tbl

    x -> error $ show x

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
