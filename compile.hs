{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (head, LT, GT)

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
        compile block
    addFunction "$_main" asm

    
compileFn :: Block -> CompileM ()
compileFn (Block stmts ret) = do
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just [e] -> do -- TODO: multiple returns
            x <- compileExp e
            emit $ B.Set 0 x
            emit B.Ret
    emit B.Leave
    emit B.Ret

compile :: Block -> CompileM ()
compile (Block stmts ret) = do
    emit B.Enter
    mapM_ compileStat stmts
    case ret of
        Nothing -> pure ()
        Just [e] -> do -- TODO: multiple returns
            x <- compileExp e
            emit $ B.Set 0 x
            emit B.Ret
    emit B.Leave
    emit B.Ret

compileStat :: Stat -> CompileM ()
compileStat = \case
    EmptyStat -> pure ()
    FunCall fc -> void $ compileFunCall fc
    Assign [VarName (Name v1)] [e1] -> do
        y <- compileExp e1
        emit $ B.SetLit v1 y

    LocalAssign [Name v1] (Just [e1]) -> do
        x <- compileExp e1
        emit $ B.Local v1
        emit $ B.SetLit v1 x
    --    error "TODO"

    While cond block -> do
        lbl_start <- fresh
        lbl_end <- fresh

        emit $ B.Label lbl_start
        c <- compileExp cond
        c' <- fresh
        emit $ B.Not c' c
        emit $ B.JumpT lbl_end c'

        compile block


        emit $ B.Jump lbl_start
        emit $ B.Label lbl_end

    FunAssign (FunName (Name fnname) _ _) funBody@(FunBody args isVararg body) -> do
        let internalName = "$_" <> fnname
        x <- fresh
        compileFunBody internalName funBody
        emit $ B.Lambda x internalName
        emit $ B.SetLit fnname x


    If ifs elseBlock -> do
        lbl_end <- fresh
        --lbl_b4_else <- fresh
        --foldM (compileIfBlock lbl_end) lbl_b4_else $ reverse ifs

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
        compile block
        emit $ B.Jump lbl_end
        emit $ B.Label lbl_not

    compileElseBlock lbl_end body = do
        compile body
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

compileFunArg = \case
   Args args -> mapM compileExp args


compilePrefixExp = \case
    PEVar var -> compileVar var
    PEFunCall funcall -> compileFunCall funcall

toByteCodeBino = \case
    GTE -> B.GTE
    GT -> B.GT
    LT -> B.LT
    LTE -> B.LTE
    Sub -> B.Sub
    Mul -> B.Mul
    Add -> B.Add

compileFunBody internalName (FunBody args isVararg body) = do
    (_, asm) <- local $ do
        emit $ B.Fun internalName
        --emit $ B.Enter
        forM_ (zip [1..] args) $ \(param, Name arg) -> do
            emit $ B.Local arg
            emit $ B.SetLit arg param
        compileFn body
        --emit $ B.Leave
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
        emit $ B.LitString reg x
        pure reg
    Number IntNum x -> do
        reg <- fresh
        emit $ B.LitInt reg x
        pure reg
    Number FloatNum x -> do
        reg <- fresh
        emit $ B.LitFloat reg x
        pure reg
    Binop binop a b -> do
        reg <- fresh
        a' <- compileExp a
        b' <- compileExp b
        emit $ (toByteCodeBino binop) reg a' b'
        pure reg
    PrefixExp e -> compilePrefixExp e

    x -> error $ show x

compileVar = \case
    VarName (Name name) -> do
        tmp <- fresh
        emit $ B.GetLit tmp name
        pure tmp


main = do
    [args] <- getArgs
    Right ast <- parseFile args
    print ast

    putStrLn "["
    let m = runCompiler $ compileScript ast
    forM_ (Map.toList m) $ \(_, asm) -> do
        forM_ asm $ \x -> do
            putStr $ B.toPython x
            putStrLn ","
    putStrLn "]"


