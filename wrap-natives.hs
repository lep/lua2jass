{-# LANGUAGE GADTs #-}

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe

import Control.Arrow ((&&&))
import Control.Monad
import Control.Exception

import System.Environment
import System.FilePath
import System.IO

import Text.Megaparsec

import Jass.Ast hiding (fmap)
import Jass.Parser
import Jass.Printer

import qualified Data.ByteString.Builder as Builder

parseFromFile p file = runParser p file . (++"\n") <$> readFile file

getName :: Ast v Toplevel -> v
getName (Native _ name _ _) = name

isNative :: Ast v Toplevel -> Bool
isNative Native{} = True
isNative _ = False

isTypedef :: Ast v Toplevel -> Bool
isTypedef Typedef{} = True
isTypedef _ = False

category :: Ast String Toplevel -> String
category (Native _ _ args ret) =
  case args of
    (ty, _):_ -> ty
    [] -> ret

basicTypes = Map.fromList [ ("integer", "Value#_Int"), ("real", "Value#_Real"), ("boolean", "Value#_Bool"), ("string", "Value#_String") ]
varOfType ty = "_value2" <> ty
extractJassValue :: Int -> (String, String) -> Ast String Expr
extractJassValue idx (ty, _) =
    let var = Var $ SVar $ "arg" <> show idx
        globalName = Map.findWithDefault (varOfType ty) ty basicTypes
    in Var $ AVar globalName var


basicSetter = Map.fromList
    [ ("integer", Call "Value#_litint" . pure )
    , ("real", Call "Value#_litfloat" . pure )
    , ("string", Call "Value#_litstring" . pure )
    , ("boolean", Call "Value#_litbool" . pure )
    ]

genericSetter _ = Call "Value#_table" []


-- TODO: cleanup
-- TODO: upcasting (numbers/handles)
mkWrapper :: Ast String Toplevel -> Ast String Toplevel
mkWrapper (Native _ name args ret) =
    Function Normal name' args' "nothing" $
        local_value:return_table:argLocals ++ call'
  where
    name' = '_':name
    args' = [("integer", "tbl"), ("integer", "ctx"), ("integer", "interpreter")]
    argLocals = zipWith toLocal [1..] args

    call :: Ast String x
    call = Call name $ zipWith extractJassValue [1..] args 

    call' =
      case ret of
        "nothing" -> [ call ]
        ty | Just setter <- Map.lookup ty basicSetter ->
            [ Call "Table#_set" [ Var $ AVar "Value#_Int" (Var $ SVar "r"), Int "1", setter call] ]
        _ ->
            [ Set (SVar "v") $ Call "Value#_table" []
            , Set (AVar (varOfType ret) (Var $ SVar "v")) call
            , Call "Table#_set" [ Var $ SVar "v", Int "'type'", Var $ SVar $ "Jass#_" <> ret ]
            , Call "Table#_set" [ Var $ AVar "Value#_Int" (Var $ SVar "r"), Int "1", Var $ SVar "v" ]
            ]

return_table =
    let init = Just $ Call "Table#_get" [ Var $ SVar "tbl", Int "0" ]
    in Local $ SDef Normal "r" "integer" init
local_value = Local $ SDef Normal "v" "integer" Nothing

toLocal :: Int -> (String, String) -> Ast String Stmt
toLocal idx (ty, _) =
    let tbl_call = Call "Table#_get" [ tbl_arg, idx_arg ]
        tbl_arg = Var $ SVar "tbl"
        idx_arg = Int $ show idx
    in Local $ SDef Normal ("arg"<>show idx) "integer" $ Just tbl_call 

mkFile ty natives =
    let globals = mkGlobals ty
        wrapper = map mkWrapper natives
    in Programm $ globals ++ wrapper

mkGlobals ty  =
  case ty of
    "nothing" -> []
    _ | Just _ <- Map.lookup ty basicSetter -> []
    _ -> [ Global $ ADef (varOfType ty) ty]

--mkGlobal :: Ast String Toplevel -> Ast String Toplevel
--mkGlobal (Typedef new _) = Global $ ADef (varOfType new) new

mkGlobal :: String -> Ast String Toplevel
mkGlobal ty = Global $ ADef (varOfType ty) ty

getBothTypes :: Ast x Toplevel -> Set String
getBothTypes (Typedef a b) = Set.fromList [a, b]
getBothTypes _ = mempty

mkConstant :: Int -> String -> Ast String Toplevel
mkConstant idx ty =
    Global $ SDef Const ("_" <> ty) "integer" (Just $ Int $ show idx)

compile :: Set String -> Ast String Programm -> IO ()
compile skip (Programm ts) = do
    let natives = filter ((`Set.notMember` skip) . getName) $ filter isNative ts
        allTypes = Set.unions $ map getBothTypes ts
        allTypes' = allTypes <> Set.fromList [ "integer", "real", "string", "boolean" ]
        wrappers = map mkWrapper natives
        globals = map mkGlobal $ Set.toList allTypes

        --prog = Programm $ globals ++ wrappers
        nativeAst = Programm $ globals ++ wrappers
        jassAst = Programm $ zipWith mkConstant [1..] $ Set.toList allTypes'

    bracket (openFile "auto/Jass.j" WriteMode) (hClose) $ \fh -> do
        hPutStrLn fh $ "// scope Jass"
        Builder.hPutBuilder fh $ pretty jassAst
        

    bracket (openFile "auto/Natives.j" WriteMode) (hClose) $ \fh -> do
        hPutStrLn fh $ "// scope Natives"
        hPutStrLn fh "// REQUIRES Table Value Jass"
        Builder.hPutBuilder fh $ pretty nativeAst


        --byCategory = Map.fromListWith (++) $ map (category &&& pure) natives
    --forM_ ( Map.toList byCategory) $ \(ty, natives) -> do
    --    let ast = mkFile ty natives
    --        fileName = "auto" </> "natives.j"
    --        --fileName = "auto" </> "Builtin" </> ty <> ".j"
    --    putStrLn fileName
    --    bracket (openFile fileName WriteMode) (hClose) $ \fh -> do
    --        hPutStrLn fh $ "// scope " <> "Builtin" </> ty
    --        hPutStrLn fh "// REQUIRES Table Value Jass"
    --        Builder.hPutBuilder fh $ pretty ast
    --    --Builder.writeFile fileName $ pretty ast


main = do
    [commonj, dont] <- getArgs
    dontCompile <- Set.fromList . lines <$> readFile dont
    eAst <- parseFromFile programm commonj 
    case eAst of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> compile dontCompile ast
