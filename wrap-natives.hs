{-# LANGUAGE GADTs #-}

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.List

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

import Data.MonoidMap (MonoidMap)
import qualified Data.MonoidMap as MM

parseFromFile p file = runParser p file . (++"\n") <$> readFile file

getName :: Ast v Toplevel -> v
getName (Native _ name _ _) = name

isNative :: Ast v Toplevel -> Bool
isNative Native{} = True
isNative _ = False

isTypedef :: Ast v Toplevel -> Bool
isTypedef Typedef{} = True
isTypedef _ = False

basicConverters :: Map String ([Ast String Expr] -> Ast String Expr)
basicConverters = Map.map Call $ Map.fromList
    [ ("integer", "Value#_2int"), ("real", "Value#_2real")
    , ("boolean", "Value#_2boolean"), ("string", "Value#_2string")
    ]

varOfType :: String -> String
varOfType ty = "_value2" <> ty

extractJassValue :: Int -> (String, String) -> Ast String Expr
extractJassValue idx (ty, _) =
    let var = Var $ SVar $ "arg" <> show idx
        --globalName = Map.findWithDefault (varOfType ty) ty basicTypes
        functionName = Map.findWithDefault ( convert2 ty) ty basicConverters
    in functionName [ var, Var $ SVar "interpreter" ]
    --in Var $ AVar globalName var

convert2 :: String -> [ Ast String Expr] -> Ast String Expr
convert2 ty = Call $ "_convert2" <> ty


basicSetter :: Map String (Ast String Expr -> Ast String x)
basicSetter = Map.fromList
    [ ("integer", Call "Value#_litint" . pure )
    , ("real", Call "Value#_litfloat" . pure )
    , ("string", Call "Value#_litstring" . pure )
    , ("boolean", Call "Value#_litbool" . pure )
    ]


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
            [ Set (SVar "v") $ Call "Value#_foreign" [ Var $ SVar $ "Jass#_" <> ret ]
            , Set (AVar (varOfType ret) (Var $ SVar "v")) call
            --, Set (AVar "Value#_Int" (Var $ SVar "v")) ()
            --, Call "Table#_set" [ Var $ AVar "Value#_Int" (Var $ SVar "v"), Int "'type'", Var $ SVar $ "Jass#_" <> ret ]
            , Call "Table#_set" [ Var $ AVar "Value#_Int" (Var $ SVar "r"), Int "1", Var $ SVar "v" ]
            ]

return_table :: Ast String Stmt
return_table =
    let init = Just $ Call "Table#_get" [ Var $ SVar "tbl", Int "0" ]
    in Local $ SDef Normal "r" "integer" init

local_value :: Ast String Stmt
local_value = Local $ SDef Normal "v" "integer" Nothing

toLocal :: Int -> w -> Ast String Stmt
toLocal idx _ =
    let tbl_call = Call "Table#_get" [ tbl_arg, idx_arg ]
        tbl_arg = Var $ SVar "tbl"
        idx_arg = Int $ show idx
    in Local $ SDef Normal ("arg"<>show idx) "integer" $ Just tbl_call 


mkGlobal :: String -> Ast String Toplevel
mkGlobal ty = Global $ ADef (varOfType ty) ty

getBothTypes :: Ast x Toplevel -> Set String
getBothTypes (Typedef a b) = Set.fromList [a, b]
getBothTypes _ = mempty

mkConstant :: Int -> String -> Ast String Toplevel
mkConstant idx ty =
    Global $ SDef Const ("_" <> ty) "integer" (Just $ Int $ show idx)

parent2children :: [ Ast x Toplevel ] -> MonoidMap String (Set String)
parent2children = foldl' go mempty
  where
    go :: MonoidMap String (Set String) -> Ast x Toplevel -> MonoidMap String (Set String)
    go m (Typedef a b) = m <> MM.singleton b (Set.singleton a)
    go m _ = m

getAllChildren :: String -> MonoidMap String (Set String) -> [ String ]
getAllChildren base m = do
    c <- Set.toList $ MM.lookup' base m
    c:getAllChildren c m



mkConvert :: MonoidMap String (Set String) -> String -> Ast String Toplevel
mkConvert types "boolexpr" =
    Function Normal "_convert2boolexpr" [("integer", "v"), ("integer", "interpreter")] "boolexpr"
        [ Set (SVar "Builtin/Boolexpr#_v") $ Var $ SVar "v"
        , Set (SVar "Builtin/Boolexpr#_i") $ Var $ SVar "interpreter"
        , Set (SVar "Builtin/Boolexpr#_r") $ Call "Value#_table" []
        , Return . Just . Var $ SVar "Builtin/Boolexpr#_filter"
        ]
mkConvert types ty =
    Function Normal ("_convert2" <> ty) [("integer", "v"), ("integer", "interpreter")] ty body
  where
    body = locals ++ mkCheckAndReturn ty
    locals =
        [ Local $ SDef Normal "lua_type" "integer" $ Just $ Var $ AVar "Value#_Type" v
        , Local $ SDef Normal "jass_type" "integer" $ Just $ Call "Value#_getJassType" [ v ]
        ]

    v = Var $ SVar "v"
    jass_type = Var $ SVar "jass_type"

    mkCheckAndReturn target_type = foldl' go errorCase $ target_type:getAllChildren target_type types
      where

      go :: [ Ast String Stmt ] -> String -> [ Ast String Stmt ]
      go elseBlock ty = pure $
        If cond
            [ Return $ Just $ Var $ AVar ( "_value2" <> ty ) v ]
            [] $
            Just elseBlock
        where
          cond = Call "==" [ jass_type, Var $ SVar $ "Jass#_" <> ty ]

      errorCase :: [ Ast String Stmt ]
      errorCase = [ Call "Print#_error" [String $ "Value not of type " <> ty ], Return $ Just Null ]

mkDispatch :: [String] -> [String] -> Ast String Toplevel
mkDispatch autogenerated custom =
    let autogenerated' = map ("Natives#_" <>) autogenerated
        calls = map mkCall $ autogenerated' <> custom
        switch = binsearch "fn" calls
    in Function Normal "_dispatch" [("integer", "fn"), ("integer", "tbl"), ("integer", "ctx"), ("integer", "interpreter")] "nothing"
        [ switch ]
  where
    mkCall name = Call name [ Var $ SVar "tbl", Var $ SVar "ctx", Var $ SVar "interpreter" ]

mkRegister :: [String] -> Ast String Toplevel
mkRegister names =
    Function Normal "_register" [("integer", "ctx")] "nothing"
        body
  where
    body = zipWith mkStmt [1..] names
    mkStmt idx name =
        Call "Context#_set" [ Var $ SVar "ctx", String name,
            Call "Value#_builtin" [ Int $ show idx, String name ] ]

compile :: Ast String Programm -> IO ()
compile (Programm ts) = do
    let natives = filter ((`Set.notMember` skip) . getName) $ filter isNative ts
        allTypes = Set.unions $ map getBothTypes ts
        allTypes' = allTypes <> Set.fromList [ "integer", "real", "string", "boolean" ]
        wrappers = map mkWrapper natives
        globals = map mkGlobal $ Set.toList allTypes

        skip = Set.fromList $ map fst extraFunctions

        
        extraFunctions =
            [ ("co_create", "Builtin/Coroutine#_create")
            , ("co_yield", "Builtin/Coroutine#_yield")
            , ("co_resume", "Builtin/Coroutine#_resume")

            , ("setmetatable", "Builtins#_setmetatable")
            , ("getmetatable", "Builtins#_getmetatable")
            , ("print", "Builtins#_print")
            , ("FourCC", "Builtins#_FourCC")
            , ("collectgarbage", "Builtins#_collectgarbage")
	    , ("rawset", "Builtins#_rawset")
	    , ("rawget", "Builtins#_rawget")

            , ("CreateTimer", "Builtin/Timer#_CreateTimer")
            , ("DestroyTimer", "Builtin/Timer#_DestroyTimer")
            , ("TimerStart", "Builtin/Timer#_TimerStart")
            , ("GetExpiredTimer", "Builtin/Timer#_GetExpiredTimer")

            , ("CreateTrigger", "Builtin/Trigger#_CreateTrigger")
            , ("TriggerAddAction", "Builtin/Trigger#_TriggerAddAction")
            
            , ("EnumDestructablesInRect", "Builtin/Boolexpr#_EnumDestructablesInRect")
            , ("EnumItemsInRect", "Builtin/Boolexpr#_EnumItemsInRect")
            , ("Filter", "Builtin/Boolexpr#_Filter")
            , ("Condition", "Builtin/Boolexpr#_Condition")
            , ("And", "Builtin/Boolexpr#_And")
            , ("Or", "Builtin/Boolexpr#_Or")
            , ("Not", "Builtin/Boolexpr#_Not")

            , ("ForForce", "Builtins#_ForForce")
            , ("ForGroup", "Builtins#_ForGroup")

            -- TODO:
            --  - DestroyBoolexpr, TriggerAddCondition, TriggerRegisterEnterRegion
            --  - TriggerRegisterLeaveRegion, TriggerRegisterPlayerUnitEvent,
            --  - TriggerRegisterFilterUnitEvent, TriggerRegisterUnitInRange,
            --  - TriggerEvaluate, SaveBooleanExprHandle, GetTriggeringTrigger,
            -- - LoadTriggerHandle, LoadTimerHandle, DestroyTrigger
            ]

        converters = map (mkConvert $ parent2children ts) $ Set.toList allTypes

        dispatch = mkDispatch (map getName natives) (map snd extraFunctions) 
        register = mkRegister $ map getName natives ++ map fst extraFunctions

        nativeAst = Programm $ globals ++ converters ++ wrappers
        jassAst = Programm $ zipWith mkConstant [1..] $ Set.toList allTypes'


        dispatchAst = Programm [  dispatch, register ]

    withFile "auto/Dispatch.j" WriteMode $ \fh -> do
        hPutStrLn fh "// scope Dispatch"
        hPutStrLn fh "// REQUIRES Builtins"
        hPutStrLn fh "// REQUIRES Builtin/Coroutine Builtin/Trigger Builtin/Timer Builtin/Boolexpr"
        hPutStrLn fh "// REQUIRES Value Context Natives"
        Builder.hPutBuilder fh $ pretty dispatchAst

    withFile "auto/Jass.j" WriteMode $ \fh -> do
        hPutStrLn fh "// scope Jass"
        Builder.hPutBuilder fh $ pretty jassAst
        

    withFile "auto/Natives.j" WriteMode $ \fh -> do
        hPutStrLn fh "// scope Natives"
        hPutStrLn fh "// REQUIRES Table Value Jass"
        Builder.hPutBuilder fh $ pretty nativeAst

binsearch :: String -> [ Ast String Stmt ] -> Ast String Stmt
binsearch n = go . zip [1..]
  where
    go :: [ (Int, Ast String Stmt) ] -> Ast String Stmt
    go [(_, stmt)] = stmt
    go xs =
        let (lhs, rhs@((idx,_):_)) = splitAt (length xs `div` 2) xs
        in If (Call "<" [ v, Int $ show idx])
            [ go lhs ]
            []
            (Just [ go rhs ] )

    v = Var $ SVar n

main :: IO ()
main = do
    [commonj] <- getArgs
    eAst <- parseFromFile programm commonj 
    case eAst of
        Left err -> putStrLn $ errorBundlePretty err
        Right ast -> void $ compile ast
