{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Jass.Parser as Jass
import qualified Jass.Ast as Jass
import qualified Jass.Printer as Jass

import Data.FileEmbed

import qualified Compiler.Preprocess as CPre
import qualified Compiler.Natives as CNative

import qualified Data.ByteString.Builder as Builder

import System.Environment (getArgs)
import System.IO (stdout)
import Control.Monad (void)
import Text.Megaparsec (errorBundlePretty)

import Data.Either (fromRight)
import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Text.Megaparsec (runParser, errorBundlePretty )

tsort :: forall x. Ord x => [(x, Set x)] -> [x]
tsort r = reverse . fst . foldl' (visit Set.empty) ([], Set.empty) $ map fst r
  where
    m = Map.fromList r

    visit :: Set x -> ([x], Set x) -> x -> ([x], Set x)
    visit temp acc@(_, g) x
        | x `Set.member` g = acc
        | x `Set.member` temp = error "tsort"
    visit temp acc x =
        let temp' = Set.insert x temp
            deps = Set.toList $ Map.findWithDefault Set.empty x m
            (xs, g) = foldl' (visit temp') acc deps
        in (x:xs, Set.insert x g)

{-
1. process embedded files (include alloc & prefix stuff)
2. process common.j
    1. generate ASTs
    2. process these ASTs
3. process input.lua
4. tsort
5. print
-}
extraFunctions = Map.fromList 
    [ ("$coroutine.create", "Builtin::Coroutine#_create")
    , ("$coroutine.yield", "Builtin::Coroutine#_yield")
    , ("$coroutine.resume", "Builtin::Coroutine#_resume")
    , ("$coroutine.status", "Builtin::Coroutine#_status_fn")
    , ("$coroutine.isyieldable", "Builtin::Coroutine#_isyieldable")

    , ("setmetatable", "Builtins#_setmetatable")
    , ("getmetatable", "Builtins#_getmetatable")
    , ("print", "Builtins#_print")
    , ("FourCC", "Builtins#_FourCC")
    , ("collectgarbage", "Builtins#_collectgarbage")
    , ("rawset", "Builtins#_rawset")
    , ("rawget", "Builtins#_rawget")
    , ("pcall", "Builtins#_pcall")
    , ("xpcall", "Builtins#_xpcall")
    , ("error", "Builtins#_error")
    , ("type", "Builtins#_type")
    , ("select", "Builtins#_select")
    , ("rawequal", "Builtins#_rawequal")
    , ("ipairs", "Builtins#_ipairs")
    , ("$ipairs_next", "Builtins#_ipairs_next")
    , ("next", "Builtins#_next")
    , ("pairs", "Builtins#_pairs")
    , ("tostring", "Builtins#_tostring")

    , ("$enable_trace", "Builtins#_enable_trace")
    , ("$disable_trace", "Builtins#_disable_trace")

    , ("$math.random", "Builtin::Math#_random")
    , ("$math.randomseed", "Builtin::Math#_randomseed")
    , ("$math.fmod", "Builtin::Math#_fmod")
    , ("$math.modf", "Builtin::Math#_modf")
    , ("$math.atan", "Builtin::Math#_atan")
    , ("$math.floor", "Builtin::Math#_floor")
    , ("$math.ceil", "Builtin::Math#_ceil")
    , ("$math.abs", "Builtin::Math#_abs")
    , ("$math.min", "Builtin::Math#_min")
    , ("$math.max", "Builtin::Math#_max")
    , ("$math.log", "Builtin::Math#_log")

    , ("$table.sort", "Builtin::Table#_sort")
    , ("$table.concat", "Builtin::Table#_concat")
    , ("$table.insert", "Builtin::Table#_insert")
    , ("$table.move", "Builtin::Table#_move")
    , ("$table.remove", "Builtin::Table#_remove")
    , ("$table.pack", "Builtin::Table#_pack")
    , ("$table.unpack", "Builtin::Table#_unpack")

    , ("CreateTimer", "Builtin::Timer#_CreateTimer")
    , ("DestroyTimer", "Builtin::Timer#_DestroyTimer")
    , ("TimerStart", "Builtin::Timer#_TimerStart")
    , ("GetExpiredTimer", "Builtin::Timer#_GetExpiredTimer")

    , ("CreateTrigger", "Builtin::Trigger#_CreateTrigger")
    , ("TriggerAddAction", "Builtin::Trigger#_TriggerAddAction")
    
    , ("EnumDestructablesInRect", "Builtin::Boolexpr#_EnumDestructablesInRect")
    , ("EnumItemsInRect", "Builtin::Boolexpr#_EnumItemsInRect")
    , ("Filter", "Builtin::Boolexpr#_Filter")
    , ("Condition", "Builtin::Boolexpr#_Filter")
    , ("And", "Builtin::Boolexpr#_And")
    , ("Or", "Builtin::Boolexpr#_Or")
    , ("Not", "Builtin::Boolexpr#_Not")

    , ("ForForce", "Builtins#_ForForce")
    , ("ForGroup", "Builtins#_ForGroup")

    -- TODO:
    --  - DestroyBoolexpr, TriggerAddCondition, TriggerRegisterEnterRegion
    --  - TriggerRegisterLeaveRegion, TriggerRegisterPlayerUnitEvent,
    --  - TriggerRegisterFilterUnitEvent, TriggerRegisterUnitInRange,
    --  - TriggerEvaluate, SaveBooleanExprHandle, GetTriggeringTrigger,
    -- - LoadTriggerHandle, LoadTimerHandle, DestroyTrigger
    ]

runtime :: [String]
runtime =
  [ $(embedStringFile "runtime/Builtin/Boolexpr.j")
  , $(embedStringFile "runtime/Builtin/Coroutine.j")
  , $(embedStringFile "runtime/Builtin/Math.j")
  , $(embedStringFile "runtime/Builtin/Table.j")
  , $(embedStringFile "runtime/Builtin/Timer.j")
  , $(embedStringFile "runtime/Builtin/Trigger.j")
  , $(embedStringFile "runtime/Builtins.j")
  , $(embedStringFile "runtime/Call.j")
  , $(embedStringFile "runtime/Context.j")
  , $(embedStringFile "runtime/Deque.j")
  , $(embedStringFile "runtime/GC.j")
  , $(embedStringFile "runtime/Helper.j")
  , $(embedStringFile "runtime/Ins.j")
  , $(embedStringFile "runtime/Interpreter.j")
  , $(embedStringFile "runtime/List.j")
  , $(embedStringFile "runtime/Print.j")
  , $(embedStringFile "runtime/StringTable.j")
  , $(embedStringFile "runtime/Table.j")
  , $(embedStringFile "runtime/Types.j")
  , $(embedStringFile "runtime/Value.j")
  , $(embedStringFile "runtime/Wrap.j")
  ] -- todo "scaffold"

processed = map justProcess runtime

justProcess = fromRight (error "justProcess") . CPre.process "lua_"


parseFromFile p file = runParser p file . (++"\n") <$> readFile file

handleCommonJ path = do
    r <- parseFromFile Jass.programm path
    case r of
        Left err -> pure . Left $ errorBundlePretty err
        Right ast ->
            let asts = CNative.compile extraFunctions ast
                asts' = map (\((scope, deps), ast) -> ((scope, deps), CPre.rename "lua_" scope ast)) asts
            in pure $ Right asts'

mergeScripts xs =
    let m = Map.fromList $ map (\((scope, _), ast) -> (scope, ast)) xs
        sorted = tsort $ map (\((scope, deps), _) -> (scope, deps)) xs
    in foldl' (\ast k -> Jass.concat ast $ Map.findWithDefault (error k) k m) (Jass.Programm []) sorted



main = do
    [mode, commonj] <- getArgs
    case mode of
        "just-preprocess" -> do
            Right ((scope, deps), j) <- CPre.process "lua_" <$> readFile commonj
            Builder.hPutBuilder stdout $ Jass.pretty j
        _ -> do
            stuff <- handleCommonJ commonj
            case stuff of
                Left err -> putStrLn err
                Right xs -> do
                    let j = mergeScripts $ xs ++ processed
                    Builder.hPutBuilder stdout $ Jass.pretty j


