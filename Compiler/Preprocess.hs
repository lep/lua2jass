{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiler.Preprocess where

import qualified Jass.Parser as Jass
import qualified Jass.Ast as Jass
import qualified Jass.Printer as Jass
import Data.Composeable

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char

import System.Environment
import System.IO

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad

import Data.FileEmbed

import Debug.Trace

type Parser = Parsec Void String

flattenModuleName = map (\case ':' -> '_'; x -> x)

lname2jname :: String -> String -> (String, Maybe String) -> String
lname2jname globalPrefix modulePrefix (nameOrModule, realName) =
    case realName of
        Nothing -> potentiallyAddPrefix nameOrModule
        Just name -> addPrefix (flattenModuleName nameOrModule) <> name

    where
        addPrefix = (globalPrefix <> )
        potentiallyAddPrefix = \case
            xs@('_':_) -> globalPrefix <> modulePrefix <> xs
            x -> x
            
pName :: Parser (String, Maybe String)
pName = do
    nameOrModule <- pModule
    realName <- optional $ char '#' *> ident
    pure (nameOrModule, realName)
  where
    ident = many $ alphaNumChar <|> char '_'

pModule = many (char ':' <|> char '_' <|> alphaNumChar )

pMetadata = do
    scope <- pScope
    requires <- pRequires
    pure (scope, requires)

pScope = string "// scope " *> pModule <* eol

pRequires = Set.fromList . concat <$> many pRequire
pRequire = do
    string "// REQUIRES "
    requirements <- pModule `sepBy` hspace1
    eol
    pure requirements

bla :: String -> String -> String -> String
bla globalPrefix modulePrefix name =
    let Right x = runParser pName name name
    in lname2jname globalPrefix modulePrefix x


rename :: String -> String -> Jass.Ast String x -> Jass.Ast String x
rename globalPrefix modulePrefix = rename' globalPrefix (flattenModuleName modulePrefix)

rename' :: String -> String -> Jass.Ast String x -> Jass.Ast String x
rename' globalPrefix modulePrefix x =
  case x of
    Jass.Function co name args ret body ->
        Jass.Function co (t name) args ret $ map c body
    Jass.Call name args
        | not $ Jass.isOp name -> Jass.Call (t name) $ map (rename' globalPrefix modulePrefix) args
    Jass.Code name -> Jass.Code $ t name
    Jass.AVar name idx -> Jass.AVar (t name) (c idx)
    Jass.SVar name -> Jass.SVar $ t name
    Jass.ADef name ty -> Jass.ADef (t name) ty
    Jass.SDef co name ty init -> Jass.SDef co (t name) ty $ fmap c init
    _ -> composeOp (rename' globalPrefix modulePrefix) x
  where
    t = bla globalPrefix modulePrefix

    c :: Jass.Ast String x -> Jass.Ast String x
    c = rename' globalPrefix modulePrefix

process :: String -> String -> Either (ParseErrorBundle String Void) ((String, Set String), Jass.Ast String Jass.Programm)
process globalPrefix prog = do
    ((scope, requirements), ast) <- runParser ((,) <$> pMetadata <*> Jass.programm) "" prog
    let ast' = rename globalPrefix scope ast
    let ast'' = addAlloc ast' scope requirements
    pure ((scope, Set.delete "Alloc" requirements), ast'')
  where
    addAlloc ast scope requirements
        | "Alloc" `Set.member` requirements = Jass.concat (name_global scope) $ Jass.concat (alloc globalPrefix scope) ast
        | otherwise = ast

    name_global scope = rename globalPrefix scope $ Jass.Programm [ Jass.Global $ Jass.SDef Jass.Const "_alloc_name" "string" $ Just $ Jass.String scope ]



allocFile :: String
allocFile = $(embedStringFile "runtime/Alloc.j")

alloc :: String -> String -> Jass.Ast String Jass.Programm
alloc globalPrefix modulePrefix =
    let Right ast = runParser Jass.programm "runtime/Alloc.j" allocFile
    in rename' globalPrefix modulePrefix ast
