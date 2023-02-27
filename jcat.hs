{-# LANGUAGE GADTs #-}

import Jass.Ast
import qualified Jass.Printer as Jass
import qualified Jass.Parser as Jass

import Text.Megaparsec

import System.Environment
import System.IO
import Control.Monad

import Data.Maybe

import Data.ByteString.Builder

parseFromFile p file = runParser p file <$> readFile file

jcat :: [Ast v Programm] -> Ast v Programm
jcat = foldl concat (Programm [])
  where
    concat :: Ast v Programm -> Ast v Programm -> Ast v Programm
    concat (Programm a) (Programm b) = Programm $ a <> b

main = do
    args <- getArgs
    asts <- forM args $ \arg -> do
        x <- parseFromFile Jass.programm arg
        case x of
            Left err -> putStr (errorBundlePretty err) >> pure Nothing
            Right ast -> pure $ Just ast
    let ast = jcat $ catMaybes asts
    
    hPutBuilder stdout $ Jass.pretty ast
    
    
