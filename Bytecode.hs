{-# LANGUAGE LambdaCase #-}

module Bytecode where

import Data.Text
import Data.Text.Encoding
import Prelude hiding (LT,GT,EQ)

import Data.Aeson

import Language.Lua.StringLiteral
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

type Register = Int
type Label = Int
type Name = Text

data Bytecode =
      Call Register Register Register
    | Enter
    | Leave
    | GetLit Register Text
    | Bind Register Register
    | LitString Register Text
    | LitInt Register Text
    | LitFloat Register Text
    | LitBool Register Bool
    | LitNil Register
    | Set Register Register
    | SetLit Name Register
    | Table Register
    | Append Int Register Register
    | GetList Int Register Register
    | SetTable Register Register Register
    | GetTable Register Register Register
    | Ret
    | Label Label
    | Jump Label
    | JumpT Label Register
    | Not Register Register
    | Neg Register Register
    | Len Register Register
    | Complement Register Register
    | GTE Register Register Register
    | GT Register Register Register
    | LTE Register Register Register
    | LT Register Register Register
    | EQ Register Register Register
    | NEQ Register Register Register
    | Mul Register Register Register
    | Div Register Register Register
    | Sub Register Register Register
    | Add Register Register Register
    | Exp Register Register Register
    | Mod Register Register Register
    | Concat Register Register Register
    | IDiv Register Register Register
    | ShiftL Register Register Register
    | ShiftR Register Register Register
    | BAnd Register Register Register
    | BOr Register Register Register
    | BXor Register Register Register
    | Fun Text
    | Lambda Register Text
    | Local Text
    deriving (Show)

-- TODO: very unholy function
bla :: Text -> Text
bla s =
  case interpretStringLiteral $ unpack s of
    Nothing -> error . show $ ("could not interp", s)
    Just bs -> decodeUtf8 $ BL.toStrict bs

instance ToJSON Bytecode where
    toJSON = \case
        Fun fn -> toJSON ( "fun", fn)
        Call a b c -> toJSON ("call", a, b, c)
        Enter -> toJSON ["enter"]
        Leave -> toJSON ["leave"]
        GetLit r t -> toJSON ("getlit", r, t)
        Bind a b -> toJSON ("bind", a, b)
        LitString r t -> toJSON ("lit", r, t)
        LitInt r t -> toJSON ("lit", r, (readT t) :: Int)
        LitFloat r t -> toJSON ("lit", r, t)
        LitBool r t -> toJSON ("lit", r, t)
        LitNil r -> toJSON ("nil", r)
        Set a b -> toJSON ("set", a, b)
        SetLit a b -> toJSON ("setlit", a, b)
        Ret -> toJSON ["ret"]
        Label lbl -> toJSON ("lbl", lbl)
        Jump lbl -> toJSON ("jmp", lbl)
        JumpT lbl r -> toJSON ("jmpt", lbl, r)
        Not a b -> toJSON ("not", a, b)
        Neg a b -> toJSON ("neg", a, b)
        Len a b -> toJSON ("len", a, b)
        Complement a b -> toJSON ("complement", a, b)
        GTE a b c -> toJSON ("gte", a, b, c)
        GT a b c -> toJSON ("gt", a, b, c)
        LTE a b c -> toJSON ("lte", a, b, c)
        LT a b c -> toJSON ("lt", a, b, c)
        EQ a b c -> toJSON ("eq", a, b, c)
        NEQ a b c -> toJSON ("neq", a, b, c)
        Mul a b c -> toJSON ("mul", a, b, c)
        Div a b c -> toJSON ("div", a, b, c)
        Exp a b c -> toJSON ("exp", a, b, c)
        Mod a b c -> toJSON ("mod", a, b, c)
        Sub a b c -> toJSON ("sub", a, b, c)
        Add a b c -> toJSON ("add", a, b, c)
        IDiv a b c -> toJSON ("idiv", a, b, c)
        ShiftL a b c -> toJSON ("shiftl", a, b, c)
        ShiftR a b c -> toJSON ("shiftr", a, b, c)
        BAnd a b c -> toJSON ("band", a, b, c)
        BOr a b c -> toJSON ("bor", a, b, c)
        BXor a b c -> toJSON ("bxor", a, b, c)
        Concat a b c -> toJSON ("concat", a, b, c)
        Lambda r n -> toJSON ("lambda", r, n)
        Local n -> toJSON ("local", n)
        Table n -> toJSON ("table", n)
        Append idx a b -> toJSON ("append", idx, a, b)
        GetList idx a b -> toJSON ("getlist", idx, a, b)
        SetTable a b c -> toJSON ("settable", a, b, c)
        GetTable a b c -> toJSON ("gettable", a, b, c)
        --x -> error $ show x

readT :: Read a => Text -> a
readT = read . unpack


