{-# LANGUAGE LambdaCase #-}

module Bytecode where

import Data.Text
import Data.Text.Encoding
import Prelude hiding (LT,GT)

import Data.Aeson

import Language.Lua.StringLiteral
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

type Register = Int
type Label = Int
type Name = Text

data Bytecode =
      Call Register Register
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
    | Mul Register Register Register
    | Sub Register Register Register
    | Add Register Register Register
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
        Call a b -> toJSON ("call", a, b)
        Enter -> toJSON ["enter"]
        Leave -> toJSON ["leave"]
        GetLit r t -> toJSON ("getlit", r, t)
        Bind a b -> toJSON ("bind", a, b)
        LitString r t -> toJSON ("lit", r, t)
        LitInt r t -> toJSON ("lit", r, (readT t) :: Int)
        LitFloat r t -> toJSON ("lit", r, t)
        LitBool r t -> toJSON ("lit", r, t)
        LitNil r -> toJSON ("lit", r, "")
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
        Mul a b c -> toJSON ("mul", a, b, c)
        Sub a b c -> toJSON ("sub", a, b, c)
        Add a b c -> toJSON ("add", a, b, c)
        Lambda r n -> toJSON ("lambda", r, n)
        Local n -> toJSON ("local", n)
        Table n -> toJSON ("table", n)
        SetTable a b c -> toJSON ("settable", a, b, c)
        GetTable a b c -> toJSON ("gettable", a, b, c)
        x -> error $ show x

readT :: Read a => Text -> a
readT = read . unpack

toPython = \case
    Fun fn -> show ("fun", fn)
    Call a b -> show ("call", a, b)
    Enter -> show ["enter"]
    Leave -> show ["leave"]
    GetLit r t -> show ("getlit", r, t)
    Bind a b -> show ("bind", a, b)
    LitString r t -> show ("lit", r, t)
    LitInt r t -> show ("lit", r, (readT t) :: Int)
    LitFloat r t -> show ("lit", r, t)
    LitBool r t -> show ("lit", r, t)
    LitNil r -> show ("lit", r, "nil")
    Set a b -> show ("set", a, b)
    SetLit a b -> show ("setlit", a, b)
    Ret -> show ["ret"]
    Label lbl -> show ("lbl", lbl)
    Jump lbl -> show ("jmp", lbl)
    JumpT lbl r -> show ("jmpt", lbl, r)
    Not a b -> show ("not", a, b)
    GTE a b c -> show ("gte", a, b, c)
    GT a b c -> show ("gt", a, b, c)
    LTE a b c -> show ("lte", a, b, c)
    LT a b c -> show ("lt", a, b, c)
    Mul a b c -> show ("mul", a, b, c)
    Sub a b c -> show ("sub", a, b, c)
    Add a b c -> show ("add", a, b, c)
    Lambda r n -> show ("lambda", r, n)
    Local n -> show ("local", n)
    x -> error $ show x
