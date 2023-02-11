{-# LANGUAGE LambdaCase #-}

module Bytecode where

import Data.Text
import Prelude hiding (LT,GT)

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
    | Ret
    | Label Label
    | Jump Label
    | JumpT Label Register
    | Not Register Register
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
