{-# LANGUAGE LambdaCase #-}

module Compiler.Bytecode where

import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.Text as Text
import Prelude hiding (LT,GT,EQ)

import Language.Lua.StringLiteral
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe

import qualified Jass.Ast as Jass
import qualified Jass.Printer as Jass


type Register = Int
type Label = Int
type Name = Text

data Bytecode =
      Call Register Register
    | Enter
    | Leave
    | GetLit Register Text
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
    | Fun Label Text
    | Lambda Register Text
    | Local Text

    | Comment Text
    deriving (Show)

-- TODO: very unholy function
bla :: Text -> Text
bla s =
  case interpretStringLiteral $ Text.unpack s of
    Nothing -> error . show $ ("could not interp", s)
    Just bs -> decodeUtf8 $ BL.toStrict bs

readT :: Read a => Text -> a
readT = read . Text.unpack

intlit = Jass.Int . show
var = Jass.Var . Jass.SVar

setins i v = Jass.Set (Jass.AVar "Ins#_ins" $ intlit i) $ var v
setop i o v = Jass.Set (Jass.AVar ("Ins#_op" <> show o) $ intlit i) $ intlit v
setstr i t = Jass.Set (Jass.AVar "Ins#_string" $ intlit i) $ string t

neg :: Jass.Ast String Jass.Expr -> Jass.Ast String a
neg = Jass.Call "-" . pure

string = Jass.String . Text.unpack

toJass :: Bytecode -> (Int -> [Jass.Ast String Jass.Stmt]) -- maybe [Stmt]
toJass x i =
  case x of
    GetLit a t ->
        [ setins i "Ins#_GetLit"
        , setop i 1 a
        , setstr i t
        ]
    SetLit t a ->
        [ setins i "Ins#_SetLit"
        , setop i 1 a
        , setstr i t
        ]
    LitString a t ->
        [ setins i "Ins#_LitString"
        , setop i 1 a
        , setstr i t
        ]
    LitBool a b ->
        [ setins i "Ins#_LitBool"
        , setop i 1 a
        , Jass.Set (Jass.AVar "Ins#_bool" $ intlit i) $ Jass.Bool b
        ]
    LitInt a t ->
        [ setins i "Ins#_LitInt"
        , setop i 1 a
        , setop i 2 $ read $ Text.unpack t
        ]
    LitFloat a t ->
        [ setins i "Ins#_LitFloat"
        , setop i 1 a
        , Jass.Set (Jass.AVar "Ins#_real" $ intlit i) $ Jass.Real (Text.unpack t)
        ]
    LitNil a ->
        [ setins i "Ins#_LitNil"
        , setop i 1 a
        ]
    Lambda a t ->
        [ setins i "Ins#_Lambda"
        , setop i 1 a
        , setstr i t
        ]
    Local t ->
        [ setins i "Ins#_Local"
        , setstr i t
        ]
    Call a b ->
        [ setins i "Ins#_Call"
        , setop i 1 a
        , setop i 2 b
        ]
    Enter -> [ setins i "Ins#_Enter" ]
    Leave -> [ setins i "Ins#_Leave" ]
    Set a b ->
        [ setins i "Ins#_Set"
        , setop i 1 a
        , setop i 2 b
        ]
    Table a ->
        [ setins i "Ins#_Table"
        , setop i 1 a
        ]
    Append a b c ->
        [ setins i "Ins#_Append"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GetList a b c ->
        [ setins i "Ins#_GetList"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    SetTable a b c ->
        [ setins i "Ins#_SetTable"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GetTable a b c ->
        [ setins i "Ins#_GetTable"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Ret -> [ setins i "Ins#_Ret" ]

    Label lbl ->
        [ Jass.Set (Jass.AVar "Ins#_Labels" (neg $ intlit lbl)) (intlit $ succ i)
        , setins i "Ins#_Label"
        ]
    Fun lbl name ->
        [ Jass.Set (Jass.AVar "Ins#_Labels" (neg $ intlit lbl)) (intlit $ succ i)
        , setstr i name
        ]
    Jump lbl ->
        [ setins i "Ins#_Jump"
        , setop i 1 lbl
        ]
    JumpT lbl reg ->
        [ setins i "Ins#_JumpT"
        , setop i 1 lbl
        , setop i 2 reg
        ]
    Not a b ->
        [ setins i "Ins#_Not"
        , setop i 1 a
        , setop i 2 b
        ]
    Neg a b ->
        [ setins i "Ins#_Neg"
        , setop i 1 a
        , setop i 2 b
        ]
    Len a b ->
        [ setins i "Ins#_Len"
        , setop i 1 a
        , setop i 2 b
        ]
    Complement a b ->
        [ setins i "Ins#_Complement"
        , setop i 1 a
        , setop i 2 b
        ]
    GTE a b c ->
        [ setins i "Ins#_GTE"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GT a b c ->
        [ setins i "Ins#_GT"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    LTE a b c ->
        [ setins i "Ins#_LTE"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    LT a b c ->
        [ setins i "Ins#_LT"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    EQ a b c ->
        [ setins i "Ins#_EQ"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    NEQ a b c ->
        [ setins i "Ins#_NEQ"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Mul a b c ->
        [ setins i "Ins#_Mul"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Div a b c ->
        [ setins i "Ins#_Div"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Add a b c ->
        [ setins i "Ins#_Add"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Sub a b c ->
        [ setins i "Ins#_Sub"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Exp a b c ->
        [ setins i "Ins#_Exp"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Mod a b c ->
        [ setins i "Ins#_Mod"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Concat a b c ->
        [ setins i "Ins#_Concat"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    IDiv a b c ->
        [ setins i "Ins#_IDiv"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    ShiftL a b c ->
        [ setins i "Ins#_ShiftL"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    ShiftR a b c ->
        [ setins i "Ins#_ShiftR"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BAnd a b c ->
        [ setins i "Ins#_BAnd"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BOr a b c ->
        [ setins i "Ins#_BOr"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BXor a b c ->
        [ setins i "Ins#_BXor"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]

--toJassFunction :: [Bytecode] -> Jass.Ast String Jass.Toplevel
toJassFunction asm =
    let stmts = concat $ zipWith toJass asm [1..]
    in Jass.Programm [ Jass.Function Jass.Normal "_init" [] "nothing" stmts ]

