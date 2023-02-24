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

import qualified Jass.Ast as Jass
import qualified Jass.Printer as Jass

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
    | Fun Label Text
    | Lambda Register Text
    | Local Text

    | Comment Text
    deriving (Show)

-- TODO: very unholy function
bla :: Text -> Text
bla s =
  case interpretStringLiteral $ unpack s of
    Nothing -> error . show $ ("could not interp", s)
    Just bs -> decodeUtf8 $ BL.toStrict bs

instance ToJSON Bytecode where
    toJSON = \case
        Fun lbl fn -> toJSON ( "fun", lbl, fn)
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

        Comment txt -> toJSON ("comment", txt)
        --x -> error $ show x

readT :: Read a => Text -> a
readT = read . unpack

intlit = Jass.Int . show
var = Jass.Var . Jass.SVar

setins i v = Jass.Set (Jass.AVar "_Ins_ins" $ intlit i) $ var v
setop i o v = Jass.Set (Jass.AVar ("_Ins_op" <> show o) $ intlit i) $ intlit v

neg :: Jass.Ast String Jass.Expr -> Jass.Ast String a
neg = Jass.Call "-" . pure

toJass :: Bytecode -> (Int -> [Jass.Ast String Jass.Stmt]) -- maybe [Stmt]
toJass x i =
  case x of
    Call a b c ->
        [ setins i "_Ins_Call"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Enter -> [ setins i "_Ins_Enter" ]
    Leave -> [ setins i "_Ins_Leave" ]
    Set a b ->
        [ setins i "_Ins_Set"
        , setop i 1 a
        , setop i 2 b
        ]
    Table a ->
        [ setins i "_Ins_Table"
        , setop i 1 a
        ]
    Append a b c ->
        [ setins i "_Ins_Append"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GetList a b c ->
        [ setins i "_Ins_GetList"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    SetTable a b c ->
        [ setins i "_Ins_SetTable"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GetTable a b c ->
        [ setins i "_Ins_GetTable"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Ret -> [ setins i "_Ins_Ret" ]

    Label lbl -> [ Jass.Set (Jass.AVar "_Ins_Labels" (neg $ intlit lbl)) (intlit i) ]
    Fun lbl name ->
        [ Jass.Set (Jass.AVar "_Ins_Labels" $ (neg $ intlit lbl)) (intlit i)
        -- set _Ins_string[-lbl] = name
        ]
    Jump lbl ->
        [ setins i "_Ins_Jump"
        , setop i 1 lbl
        ]
    JumpT lbl reg ->
        [ setins i "_Ins_JumpT"
        , setop i 1 lbl
        , setop i 2 reg
        ]
    Not a b ->
        [ setins i "_Ins_Not"
        , setop i 1 a
        , setop i 2 b
        ]
    Neg a b ->
        [ setins i "_Ins_Neg"
        , setop i 1 a
        , setop i 2 b
        ]
    Len a b ->
        [ setins i "_Ins_Len"
        , setop i 1 a
        , setop i 2 b
        ]
    Complement a b ->
        [ setins i "_Ins_Complement"
        , setop i 1 a
        , setop i 2 b
        ]
    GTE a b c ->
        [ setins i "_Ins_GTE"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    GT a b c ->
        [ setins i "_Ins_GT"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    LTE a b c ->
        [ setins i "_Ins_LTE"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    LT a b c ->
        [ setins i "_Ins_LT"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    EQ a b c ->
        [ setins i "_Ins_EQ"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    NEQ a b c ->
        [ setins i "_Ins_NEQ"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Mul a b c ->
        [ setins i "_Ins_Mul"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Div a b c ->
        [ setins i "_Ins_Div"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Add a b c ->
        [ setins i "_Ins_Add"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Sub a b c ->
        [ setins i "_Ins_Sub"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Exp a b c ->
        [ setins i "_Ins_Exp"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Mod a b c ->
        [ setins i "_Ins_Mod"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    Concat a b c ->
        [ setins i "_Ins_Concat"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    IDiv a b c ->
        [ setins i "_Ins_IDiv"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    ShiftL a b c ->
        [ setins i "_Ins_ShiftL"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    ShiftR a b c ->
        [ setins i "_Ins_ShiftR"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BAnd a b c ->
        [ setins i "_Ins_BAnd"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BOr a b c ->
        [ setins i "_Ins_BOr"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]
    BXor a b c ->
        [ setins i "_Ins_BXor"
        , setop i 1 a
        , setop i 2 b
        , setop i 3 c
        ]


