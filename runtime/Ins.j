// scope Ins
// REQUIRES

globals
    constant integer _Call      =  1
    constant integer _Enter     =  2 // XXX
    constant integer _Leave     =  3 // XXX
    constant integer _GetLit    =  4 // XXX
    constant integer _Bind      =  5
    constant integer _LitString =  6 // XXX
    constant integer _LitInt    =  7 // XXX
    constant integer _LitFloat  =  8
    constant integer _LitBool   =  9
    constant integer _LitNil    = 10
    constant integer _Set       = 11
    constant integer _SetLit    = 12 // XXX
    constant integer _Table     = 13 // XXX
    constant integer _Append    = 14
    constant integer _GetList   = 15
    constant integer _SetTable  = 16 // XXX
    constant integer _GetTable  = 17
    constant integer _Ret       = 18 // XXX
    constant integer _Label     = 19 // XXX
    constant integer _Jump      = 20 // XXX
    constant integer _JumpT     = 21 // XXX
    constant integer _Not       = 22 // XXX
    constant integer _Neg       = 23
    constant integer _Len       = 24
    constant integer _Complement= 25
    constant integer _GTE       = 26
    constant integer _GT        = 27
    constant integer _LTE       = 28 // XXX
    constant integer _LT        = 29
    constant integer _EQ        = 30 // XXX
    constant integer _NEQ       = 31 // XXX
    constant integer _Mul       = 32 // XXX
    constant integer _Div       = 33 // XXX
    constant integer _Sub       = 34 // XXX
    constant integer _Add       = 35 // XXX
    constant integer _Exp       = 36
    constant integer _Mod       = 37
    constant integer _Concat    = 38
    constant integer _IDiv      = 39
    constant integer _ShiftL    = 40
    constant integer _ShiftR    = 41
    constant integer _BAnd      = 42
    constant integer _BOr       = 43
    constant integer _BXor      = 44
    constant integer _Fun       = 45
    constant integer _Lambda    = 46 // XXX
    constant integer _Local     = 47 // XXX

    constant integer _Comment   = 99

    integer array _Labels

    integer array _ins
    integer array _op1
    integer array _op2
    integer array _op3
    
    string array _string
    real array _real


    ////

    string array _Name
endglobals

function _init takes nothing returns nothing
    set _Name[_Call] = "Call"
    set _Name[_Enter] = "Enter"
    set _Name[_Leave] = "Leave"
    set _Name[_GetLit] = "GetLit"
    set _Name[_Bind] = "Bind"
    set _Name[_LitString] = "LitString"
    set _Name[_LitInt] = "LitInt"
    set _Name[_LitFloat] = "LitFloat"
    set _Name[_LitBool] = "LitBool"
    set _Name[_LitNil] = "LitNil"
    set _Name[_Set] = "Set"
    set _Name[_SetLit] = "SetLit"
    set _Name[_Table] = "Table"
    set _Name[_Append] = "Append"
    set _Name[_GetList] = "GetList"
    set _Name[_SetTable] = "SetTable"
    set _Name[_GetTable] = "GetTable"
    set _Name[_Ret] = "Ret"
    set _Name[_Label] = "Label"
    set _Name[_Jump] = "Jump"
    set _Name[_JumpT] = "JumpT"
    set _Name[_Not] = "Not"
    set _Name[_Neg] = "Neg"
    set _Name[_Len] = "Len"
    set _Name[_Complement] = "Complement"
    set _Name[_GTE] = "GTE"
    set _Name[_GT] = "GT"
    set _Name[_LTE] = "LTE"
    set _Name[_LT] = "LT"
    set _Name[_EQ] = "EQ"
    set _Name[_NEQ] = "NEQ"
    set _Name[_Mul] = "Mul"
    set _Name[_Div] = "Div"
    set _Name[_Sub] = "Sub"
    set _Name[_Add] = "Add"
    set _Name[_Exp] = "Exp"
    set _Name[_Mod] = "Mod"
    set _Name[_Concat] = "Concat"
    set _Name[_IDiv] = "IDiv"
    set _Name[_ShiftL] = "ShiftL"
    set _Name[_ShiftR] = "ShiftR"
    set _Name[_BAnd] = "BAnd"
    set _Name[_BOr] = "BOr"
    set _Name[_BXor] = "BXor"
    set _Name[_Fun] = "Fun"
    set _Name[_Lambda] = "Lambda"
    set _Name[_Local] = "Local"
endfunction
