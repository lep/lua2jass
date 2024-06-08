// scope Ins
// REQUIRES

globals
    constant integer _Call      =  1 // XXX
    constant integer _Enter     =  2 // XXX
    constant integer _Leave     =  3 // XXX
    constant integer _GetLit    =  4 // XXX
    constant integer _LitString =  5 // XXX
    constant integer _LitInt    =  6 // XXX
    constant integer _LitFloat  =  7 // XXX
    constant integer _LitBool   =  8 // XXX
    constant integer _LitNil    =  9 // XXX
    constant integer _Set       = 10 // XXX
    constant integer _SetLit    = 11 // XXX
    constant integer _Table     = 12 // XXX
    constant integer _Append    = 13 // XXX
    constant integer _GetList   = 14 // XXX
    constant integer _SetTable  = 15 // XXX
    constant integer _GetTable  = 16 // XXX
    constant integer _Ret       = 17 // XXX
    constant integer _Label     = 18 // XXX // TODO: remove
    constant integer _Jump      = 19 // XXX
    constant integer _JumpT     = 20 // XXX
    constant integer _Not       = 21 // XXX
    constant integer _Neg       = 22 // XXX
    constant integer _Len       = 23 // XXX
    constant integer _Complement= 24 // XXX
    constant integer _GTE       = 25 // XXX
    constant integer _GT        = 26 // XXX
    constant integer _LTE       = 27 // XXX
    constant integer _LT        = 28 // XXX
    constant integer _EQ        = 29 // XXX
    constant integer _NEQ       = 30 // XXX
    constant integer _Mul       = 31 // XXX
    constant integer _Div       = 32 // XXX
    constant integer _Sub       = 33 // XXX
    constant integer _Add       = 34 // XXX
    constant integer _Exp       = 35 // XXX
    constant integer _Mod       = 36 // XXX
    constant integer _Concat    = 37 // XXX
    constant integer _IDiv      = 38 // XXX
    constant integer _ShiftL    = 39 // XXX
    constant integer _ShiftR    = 40 // XXX
    constant integer _BAnd      = 41 // XXX
    constant integer _BOr       = 42 // XXX
    constant integer _BXor      = 43 // XXX
    constant integer _Fun       = 44 // TODO: remove
    constant integer _Lambda    = 45 // XXX
    constant integer _Local     = 46 // XXX

    integer array _Labels

    integer array _ins
    integer array _op1
    integer array _op2
    integer array _op3
    
    string array _string
    real array _real
    boolean array _bool


    ////

    string array _Name
endglobals

function _init takes nothing returns nothing
    set _Name[_Call] = "Call"
    set _Name[_Enter] = "Enter"
    set _Name[_Leave] = "Leave"
    set _Name[_GetLit] = "GetLit"
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

function _show takes integer ip returns string
    local integer ins = _ins[ip]
    local string s = _Name[ins] + " "

    // ii
    if ins == _Call or ins == _Set or (ins >=  _JumpT and ins <= _Complement) then
        set s = s + I2S(_op1[ins]) +" "+I2S(_op2[ins])
    elseif (ins >= _GTE and ins <= _BXor) or (ins >= _Append and ins <= _GetTable) then
        set s = s + I2S(_op1[ins]) +" "+I2S(_op2[ins]) +" "+I2S(_op3[ins])
    elseif ins == _SetLit then // si
        set s = s + _string[ins] +" "+I2S(_op1[ins])
    elseif ins == _GetLit or ins == _Lambda or ins == _LitString then // is
        set s = s + _string[ins] +" "+I2S(_op1[ins])
    else
        set s = s + "<...>"
    endif
    return s
endfunction
