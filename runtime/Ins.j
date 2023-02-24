// scope Ins
// REQUIRES

globals
    constant integer _Call      =  1
    constant integer _Enter     =  2
    constant integer _Leave     =  3
    constant integer _GetLit    =  4
    constant integer _Bind      =  5
    constant integer _LitString =  6
    constant integer _LitInt    =  7
    constant integer _LitFloat  =  8
    constant integer _LitBool   =  9
    constant integer _LitNil    = 10
    constant integer _Set       = 11
    constant integer _SetLit    = 12
    constant integer _Table     = 13
    constant integer _Append    = 14
    constant integer _GetList   = 15
    constant integer _SetTable  = 16
    constant integer _GetTable  = 17
    constant integer _Ret       = 18
    constant integer _Label     = 19
    constant integer _Jump      = 20
    constant integer _JumpT     = 21
    constant integer _Not       = 22
    constant integer _Neg       = 23
    constant integer _Len       = 24
    constant integer _Complement= 25
    constant integer _GTE       = 26
    constant integer _GT        = 27
    constant integer _LTE       = 28
    constant integer _LT        = 29
    constant integer _EQ        = 30
    constant integer _NEQ       = 31
    constant integer _Mul       = 32
    constant integer _Div       = 33
    constant integer _Sub       = 34
    constant integer _Add       = 35
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
    constant integer _Lambda    = 46
    constant integer _Local     = 47

    constant integer _Comment   = 99

    integer array _Labels

    integer array _ins
    integer array _op1
    integer array _op2
    integer array _op3
    
    string array _string
    real array _real
endglobals
