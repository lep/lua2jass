// scope Interpreter
// REQUIRES Value Ins Context Table Print Dispatch Helper List Call Alloc
// REQUIRES Builtin::Coroutine Builtin::Math Builtin::Table StringTable

globals
    integer _GlobalInterpreter

    constant integer _StopInterpreter = 1
    constant integer _CoroutineYield = 2
    
    integer array _stack_top


    // list _stack
    integer array _ctx


    // DEBUG

    integer _GlobalIp = 0
    integer _GlobalIns = 0
endglobals

function _Not takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Value#_not(v1)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Complement takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)

    local integer aMetamethod = Value#_getMetamethod(a, "__bnot")

    if aMetamethod != Value#_Nil then
	call Call#_call1( aMetamethod, a, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_complement(a) )
	endif
    endif
endfunction

function _ShiftL takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)
    local integer bAsNumber = Value#_integercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__shl")
    local integer bMetamethod = Value#_getMetamethod(b, "__shl")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_shiftl(a, b) )
	endif
    endif
endfunction

function _ShiftR takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)
    local integer bAsNumber = Value#_integercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__shr")
    local integer bMetamethod = Value#_getMetamethod(b, "__shr")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_shiftr(a, b) )
	endif
    endif
endfunction

function _BAnd takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)
    local integer bAsNumber = Value#_integercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__band")
    local integer bMetamethod = Value#_getMetamethod(b, "__band")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_band(a, b) )
	endif
    endif
endfunction

function _BOr takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)
    local integer bAsNumber = Value#_integercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__bor")
    local integer bMetamethod = Value#_getMetamethod(b, "__bor")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_bor(a, b) )
	endif
    endif
endfunction

function _BXor takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_integercontext(a)
    local integer bAsNumber = Value#_integercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__bxor")
    local integer bMetamethod = Value#_getMetamethod(b, "__bxor")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("number has no integer representation")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_bxor(a, b) )
	endif
    endif
endfunction

function _Neg takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)

    local integer aMetamethod = Value#_getMetamethod(a, "__unm")

    if aMetamethod != Value#_Nil then
	call Call#_call1( aMetamethod, a, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil then
	    call Value#_error_str("_Neg: Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_neg(aAsNumber) )
	endif
    endif
endfunction

function _Add takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__add")
    local integer bMetamethod = Value#_getMetamethod(b, "__add")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("_Add: Attempt to perform arithmethic on a string value " + Value#_tostring_debug(a) +" + "+Value#_tostring_debug(b))
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_add(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _Sub takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__sub")
    local integer bMetamethod = Value#_getMetamethod(b, "__sub")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_sub(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _Mul takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__mul")
    local integer bMetamethod = Value#_getMetamethod(b, "__mul")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_mul(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _Div takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__div")
    local integer bMetamethod = Value#_getMetamethod(b, "__div")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_div(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _IDiv takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__idiv")
    local integer bMetamethod = Value#_getMetamethod(b, "__idiv")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_idiv(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _Mod takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__mod")
    local integer bMetamethod = Value#_getMetamethod(b, "__mod")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_mod(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _Exp takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aAsNumber = Value#_numbercontext(a)
    local integer bAsNumber = Value#_numbercontext(b)

    local integer aMetamethod = Value#_getMetamethod(a, "__exp")
    local integer bMetamethod = Value#_getMetamethod(b, "__exp")

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    else
	if aAsNumber == Value#_Nil or bAsNumber == Value#_Nil then
	    call Value#_error_str("Attempt to perform arithmethic on a string value")
	else
	    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_exp(aAsNumber, bAsNumber) )
	endif
    endif
endfunction

function _eq_value takes integer ctx, integer ip, integer interpreter returns boolean
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer ret = Value#_table()

    local integer aType = Value#_Type[a]
    local integer bType = Value#_Type[b]

    local boolean eq = Value#_rawequal_noalloc(a, b)

    local integer aMetamethod = Value#_getMetamethod(a, "__eq")
    local integer bMetamethod = Value#_getMetamethod(b, "__eq")

    if eq then
	return eq
    // TODO: _Foreign?
    elseif aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	return Value#_truthy( Table#_get( Value#_Int[ret], 1 ) )
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	return Value#_truthy( Table#_get( Value#_Int[ret], 1 ) )
    else
	return eq
    endif
endfunction

function _EQ takes integer ctx, integer ip, integer interpreter returns nothing
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(_eq_value(ctx, ip, interpreter) ))
endfunction

function _NEQ takes integer ctx, integer ip, integer interpreter returns nothing
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(not _eq_value(ctx, ip, interpreter) ))
endfunction



function _lt_value takes integer v1, integer v2, integer interpreter returns boolean
    local integer type1 = Value#_Type[v1]
    local integer type2 = Value#_Type[v2]

    local integer aMetamethod = Value#_getMetamethod(v1, "__lt")
    local integer bMetamethod = Value#_getMetamethod(v2, "__lt")

    local integer ret = Value#_table() // lol GC

    if type1 == Jass#_string and type2 == Jass#_string then
	return Helper#_lt_string(Value#_String[v1], Value#_String[v2] )
    elseif (type1 == Jass#_integer or type1 == Jass#_real) and (type2 == Jass#_integer or type2 == Jass#_real) then
	return Value#_lt_numeric_noalloc(v1, v2)
    elseif aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, v1, v2, ret, interpreter )
	return ( Value#_truthy( Table#_get( Value#_Int[ret], 1 ) ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, v1, v2, ret, interpreter )
	return ( Value#_truthy( Table#_get( Value#_Int[ret], 1 ) ))
    else
	call Value#_error_str("attempt to compare two non-comparable values")
	return false
    endif

endfunction

function _lte_value takes integer v1, integer v2, integer interpreter returns boolean
    local integer type1 = Value#_Type[v1]
    local integer type2 = Value#_Type[v2]

    local integer aMetamethod = Value#_getMetamethod(v1, "__le")
    local integer bMetamethod = Value#_getMetamethod(v2, "__le")

    local integer ret = Value#_table() // lol GC
    
    if type1 == Jass#_string and type2 == Jass#_string then
	return not Helper#_lt_string(Value#_String[v2], Value#_String[v1])
    elseif (type1 == Jass#_integer or type1 == Jass#_real) and (type2 == Jass#_integer or type2 == Jass#_real) then
	return Value#_lte_numeric_noalloc(v1, v2)
    elseif aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, v1, v2, ret, interpreter )
	return Value#_truthy( Table#_get( Value#_Int[ret], 1 ) )
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, v1, v2, ret, interpreter )
	return Value#_truthy( Table#_get( Value#_Int[ret], 1 ) )
    else
	return not _lt_value( v2, v1, interpreter )
    endif
endfunction

// a >= b ~~ b <= a
function _GTE takes integer ctx, integer ip, integer interpreter returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(_lte_value(v2, v1, interpreter) ))
endfunction

// a > b ~~ b < a
function _GT takes integer ctx, integer ip, integer interpreter returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(_lt_value(v2, v1, interpreter) ))
endfunction

function _LTE takes integer ctx, integer ip, integer interpreter returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(_lte_value(v1, v2, interpreter) ))
endfunction

function _LT takes integer ctx, integer ip, integer interpreter returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_litbool(_lt_value(v1, v2, interpreter) ))
endfunction

function _Table takes integer ctx, integer ip returns nothing
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_table() )
endfunction


function _Jump takes integer ctx, integer ip returns nothing
    set Context#_ip[ctx] = Ins#_Labels[ - Ins#_op1[ip] ]
endfunction

function _JumpT takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    if Value#_truthy(v1) then
	set Context#_ip[ctx] = Ins#_Labels[ - Ins#_op1[ip] ]
    endif
endfunction

function _Local takes integer ctx, integer ip returns nothing
    //call Print#_print("_Local")
    //call Print#_print("  - local "+Ins#_string[ip])
    call StringTable#_set( Context#_locals[ctx], Ins#_string[ip], Value#_litnil())
endfunction

function _Lambda takes integer ctx, integer ip returns nothing
    local integer new_ctx = Context#_new()
    call Context#_initialize(new_ctx)
    set Context#_parent[new_ctx] = ctx
    set Context#_chunk_name[new_ctx] = Ins#_string[ip]
    set Context#_ip[new_ctx] = Ins#_Labels[ - Ins#_op1[ip] ]
    
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_lambda(new_ctx, Ins#_string[ip]))
endfunction

function _SetLit takes integer ctx, integer ip returns nothing
    local integer v = Table#_get(Context#_tmps[ctx], Ins#_op1[ip])
    local string name = Ins#_string[ip]
    //call Print#_print("_SetLit")
    //call Print#_print("  - setting "+name+" to value "+I2S(v))
    call Context#_set( ctx, name, v)
endfunction

function _Set takes integer ctx, integer ip returns nothing
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Context#_tmps[ctx], Ins#_op2[ip] ) )
endfunction

function _GetLit takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local string name = Ins#_string[ip]
    local integer v
    //call Print#_print("_GetLit")
    if name == "$params" then
	set v = Value#_table()
	set Value#_Int[v] = Context#_tmps[ctx]
	//call Print#_print("  - $params[1] = "+Value#_tostring(Table#_get( Context#_tmps[ctx], 1 )))
	//call Builtins#_print(Context#_tmps[ctx], 0, 0)
	call Table#_set( Context#_tmps[ctx], reg, v)
    else
	set v = Context#_get( ctx, name )
	//call Print#_print("  - name "+name+" is "+I2S(v))
	call Table#_set( Context#_tmps[ctx], reg, v)
    endif
endfunction

function _LitNil takes integer ctx, integer ip returns nothing
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
endfunction

function _LitString takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local integer value = Value#_litstring( Ins#_string[ip] )
    //call Print#_print("_LitString")
    //call Print#_print("  - from ins: "+Ins#_string[ip])
    //call Print#_print("  - from value: "+Value#_String[value])
    call Table#_set( Context#_tmps[ctx], reg, value )
endfunction

function _LitInt takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local integer value = Value#_litint( Ins#_op2[ip] )
    call Table#_set( Context#_tmps[ctx], reg, value )
endfunction

function _LitFloat takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local integer value = Value#_litfloat( Ins#_real[ip] )
    call Table#_set( Context#_tmps[ctx], reg, value )
endfunction

function _LitBool takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local integer value = Value#_litbool( Ins#_bool[ip] )
    call Table#_set( Context#_tmps[ctx], reg, value )
endfunction

function _Enter takes integer ctx, integer interpreter returns nothing
    local integer new_ctx = Context#_clone(ctx)
    //call Print#_print("Enter")
    set Context#_parent[new_ctx] = ctx
    set Context#_type[new_ctx] = Context#_Block
    // stack.push(new_ctx)
    set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
    set _ctx[_stack_top[interpreter]] = new_ctx
    //call Print#_print("  - "+I2S(ctx)+" --> "+I2S(new_ctx))
endfunction

function _Leave takes integer ctx, integer interpreter returns nothing
    local integer head = _stack_top[interpreter]
    local integer old_ctx = _ctx[head]
    local integer new_old_ctx
    // stack.pop()
    set _stack_top[interpreter] = List#_next[head]
    set new_old_ctx = _ctx[_stack_top[interpreter]]
    set Context#_ip[new_old_ctx] = Context#_ip[old_ctx]
endfunction

function _Call takes integer ctx, integer ip, integer interpreter returns nothing
    local integer reg_fn = Ins#_op1[ip]
    local integer reg_params = Ins#_op2[ip]
    local integer fn = Table#_get( Context#_tmps[ctx], reg_fn)
    local integer ty = Value#_Type[fn]
    local integer params = Table#_get( Context#_tmps[ctx], reg_params ) // params : Value

    local integer metamethod
    local integer metatable
    local integer metaparams

    local integer new_ctx

    //call Print#_print("_Call")

    if ty == Types#_Lambda then
	set new_ctx = Context#_clone(Value#_Int[fn])
	set Context#_tmps[new_ctx] = Value#_Int[params] // this might need some refactoring
	set Context#_type[new_ctx] = Context#_Function

	// stack.push(new_ctx)
	set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
	set _ctx[_stack_top[interpreter]] = new_ctx
    elseif ty == Types#_BuiltInFunction then
	call Dispatch#_dispatch(Value#_Int[fn], Value#_Int[params], ctx, interpreter)
    elseif ty == Types#_Table then

	// Basically a copy of the lambda case with some more checks
	// and value extractions
	set metatable = Value#_Int3[fn]
	if metatable != 0 then
	    set metamethod = Value#_gettable( metatable, Value#_litstring("__call"))
	    if metamethod != Value#_Nil and Value#_Type[metamethod] == Types#_Lambda then
		set metaparams = Table#_alloc()
		call Table#_set( metaparams, 0, Table#_get( Value#_Int[params], 0 ) )
		call Table#_set( metaparams, 1, fn )
		call Table#_append( metaparams, Value#_Int[params], 1 )
		set new_ctx = Context#_clone(Value#_Int[metamethod])
		set Context#_tmps[new_ctx] = metaparams // this might need some refactoring
		set Context#_type[new_ctx] = Context#_Function

		// stack.push(new_ctx)
		set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
		set _ctx[_stack_top[interpreter]] = new_ctx
		return

	    endif
	endif
	call Print#_print("Cannot call object of type "+I2S(ty))
    else
	call Print#_print("Cannot call object of type "+I2S(ty))
    endif
endfunction

function _Len takes integer ctx, integer ip, integer interpreter returns nothing
    local integer r = Ins#_op1[ip]
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer metamethod = Value#_getMetamethod(a, "__len")
    local integer ty = Value#_Type[a]
    local integer ret = Value#_table()

    if ty == Jass#_string or ( ty == Types#_Table and metamethod == Value#_Nil) then
	call Table#_set( Context#_tmps[ctx], r, Value#_len(a) )
    elseif metamethod != Value#_Nil then
	call Call#_call1( metamethod, a, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], r, Table#_get( Value#_Int[ret], 1 ))
    else
	call Value#_error_str("attempt to get length of wrong type")
    endif

endfunction

function _GetList takes integer ctx, integer ip returns nothing
    local integer offset = Ins#_op1[ip]
    local integer val_target = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer val_source = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    local integer tbl_target = Value#_Int[val_target]
    local integer tbl_source = Value#_Int[val_source]

    local integer k = offset
    local integer v

    //call Print#_print("_GetList")
    //call Print#_print("  - offset = "+I2S(offset))

    if Value#_Type[val_target] != Types#_Table then
	call Value#_error_str("Target table not of type table but "+I2S(Value#_Type[val_target]))
    endif
    if Value#_Type[val_source] != Types#_Table then
	call Value#_error_str("Source table not of type table but "+I2S(Value#_Type[val_source]))
    endif

    //call Builtins#_print(tbl_source, 0, 0)

    call Table#_getlist( tbl_target, tbl_source, offset )
    
    //loop
    //    //call Print#_print("  - checking key k = "+ I2S(k))
    //    if Table#_has( tbl_source, k ) then
    //        //call Print#_print("  - table has key k = "+I2S(k))
    //        set v = Table#_get( tbl_source, k )
    //        //call Print#_print("  - "+Value#_tostring(v))
    //        call Table#_set( tbl_target, k - offset +1, Table#_get( tbl_source, k ))
    //    else
    //        exitwhen true
    //    endif
    //    set k = k +1
    //endloop
endfunction

function _Append takes integer ctx, integer ip returns nothing
    // let's try something different:
    // instead of doing the bulk in a Value# function we do the typechecking
    // and extraction in here and see how that feels
    local integer offset = Ins#_op1[ip] - 1

    local integer val_target = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer val_source = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    local integer tbl_target = Value#_Int[val_target]
    local integer tbl_source = Value#_Int[val_source]

    local integer k = 1

    if Value#_Type[val_target] != Types#_Table then
	call Value#_error_str("Target table not of type table but "+I2S(Value#_Type[val_target]))
    endif
    if Value#_Type[val_source] != Types#_Table then
	call Value#_error_str("Target table not of type table but "+I2S(Value#_Type[val_source]))
    endif

    call Table#_append( tbl_target, tbl_source, offset )

endfunction

function _Ret takes integer ctx, integer interpreter returns boolean
    local integer head = _stack_top[interpreter]
    local boolean ret = false

    local integer tmp_table
    local integer co_value
    local integer base_context_returntable_value

    local integer base_ctx

    //call Print#_print("_Ret")
    //call Print#_print("  - current ctx: "+I2S(ctx))

    loop
    exitwhen head == 0
    exitwhen Context#_type[_ctx[head]] != Context#_Block
	set head = List#_next[head]
    endloop

    if head != 0 then
	set base_ctx = _ctx[head]
	//call Print#_print("  - found matching context: "+I2S(_ctx[head]) )
	set ret = Context#_ret_behaviour[ base_ctx ] == _StopInterpreter 

	if Context#_ret_behaviour[ base_ctx ] == _CoroutineYield then
	    //call Print#_print("  - returning from coroutine")
	    set co_value = Builtin::Coroutine#_ctx2value[ base_ctx ]

	    // prepend true to return table
	    set base_context_returntable_value = Builtin::Coroutine#_return_resume[co_value]
	    set tmp_table = Table#_alloc()
	    call Table#_append( tmp_table, Value#_Int[base_context_returntable_value], 1 )
	    call Table#_set( tmp_table, 1, Value#_litbool(true) )
	    call Table#_getlist( Value#_Int[base_context_returntable_value], tmp_table, 1 )

	    set Builtin::Coroutine#_status[co_value] = Builtin::Coroutine#_StatusDead
	endif

	set _stack_top[interpreter] = List#_next[head]
    else
	call Value#_error_str("  - no correct context found")
    endif
    return ret
endfunction

function _gettable_rec takes integer tbl, integer key, integer interpreter returns integer
    local integer metamethod = Value#_getMetamethod(tbl, "__index")
    local integer val = Value#_gettable( tbl, key )
    local integer ret = Value#_table()

    if val == Value#_Nil and metamethod != Value#_Nil then
	if Value#_Type[metamethod] == Types#_Table then
	    return _gettable_rec(metamethod, key, interpreter)
	else
	    call Call#_call2( metamethod, tbl, key, ret, interpreter )
	    return Table#_get( Value#_Int[ret], 1 )
	endif
    else
	return val
    endif
endfunction

/// I guess in the future when we handle operators by firstly checking for
/// tables and metatables and such and handle the "native" behaviour in the
/// Value "struct".
function _GetTable takes integer ctx, integer ip, integer interpreter returns nothing
    local integer reg = Ins#_op1[ip]
    local integer tbl = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer key = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    call Table#_set( Context#_tmps[ctx], reg, _gettable_rec(tbl, key, interpreter) )
endfunction

function _settable_rec takes integer tbl, integer key, integer val, integer interpreter returns nothing
    local integer metamethod = Value#_getMetamethod(tbl, "__index")
    local integer ret = Value#_table()

    if metamethod != Value#_Nil then
	if Value#_Type[metamethod] == Types#_Table then
	    call _settable_rec(metamethod, key, val, interpreter)
	else
	    call Call#_call3( metamethod, tbl, key, val, ret, interpreter )
	endif
    else
	call Value#_settable(tbl, key, val)
    endif
endfunction

function _SetTable takes integer ctx, integer ip, integer interpreter returns nothing
    local integer value_tbl = Table#_get( Context#_tmps[ctx], Ins#_op1[ip] )
    local integer value_key = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer value_val = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    call _settable_rec( value_tbl, value_key, value_val, interpreter )
endfunction

function _stringOrNumber takes integer v returns boolean
    local integer ty = Value#_Type[v]
    return ty == Jass#_string or ty == Jass#_real or ty == Jass#_integer
endfunction

function _Concat takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    local integer aMetamethod = Value#_getMetamethod(a, "__concat")
    local integer bMetamethod = Value#_getMetamethod(b, "__concat")

    local integer ret = Value#_table()

    if aMetamethod != Value#_Nil then
	call Call#_call2( aMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif bMetamethod != Value#_Nil then
	call Call#_call2( bMetamethod, a, b, ret, interpreter )
	call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Table#_get( Value#_Int[ret], 1 ))
    elseif _stringOrNumber(a) and _stringOrNumber(b) then
	call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_concat(a,b) )
    else
	call Value#_error_str("attempt to concatenate non-string, non-number values")
    endif

endfunction

function _step takes integer interpreter returns boolean
    local integer ctx = _ctx[_stack_top[interpreter]]
    // TODO: check if in range
    local integer ip = Context#_ip[ctx]
    local integer ins = Ins#_ins[ip]

    //call Print#_print("_step ("+I2S(ctx)+")")

    if ctx == 0 or ip == 0 or ins == 0 then
	call Print#_warn("  - stopping via: ctx="+I2S(ctx)+",ip="+I2S(ip)+",ins="+I2S(ins))
	return false
    endif

    set Context#_ip[ctx] = Context#_ip[ctx] + 1
    
    if Builtins#_trace then
        call Print#_print("Executing instruction ("+I2S(ip)+") "+Ins#_show(ip) )
    endif

    set _GlobalIns = ins
    set _GlobalIp = ip

    // TODO: binsearch
    if ins == Ins#_Not then
	call _Not(ctx, ip)
    elseif ins == Ins#_Complement then
	call _Complement(ctx, ip, interpreter)
    elseif ins == Ins#_ShiftL then
	call _ShiftL(ctx, ip, interpreter)
    elseif ins == Ins#_ShiftR then
	call _ShiftR(ctx, ip, interpreter)
    elseif ins == Ins#_BAnd then
	call _BAnd(ctx, ip, interpreter)
    elseif ins == Ins#_BOr then
	call _BOr(ctx, ip, interpreter)
    elseif ins == Ins#_BXor then
	call _BXor(ctx, ip, interpreter)
    elseif ins == Ins#_Neg then
	call _Neg(ctx, ip, interpreter)
    elseif ins == Ins#_Add then
	call _Add(ctx, ip, interpreter)
    elseif ins == Ins#_Len then
	call _Len(ctx, ip, interpreter)
    elseif ins == Ins#_Sub then
	call _Sub(ctx, ip, interpreter)
    elseif ins == Ins#_Mul then
	call _Mul(ctx, ip, interpreter)
    elseif ins == Ins#_Div then
	call _Div(ctx, ip, interpreter)
    elseif ins == Ins#_IDiv then
	call _IDiv(ctx, ip, interpreter)
    elseif ins == Ins#_Mod then
	call _Mod(ctx, ip, interpreter)
    elseif ins == Ins#_Exp then
	call _Exp(ctx, ip, interpreter)
    elseif ins == Ins#_EQ then
	call _EQ(ctx, ip, interpreter)
    elseif ins == Ins#_NEQ then
	call _NEQ(ctx, ip, interpreter)
    elseif ins == Ins#_LTE then
	call _LTE(ctx, ip, interpreter)
    elseif ins == Ins#_LT then
	call _LT(ctx, ip, interpreter)
    elseif ins == Ins#_GTE then
	call _GTE(ctx, ip, interpreter)
    elseif ins == Ins#_GT then
	call _GT(ctx, ip, interpreter)
    elseif ins == Ins#_Table then
	call _Table(ctx, ip)
    elseif ins == Ins#_Jump then
	call _Jump(ctx, ip)
    elseif ins == Ins#_JumpT then
	call _JumpT(ctx, ip)
    elseif ins == Ins#_Enter then
	call _Enter(ctx, interpreter)
    elseif ins == Ins#_Leave then
	call _Leave(ctx, interpreter)
    elseif ins == Ins#_Local then
	call _Local(ctx, ip)
    elseif ins == Ins#_Lambda then
	call _Lambda(ctx, ip)
    elseif ins == Ins#_SetLit then
	call _SetLit(ctx, ip)
    elseif ins == Ins#_Set then
	call _Set(ctx, ip)
    elseif ins == Ins#_GetLit then
	call _GetLit(ctx, ip)
    elseif ins == Ins#_LitString then
	call _LitString(ctx, ip)
    elseif ins == Ins#_LitInt then
	call _LitInt(ctx, ip)
    elseif ins == Ins#_LitNil then
	call _LitNil(ctx, ip)
    elseif ins == Ins#_LitFloat then
	call _LitFloat(ctx, ip)
    elseif ins == Ins#_LitBool then
	call _LitBool(ctx, ip)
    elseif ins == Ins#_Call then
	call _Call(ctx, ip, interpreter)
    elseif ins == Ins#_Ret then
	if _Ret(ctx, interpreter) then
	    //call Print#_print("  - stopping via StopInterpreter")
	    return false
	endif
	//if Context#_ret_behaviour[_ctx[_stack_top[interpreter]]] == _StopInterpreter then
	//endif
    elseif ins == Ins#_SetTable then
	//call Print#_print(" SetTable at IP = "+I2S(ip))
	call _SetTable(ctx, ip, interpreter)
    elseif ins == Ins#_GetTable then
	call _GetTable(ctx, ip, interpreter)
    elseif ins == Ins#_Append then
	call _Append(ctx, ip)
    elseif ins == Ins#_GetList then
	call _GetList(ctx, ip)
    elseif ins == Ins#_Concat then
	call _Concat(ctx, ip, interpreter)
    elseif ins == Ins#_Label then
	// NOP
    else
	call Print#_print("OP not implemented "+I2S(ins))
    endif
    return true
endfunction


function _initialize takes integer interpreter returns nothing
    local integer ctx = Context#_new()

    set _GlobalInterpreter = interpreter

    set Value#_Nil = Value#_new()
    set Value#_Type[Value#_Nil] = Types#_Nil

    call Context#_initialize(ctx)
    set Context#_ip[ctx] = Ins#_Labels[0]
    set Context#_ret_behaviour[ctx] = _StopInterpreter
    set Context#_type[ctx] = Context#_Function

    set _stack_top[interpreter] = List#_cons(0)
    set _ctx[_stack_top[interpreter]] = ctx

    call Dispatch#_register(ctx)
    call Builtin::Coroutine#_register(ctx)
    call Builtin::Math#_register(ctx)
    call Builtin::Table#_register(ctx)
    call Builtins#_register(ctx)
endfunction

function _run_interpreter takes nothing returns nothing
    local integer interpreter = _GlobalInterpreter
    loop
	exitwhen not _step(interpreter)
    endloop
endfunction

function _start_main takes nothing returns nothing
    set _GlobalInterpreter = _alloc()
    call _initialize(_GlobalInterpreter)
    call TimerStart(CreateTimer(), 0, false, function _run_interpreter)
endfunction

function _call_function takes integer fn, integer params, integer interpreter returns nothing
    local integer ctx = Context#_clone( Value#_Int[fn] )
    local integer ret_table = Table#_get( params, 0 ) // : Value
    local integer tmp
    set Context#_tmps[ctx] = params
    set Context#_type[ctx] = Context#_Function
    set Context#_ret_behaviour[ctx] = _StopInterpreter

    // stack.push(ctx)
    set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
    set _ctx[_stack_top[interpreter]] = ctx

    loop
	exitwhen not _step(interpreter)
    endloop
endfunction

function _call_function_wrap takes nothing returns boolean
    call _call_function(Wrap#_Param1, Wrap#_Param2, Wrap#_Param3)
    return true
endfunction

function _init takes nothing returns nothing
    call TriggerAddCondition( Wrap#_WrapAround, Filter( function _call_function_wrap ))
endfunction
