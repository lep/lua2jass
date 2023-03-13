// scope Interpreter
// REQUIRES Value Ins Context Table Print Builtins

globals
    #include "alloc-globals.j"

    integer _GlobalInterpreter

    constant integer _StopInterpreter = 1
    constant integer _CoroutineYield = 2
    
    // struct interpreter
    integer array _stack_top


    // list _stack
    integer array _ctx
endglobals
#include "alloc.j"

// @alloc
function _numbercontext takes integer v returns integer
    local integer ty = Value#_Type[v]
    if ty == Jass#_integer then
	return v
    elseif ty == Jass#_real then
	return v
    elseif ty == Jass#_string then
	set v = Value#_litfloat(Value#_parse_number(Value#_String[v]))
	if Value#_error then
	    call Print#_error("Error: cannot coerce string to number")
	endif
	return v
    else
	// TODO: metatables
	return Value#_litnil()
    endif
	
endfunction

// @alloc
function _integercontext takes integer v returns integer
    local integer ty = Value#_Type[v]
    if ty == Jass#_integer then
	return v
    elseif ty == Jass#_string then
	set ty = Jass#_real
	set v = Value#_litfloat(Value#_parse_number(Value#_String[v]))
	if Value#_error then
	    call Print#_error("Error: cannot coerce string to number")
	    return Value#_litnil()
	endif
    endif

    if ty == Jass#_real then
	set ty = R2I(Value#_Real[v]) // casually reusing variable
	if Value#_Real[v] == ty then
	    return Value#_litint(ty)
	endif
    endif

    // TODO: metatables
    call Print#_error("Error: cannot coerce value to integer")
    return Value#_litnil()
endfunction

function _Not takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Value#_not(v1)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Complement takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer v2 = Value#_complement( _integercontext(v1) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _ShiftL takes integer ctx, integer ip returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_shiftl( _integercontext(a), _integercontext(b) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _ShiftR takes integer ctx, integer ip returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_shiftr( _integercontext(a), _integercontext(b) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _BAnd takes integer ctx, integer ip returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_band( _integercontext(a), _integercontext(b) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _BOr takes integer ctx, integer ip returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_bor( _integercontext(a), _integercontext(b) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _BXor takes integer ctx, integer ip returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_bxor( _integercontext(a), _integercontext(b) )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Neg takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer v2 = Value#_neg(v1)
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Add takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_add(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Sub takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_sub(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Mul takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_mul(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Div takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_div(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _IDiv takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_idiv(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Mod takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_mod(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _Exp takes integer ctx, integer ip returns nothing
    local integer a = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer b = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer r = Value#_exp(_numbercontext(a), _numbercontext(b))
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], r)
endfunction

function _EQ takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_eq(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _NEQ takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_neq(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _GTE takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_gte(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _GT takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_gt(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _LTE takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_lte(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _LT takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_lt(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
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
    local integer new_ctx = Context#_alloc()
    call Context#_init(new_ctx)
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
    //call Print#_print("Leave")
    // stack.pop()
    set _stack_top[interpreter] = List#_next[head]
    set new_old_ctx = _ctx[_stack_top[interpreter]]
    set Context#_ip[new_old_ctx] = Context#_ip[old_ctx]
    call List#_free(head)
    //call Context#_dealloc(ctx) // Can't do. Needs GC
    //call Print#_print("  - "+I2S(ctx)+"("+I2S(old_ctx)+") --> "+I2S(new_old_ctx))

    //call Context#_free(old_ctx) // this is done via GC?
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
	// TODO: this leaks new_ctx freshly allocated _tmps
	set Context#_tmps[new_ctx] = Value#_Int[params] // this might need some refactoring
	set Context#_type[new_ctx] = Context#_Function
	//set Context#_parent_call[new_ctx] = ctx

	// stack.push(new_ctx)
	set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
	set _ctx[_stack_top[interpreter]] = new_ctx
	//call Print#_print("  - "+I2S(ctx)+" --> "+I2S(new_ctx))
    elseif ty == Types#_BuiltInFunction then
	call Builtins#_dispatch_builtin(fn, params, ctx, interpreter)
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
		//set Context#_tmps[new_ctx] = Value#_Int[params] // this might need some refactoring
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

function _Len takes integer ctx, integer ip returns nothing
    local integer reg_target = Ins#_op1[ip]
    local integer val_source = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer val_res    = Value#_len( val_source )

    call Table#_set( Context#_tmps[ctx], reg_target, val_res )
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
	call Print#_error("Target table not of type table but "+I2S(Value#_Type[val_target]))
    endif
    if Value#_Type[val_source] != Types#_Table then
	call Print#_error("Source table not of type table but "+I2S(Value#_Type[val_source]))
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
	call Print#_error("Target table not of type table but "+I2S(Value#_Type[val_target]))
    endif
    if Value#_Type[val_source] != Types#_Table then
	call Print#_error("Target table not of type table but "+I2S(Value#_Type[val_source]))
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
	    set co_value = Builtin/Coroutine#_ctx2value[ base_ctx ]

	    // prepend true to return table
	    set base_context_returntable_value = Table#_get( Value#_Int[co_value], 'retr' )
	    set tmp_table = Table#_alloc()
	    call Table#_append( tmp_table, Value#_Int[base_context_returntable_value], 1 )
	    call Table#_set( tmp_table, 1, Value#_litbool(true) )
	    call Table#_getlist( Value#_Int[base_context_returntable_value], tmp_table, 1 )


	    call Table#_set( Value#_Int[co_value], 'stat', Builtin/Coroutine#_StatusDead )
	endif

	set _stack_top[interpreter] = List#_next[head]
    else
	call Print#_error("  - no correct context found")
    endif
    return ret
endfunction

/// I guess in the future when we handle operators by firstly checking for
/// tables and metatables and such and handle the "native" behaviour in the
/// Value "struct".
function _GetTable takes integer ctx, integer ip returns nothing
    local integer reg	    = Ins#_op1[ip]
    local integer value_tbl = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer value_key = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    call Table#_set( Context#_tmps[ctx], reg, Value#_gettable( value_tbl, value_key))
endfunction

function _SetTable takes integer ctx, integer ip returns nothing
    local integer value_tbl = Table#_get( Context#_tmps[ctx], Ins#_op1[ip] )
    local integer value_key = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer value_val = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )

    call Value#_settable(value_tbl, value_key, value_val)
endfunction

function _Concat takes integer ctx, integer ip, integer interpreter returns nothing
    local integer a = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer b = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    local integer r = Value#_concat( a, b, interpreter )
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], r )
endfunction

function _step takes integer interpreter returns boolean
    local integer ctx = _ctx[_stack_top[interpreter]]
    // TODO: check if in range
    local integer ip = Context#_ip[ctx]
    local integer ins = Ins#_ins[ip]

    //call Print#_print("_step ("+I2S(ctx)+")")

    if ctx == 0 or ip == 0 or ins == 0 then
	call Print#_print("  - stopping via 0")
	return false
    endif

    set Context#_ip[ctx] = Context#_ip[ctx] + 1
    
    //call Print#_print("Executing instruction ("+I2S(ip)+") "+ Ins#_Name[ins]+" ctx("+I2S(ctx)+")")

    // TODO: binsearch
    if ins == Ins#_Not then
	call _Not(ctx, ip)
    elseif ins == Ins#_Complement then
	call _Complement(ctx, ip)
    elseif ins == Ins#_ShiftL then
	call _ShiftL(ctx, ip)
    elseif ins == Ins#_ShiftR then
	call _ShiftR(ctx, ip)
    elseif ins == Ins#_BAnd then
	call _BAnd(ctx, ip)
    elseif ins == Ins#_BOr then
	call _BOr(ctx, ip)
    elseif ins == Ins#_BXor then
	call _BXor(ctx, ip)
    elseif ins == Ins#_Neg then
	call _Neg(ctx, ip)
    elseif ins == Ins#_Add then
	call _Add(ctx, ip)
    elseif ins == Ins#_Len then
	call _Len(ctx, ip)
    elseif ins == Ins#_Sub then
	call _Sub(ctx, ip)
    elseif ins == Ins#_Mul then
	call _Mul(ctx, ip)
    elseif ins == Ins#_Div then
	call _Div(ctx, ip)
    elseif ins == Ins#_IDiv then
	call _IDiv(ctx, ip)
    elseif ins == Ins#_Mod then
	call _Mod(ctx, ip)
    elseif ins == Ins#_Exp then
	call _Exp(ctx, ip)
    elseif ins == Ins#_EQ then
	call _EQ(ctx, ip)
    elseif ins == Ins#_NEQ then
	call _NEQ(ctx, ip)
    elseif ins == Ins#_LTE then
	call _LTE(ctx, ip)
    elseif ins == Ins#_LT then
	call _LT(ctx, ip)
    elseif ins == Ins#_GTE then
	call _GTE(ctx, ip)
    elseif ins == Ins#_GT then
	call _GT(ctx, ip)
    elseif ins == Ins#_Table then
	call _Table(ctx, ip)
    elseif ins == Ins#_Jump then
	call _Jump(ctx, ip)
    elseif ins == Ins#_JumpT then
	call _JumpT(ctx, ip)
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
	call _SetTable(ctx, ip)
    elseif ins == Ins#_GetTable then
	call _GetTable(ctx, ip)
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


function _debug_start_main takes nothing returns nothing
    local integer interpreter = _alloc()
    local integer ctx = Context#_alloc()

    //local integer intp_stack = List#_cons(0)
    //set _current_interpreter[intp_stack] = interpreter

    set _GlobalInterpreter = interpreter

    call Value#_D_move_all( Value#_all_objects, Value#_recycler )
    set Value#_Nil = Value#_new()
    set Value#_Type[Value#_Nil] = Types#_Nil

    call Context#_init(ctx)
    set Context#_ip[ctx] = Ins#_Labels[0]
    set Context#_ret_behaviour[ctx] = _StopInterpreter
    set Context#_type[ctx] = Context#_Function

    set _stack_top[interpreter] = List#_cons(0)
    set _ctx[_stack_top[interpreter]] = ctx

    call Print#_print("_debug_start_main initial context: "+I2S(ctx))


    call Builtins#_register_builtin(ctx, "print", 1)
    call Builtins#_register_builtin(ctx, "setmetatable", 2)
    call Builtins#_register_builtin(ctx, "CreateTrigger", 3)
    call Builtins#_register_builtin(ctx, "TriggerAddAction", 4)
    call Builtins#_register_builtin(ctx, "TriggerRegisterPlayerChatEvent", 4)
    call Builtins#_register_builtin(ctx, "Player", 5)
    call Builtins#_register_builtin(ctx, "GetEventPlayerChatString", 6)
    call Builtins#_register_builtin(ctx, "CreateTimer", 7)
    call Builtins#_register_builtin(ctx, "TimerStart", 8)
    call Builtins#_register_builtin(ctx, "co_create", 9)
    call Builtins#_register_builtin(ctx, "co_resume", 10)
    call Builtins#_register_builtin(ctx, "co_yield", 11)
    call Builtins#_register_builtin(ctx, "TriggerExecute", 12)



    loop
	exitwhen not _step(interpreter)
    endloop
    call Print#_print("reached end of loop")

    //call _free(interpreter)
endfunction

function _call_function takes integer fn, integer params, integer interpreter returns nothing
    local integer ctx = Context#_clone( Value#_Int[fn] )
    local integer ret_table = Table#_get( params, 0 ) // : Value
    local integer tmp
    set Context#_tmps[ctx] = params
    set Context#_type[ctx] = Context#_Function
    set Context#_ret_behaviour[ctx] = _StopInterpreter

    //call Print#_print("_call_function")
    ////call Print#_print("  - previous stack top ctx: "+I2S(_ctx[_stack_top[interpreter]]))
    //call Print#_print("  - new stack top ctx: "+I2S(ctx))
    //call Print#_print("  - chunk name: "+ Value#_String[fn])
    //call Print#_print("  - ret table: "+I2S(ret_table))
    //call Print#_print("  - ret table _Int id: "+I2S(Value#_Int[ret_table]))

    // stack.push(ctx)
    set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
    set _ctx[_stack_top[interpreter]] = ctx

    loop
	exitwhen not _step(interpreter)
    endloop
    //set tmp = Table#_get( Value#_Int[ret_table], 1 )
    //call Print#_print("  - _Int[ret_table][1]: "+I2S(tmp))
    //call Print#_print("  - type thereof: "+I2S(Value#_Type[tmp]))
    //call Print#_print("  - current stack top ctx: "+I2S(_ctx[_stack_top[interpreter]]))
    //call Print#_print("  - returning")
endfunction

function _call_function_wrap takes nothing returns boolean
    call _call_function(Wrap#_Param1, Wrap#_Param2, Wrap#_Param3)
    return true
endfunction

function _init takes nothing returns nothing
    call TriggerAddCondition( Wrap#_WrapAround, Filter( function _call_function_wrap ))
endfunction
