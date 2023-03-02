// scope Interpreter
// REQUIRES Value Ins Context Table Print Builtins

globals
    #include "alloc-globals.j"
    
    // struct interpreter
    integer array _stack_top


    // list _stack
    integer array _ctx
endglobals
#include "alloc.j"


function _Not takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Value#_not(v1)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Complement takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer v2 = Value#_complement(v1)
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Neg takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer v2 = Value#_neg(v1)
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], v2)
endfunction

function _Add takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_add(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _Sub takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_sub(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _Mul takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_mul(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
endfunction

function _Div takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    local integer v2 = Table#_get(Context#_tmps[ctx], Ins#_op3[ip])
    local integer v3 = Value#_div(v1, v2)
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], v3)
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
    // Probably gonna create a LuaTable struct at some point
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
    // TODO: do proper nil
    call StringTable#_set( Context#_locals[ctx], Ins#_string[ip], 0)
endfunction

function _Lambda takes integer ctx, integer ip returns nothing
    local integer new_ctx = Context#_alloc()
    call Context#_init(new_ctx)
    set Context#_parent[new_ctx] = ctx
    set Context#_chunk_name[new_ctx] = Ins#_string[ip]
    set Context#_ip[new_ctx] = Ins#_Labels[ - Ins#_op1[ip] ]
    
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_lambda(new_ctx))
endfunction

function _SetLit takes integer ctx, integer ip returns nothing
    local integer v = Table#_get(Context#_tmps[ctx], Ins#_op1[ip])
    local string name = Ins#_string[ip]
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
    //call Print#_print("  - name = "+name)
    if name == "$params" then
	set v = Value#_table()
	set Value#_Int[v] = Context#_tmps[ctx]
	//call Print#_print("  - $params[1] = "+Value#_tostring(Table#_get( Context#_tmps[ctx], 1 )))
	//call Builtins#_print(Context#_tmps[ctx], 0, 0)
	call Table#_set( Context#_tmps[ctx], reg, v)
    else
	set v = Context#_get( ctx, name )
	call Table#_set( Context#_tmps[ctx], reg, v)
    endif
endfunction

function _LitNil takes integer ctx, integer ip returns nothing
    call Table#_set( Context#_tmps[ctx], Ins#_op1[ip], Value#_litnil() )
endfunction

function _LitString takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local integer value = Value#_litstring( Ins#_string[ip] )
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
    //call List#_free(head)
    //call Print#_print("  - "+I2S(ctx)+"("+I2S(old_ctx)+") --> "+I2S(new_old_ctx))

    //call Context#_free(old_ctx) // this is done via GC?
endfunction

function _Call takes integer ctx, integer ip, integer interpreter returns nothing
    local integer reg_res = Ins#_op1[ip]
    local integer reg_fn = Ins#_op2[ip]
    local integer reg_params = Ins#_op3[ip]
    local integer fn = Table#_get( Context#_tmps[ctx], reg_fn)
    local integer ty = Value#_Type[fn]
    local integer params = Table#_get( Context#_tmps[ctx], reg_params ) // params : Value

    local integer new_ctx

    //call Print#_print("Call")

    if ty == Types#_Lambda then
	set new_ctx = Context#_clone(Value#_Int[fn])
	 // this leaks new_ctx freshly allocated _tmps
	set Context#_tmps[new_ctx] = Value#_Int[params] // this might need some refactoring
	set Context#_parent_call[new_ctx] = ctx

	// stack.push(new_ctx)
	set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
	set _ctx[_stack_top[interpreter]] = new_ctx
	//call Print#_print("  - "+I2S(ctx)+" --> "+I2S(new_ctx))
    elseif ty == Types#_BuiltInFunction then
	call Builtins#_dispatch_builtin(fn, params, ctx, reg_res)
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
    
    loop
	//call Print#_print("  - checking key k = "+ I2S(k))
	if Table#_has( tbl_source, k ) then
	    //call Print#_print("  - table has key k = "+I2S(k))
	    set v = Table#_get( tbl_source, k )
	    //call Print#_print("  - "+Value#_tostring(v))
	    call Table#_set( tbl_target, k - offset +1, Table#_get( tbl_source, k ))
	else
	    exitwhen true
	endif
	set k = k +1
    endloop
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

    loop
	if Table#_has( tbl_source, k ) then
	    call Table#_set( tbl_target, k + offset, Table#_get( tbl_source, k ))
	else
	    exitwhen true
	endif
	set k = k +1
    endloop
endfunction

function _Ret takes integer ctx, integer interpreter returns nothing
    local integer parent_ctx = Context#_parent_call[ctx]
    local integer parent_ip
    local integer return_value
    local integer head = _stack_top[interpreter]
    local integer parent_ctx_check
    //call Print#_print("Ret")
    //call Print#_print("  - head "+I2S(head))

    // stack.pop()
    set _stack_top[interpreter] = List#_next[head]
    set parent_ctx_check = _ctx[_stack_top[interpreter]]
    //call List#_free(head)
    if parent_ctx == 0 then
	//call Print#_print("  - "+ I2S(ctx)+" --> "+I2S(0))
	call I2S(1 / 0) // TODO
	return
    endif
    set parent_ip = Context#_ip[parent_ctx] - 1
    set return_value = Context#_get( ctx, "$ret" )
    //call Builtins#_print(Value#_Int[return_value], 0, 0)
    //call Print#_print("  - "+ I2S(ctx)+" --> "+I2S(parent_ctx)+" ("+I2S(parent_ctx_check)+")")
    //call Print#_print("  - parent_ip = "+ I2S(parent_ip))
    //call Print#_print("  - return_value = "+ I2S(Value#_Int[return_value]))

    call Table#_set( Context#_tmps[parent_ctx], Ins#_op1[parent_ip], return_value )
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

function _step takes integer interpreter returns boolean
    local integer ctx = _ctx[_stack_top[interpreter]]
    // TODO: check if in range
    local integer ip = Context#_ip[ctx]
    local integer ins = Ins#_ins[ip]

    //call Print#_print("_step ("+I2S(ctx)+")")

    if ctx == 0 or ip == 0 or ins == 0 then
	//call Print#_print("out of gas")
	return false
    endif

    set Context#_ip[ctx] = Context#_ip[ctx] + 1
    
    //call Print#_print("Executing instruction ("+I2S(ip)+") "+ Ins#_Name[ins])

    // TODO: binsearch
    if ins == Ins#_Not then
	call _Not(ctx, ip)
    elseif ins == Ins#_Complement then
	call _Complement(ctx, ip)
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
	call _Ret(ctx, interpreter)
    elseif ins == Ins#_SetTable then
	call _SetTable(ctx, ip)
    elseif ins == Ins#_GetTable then
	call _GetTable(ctx, ip)
    elseif ins == Ins#_Append then
	call _Append(ctx, ip)
    elseif ins == Ins#_GetList then
	call _GetList(ctx, ip)
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

    call Context#_init(ctx)
    set Context#_ip[ctx] = Ins#_Labels[0]

    call Builtins#_register_builtin(ctx, "print", 1)

    set _stack_top[interpreter] = List#_cons(0)
    set _ctx[_stack_top[interpreter]] = ctx

    loop
	exitwhen not _step(interpreter)
    endloop
    call Print#_print("reached end of loop")

    call _free(interpreter)
endfunction
