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

function _Table takes integer ctx, integer ip returns nothing
    // Probably gonna create a LuaTable struct at some point
    call Table#_set(Context#_tmps[ctx], Ins#_op1[ip], Value#_table() )
endfunction


function _Jump takes integer ctx, integer ip returns nothing
    set Context#_ip[ctx] = Ins#_op1[ip]
endfunction

function _JumpT takes integer ctx, integer ip returns nothing
    local integer v1 = Table#_get(Context#_tmps[ctx], Ins#_op2[ip])
    if Value#_truthy(v1) then
	set Context#_ip[ctx] = Ins#_op1[ip]
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

function _GetLit takes integer ctx, integer ip returns nothing
    local integer reg = Ins#_op1[ip]
    local string name = Ins#_string[ip]
    local integer v = Context#_get( ctx, name )
    //call Print#_print("from name "+name+" got value "+I2S(v)+" of type "+I2S(Value#_Type[v])+" and gonna store it in reg "+I2S(reg))
    call Table#_set( Context#_tmps[ctx], reg, v)
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

function _Enter takes integer ctx, integer interpreter returns nothing
    local integer new_ctx = Context#_clone(ctx)
    set _stack_top[interpreter] = List#_cons(_stack_top[interpreter])
    set _ctx[_stack_top[interpreter]] = new_ctx
endfunction

function _Leave takes integer ctx, integer interpreter returns nothing
    local integer head = _stack_top[interpreter]
    local integer old_ctx = _ctx[head]
    local integer new_old_ctx
    set _stack_top[interpreter] = List#_next[head]
    set new_old_ctx = _ctx[_stack_top[interpreter]]
    set Context#_ip[new_old_ctx] = Context#_ip[old_ctx]
    call List#_free(head)
    //call Context#_free(old_ctx) // this is done via GC?
endfunction

function _Call takes integer ctx, integer ip returns nothing
    local integer reg_res = Ins#_op1[ip]
    local integer reg_fn = Ins#_op2[ip]
    local integer reg_params = Ins#_op3[ip]
    local integer fn = Table#_get( Context#_tmps[ctx], reg_fn)
    local integer ty = Value#_Type[fn]
    local integer params = Table#_get( Context#_tmps[ctx], reg_params )

    if ty == Types#_Lambda then
	call Print#_print("Cannot call lambda yet")
    elseif ty == Types#_BuiltInFunction then
	call Builtins#_dispatch_builtin(fn, params, ctx, reg_res)
    else
	call Print#_print("Cannot call object of type "+I2S(ty))
    endif
endfunction

function _Ret takes integer ctx, integer interpreter returns nothing
    local integer parent_ctx = Context#_parent_call[ctx]
    local integer parent_ip
    local integer return_value

    set _stack_top[interpreter] = List#_next[_stack_top[interpreter]]
    if parent_ctx == 0 then
	return
    endif
    set parent_ip = Context#_ip[parent_ctx]
    set return_value = Context#_get( ctx, "$_ret" )

    call Table#_set( Context#_tmps[parent_ctx], Ins#_op1[parent_ip], return_value )
endfunction


function _SetTable takes integer ctx, integer ip returns nothing
    local integer value_tbl = Table#_get( Context#_tmps[ctx], Ins#_op1[ip] )
    local integer value_key = Table#_get( Context#_tmps[ctx], Ins#_op2[ip] )
    local integer value_val = Table#_get( Context#_tmps[ctx], Ins#_op3[ip] )
    //call Print#_print("Interpreter#_SetTable(...)")
    //call Print#_print("  - "+I2S(Ins#_op1[ip])+","+I2S(Ins#_op2[ip])+","+I2S(Ins#_op3[ip]))
    //call Print#_print("  - "+I2S(value_tbl)+","+I2S(value_key)+","+I2S(value_val))

    call Value#_settable(value_tbl, value_key, value_val)
endfunction

function _step takes integer interpreter returns boolean
    local integer ctx = _ctx[_stack_top[interpreter]]
    // TODO: check if in range
    local integer ip = Context#_ip[ctx]
    local integer ins = Ins#_ins[ip]

    if ctx == 0 or ip == 0 or ins == 0 then
	return false
    endif

    set Context#_ip[ctx] = Context#_ip[ctx] + 1
    
    call Print#_print("Executing instruction "+ Ins#_Name[ins])

    // TODO: binsearch
    if ins == Ins#_Not then
	call _Not(ctx, ip)
    elseif ins == Ins#_Add then
	call _Add(ctx, ip)
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
    elseif ins == Ins#_Local then
	call _Local(ctx, ip)
    elseif ins == Ins#_Lambda then
	call _Lambda(ctx, ip)
    elseif ins == Ins#_SetLit then
	call _SetLit(ctx, ip)
    elseif ins == Ins#_GetLit then
	call _GetLit(ctx, ip)
    elseif ins == Ins#_LitString then
	call _LitString(ctx, ip)
    elseif ins == Ins#_LitInt then
	call _LitInt(ctx, ip)
    elseif ins == Ins#_Call then
	call _Call(ctx, ip)
    elseif ins == Ins#_Ret then
	call _Ret(ctx, ip)
    elseif ins == Ins#_SetTable then
	call _SetTable(ctx, ip)
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
    //call Print#_print("Starting at ip "+I2S(Ins#_Labels[0]))

    call Builtins#_register_builtin(ctx, "print", 1)

    set _stack_top[interpreter] = List#_cons(0)
    set _ctx[_stack_top[interpreter]] = ctx

    loop
	exitwhen not _step(interpreter)
    endloop

    call _free(interpreter)
endfunction
