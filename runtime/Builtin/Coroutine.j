// scope Builtin/Coroutine
// REQUIRES Print Table Context Value Error

globals
    integer array _ctx2value
    constant integer _StatusSuspended = 1
    constant integer _StatusDead = 2

    integer array _interpreter

    // _base_frame : ListEntry into Interpreter#_ctx
    integer array _base_frame
    // _stop_frame : ListEntry into Interpreter#_ctx
    integer array _stop_frame

    integer array _status

    // _return_yield : Table
    integer array _return_yield

    // _return_resume : Value
    integer array _return_resume
endglobals

function _create takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer fn_value = Table#_get( tbl, 1 ) // _Lambda
    local integer returntable_value = Table#_get( tbl, 0 )
    local integer co_value = Value#_coroutine() 
    local integer new_ctx = Context#_clone( Value#_Int[fn_value] )

    local integer base_frame = List#_cons( 0 )

    //call Print#_print("_co_create")
    //call Print#_print("  - base ctx "+I2S(new_ctx))

    set Interpreter#_ctx[base_frame] = new_ctx

    set Context#_ret_behaviour[new_ctx] = Interpreter#_CoroutineYield
    set Context#_type[new_ctx] = Context#_Coroutine

    set _interpreter[co_value]	= interpreter
    set _base_frame[co_value]	= base_frame
    set _stop_frame[co_value]	= base_frame
    set _status[co_value]	= _StatusSuspended
    set _return_yield[co_value]	= Context#_tmps[new_ctx]
    set _return_resume[co_value]= 0

    call Table#_set( Value#_Int[returntable_value], 1, co_value )

    set _ctx2value[new_ctx] = co_value
endfunction

function _yield takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer returntable_value = Table#_get( tbl, 0 )

    local integer head = Interpreter#_stack_top[interpreter]
    local integer stop_point = head
    local integer co_value
    local integer resume_returntable_value
    //call Print#_print("_co_yield")
    // find nearest context with _ret_behaviour == _CoroutineYield
    loop
        exitwhen head == 0
        exitwhen Context#_ret_behaviour[Interpreter#_ctx[head]] == Interpreter#_CoroutineYield

        set head = List#_next[head]
    endloop

    if head != 0 then
        //call Print#_print("  - found something")
	set co_value = _ctx2value[Interpreter#_ctx[head]]

	set _stop_frame[co_value]   = stop_point
	set _return_yield[co_value] = Value#_Int[returntable_value]
	set resume_returntable_value = _return_resume[co_value]

	call Table#_append( Value#_Int[resume_returntable_value], tbl, 1 )

        set Interpreter#_stack_top[interpreter] = List#_next[head]
    else
        call Error#_error_str("attempt to yield from outside a coroutine")
    endif
    
endfunction

function _resume takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer returntable_value = Table#_get( tbl, 0 )
    local integer co_value = Table#_get( tbl, 1 )
    local integer co_status_int = _status[co_value]
    local integer base_frame = _base_frame[co_value]
    local integer stop_frame = _stop_frame[co_value]
    local integer yield_returntable_table = _return_yield[co_value]

    local integer base_ctx = Interpreter#_ctx[base_frame]

    //call Print#_print("_co_resume")
    //call Print#_print("  - yield table "+I2S(yield_returntable_table))
    //call Print#_print("  - with returntable "+I2S(returntable_value))
    //call Print#_print("  - base ctx "+I2S(base_ctx))


    call Table#_set( yield_returntable_table, 0, returntable_value )
    call Table#_getlist( yield_returntable_table, tbl, 2 )

    if co_status_int == _StatusSuspended then
	call Table#_set( Value#_Int[returntable_value], 1, Value#_litbool(true) )

	// set both return table for the next yield and the contexts return
	// table for possible returns
	set _return_resume[co_value] = returntable_value
	call StringTable#_set( Context#_locals[base_ctx], "$ret", returntable_value )

	set List#_next[base_frame] = Interpreter#_stack_top[interpreter]
	set Interpreter#_stack_top[interpreter] = stop_frame
    else
	call Table#_set( Value#_Int[returntable_value], 1, Value#_litbool(false) )
	call Table#_set( Value#_Int[returntable_value], 2, Value#_litstring("cannot resume dead coroutine") )
    endif
endfunction


function _register takes integer ctx returns nothing
    local integer coroutine_table = Value#_table()
    call Value#_settable( coroutine_table, Value#_litstring("create"), Context#_get(ctx, "$co_create") )
    call Value#_settable( coroutine_table, Value#_litstring("yield"), Context#_get(ctx, "$co_yield") )
    call Value#_settable( coroutine_table, Value#_litstring("resume"), Context#_get(ctx, "$co_resume") )

    call Context#_set( ctx, "coroutine", coroutine_table )
endfunction

