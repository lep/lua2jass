// scope Builtin/Coroutine
// REQUIRES Print Table Context Value

globals
    // coroutines
    integer array _ctx2value // for coroutine usage
    constant integer _StatusSuspended = 1
    constant integer _StatusDead = 2
endglobals

function _create takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer fn_value = Table#_get( tbl, 1 ) // _Lambda
    local integer returntable_value = Table#_get( tbl, 0 )
    local integer co_value = Value#_table() 
    local integer new_ctx = Context#_clone( Value#_Int[fn_value] )

    local integer base_frame = List#_cons( 0 )

    //call Print#_print("_co_create")
    //call Print#_print("  - base ctx "+I2S(new_ctx))

    set Interpreter#_ctx[base_frame] = new_ctx

    set Context#_ret_behaviour[new_ctx] = Interpreter#_CoroutineYield
    set Context#_type[new_ctx] = Context#_Coroutine

    // With this amount of data we might put it into arrays
    call Table#_set( Value#_Int[co_value], 'type', 'crtn' )
    call Table#_set( Value#_Int[co_value], 'intp', interpreter )
    call Table#_set( Value#_Int[co_value], 'cntx', new_ctx )
    call Table#_set( Value#_Int[co_value], 'base', base_frame )
    call Table#_set( Value#_Int[co_value], 'stop', base_frame )
    call Table#_set( Value#_Int[co_value], 'stat', _StatusSuspended )
    call Table#_set( Value#_Int[co_value], 'rety', Context#_tmps[new_ctx] )

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
	call Table#_set( Value#_Int[co_value], 'stop', stop_point )
	call Table#_set( Value#_Int[co_value], 'rety', Value#_Int[returntable_value] )

	set resume_returntable_value = Table#_get( Value#_Int[co_value], 'retr' )
	call Table#_append( Value#_Int[resume_returntable_value], tbl, 1 )

        set Interpreter#_stack_top[interpreter] = List#_next[head]
    else
        call Print#_error("  - no coroutine root found")
    endif
    
endfunction

function _resume takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer returntable_value = Table#_get( tbl, 0 )
    local integer co_value = Table#_get( tbl, 1 )
    local integer co_status_int = Table#_get( Value#_Int[co_value], 'stat' )

    local integer base_frame = Table#_get( Value#_Int[co_value], 'base' )
    local integer stop_frame = Table#_get( Value#_Int[co_value], 'stop' )
    local integer yield_returntable_table = Table#_get( Value#_Int[co_value], 'rety' )

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
	call Table#_set( Value#_Int[co_value], 'retr', returntable_value )
	call StringTable#_set( Context#_locals[base_ctx], "$ret", returntable_value )

	set List#_next[base_frame] = Interpreter#_stack_top[interpreter]
	set Interpreter#_stack_top[interpreter] = stop_frame
    else
	call Table#_set( Value#_Int[returntable_value], 1, Value#_litbool(false) )
	call Table#_set( Value#_Int[returntable_value], 2, Value#_litstring("cannot resume dead coroutine") )
    endif
endfunction

