// scope Builtin/Timer
// REQUIRES Print Value Table Jass Natives

globals
    // Table
    integer _timer2value
endglobals

function _CreateTimer takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer v = Value#_table()
    local timer t = CreateTimer()

    local integer return_table = Table#_get( tbl, 0 )

    //call Print#_print("_CreateTimer")
    //call Print#_print("  - timer obj: "+I2S(v))

    call Table#_set( Value#_Int[v], 'type', Jass#_timer )
    call Table#_set( Value#_Int[v], 'intp', interpreter )

    set Natives#_value2timer[v] = t
    call Table#_set( _timer2value, GetHandleId(t), v )

    call Table#_set( Value#_Int[return_table], 1, v )
endfunction

function _execute_timer_action takes nothing returns nothing
    local timer t = GetExpiredTimer()
    local integer timer_value = Table#_get( _timer2value, GetHandleId(t) )
    local integer ret_value = Value#_table() // not used
    local integer fn_value = Table#_get( Value#_Int[timer_value], 'func' )
    local integer interpreter = Table#_get( Value#_Int[timer_value], 'intp' )

    //call Print#_print("_execute_timer_action")

    call Call#_call0( fn_value, ret_value, interpreter )
endfunction

function _TimerStart takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer timer_value = Table#_get(tbl, 1)
    local integer real_value = Table#_get(tbl, 2)
    local integer bool_value = Table#_get(tbl, 3)
    local integer fn_value = Table#_get(tbl, 4)

    call Table#_set( Value#_Int[timer_value], 'func', fn_value )

    call TimerStart( Natives#_convert2timer(timer_value, interpreter), Value#_2real(real_value, interpreter), Value#_2boolean(bool_value, interpreter), function _execute_timer_action )

endfunction

function _init takes nothing returns nothing
    set _timer2value = Table#_alloc()
endfunction
