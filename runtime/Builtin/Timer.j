// scope Builtin/Timer
// REQUIRES Print Value Table Jass Natives Call

globals
    // GC Root
    integer _cnt = 0
    integer array _value2index
    integer array _timer

    // Table
    integer _timer2value
endglobals


function _CreateTimer takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local integer v = Value#_foreign(Jass#_timer)
    local timer t = CreateTimer()

    set _cnt = _cnt +1
    set _timer[_cnt] = v
    set _value2index[v] = _cnt


    set Value#_Int2[v] = interpreter
    set Natives#_value2timer[v] = t
    call Table#_set( _timer2value, GetHandleId(t), v )

    call Table#_set( Value#_Int[return_table], 1, v )
endfunction

function _GetExpiredTimer takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local timer t = GetExpiredTimer()
    local integer v = Table#_get( _timer2value, GetHandleId(t) )
    call Table#_set( Value#_Int[return_table], 1, v )
endfunction

function _DestroyTimer takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer timer_value = Table#_get(tbl, 1)
    local integer idx = _value2index[timer_value]

    set Natives#_value2timer[timer_value] = null
    set _timer[idx] = _timer[_cnt]
    set _cnt = _cnt -1

    call DestroyTimer( Natives#_convert2timer(timer_value, interpreter) )
endfunction

function _execute_timer_action takes nothing returns nothing
    local timer t = GetExpiredTimer()
    local integer timer_value = Table#_get( _timer2value, GetHandleId(t) )
    local integer ret_value = Value#_table() // not used
    local integer interpreter = Value#_Int2[timer_value]
    local integer fn_value = Value#_Int3[timer_value]

    call Call#_call0( fn_value, ret_value, interpreter )
endfunction

function _TimerStart takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer timer_value = Table#_get(tbl, 1)
    local integer real_value = Table#_get(tbl, 2)
    local integer bool_value = Table#_get(tbl, 3)
    local integer fn_value = Table#_get(tbl, 4)

    set Value#_Int3[timer_value] = fn_value

    call TimerStart( Natives#_convert2timer(timer_value, interpreter), Value#_2real(real_value, interpreter), Value#_2boolean(bool_value, interpreter), function _execute_timer_action )

endfunction

function _init takes nothing returns nothing
    set _timer2value = Table#_alloc()
endfunction
