// scope Builtin/Trigger
// REQUIRES Print Value Table Jass 

globals
    // GC root
    integer _cnt = 0
    integer array _value2index
    integer array _trigger

    // Table
    integer _trigger2value
    // List
    integer array _trigger_actions
    // List
    integer array _trigger_conditions

    // Indexed by _trigger_actions
    integer array _trigger_action
endglobals

// TODO: conditions
// TODO: bunch of checks
// TODO: reverse actions
function _trigger_execute_all_actions takes nothing returns nothing
    local integer trigger_value = Table#_get( _trigger2value, GetHandleId(GetTriggeringTrigger()) )
    local integer ls = _trigger_actions[trigger_value]
    local integer interpreter = Value#_Int2[trigger_value]
    local integer ret = Value#_table() // not used
    local integer fn_value

    loop
    exitwhen ls == 0
	set fn_value = Value#_Int2[_trigger_action[ls]]
	call Call#_call0( fn_value, ret, interpreter )
	set ls = List#_next[ls]
    endloop

endfunction

function _CreateTrigger takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer v = Value#_foreign(Jass#_trigger)
    local trigger t = CreateTrigger()

    local integer return_table = Table#_get( tbl, 0 )

    set _cnt = _cnt +1
    set _trigger[_cnt] = v
    set _value2index[v] = _cnt

    set Value#_Int2[v] = interpreter

    set Natives#_value2trigger[v] = t
    call Table#_set( _trigger2value, GetHandleId(t), v )

    //call TriggerAddCondition( t, Condition( function _ // TODO: not sure if this is not better handled totally custom
    call TriggerAddAction( t, function _trigger_execute_all_actions )


    call Table#_set( Value#_Int[return_table], 1, v )
    set t = null
endfunction

function _TriggerAddAction takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer trigger_value = Table#_get(tbl, 1)
    local integer fn_value = Table#_get(tbl, 2)
    local integer returntable_value = Table#_get(tbl, 0)

    local integer triggeraction_value = Value#_foreign(Jass#_triggeraction)

    local integer ls = _trigger_actions[trigger_value]

    set ls = List#_cons(ls)
    set _trigger_action[ls] = triggeraction_value
    set _trigger_actions[trigger_value] = ls

    set Value#_Int2[triggeraction_value] = fn_value

    call Table#_set( Value#_Int[returntable_value], 1, triggeraction_value )
endfunction


function _init takes nothing returns nothing
    set _trigger2value = Table#_alloc()
endfunction
