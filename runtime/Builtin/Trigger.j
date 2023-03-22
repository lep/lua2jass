// scope Builtin/Trigger
// REQUIRES Print Value Table Jass 

globals
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

    //call Print#_print("_trigger_execute_all_actions()")
    //call Print#_print("  - trigger lua obj: "+I2S(trigger_value))
    //call Print#_print("  - interpreter: "+I2S(interpreter))


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

    //call Print#_print("_CreateTrigger()")
    //call Print#_print("  - trigger lua obj: "+I2S(v))
    //call Print#_print("  - interpreter: "+I2S(interpreter))
    //call Print#_print("  - trigger obj: "+I2S(GetHandleId(t)))

    set Value#_Int2[v] = interpreter

    set Natives#_value2trigger[v] = t
    call Table#_set( _trigger2value, GetHandleId(t), v )

    //call TriggerAddCondition( t, Condition( function _ // TODO: not sure if this is not better handled totally custom
    call TriggerAddAction( t, function _trigger_execute_all_actions )


    call Table#_set( Value#_Int[return_table], 1, v )
    //call Print#_print("  - done")


    set t = null
endfunction

function _TriggerAddAction takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer trigger_value = Table#_get(tbl, 1)
    local integer fn_value = Table#_get(tbl, 2)
    local integer returntable_value = Table#_get(tbl, 0)

    local integer triggeraction_value = Value#_foreign(Jass#_triggeraction)

    local integer ls = _trigger_actions[trigger_value]
    //call Print#_print("_TriggerAddAction")
    //call Print#_print("  - lua trigger obj: "+I2S(

    set ls = List#_cons(ls)
    set _trigger_action[ls] = triggeraction_value
    set _trigger_actions[trigger_value] = ls

    set Value#_Int2[triggeraction_value] = fn_value

    call Table#_set( Value#_Int[returntable_value], 1, triggeraction_value )
    //call Print#_print("  - done")

endfunction


function _init takes nothing returns nothing
    set _trigger2value = Table#_alloc()
endfunction
