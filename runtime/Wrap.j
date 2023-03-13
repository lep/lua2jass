// scope Wrap
// REQUIRES 
globals
    trigger _WrapAround
    integer _Param1
    integer _Param2
    integer _Param3
    integer _Ret
endglobals

function _call_function takes integer v, integer params, integer interpreter returns nothing
    set _Param1 = v
    set _Param2 = params
    set _Param3 = interpreter
    call TriggerEvaluate(_WrapAround)
endfunction

function _init takes nothing returns nothing
    set _WrapAround = CreateTrigger()
endfunction

