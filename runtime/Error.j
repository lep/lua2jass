// scope Error
// REQUIRES Print Value

globals
    integer _lastErrorValue
    integer _inProtectedCall = 0
endglobals

function _getLastErrorValue takes nothing returns integer
    local integer i = _lastErrorValue
    set _lastErrorValue = 0
    return i
endfunction

function _error takes integer v returns nothing
    set _lastErrorValue = v
    if _inProtectedCall == 0 then
        if Value#_Type[v] == Jass#_string then
            call Print#_error("error object not a string")
        else
            call Print#_error(Value#_String[v])
        endif
    endif
    call I2S(1 / 0)
endfunction

function _error_str takes string s returns nothing
    call _error(Value#_litstring(s))
endfunction
