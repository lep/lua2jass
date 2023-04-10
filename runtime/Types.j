// scope Types

globals
    constant integer _Foreign		= -3
    constant integer _Coroutine		= -4
    constant integer _Lambda		= -5
    constant integer _Table		= -6
    constant integer _Nil		= -7
    constant integer _BuiltInFunction	= -8

    constant integer _offset = 23

    string array _Names

    boolean _init_done = false
endglobals

function _getName takes integer ty returns string
    if not _init_done then
        set _Names[_offset + _Foreign] = "userdata"
        set _Names[_offset + _Coroutine] = "thread"
        set _Names[_offset + _Lambda] = "function"
        set _Names[_offset + _Table] = "table"
        set _Names[_offset + _Nil] = "nil"
        set _Names[_offset + _BuiltInFunction] = "function"
        set _Names[_offset + Jass#_string] = "string"
        set _Names[_offset + Jass#_boolean] = "boolean"
        set _Names[_offset + Jass#_integer] = "number"
        set _Names[_offset + Jass#_real] = "number"
        set _init_done = true
    endif
    return _Names[ _offset + ty ]
endfunction

