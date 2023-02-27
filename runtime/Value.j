// scope Value
// REQUIRES Table Types

globals
    #include "alloc-globals.j"

    integer array _Int
    real array    _Real
    string array  _String
    boolean array _Bool
    integer array _Table
    integer array _Type
endglobals

#include "alloc.j"


// @alloc
function _not takes integer v returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = not _Bool[v]
    return new
endfunction

// @alloc
function _neg takes integer v returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = - _Int[v]
    return new
endfunction

// @alloc
function _complement takes integer v returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = - _Int[v] -1
    return new
endfunction

// @alloc
function _add takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = _Int[a] + _Int[b]
    return new
endfunction

// @alloc
function _sub takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = _Int[a] - _Int[b]
    return new
endfunction

// @alloc
function _mul takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = _Int[a] * _Int[b]
    return new
endfunction

// @alloc
function _div takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = _Int[a] / _Int[b]
    return new
endfunction

// @alloc
function _gt takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] > _Int[b]
    return new
endfunction

// @alloc
function _gte takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] >= _Int[b]
    return new
endfunction

// @alloc
function _lt takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] < _Int[b]
    return new
endfunction

// @alloc
function _lte takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] <= _Int[b]
    return new
endfunction

// @alloc
function _eq takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] == _Int[b]
    return new
endfunction

// @alloc
function _neq takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _Int[a] != _Int[b]
    return new
endfunction


// @alloc
function _litint takes integer a returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Int
    set _Int[new] = a
    return new
endfunction

// @alloc
function _litstring takes string a returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_String
    set _String[new] = a
    return new
endfunction

// @alloc
function _table takes nothing returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Table
    set _Int[new] = Table#_alloc()
    return new
endfunction


// @alloc
function _lambda takes integer a returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Lambda
    set _Int[new] = a
    return new
endfunction

// @alloc
function _builtin takes string f returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_BuiltInFunction
    set _String[new] = f
    return new
endfunction

// @noalloc
function _truthy takes integer a returns boolean
    return (_Type[a] == Types#_Int and _Int[a] != 0) or (_Type[a] == Types#_String and _String[a] != "") or (_Type[a] == Types#_Bool and _Bool[a])
endfunction


// @noalloc
function _tostring takes integer v returns string
    local integer ty = _Type[v]
    if ty == Types#_Int then
	return I2S(_Int[v])
    elseif ty == Types#_String then
	return _String[v]
    elseif ty == Types#_Lambda then
	return "Fun@"+ I2S(_Int[v])
    elseif ty == Types#_BuiltInFunction then
	return "Native@" + I2S(_Int[v])
    elseif ty == Types#_Table then
	return "Table@" + I2S(_Int[v])
    else
	return "unknown"
    endif
endfunction

// @noalloc
function _settable takes integer tbl, integer k, integer v returns nothing
    // only integer indizes for now.
    // this will require a LuaTable struct in the future
    //call Print#_print("Value#_settable("+I2S(tbl)+","+I2S(k)+","+I2S(v)+")")
    //call Print#_print("  - key is "+ I2S(_Int[k]))
    call Table#_set( _Int[tbl], _Int[k], v )
endfunction
