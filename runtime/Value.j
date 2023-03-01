// scope Value
// REQUIRES Table Types List

globals
    #include "alloc-globals.j"

    integer array _Int
    real array    _Real
    string array  _String
    boolean array _Bool
    integer array _Table
    integer array _Type

    integer array _Int2

    // List
    integer array _val
    integer array _key
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
    //call Print#_print("_add("+I2S(_Int[a])+","+I2S(_Int[b])+")")
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
function _litfloat takes real a returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Real
    set _Real[new] = a
    return new
endfunction

// @alloc
function _litbool takes boolean a returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = a
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
    set _Int2[new] = Table#_alloc()
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
function _rawequal_noalloc takes integer a, integer b returns boolean
    local integer type_a = _Type[a]
    local integer type_b = _Type[b]
    if type_a == Types#_Int and type_b == Types#_Int then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_Real and type_b == Types#_Real then
	// TODO: see if wc3 lua has the same quirky behavior for
	// real comparison
	return not (_Real[a] != _Real[b]) 
    elseif type_a == Types#_Int and type_b == Types#_Real then
	return _Int[a] == R2I(_Real[b])
    elseif type_b == Types#_Int and type_a == Types#_Real then
	return _Int[b] == R2I(_Real[a])
    elseif type_a == Types#_Bool and type_b == Types#_Bool then
	return _Bool[a] == _Bool[b]
    elseif type_a == Types#_String and type_b == Types#_String then
	return _String[a] == _String[b]
    elseif type_a == Types#_Table and type_b == Types#_Table then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_Lambda and type_b == Types#_Lambda then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_BuiltInFunction and type_b == Types#_BuiltInFunction then
	return _String[a] == _String[b]
    else
	return false
    endif
endfunction

// @noalloc
function _hash takes integer v returns integer
    local integer ty = _Type[v]
    local integer i
    if ty == Types#_String then
	return StringHash(_String[v])
    elseif ty == Types#_Table then
	return _Int[v] * 23 + 1337
    elseif ty == Types#_Real then
	set i = R2I(_Real[v])
	if _Real[v] == i then
	    return i * 23 + 5
	else
	    return R2I( _Real[v] * 16180.33 )
	endif
    elseif ty == Types#_Lambda then
	return _Int[v]
    elseif ty == Types#_BuiltInFunction then // TODO: we want to use IDs anyway i think
	return StringHash(_String[v])
    elseif ty == Types#_Bool then
	if _Bool[v] then
	    return 0x11111111
	else
	    return 0xffffffff
	endif
    else
	return 0
    endif
endfunction

// @noalloc
function _settable takes integer t, integer k, integer v returns nothing
    local integer ty = _Type[k]
    local integer tbl
    local integer ls
    local integer prev

    //call Print#_print("_settable")

    if _Type[t] != Types#_Table then
	call Print#_error("Expected table but got "+I2S(_Type[t]))
    endif

    if ty == Types#_Int then
	//call Print#_print("  - int key")
	call Table#_set( _Int[t], _Int[k], v )
    elseif ty == Types#_Real and _Real[k] == R2I(_Real[k]) then
	//call Print#_print("  - real type but int key")
	call Table#_set( _Int[t], R2I(_Real[k]), v )
    else
	//call Print#_print("  - key of type "+I2S(ty))
	set tbl = _Int2[t]
	set ls = Table#_get( tbl, _hash(k) )
	set prev = ls
	loop
	exitwhen ls == 0
	    //call Print#_print("  - "+I2S(ls))
	    if _rawequal_noalloc(_key[ls], k) then
		set _val[ls] = v
		//return _val[ls]
		return
	    endif
	    set prev = ls
	    set ls = List#_next[ls]
	endloop
	set prev = List#_cons(prev)
	set _key[prev] = k
	set _val[prev] = v
	call Table#_set( tbl, _hash(k), prev )
    endif
endfunction


// @noalloc
function _gettable takes integer v, integer k returns integer
    local integer ty = _Type[k]
    local integer tbl
    local integer ls
    //call Print#_print("_gettable")
    if ty == Types#_Int then
	//call Print#_print("  - int key")
	return Table#_get( _Int[v], _Int[k] )
    elseif ty == Types#_Real and _Real[k] == R2I(_Real[k]) then
	//call Print#_print("  - real type but int key")
	return Table#_get( _Int[v], R2I(_Real[k]) )
    else
	//call Print#_print("  - key of type "+I2S(ty))
	set tbl = _Int2[v]
	set ls = Table#_get( tbl, _hash(k) )
	loop
	exitwhen ls == 0
	    //call Print#_print("  - " +I2S(ls))
	    if _rawequal_noalloc(_key[ls], k) then
		return _val[ls]
	    endif
	    set ls = List#_next[ls]
	endloop
	return 0 // TODO: real nil
    endif
endfunction

function _len takes integer v returns integer
    local integer ty = _Type[v]
    local integer k = 1
    local integer tbl
    //call Print#_print("_len")
    if ty == Types#_String then
	return Value#_litint(StringLength(_String[v]))
    elseif ty == Types#_Table then
	set tbl = _Int[v]
	loop
	    //call Print#_print("  - checking key k = "+ I2S(k))
	    if Table#_has( tbl, k ) then
		//call Print#_print("  - v = "+_tostring(Table#_get(tbl, k)))
		set k = k +1
	    else
		//call Print#_print("  - table does not have key, returning k = "+I2S(k))
		return _litint(k)
	    endif
	endloop
    endif
    return Value#_litint(0) // TODO
endfunction
