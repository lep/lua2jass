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


    // constant
    integer _Nil


    //

    boolean _error
endglobals

#include "alloc.j"

// @noalloc
function _litnil takes nothing returns integer
    return _Nil
endfunction


// @alloc
function _neg takes integer v returns integer
    local integer new = _alloc()
    local integer ty = _Type[v]
    if ty == Types#_Int then
	set _Type[new] = Types#_Int
	set _Int[new] = - _Int[v]
    elseif ty == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = - _Real[v]
    else
	call Print#_error("_neg: should not happen")
    endif
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
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Type[new] = Types#_Int
	set _Int[new] = _Int[a] + _Int[b]
    elseif ty_a == Types#_Int  and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Int[a] + _Real[b]
    elseif ty_b == Types#_Int  and ty_a == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] + _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] + _Real[b]
    else
	call Print#_error("_add: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _sub takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Type[new] = Types#_Int
	set _Int[new] = _Int[a] - _Int[b]
    elseif ty_a == Types#_Int  and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Int[a] - _Real[b]
    elseif ty_b == Types#_Int  and ty_a == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] - _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] - _Real[b]
    else
	call Print#_error("_sub: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _mul takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Type[new] = Types#_Int
	set _Int[new] = _Int[a] * _Int[b]
    elseif ty_a == Types#_Int  and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Int[a] * _Real[b]
    elseif ty_b == Types#_Int  and ty_a == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] * _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] * _Real[b]
    else
	call Print#_error("_mul: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _div takes integer a, integer b returns integer
    // always real division
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Type[new] = Types#_Real
	set _Real[new] = I2R(_Int[a]) / _Int[b]
    elseif ty_a == Types#_Int  and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Int[a] / _Real[b]
    elseif ty_b == Types#_Int  and ty_a == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] / _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Type[new] = Types#_Real
	set _Real[new] = _Real[a] / _Real[b]
    else
	call Print#_error("_div: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _gt takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]
    set _Type[new] = Types#_Bool

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Bool[new] = _Int[a] > _Int[b]
    elseif ty_a == Types#_Int and ty_b == Types#_Real then
	set _Bool[new] = _Int[a] > _Real[b]
    elseif ty_b == Types#_Int and ty_a == Types#_Real then
	set _Bool[new] = _Real[a] > _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Bool[new] = _Real[a] > _Real[b]
    else
	call Print#_error("_gt: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _gte takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]
    set _Type[new] = Types#_Bool

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Bool[new] = _Int[a] >= _Int[b]
    elseif ty_a == Types#_Int and ty_b == Types#_Real then
	set _Bool[new] = _Int[a] >= _Real[b]
    elseif ty_b == Types#_Int and ty_a == Types#_Real then
	set _Bool[new] = _Real[a] >= _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Bool[new] = _Real[a] >= _Real[b]
    else
	call Print#_error("_gte: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _lt takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]
    set _Type[new] = Types#_Bool

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Bool[new] = _Int[a] < _Int[b]
    elseif ty_a == Types#_Int and ty_b == Types#_Real then
	set _Bool[new] = _Int[a] < _Real[b]
    elseif ty_b == Types#_Int and ty_a == Types#_Real then
	set _Bool[new] = _Real[a] < _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Bool[new] = _Real[a] < _Real[b]
    else
	call Print#_error("_lt: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _lte takes integer a, integer b returns integer
    local integer new = _alloc()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]
    set _Type[new] = Types#_Bool

    if ty_a == Types#_Int and ty_b == Types#_Int then
	set _Bool[new] = _Int[a] <= _Int[b]
    elseif ty_a == Types#_Int and ty_b == Types#_Real then
	set _Bool[new] = _Int[a] <= _Real[b]
    elseif ty_b == Types#_Int and ty_a == Types#_Real then
	set _Bool[new] = _Real[a] <= _Int[b]
    elseif ty_a == Types#_Real and ty_b == Types#_Real then
	set _Bool[new] = _Real[a] <= _Real[b]
    else
	call Print#_error("_lte: Error. Should not happen")
    endif
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
    local integer ty = _Type[a]
    if ty == Types#_Bool then
	return _Bool[a]
    elseif ty == Types#_Nil then
	return false
    else
	return true
    endif
endfunction


// @noalloc
function _tostring takes integer v returns string
    local integer ty = _Type[v]
    if ty == Types#_Int then
	return I2S(_Int[v])
    elseif ty == Types#_Real then
	return R2S(_Real[v])
    elseif ty == Types#_String then
	return _String[v]
    elseif ty == Types#_Lambda then
	return "Fun: "+ I2S(_Int[v])
    elseif ty == Types#_BuiltInFunction then
	return "Native: " + I2S(_Int[v])
    elseif ty == Types#_Table then
	return "Table: " + I2S(_Int[v])
    elseif ty == Types#_Nil then
	return "nil"
    else
	return "unknown: " + I2S(_Type[v])
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
    // TODO: use some random seed
    local integer ty = _Type[v]
    if ty == Types#_String then
	return StringHash(_String[v])
    elseif ty == Types#_Table then
	return _Int[v] * 23 + 1337
    elseif ty == Types#_Real then
	return R2I( _Real[v] * 16180.33 )
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

    if _Type[v] == Types#_Nil then
	call Print#_error("table index is nil")
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
	return _Nil
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

// @alloc
function _eq takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = _rawequal_noalloc(a, b)
    return new
endfunction

// @alloc
function _neq takes integer a, integer b returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = not _rawequal_noalloc(a, b)
    return new
endfunction

// @alloc
function _not takes integer v returns integer
    local integer new = _alloc()
    set _Type[new] = Types#_Bool
    set _Bool[new] = not _truthy(v)
    return new
endfunction


function _init takes nothing returns nothing
    set _Nil = _alloc()
    set _Type[_Nil] = Types#_Nil
endfunction

function _parse_digit takes string c returns integer
    if c == "F" then
	return 15
    elseif c == "E" then
	return 14
    elseif c == "D" then
	return 13
    elseif c == "C" then
	return 12
    elseif c == "B" then
	return 11
    elseif c == "A" then
	return 10
    elseif c == "9" then
	return 9
    elseif c == "8" then
	return 8
    elseif c == "7" then
	return 7
    elseif c == "6" then
	return 6
    elseif c == "5" then
	return 5
    elseif c == "4" then
	return 4
    elseif c == "3" then
	return 3
    elseif c == "2" then
	return 2
    elseif c == "1" then
	return 1
    elseif c == "0" then
	return 0
    else
	return -1
    endif
endfunction

function _B2S takes boolean b returns string
    if b then
	return "True"
    else
	return "False"
    endif
endfunction

function _parse_number takes string s returns real
    local integer i = 0
    local integer len = StringLength(s)
    local string c
    local real res = 0.0
    local real frac = 0.0
    local integer tmp
    local boolean dot = false
    local integer dotpos = -1
    local integer base = 3
    local integer exp = 0
    local real exp_sign = 1
    local real sign = 1

    //call Print#_print("_parse_number("+s+")")

    set _error = false


    // ltrim
    loop
	set c = SubString(s, i, i+1)
	exitwhen c != " " and c != "\t" and c != "\n" and c != "\r"
	set i = i +1
    endloop
    //set s = SubString(s, i, len)

    set c = SubString(s, i, i+1)
    if c == "+" then
	set i = i +1
    elseif c == "-" then
	set i = i +1
	set sign = -1
    endif

    // base detection
    if StringCase(SubString(s, i, i+2), true) == "0X" then
	set base = 16
	set i = i+2
    else
	set base = 10
    endif

    // parsing number
    loop
    exitwhen i >= len
	set c = StringCase(SubString(s, i, i+1), true)
	set tmp = _parse_digit(c)

	if c == "." and dot then
	    set _error = true
	    return 0.0
	elseif c == "." then
	    set dot = true
	    set dotpos = i
	elseif c == "E" and base == 10 then
	    // scientific notation
	    set i = i +1
	    set c = SubString(s, i, i+1)
	    if c == "+" then
		set i = i +1
	    elseif c == "-" then
		set i = i +1
		set exp_sign = -1
	    else
		set tmp = _parse_digit(c)
		if tmp < 0 or tmp > 9 then
		    set _error = true
		    return 0.0
		endif
	    endif

	    loop
	    exitwhen i >= len
		set c = SubString(s, i, i+1)
		set tmp = _parse_digit(c)
		if tmp < 0 or tmp >= 10 then
		    set _error = true
		    return 0.0
		endif
		set exp = exp * 10 + tmp
		set i = i +1
	    endloop
	    exitwhen true
	elseif tmp >= base then
	    set _error = true
	    return 0.0
	elseif tmp < 0 then
	    set i = i -1
	    exitwhen true
	elseif tmp >= 0 then
	    if dot then
		set frac = frac + tmp/Pow(base, i-dotpos)
	    else
		set res = res * base + tmp
	    endif
	else
	endif
	set i = i + 1
    endloop

    // error checking and rtrim
    loop
    exitwhen i >= len
	set c = SubString(s, i, i+1)
	if c != " " and c != "\t" and c != "\n" and c != "\r" then
	    set _error = true
	    return 0.0
	endif
	set i = i + 1
    endloop

    return sign * (res + frac) * Pow(10, exp*exp_sign)
endfunction
