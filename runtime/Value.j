// scope Value
// REQUIRES Table List Call Helper

globals

    integer array _Int
    real array    _Real
    string array  _String
    boolean array _Bool
    integer array _Table
    integer array _Type

    integer array _Int2
    integer array _Int3

    // List
    integer array _val
    integer array _key


    // error stuff
    integer _lastErrorValue
    integer _inProtectedCall = 0


    // constant
    integer _Nil

    boolean _parse_number_error

    boolean array _mark

    #include "alloc-globals.j"
endglobals

#include "alloc.j"


function _new takes nothing returns integer
    return _alloc()
endfunction

// @alloc
function _litstring takes string a returns integer
    local integer new = _new()
    set _Type[new] = Jass#_string
    set _String[new] = a
    return new
endfunction


function _getLastErrorValue takes nothing returns integer
    local integer i = _lastErrorValue
    set _lastErrorValue = 0
    return i
endfunction

function _error takes integer v returns nothing
    set _lastErrorValue = v
    if _inProtectedCall == 0 then
        if _Type[v] != Jass#_string then
            call Print#_error("error object not a string" )
        else
            call Print#_error(_String[v])
        endif
    endif
    call I2S(1 / 0)
endfunction

function _error_str takes string s returns nothing
    call _error(_litstring(s))
endfunction

function _B2S takes boolean b returns string
    if b then
	return "|cffabcd00true|r"
    else
	return "|cffabcd00false|r"
    endif
endfunction

function _isnumber takes integer v returns boolean
    local integer ty = _Type[v]
    return ty == Jass#_real or ty == Jass#_integer
endfunction

// @noalloc
function _litnil takes nothing returns integer
    return _Nil
endfunction



// @alloc
function _neg takes integer v returns integer
    local integer new = _new()
    local integer ty = _Type[v]
    if ty == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = - _Int[v]
    elseif ty == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = - _Real[v]
    else
	call Print#_error("_neg: should not happen")
    endif
    return new
endfunction

// @alloc
function _complement takes integer v returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    set _Int[new] = - _Int[v] -1
    return new
endfunction

// @alloc
function _add takes integer a, integer b returns integer
    local integer new = _new()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = _Int[a] + _Int[b]
    elseif ty_a == Jass#_integer  and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Int[a] + _Real[b]
    elseif ty_b == Jass#_integer  and ty_a == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] + _Int[b]
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] + _Real[b]
    else
	call Print#_error("_add: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _sub takes integer a, integer b returns integer
    local integer new = _new()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = _Int[a] - _Int[b]
    elseif ty_a == Jass#_integer  and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Int[a] - _Real[b]
    elseif ty_b == Jass#_integer  and ty_a == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] - _Int[b]
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] - _Real[b]
    else
	call Print#_error("_sub: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _mul takes integer a, integer b returns integer
    local integer new = _new()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = _Int[a] * _Int[b]
    elseif ty_a == Jass#_integer  and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Int[a] * _Real[b]
    elseif ty_b == Jass#_integer  and ty_a == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] * _Int[b]
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] * _Real[b]
    else
	call Print#_error("_mul: Error. Should not happen")
    endif
    return new
endfunction

// @alloc
function _div takes integer a, integer b returns integer
    // always real division
    local integer new = _new()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	set _Type[new] = Jass#_real
	set _Real[new] = I2R(_Int[a]) / _Int[b]
    elseif ty_a == Jass#_integer  and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Int[a] / _Real[b]
    elseif ty_b == Jass#_integer  and ty_a == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] / _Int[b]
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _Real[a] / _Real[b]
    else
	call Print#_error("_div: Error. Should not happen")
    endif
    return new
endfunction

// round towards negative inf
function _floor takes real r returns integer
    local integer i = R2I(r)
    if i == r then
	return i
    elseif r < 0 then
	return R2I(r - 1)
    else
	return i
    endif
endfunction

function _ceil takes real r returns integer
    local integer i = R2I(r)
    if i == r then
	return i
    elseif r > 0 then
	return R2I(r + 1)
    else
	return i
    endif
endfunction


// @noalloc
function _mod_real takes real dividend, real divisor returns real
    return dividend - divisor * _floor(dividend / divisor )
endfunction

// @noalloc
function _mod_int takes integer dividend, integer divisor returns integer
    return dividend - divisor * _floor(I2R(dividend)/divisor)
endfunction

// @alloc
function _idiv takes integer a, integer b returns integer
    // always returning an integer
    local integer new = _new()
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    //call Print#_print("_idiv "+I2S(ty_a)+","+I2S(ty_b))

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = _floor( I2R(_Int[a]) / _Int[b] )
    elseif ty_a == Jass#_integer and ty_b == Jass#_real then
	set _Type[new] = Jass#_integer
	set _Int[new] = _floor( _Int[a] / _Real[b] )
    elseif ty_b == Jass#_integer and ty_a == Jass#_real then
	set _Type[new] = Jass#_integer
	set _Int[new] = _floor( _Real[a] / _Int[b] )
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	set _Type[new] = Jass#_integer
	set _Int[new] = _floor( _Real[a] / _Real[b] )
    else
	call Print#_error("_idiv: Error. Should not happen")
    endif
    return new
endfunction

function _lt_numeric_noalloc takes integer a, integer b returns boolean
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]

    if ty_a == Jass#_integer and ty_b == Jass#_integer then
	return _Int[a] < _Int[b]
    elseif ty_a == Jass#_integer and ty_b == Jass#_real then
	return _Int[a] < _Real[b]
    elseif ty_b == Jass#_integer and ty_a == Jass#_real then
	return _Real[a] < _Int[b]
    elseif ty_a == Jass#_real and ty_b == Jass#_real then
	return _Real[a] < _Real[b]
    else
	call _error_str("bad comparison (<)")
	return false
    endif
endfunction

function _lt_numeric_or_string_noalloc takes integer a, integer b returns boolean
    if _Type[a] == Jass#_string and _Type[b] == Jass#_string then
	return Helper#_lt_string(_String[a], _String[b])
    else
	return _lt_numeric_noalloc(a, b)
    endif
endfunction

// a <= b ~~ not (b > a)
function _lte_numeric_noalloc takes integer a, integer b returns boolean
    return not _lt_numeric_noalloc(b, a)
endfunction


// @alloc
function _litint takes integer a returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    set _Int[new] = a
    return new
endfunction

// @alloc
function _litfloat takes real a returns integer
    local integer new = _new()
    set _Type[new] = Jass#_real
    set _Real[new] = a
    return new
endfunction

// @alloc
function _litbool takes boolean a returns integer
    local integer new = _new()
    set _Type[new] = Jass#_boolean
    set _Bool[new] = a
    return new
endfunction

// @alloc
function _table takes nothing returns integer
    local integer new = _new()
    set _Type[new] = Types#_Table
    set _Int[new] = Table#_alloc()  // int keys
    set _Int2[new] = Table#_alloc() // non-int keys
    set _Int3[new] = 0 // metatable
    return new
endfunction

// @alloc
function _coroutine takes nothing returns integer
    local integer new = _new()
    set _Type[new] = Types#_Coroutine
    return new
endfunction

// @alloc
function _foreign takes integer ty returns integer
    local integer new = _new()
    set _Type[new] = Types#_Foreign
    set _Int[new] = ty
    return new
endfunction


// @alloc
function _lambda takes integer a, string name returns integer
    local integer new = _new()
    set _Type[new] = Types#_Lambda
    set _Int[new] = a
    set _String[new] = name
    return new
endfunction

// @alloc
function _builtin takes integer id, string f returns integer
    local integer new = _new()
    set _Type[new] = Types#_BuiltInFunction
    set _Int[new] = id
    set _String[new] = f
    return new
endfunction

// @noalloc
function _truthy takes integer a returns boolean
    local integer ty = _Type[a]
    if ty == Jass#_boolean then
	return _Bool[a]
    elseif ty == Types#_Nil then
	return false
    else
	return true
    endif
endfunction


// @noalloc
function _rawequal_noalloc takes integer a, integer b returns boolean
    local integer type_a = _Type[a]
    local integer type_b = _Type[b]
    if type_a == Jass#_integer and type_b == Jass#_integer then
	return _Int[a] == _Int[b]
    elseif type_a == Jass#_real and type_b == Jass#_real then
	// TODO: see if wc3 lua has the same quirky behavior for
	// real comparison
	return not (_Real[a] != _Real[b]) 
    elseif type_a == Jass#_integer and type_b == Jass#_real then
	return I2R(_Int[a]) == _Real[b]
    elseif type_b == Jass#_integer and type_a == Jass#_real then
	return I2R(_Int[b]) == _Real[a]
    elseif type_a == Jass#_boolean and type_b == Jass#_boolean then
	return _Bool[a] == _Bool[b]
    elseif type_a == Jass#_string and type_b == Jass#_string then
	return _String[a] == _String[b]
    elseif type_a == Types#_Table and type_b == Types#_Table then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_Lambda and type_b == Types#_Lambda then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_BuiltInFunction and type_b == Types#_BuiltInFunction then
	return _Int[a] == _Int[b]
    elseif type_a == Types#_Foreign and type_b == Types#_Foreign then
	return _Int[a] == _Int[b] // idk. don't wanna compare jass types
    elseif type_a == Types#_Nil and type_b == Types#_Nil then
	return true
    else
	return false
    endif
endfunction

// @noalloc
function _hash takes integer v returns integer
    // TODO: use some random seed
    local integer ty = _Type[v]
    if ty == Jass#_string then
	return StringHash(_String[v])
    elseif ty == Types#_Table then
	return _Int[v] * 23 + 1337
    elseif ty == Jass#_real then
	return R2I( _Real[v] * 16180.33 )
    elseif ty == Types#_Lambda then
	return _Int[v]
    elseif ty == Types#_BuiltInFunction then
	return _Int[v]
	return StringHash(_String[v])
    elseif ty == Types#_Foreign then
	return _Int[v]
    elseif ty == Jass#_boolean then
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

    //call Print#_print("_settable("+I2S(t)+","+I2S(k)+","+I2S(v)+")")

    if _Type[t] != Types#_Table then
	call _error_str("Expected table but got "+I2S(_Type[t]))
        return
    endif

    if _Type[k] == Types#_Nil then
	call _error_str("table index is nil")
        return
    endif

    if ty == Jass#_integer then
	call Table#_set( _Int[t], _Int[k], v )
    elseif ty == Jass#_real and _Real[k] == R2I(_Real[k]) then
	call Table#_set( _Int[t], R2I(_Real[k]), v )
    else
	set tbl = _Int2[t]
	set ls = Table#_get( tbl, _hash(k) )
	set prev = ls
	loop
	exitwhen ls == 0
	    if _rawequal_noalloc(_key[ls], k) then
		set _val[ls] = v
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
    if ty == Jass#_integer then
	if Table#_has( _Int[v], _Int[k] ) then
	    return Table#_get( _Int[v], _Int[k] )
	else
	    return _Nil
	endif
    elseif ty == Jass#_real and _Real[k] == R2I(_Real[k]) then
	if Table#_has( _Int[v], R2I(_Real[k]) ) then
	    return Table#_get( _Int[v], R2I(_Real[k]) )
	else
	    return _Nil
	endif
    else
	set tbl = _Int2[v]
	set ls = Table#_get( tbl, _hash(k) )
	loop
	exitwhen ls == 0
	    if _rawequal_noalloc(_key[ls], k) then
		return _val[ls]
	    endif
	    set ls = List#_next[ls]
	endloop
	return _Nil
    endif
endfunction

// @alloc
function _len takes integer v returns integer
    local integer ty = _Type[v]
    local integer k = 1
    local integer tbl
    //call Print#_print("_len")
    if ty == Jass#_string then
	return Value#_litint(StringLength(_String[v]))
    elseif ty == Types#_Table then
	return _litint( Table#_len( _Int[v] ) )
	set tbl = _Int[v]
    endif
    return Value#_litint(0) // TODO
endfunction

// @alloc
function _mod takes integer a, integer b returns integer
    local integer new = _new()
    local integer tya = _Type[a]
    local integer tyb = _Type[b]
    if tya == Jass#_integer and tyb == Jass#_integer then
	set _Type[new] = Jass#_integer
	set _Int[new] = _mod_int(_Int[a],  _Int[b])
    elseif tya == Jass#_real and tyb == Jass#_real then
	set _Type[new] = Jass#_real
	set _Real[new] = _mod_real( _Real[a], _Real[b] )
    elseif tya == Jass#_real and tyb == Jass#_integer then
	set _Type[new] = Jass#_real
	set _Real[new] = _mod_real( _Real[a], _Int[b] )
    elseif tyb == Jass#_real and tya == Jass#_integer then
	set _Type[new] = Jass#_real
	set _Real[new] = _mod_real( _Int[a], _Real[b] )
    endif

    return new
endfunction

// @alloc
function _exp takes integer a, integer b returns integer
    local integer new = _new()
    local integer tya = _Type[a]
    local integer tyb = _Type[b]
    set _Type[new] = Jass#_real
    if tya == Jass#_integer and tyb == Jass#_integer then
	set _Real[new] = Pow(_Int[a], _Int[b])
    elseif tya == Jass#_real and tyb == Jass#_real then
	set _Real[new] = Pow( _Real[a], _Real[b] )
    elseif tya == Jass#_real and tyb == Jass#_integer then
	set _Real[new] = Pow( _Real[a], _Int[b] )
    elseif tyb == Jass#_real and tya == Jass#_integer then
	set _Real[new] = Pow( _Int[a], _Real[b] )
    endif

    return new
endfunction

// bit functions

// @alloc
function _shiftl takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    if _Int[b] < 0 then
	set _Int[new] = _Int[a] / R2I(Pow(2, -_Int[b]))
    else
	set _Int[new] = _Int[a] * R2I(Pow(2, _Int[b]) )
    endif
    return new
endfunction

// @alloc
function _shiftr takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    if _Int[b] < 0 then
	set _Int[new] = _Int[a] * R2I(Pow(2, -_Int[b]) )
    else
	set _Int[new] = _Int[a] / R2I(Pow(2, _Int[b]) )
    endif
    return new
endfunction

// @alloc
function _band takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    set _Int[new] = BlzBitAnd( _Int[a], _Int[b] ) // TODO: pre 1.31 patches
    return new
endfunction

// @alloc
function _bor takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    set _Int[new] = BlzBitOr( _Int[a], _Int[b] ) // TODO: pre 1.31 patches
    return new
endfunction

// @alloc
function _bxor takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_integer
    set _Int[new] = BlzBitXor( _Int[a], _Int[b] ) // TODO: pre 1.31 patches
    return new
endfunction


// @alloc
function _eq takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_boolean
    set _Bool[new] = _rawequal_noalloc(a, b)
    return new
endfunction

// @alloc
function _neq takes integer a, integer b returns integer
    local integer new = _new()
    set _Type[new] = Jass#_boolean
    set _Bool[new] = not _rawequal_noalloc(a, b)
    return new
endfunction

// @alloc
function _not takes integer v returns integer
    local integer new = _new()
    set _Type[new] = Jass#_boolean
    set _Bool[new] = not _truthy(v)
    return new
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

    set _parse_number_error = false


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
	    set _parse_number_error = true
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
		    set _parse_number_error = true
		    return 0.0
		endif
	    endif

	    loop
	    exitwhen i >= len
		set c = SubString(s, i, i+1)
		set tmp = _parse_digit(c)
		if tmp < 0 or tmp >= 10 then
		    set _parse_number_error = true
		    return 0.0
		endif
		set exp = exp * 10 + tmp
		set i = i +1
	    endloop
	    exitwhen true
	elseif tmp >= base then
	    set _parse_number_error = true
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
	    set _parse_number_error = true
	    return 0.0
	endif
	set i = i + 1
    endloop

    return sign * (res + frac) * Pow(10, exp*exp_sign)
endfunction

function _tostring_debug takes integer v returns string
    local integer ty = _Type[v]
    if ty == Jass#_integer then
        return "Int: "+I2S(_Int[v])
    elseif ty == Jass#_real then
        return "Real: "+R2S(_Real[v])
    elseif ty == Jass#_string then
        return "String: "+_String[v]
    elseif ty == Jass#_boolean then
        return "Bool: "+_B2S(_Bool[v])
    elseif ty == Types#_Lambda then
	return "Lambda: "+I2S(v)+","+_String[v]+","+I2S(_Int[v])
    elseif ty == Types#_BuiltInFunction then
	return "Native: "+I2S(v)+","+_String[v]+","+I2S(_Int[v])
    elseif ty == Types#_Nil then
	return "Nil: "+I2S(v)
    elseif ty == Types#_Table then
	return "Table: "+I2S(v)+","+I2S(Table#_get( _Int[v], 'type' ))
    elseif ty == Types#_Coroutine then
	return "Thread: "+I2S(v)
    elseif ty == Types#_Foreign then
	return "Foreign: "+I2S(v)+" jass: "+I2S(_Int[v])
    else
	return "Unk: "+I2S(v)
    endif
endfunction

function _tostring_concat takes integer v returns string
    local integer ty = _Type[v]
    if ty == Jass#_integer then
        return I2S(_Int[v])
    elseif ty == Jass#_real then
        return R2S(_Real[v])
    elseif ty == Jass#_string then
        return _String[v]
    endif

    call Print#_error("Cannot convert to string")
    return ""
endfunction

// @recursive
function _tostring takes integer v, integer interpreter returns string
    local integer ty = _Type[v]
    local integer metatable
    local integer metamethod
    local integer ret
    if ty == Jass#_integer then
	return I2S(_Int[v])
    elseif ty == Jass#_real then
	return R2S(_Real[v])
    elseif ty == Jass#_string then
	return _String[v]
    elseif ty == Types#_Lambda then
	return "Fun: "+ I2S(_Int[v])
    elseif ty == Types#_BuiltInFunction then
	return "Native: " + I2S(_Int[v])
    elseif ty == Types#_Coroutine then
	return "Thread: " + I2S(v)
    elseif ty == Types#_Foreign then
	return "Foreign: " + I2S(v)
    elseif ty == Types#_Table then
	set metatable = _Int3[v]
	if metatable != 0 then
	    set metamethod = _gettable( metatable, _litstring("__tostring"))
	    if metamethod != _Nil then
		set ret = _table()
		call Call#_call1( metamethod, v, ret, interpreter )
		return _tostring( Table#_get( _Int[ret], 1), interpreter )
	    endif
	endif
	return "Table: " + I2S(_Int[v])
    elseif ty == Jass#_boolean then
	if Value#_Bool[v] then
	    return "true"
	else
	    return "false"
	endif
    elseif ty == Types#_Nil then
	return "nil"
    else
	return "unknown: " + I2S(_Type[v])
    endif
endfunction

function _concat takes integer a, integer b returns integer
    local integer ty_a = _Type[a]
    local integer ty_b = _Type[b]
    local string sa = _tostring_concat(a)
    local string sb = _tostring_concat(b)
    return Value#_litstring( sa + sb )
endfunction

// @noalloc
function _getJassType takes integer v returns integer
    if _Type[v] != Types#_Foreign then
        call Print#_error("Not a Warcraft type")
        return 0
    endif
    return _Int[v]
endfunction

function _2int takes integer v, integer interpreter returns integer
    local integer ty = _Type[v]
    local real r
    if ty == Jass#_integer then
        return _Int[v]
//    elseif ty == Jass#_string then
//	set ty = Jass#_real
//	set r = _parse_number(_String[v])
//	if _error then
//	    call Print#_error("Error: cannot coerce string to number")
//            return 0
//	endif
    endif

    if ty == Jass#_real then
        set ty = R2I(_Real[v])
        if _Real[v] == ty then
            return ty
        endif
    endif

    call Print#_error("Not of type integer")
    return 0
endfunction

function _2real takes integer v, integer interpreter returns real
    local real r
    if _Type[v] == Jass#_integer then
        return _Int[v] + 0.0
    elseif _Type[v] == Jass#_real then
        return _Real[v]
    //elseif _Type[v] == Jass#_string then
    //    set r = _parse_number(_String[v])
    //    if _error then
    //        call Print#_error("Error: cannot coerce string to number")
    //        return 0.0
    //    endif
    //    return r
    endif
    call Print#_error("Not of type real")
    return 0.0
endfunction

function _2string takes integer v, integer interpreter returns string
    //return _tostring(v, interpreter)

    // TODO: check of __tostring is called here
    if _Type[v] != Jass#_string then
        call Print#_error("Not of type string")
        return ""
    endif
    return _String[v]
endfunction

function _2boolean takes integer v, integer interpreter returns boolean
    //return _truthy(v)

    if _Type[v] != Jass#_boolean then
        call Print#_error("Not of type boolean")
        return false
    endif
    return _Bool[v]
endfunction


// @alloc
function _numbercontext takes integer v returns integer
    local integer ty = _Type[v]
    if ty == Jass#_integer then
	return v
    elseif ty == Jass#_real then
	return v
    elseif ty == Jass#_string then
	set v = _litfloat(_parse_number(Value#_String[v]))
	if _parse_number_error then
	    return _litnil()
	endif
	return v
    else
	return _litnil()
    endif
	
endfunction

// @alloc
function _integercontext takes integer v returns integer
    local integer ty = Value#_Type[v]
    if ty == Jass#_integer then
	return v
    elseif ty == Jass#_string then
	set ty = Jass#_real
	set v = _litfloat(_parse_number(_String[v]))
	if Value#_parse_number_error then
	    return _litnil()
	endif
    endif

    if ty == Jass#_real then
	set ty = R2I(_Real[v]) // casually reusing variable
	if _Real[v] == ty then
	    return _litint(ty)
	endif
    endif

    return _litnil()
endfunction

function _getMetamethod takes integer value, string method returns integer
    if _Type[value] != Types#_Table then
	return _litnil()
    elseif _Int3[value] == 0 then
	return _litnil()
    else
	return _gettable( _Int3[value], _litstring(method) )
    endif
endfunction

function _mark_used takes integer value returns nothing
    set _mark[value] = GC#_inqueue_flag
endfunction

function _sweep takes nothing returns nothing
    local integer i = 1
    loop
    exitwhen i >= _I
	if _V[i] == -1 and _mark[i] != GC#_inqueue_flag then
	    call _free(i)
	endif
	set i = i +1
    endloop
endfunction

function _init takes nothing returns nothing
endfunction
