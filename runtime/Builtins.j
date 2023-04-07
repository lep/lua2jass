// scope Builtins
// REQUIRES Print Value Table Context Natives Builtin/Boolexpr GC Helper



globals
    boolean _trace = false
endglobals

function _enable_trace takes integer tbl, integer ctx, integer interpreter returns nothing
    set _trace = true
endfunction

function _disable_trace takes integer tbl, integer ctx, integer interpreter returns nothing
    set _trace = false
endfunction

function _S2A takes string s returns integer
    local integer a=0
    local integer l=StringLength(s)
    local integer j=0
    local string m
    local integer h
    loop
	exitwhen j==l
	set a = a*256 + Helper#_Char2Ascii(SubString(s,j,j+1))
	set j=j+1
    endloop
    return a
endfunction

function _FourCC takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local integer str = Table#_get( tbl, 1 )
    call Table#_set( Value#_Int[return_table], 1, Value#_litint(_S2A(Value#_2string(str, interpreter))) )
endfunction

function _collectgarbage takes integer tbl, integer ctx, integer interpreter returns nothing
    call GC#_full_mark_and_sweep()
endfunction

function _rawset takes integer ptbl, integer ctx, integer interpreter returns nothing
    local integer tbl = Table#_get( ptbl, 1 )
    local integer key = Table#_get( ptbl, 2 )
    local integer val = Table#_get( ptbl, 3 )

    call Value#_settable( tbl, key, val )
endfunction

function _rawget takes integer ptbl, integer ctx, integer interpreter returns nothing
    local integer ret = Table#_get( ptbl, 0 )
    local integer tbl = Table#_get( ptbl, 1 )
    local integer key = Table#_get( ptbl, 2 )

    call Table#_set( Value#_Int[ret], 1, Value#_gettable(tbl, key) )
endfunction

function _rawequal takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer ret = Table#_get( tbl, 0 )
    local integer a = Table#_get( tbl, 1 )
    local integer b = Table#_get( tbl, 2 )

    call Table#_set( Value#_Int[ret], 1, Value#_litbool(Value#_rawequal_noalloc(a,b)))

endfunction

function _print takes integer tbl, integer ctx, integer interpreter returns nothing
    local string r = ""
    local integer k = 1
    local integer v
    //call Print#_print("_print("+I2S(tbl)+")")


    //if ctx == 0 then
    //    set k = 0
    //endif

    //call Print#_print("  - starting at k = "+I2S(k))

    loop
        if Table#_has( tbl, k ) then
            set v = Table#_get(tbl, k)

	    //if interpreter == 0 then
	    //    if Value#_Type[v] == Types#_Int then
	    //        set r = r + I2S(Value#_Int[v])+".   "
	    //    elseif Value#_Type[v] == Types#_Real then
	    //        set r = r + R2S(Value#_Real[v])+".   "
	    //    elseif Value#_Type[v] == Types#_String then
	    //        set r = r + (Value#_String[v])+".   "
	    //    else
	    //        set r = r + "(type "+I2S(Value#_Type[v])+") .  "
	    //    endif
	    //else
		set r = r + Value#_tostring(v, interpreter) + "   "
	    //endif
            set k = k +1
        else
            exitwhen true
        endif
    endloop
    call Print#_print("|c00aaaaff"+r+"|r")
endfunction

// https://www.lua.org/pil/13.3.html
// function setmetatable(table, metatable)
function _setmetatable takes integer params_tbl, integer ctx, integer interpreter returns nothing
    local integer table = Table#_get( params_tbl, 1 )
    local integer metatable = Table#_get( params_tbl, 2 )
    local integer return_table = Table#_get( params_tbl, 0 )
    local integer current_metatable = Value#_Int3[table]

    if Value#_Type[table] != Types#_Table then
	call Value#_error_str("Bad argument #1 to 'setmetatable' (table expected)")
	return
    endif

    if Value#_Type[metatable] != Types#_Table and Value#_Type[metatable] != Types#_Nil  then
	call Value#_error_str("Bad argument #2 to 'setmetatable' (table or nil expected)")
	return
    endif

    if Value#_gettable(metatable, Value#_litstring("__metatable")) != Value#_Nil then
	call Value#_error_str("Cannot change protected metatable")
	return
    endif

    if metatable == Value#_Nil then
	set Value#_Int3[table] = 0
    else
	set Value#_Int3[table] = metatable
    endif

    call Table#_set( Value#_Int[return_table], 1, table )
endfunction

function _getmetatable takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local integer table = Table#_get( tbl, 1 )
    local integer metatable
    if Value#_Type[table] != Types#_Table then
	call Table#_set( Value#_Int[return_table], 1, Value#_Nil )
    elseif Value#_Int3[table] == 0 then
	call Table#_set( Value#_Int[return_table], 1, Value#_Nil )
    else
	set metatable = Value#_gettable( Value#_Int3[table], Value#_litstring("__metatable"))
	if metatable != Value#_Nil then
	    call Table#_set( Value#_Int[return_table], 1, metatable )
	else
	    call Table#_set( Value#_Int[return_table], 1, Value#_Int3[table] )
	endif
    endif
endfunction


function _ForForce takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)

    set Builtin/Boolexpr#_code_r = r
    set Builtin/Boolexpr#_code_i = interpreter
    set Builtin/Boolexpr#_code_v = arg2

    call ForForce( Natives#_convert2force(arg1, interpreter), function Builtin/Boolexpr#_cb)
endfunction

function _ForGroup takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)

    set Builtin/Boolexpr#_code_r = r
    set Builtin/Boolexpr#_code_i = interpreter
    set Builtin/Boolexpr#_code_v = arg2

    call ForGroup( Natives#_convert2group(arg1, interpreter), function Builtin/Boolexpr#_cb)
endfunction


function _pcall takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r0 = Table#_get(tbl, 0)
    local integer f = Table#_get(tbl, 1)
    local integer args = Table#_alloc()
    local integer r1 = Value#_table()
    local integer frame = Interpreter#_stack_top[interpreter]
    local boolean result

    call Table#_getlist( args, tbl, 2 )
    call Table#_set( args, 0, r1 )

    set Value#_inProtectedCall = Value#_inProtectedCall + 1
    set result = Wrap#_call_function( f, args, interpreter )
    set Value#_inProtectedCall = Value#_inProtectedCall - 1

    call Table#_set( Value#_Int[r0], 1, Value#_litbool(result) )

    if not result then
	call Table#_set( Value#_Int[r0], 2, Value#_getLastErrorValue() )
	set Interpreter#_stack_top[interpreter] = frame
    else
	call Table#_append( Value#_Int[r0], Value#_Int[r1], 1 )
    endif
endfunction

function _xpcall takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r0 = Table#_get(tbl, 0)
    local integer f = Table#_get(tbl, 1)
    local integer msgh = Table#_get(tbl, 2)
    local integer args = Table#_alloc()
    local integer r1 = Value#_table()
    local integer r2 = Value#_table()
    local integer frame = Interpreter#_stack_top[interpreter]
    local boolean result

    call Table#_getlist( args, tbl, 3 )
    call Table#_set( args, 0, r1 )

    set Value#_inProtectedCall = Value#_inProtectedCall + 1
    set result = Wrap#_call_function( f, args, interpreter )
    set Value#_inProtectedCall = Value#_inProtectedCall - 1

    call Table#_set( Value#_Int[r0], 1, Value#_litbool(result) )

    call Call#_call1( msgh, Value#_getLastErrorValue(), r2, interpreter )


    if not result then
	call Table#_set( Value#_Int[r0], 2, Table#_get( Value#_Int[r2], 1 ) )
	set Interpreter#_stack_top[interpreter] = frame
    else
	call Table#_append( Value#_Int[r0], Value#_Int[r1], 1 )
    endif
endfunction

function _error takes integer tbl, integer ctx, integer interpreter returns nothing
    call Value#_error( Table#_get(tbl, 1) )
endfunction

function _type takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer v = Table#_get(tbl, 1)
    local integer ty = Value#_litstring( Types#_getName(Value#_Type[v]) )

    call Table#_set( Value#_Int[r], 1, ty )
endfunction

function _select takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer index = Table#_get(tbl, 1)

    if Value#_Type[index] == Jass#_string and Value#_String[index] == "#" then
	call Table#_set( Value#_Int[r], 1, Value#_litint( Table#_len(tbl) -1 ))
    else
	set index = Value#_integercontext( index )
	if index == Value#_Nil then
	    call Value#_error_str("bad argument #1 to 'select' (number expected)")
	    return
	endif
	if Value#_Int[index] < 1 then
	    call Value#_error_str("bad argument #1 to 'select' (index out of range)")
	    return
	endif
	call Table#_getlist( Value#_Int[r], tbl, 1 + Value#_Int[index] )
    endif
    
endfunction

function _ipairs_next takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer a_tbl = Table#_get(tbl, 1)
    local integer a_idx = Value#_add( Table#_get(tbl, 2), Value#_litint(1) )
    local integer r0 = Value#_gettable(a_tbl, a_idx)

    if r0 == Value#_Nil then
	call Table#_set( Value#_Int[r], 1, r0 )
    else
	call Table#_set( Value#_Int[r], 1, a_idx)
	call Table#_set( Value#_Int[r], 2, r0 )
    endif

endfunction

function _ipairs takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer a_tbl  =Table#_get(tbl, 1)

    call Table#_set( Value#_Int[r], 1, Context#_get( ctx, "$ipairs_next" ))
    call Table#_set( Value#_Int[r], 2, a_tbl )
    call Table#_set( Value#_Int[r], 3, Value#_litint(0) )
endfunction

