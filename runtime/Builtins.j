// scope Builtins
// REQUIRES Print Value Table Context Natives Builtin/Boolexpr


globals
endglobals

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

// function setmetatable(table, metatable)
function _setmetatable takes integer params_tbl, integer ctx, integer interpreter returns nothing
    local integer table = Table#_get( params_tbl, 1 )
    local integer metatable = Table#_get( params_tbl, 2 )
    local integer return_table = Table#_get( params_tbl, 0 )
    //call Print#_print("_setmetatable")
    //call Print#_print("  - setting metable of table "+I2S(Value#_Int[table])+" to "+I2S(Value#_Int[metatable]))

    // TODO: check if _Int3 is allready set
    if metatable == Value#_Nil then
	//call Print#_print("  - metatable is nil")
	set Value#_Int3[table] = 0
    else
	//call Print#_print("  - metatable is not nil")
	set Value#_Int3[table] = metatable
    endif

    call Table#_set( Value#_Int[return_table], 1, table )
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

