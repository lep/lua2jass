// scope Builtins
// REQUIRES Print Value Table Context
// REQUIRES Builtin/Trigger Builtin/Timer
// REQUIRES Builtin/Coroutine
// REQUIRES Natives


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

function _dispatch_builtin takes integer value, integer params, integer ctx, integer interpreter returns nothing
    local string name = Value#_String[value]
    local integer tbl = Value#_Int[params]
    //call Print#_print("_dispatch_builtin("+I2S(value)+","+I2S(params)+","+I2S(ctx)+","+I2S(reg_res)+")")
    //call Print#_print("  - tbl = "+I2S(tbl))
    //call Print#_print("  - name = "+name)
    if name == "print" then
        call _print(tbl, ctx, interpreter)
    elseif name == "setmetatable" then
	call _setmetatable(tbl, ctx, interpreter)
    elseif name == "Player" then
	call Natives#_Player(tbl, ctx, interpreter)
    elseif name == "CreateTrigger" then
	call Builtin/Trigger#_CreateTrigger(tbl, ctx, interpreter)
    elseif name == "TriggerAddAction" then
	call Builtin/Trigger#_TriggerAddAction(tbl, ctx, interpreter)
    elseif name == "TriggerRegisterPlayerChatEvent" then
	call Natives#_TriggerRegisterPlayerChatEvent(tbl, ctx, interpreter)
    elseif name == "GetEventPlayerChatString" then
	call Natives#_GetEventPlayerChatString(tbl, ctx, interpreter)
    elseif name == "TimerStart" then
	call Builtin/Timer#_TimerStart(tbl, ctx, interpreter)
    elseif name == "CreateTimer" then
	call Builtin/Timer#_CreateTimer(tbl, ctx, interpreter)
    elseif name == "co_create" then
	call Builtin/Coroutine#_create(tbl, ctx, interpreter)
    elseif name == "co_yield" then
	call Builtin/Coroutine#_yield(tbl, ctx, interpreter)
    elseif name == "co_resume" then
	call Builtin/Coroutine#_resume(tbl, ctx, interpreter)
    elseif name == "TriggerExecute" then
	call Natives#_TriggerExecute(tbl, ctx, interpreter)
    else
        call Print#_print("Unknown builtin function "+name)
    endif
endfunction


function _register_builtin takes integer ctx, string name, integer id returns nothing
    call Context#_set( ctx, name, Value#_builtin(name) )
endfunction

function _init takes nothing returns nothing
endfunction

