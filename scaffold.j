globals
    constant boolean NEW_INTP_ON_CHAT = true
endglobals

function print_hello takes nothing returns nothing
    local integer i = 2342
    call BJDebugMsg("Hallo von JHCR " + I2S(i))
endfunction

function reload_script takes nothing returns nothing
    call BJDebugMsg("|c00ff0000Reloading script...|r")
    call ExecuteFunc("JHCR_Init_parse")
endfunction


function start_interpreter_a takes nothing returns nothing
    call BJDebugMsg("Starting one shot interpreter")
    call lua_Interpreter_debug_start_main()
    call BJDebugMsg("  - done")
endfunction

function start_interpreter takes nothing returns nothing
    local string x = GetEventPlayerChatString()
    if x == "a" then
        call BJDebugMsg("|c0000ff00Starting interpreter...|r")
        call lua_Interpreter_debug_start_main()
    elseif x == "x" then
        //call lua_Print_print( I2S(lua_Deque_fresh( Scope, Recycler ) ))
        //call lua_Print_print( R2S( lua_Value_parse_number(GetEventPlayerChatString())) )
    elseif x == "r" then
        call BJDebugMsg("|cffffab3fReseting|r")
        //call lua_Deque_move_all( Scope, Recycler )
    endif
endfunction

// our entry point to the map
function InitCustomTriggers takes nothing returns nothing
    local trigger t = CreateTrigger()
    call TriggerRegisterPlayerEvent(t, Player(0), EVENT_PLAYER_END_CINEMATIC)
    call TriggerAddAction(t, function reload_script)

    if NEW_INTP_ON_CHAT then
	set t = CreateTrigger()
	//call TriggerRegisterPlayerEvent(t, Player(0), EVENT_PLAYER_CHAT)
	call TriggerRegisterPlayerChatEvent(t, Player(0), "", true)
	call TriggerAddAction(t, function start_interpreter)
    else
	call TimerStart(CreateTimer(), 0.0, false, function start_interpreter_a)
    endif

    call lua_Context_init()
    call lua_Auto_init()
    call lua_Ins_init()
    call lua_Value_init()
    call lua_Wrap_init()
    call lua_Interpreter_init()
    call lua_GC_init()
    call lua_Builtins_init()

    call lua_Builtin__Trigger_init()
    call lua_Builtin__Timer_init()
    call lua_Builtin__Boolexpr_init()

    //set Scope = lua_Deque_alloc()
    //set Recycler = lua_Deque_alloc()

    //call TimerStart(CreateTimer(), 1.0, true, function print_hello)
    call CreateUnit(Player(0), 'Hpal', 0, 0, 0)

endfunction
