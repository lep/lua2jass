// scope Main
// REQUIRES Interpreter Auto Ins Wrap GC Helper
// REQUIRES Builtin::Trigger Builtin::Timer Builtin::Boolexpr

globals
    boolean _init_done = false
endglobals

function _start_main_interpreter takes nothing returns nothing
    call Interpreter#_start_main()
endfunction

function _init takes nothing returns nothing
    if not _init_done then

	call Auto#_init()
	call Ins#_init()
	call Wrap#_init()
	call Interpreter#_init()
	call GC#_init()
	call Helper#_init()

	call Builtin::Trigger#_init()
	call Builtin::Timer#_init()
	call Builtin::Boolexpr#_init()

	call TimerStart(CreateTimer(), 0, false, function _start_main_interpreter)

	set _init_done = true
    endif
endfunction
