// scope GC
// REQUIRES Deque Print

globals
    constant integer VALUE_MAX_STEPS = 1000
    constant integer CONTEXT_MAX_STEPS = 1000

    boolean _inqueue_flag = true
    integer _interpreter_queue
    integer array _interpreter
    boolean array _interpreter_inqueue

    integer _context_queue
    integer array _context
    boolean array _context_inqueue

    integer _value_queue
    integer array _value
    boolean array _value_inqueue
endglobals

function _push_context takes integer ctx returns nothing
    if ctx <= 0 then
	call Print#_warn("_push_context: 0")
    elseif ctx >= JASS_MAX_ARRAY_SIZE then
	call Print#_warn("_push_context: inf")
    else
	if _context_inqueue[ctx] != _inqueue_flag then
	    set _context[ Deque#_push( _context_queue )] = ctx
	    set _context_inqueue[ctx] = _inqueue_flag
	endif
    endif
endfunction

function _push_value takes integer value returns nothing
    if value <= 0 then
	call Print#_warn("_push_value: 0")
    elseif value >= JASS_MAX_ARRAY_SIZE then
	call Print#_warn("_push_value: inf")
    else
	if _value_inqueue[value] != _inqueue_flag then
	    set _value[ Deque#_push( _value_queue )] = value
	    set _value_inqueue[value] = _inqueue_flag
	endif
    endif
endfunction

function _work_iqueue takes nothing returns nothing
    local integer interpreter = Interpreter#_GlobalInterpreter
    local integer ls = Interpreter#_stack_top[interpreter]
    local integer i

    //call Print#_print("_work_iqueue")

    call _push_context( Interpreter#_ctx[interpreter] )
    call _push_value( Value#_Nil ) // lol

    loop
    exitwhen ls == 0
	call _push_context( Interpreter#_ctx[ls] )
	set ls = List#_next[ls]
    endloop


    set i = Builtin/Trigger#_cnt
    loop
    exitwhen i <= 0 
	call _push_value( Builtin/Trigger#_trigger[i])
	set i = i - 1
    endloop

    set i = Builtin/Timer#_cnt
    loop
    exitwhen i <= 0 
	call _push_value( Builtin/Timer#_timer[i])
	set i = i - 1
    endloop
endfunction

function _work_vqueue takes nothing returns nothing
    local integer i = VALUE_MAX_STEPS
    local integer ls
    local integer value
    local integer ty
    local integer val2

    local integer ls2
    //call Print#_print("_work_vqueue")

    loop
    exitwhen i <= 0
	set ls = Deque#_shift( _value_queue )
	exitwhen ls == 0
	set value = _value[ls]

	call Value#_mark_used( value )

	set ty = Value#_Type[value]

	if ty == Types#_Table then

	    set ls2 = Table#_head[ Value#_Int[value] ]
	    loop
	    exitwhen ls2 == 0
		call _push_value( Table#_val[ls2])
		set ls2 = List#_next[ls2]
	    endloop

	    // TODO: _Int2
	    // TODO: _Int3

	elseif ty == Types#_Lambda then
	    call _push_context( Value#_Int[value] )
	elseif ty == Types#_Coroutine then
	    if Builtin/Coroutine#_return_resume[value] != 0 then
		call _push_context(Builtin/Coroutine#_return_resume[value])
	    endif
	    call _push_context(Builtin/Coroutine#_return_yield[value])
	    set ls2 = Builtin/Coroutine#_stop_frame[value]
	    loop
	    exitwhen ls2 == Builtin/Coroutine#_base_frame[value]
	    exitwhen ls2 == 0
		call _push_context( Interpreter#_ctx[ls2] )
		set ls2 = List#_next[ls2]
	    endloop
	elseif ty == Types#_Foreign then
	    if Value#_Int[value] == Jass#_trigger then
		set ls2 = Builtin/Trigger#_trigger_actions[value]
		loop
		exitwhen ls2 == 0
		    call _push_value( Builtin/Trigger#_trigger_action[ls2] )
		    set ls2 = List#_next[ls2]
		endloop
	    elseif Value#_Int[value] == Jass#_triggeraction then
		call _push_value( Value#_Int2[ value ] )
	    elseif Value#_Int[value] == Jass#_timer then
		call _push_value( Value#_Int3[ value ] )
	    endif
	    
	endif

	set i = i -1
    endloop
endfunction

function _work_cqueue takes nothing returns nothing
    local integer i = CONTEXT_MAX_STEPS
    local integer ls
    local integer ctx
    local integer j
    local integer value
    local integer ls2
    local integer val2
    local integer ls3
    local integer val3
    //call Print#_print("_work_cqueue")
    loop
    exitwhen i <= 0
	set ls = Deque#_shift( _context_queue )
	exitwhen ls == 0
	set ctx = _context[ls]

	call Context#_mark_used( ctx )

	// walk tmps
	set ls2 = Table#_head[ Context#_tmps[ctx] ]
	loop
	exitwhen ls2 == 0
	    call _push_value( Table#_val[ls2] )
	    set ls2 = List#_next[ls2]
	endloop


	// walk locals
	set ls2 = Table#_head[ Context#_locals[ctx] ]
	loop
	exitwhen ls2 == 0
	    set ls3 = Table#_val[ls2]
	    loop
	    exitwhen ls3 == 0
		call _push_value( StringTable#_value[ls3] )
		set ls3 = List#_next[ls3]
	    endloop
	    set ls2 = List#_next[ls2]
	endloop

	// add parent contexts to GC workqueue
	loop
	    set ctx = Context#_parent[ctx]
	    exitwhen ctx == 0
	    call _push_context( ctx )
	endloop

	set i = i -1
    endloop
endfunction

function _full_mark_and_sweep takes nothing returns nothing
    //call Print#_print("_full_mark_and_sweep")
    loop
	call _work_iqueue()
	call _work_cqueue()
	call _work_vqueue()

	exitwhen Deque#_isEmpty(_context_queue) and Deque#_isEmpty(_value_queue)
    endloop

    call Value#_sweep()
    call Context#_sweep()
    set _inqueue_flag = not _inqueue_flag
endfunction

function _init takes nothing returns nothing
    set _interpreter_queue = Deque#_alloc()
    set _context_queue = Deque#_alloc()
    set _value_queue = Deque#_alloc()

    call TimerStart(CreateTimer(), 5, true, function _full_mark_and_sweep )
endfunction
