// scope GC
// REQUIRES Deque Print

globals
    constant integer VALUE_MAX_STEPS = 1000
    constant integer CONTEXT_MAX_STEPS = 1000

    real _timeout = 5.0
    boolean _enabled = true

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

    string _Debug = ""
endglobals

function _next takes integer ls returns integer
    call List#_mark_used(ls)
    return List#_next[ls]
endfunction

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
	call Print#_warn("_push_value: 0 "+ _Debug)
    elseif value >= JASS_MAX_ARRAY_SIZE then
	call Print#_warn("_push_value: inf")
    else
	if _value_inqueue[value] != _inqueue_flag then
	    set _value[ Deque#_push( _value_queue )] = value
	    set _value_inqueue[value] = _inqueue_flag
	endif
    endif
    set _Debug = ""
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
	set ls = _next(ls)
    endloop

    call Table#_mark_used( Builtin::Trigger#_trigger2value  )
    set ls = Table#_head[ Builtin::Trigger#_trigger2value ]
    loop
    exitwhen ls == 0
	set ls = _next(ls)
    endloop

    call Table#_mark_used( Builtin::Timer#_timer2value  )
    set ls = Table#_head[ Builtin::Timer#_timer2value ]
    loop
    exitwhen ls == 0
	set ls = _next(ls)
    endloop


    set i = Builtin::Trigger#_cnt
    loop
    exitwhen i <= 0 
	call _push_value( Builtin::Trigger#_trigger[i])
	set i = i - 1
    endloop

    set i = Builtin::Timer#_cnt
    loop
    exitwhen i <= 0 
	call _push_value( Builtin::Timer#_timer[i])
	set i = i - 1
    endloop
endfunction


function _handle_boolexpr takes integer value returns nothing
    local integer ty = Value#_Int3[value]
    call _push_value( value )

    if ty == Builtin::Boolexpr#_NORMAL then
        call _push_value( Builtin::Boolexpr#_Fun1[value] )
    elseif ty == Builtin::Boolexpr#_AND then
        call _handle_boolexpr( Builtin::Boolexpr#_Fun1[value] )
        call _handle_boolexpr( Builtin::Boolexpr#_Fun2[value] )
    elseif ty == Builtin::Boolexpr#_OR then
        call _handle_boolexpr( Builtin::Boolexpr#_Fun1[value] )
        call _handle_boolexpr( Builtin::Boolexpr#_Fun2[value] )
    elseif ty == Builtin::Boolexpr#_NOT then
        call _handle_boolexpr( Builtin::Boolexpr#_Fun1[value] )
    endif
endfunction

function _handle_coroutine takes integer co returns nothing
    local integer ls = Table#_head[ Builtin::Coroutine#_return_yield[co] ]
    call Table#_mark_used( Builtin::Coroutine#_return_yield[co] )

    if Builtin::Coroutine#_return_resume[co] != 0 then
	call _push_value( Builtin::Coroutine#_return_resume[co] )
    endif

    set ls = Builtin::Coroutine#_stop_frame[co]
    loop
    exitwhen ls == 0
	call _push_context( Interpreter#_ctx[ls] )
	call List#_mark_used(ls) // meh
	exitwhen ls == Builtin::Coroutine#_base_frame[co]
	set ls = _next(ls)
    endloop
endfunction

function _handle_table takes integer value returns nothing
    local integer ls = Table#_head[ Value#_Int[value] ]
    local integer ls2

    call Table#_mark_used(Value#_Int[value] )
    call Table#_mark_used(Value#_Int2[value] )
    if Value#_Int3[value] != 0 then
	call Table#_mark_used(Value#_Int3[value] )
    endif

    loop
    exitwhen ls == 0
	//set _Debug = "_work_vqueue 1"
	call _push_value( Table#_val[ls])
	set ls = _next(ls)
    endloop

    set ls = Table#_head[ Value#_Int2[value] ]
    loop
    exitwhen ls == 0
	set ls2 = Table#_val[ls]
	loop
	exitwhen ls2 == 0
	    call _push_value( Value#_key[ls2] )
	    call _push_value( Value#_val[ls2] )
	    set ls2 = _next(ls2)
	endloop
	set ls = _next(ls)
    endloop

    if Value#_Int3[value] != 0 then
	call _push_value( Value#_Int3[value] )
    endif
endfunction

function _work_vqueue takes nothing returns nothing
    local integer i = VALUE_MAX_STEPS
    local integer ls
    local integer value
    local integer ty
    local integer val2

    local integer ls2
    local integer ls3
    //call Print#_print("_work_vqueue")


    loop
	set ls = Deque#_shift( _value_queue )
	exitwhen ls == 0
	set value = _value[ls]

	call Value#_mark_used( value )

	set ty = Value#_Type[value]

	//set bla =  "_work_vqueue table "+Value#_tostring_debug(value)

	if ty == Types#_Table then
	    call _handle_table(value)

	elseif ty == Types#_Lambda then
	    call _push_context( Value#_Int[value] )

	elseif ty == Types#_Coroutine then
	    call _handle_coroutine( value )
	elseif ty == Types#_Foreign then
	    if Value#_Int[value] == Jass#_trigger then
		set ls2 = Builtin::Trigger#_trigger_actions[value]
		loop
		exitwhen ls2 == 0
		    //set _Debug = "_work_vqueue 4"
		    call _push_value( Builtin::Trigger#_trigger_action[ls2] )
		    set ls2 = _next(ls2)
		endloop
	    elseif Value#_Int[value] == Jass#_triggeraction then
		//set _Debug = "_work_vqueue 5"
		call _push_value( Value#_Int2[ value ] )
	    elseif Value#_Int[value] == Jass#_timer then
		//set _Debug = "_work_vqueue 6"
		call _push_value( Value#_Int3[ value ] )
            elseif Value#_Int[value] == Jass#_boolexpr then
                call _handle_boolexpr( value )
	    endif
	    
	endif
    endloop
endfunction

function _work_cqueue takes nothing returns nothing
    local integer i = CONTEXT_MAX_STEPS
    local integer ls
    local integer ctx
    local integer j
    local integer ls2
    local integer val2
    local integer ls3
    local integer val3
    //call Print#_print("_work_cqueue")
    loop
	set ls = Deque#_shift( _context_queue )
	exitwhen ls == 0
	set ctx = _context[ls]

	call Context#_mark_used( ctx )

	call Table#_mark_used( Context#_tmps[ctx] )
	// walk tmps
	set ls2 = Table#_head[ Context#_tmps[ctx] ]
	loop
	exitwhen ls2 == 0
	    call _push_value( Table#_val[ls2] )
	    set ls2 = _next(ls2)
	endloop


	call Table#_mark_used( Context#_locals[ctx] )
	// walk locals
	set ls2 = Table#_head[ Context#_locals[ctx] ]
	loop
	exitwhen ls2 == 0
	    set ls3 = Table#_val[ls2]
	    loop
	    exitwhen ls3 == 0
		call _push_value( StringTable#_value[ls3] )
		set ls3 = _next(ls3)
	    endloop
	    set ls2 = _next(ls2)
	endloop

	// add parent contexts to GC workqueue
	loop
	    set ctx = Context#_parent[ctx]
	    exitwhen ctx == 0
	    call _push_context( ctx )
	endloop
    endloop
endfunction

function _full_mark_and_sweep takes nothing returns nothing
    loop
	call _work_iqueue()
	call _work_cqueue()
	call _work_vqueue()

	exitwhen Deque#_isEmpty(_context_queue) and Deque#_isEmpty(_value_queue)
    endloop

    call Value#_sweep()
    call Context#_sweep()
    call List#_sweep()
    call Table#_sweep()

    set _inqueue_flag = not _inqueue_flag
endfunction

function _trigger_gc takes nothing returns nothing
    local timer t = GetExpiredTimer()
    if _enabled then
	call _full_mark_and_sweep()
    endif
    call TimerStart(t, _timeout, false, function _trigger_gc)
    set t = null
endfunction

function _init takes nothing returns nothing
    set _interpreter_queue = Deque#_alloc()
    set _context_queue = Deque#_alloc()
    set _value_queue = Deque#_alloc()

    call TimerStart(CreateTimer(), _timeout, false, function _trigger_gc )
endfunction
