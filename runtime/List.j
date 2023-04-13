// scope List
// REQUIRES Print Alloc

globals
    integer array _next

    boolean array _mark
endglobals

function _cons takes integer tl returns integer
    local integer new = _alloc()
    set _next[new] = tl
    return new
endfunction

function _destroy takes integer ls returns nothing
    set _next[ls] = 0
    call _free(ls)
endfunction

function _mark_used takes integer l returns nothing
    set _mark[l] = GC#_inqueue_flag
endfunction

function _sweep takes nothing returns nothing
    local integer i = 1
    loop
    exitwhen i >= _I
	if _V[i] == -1 and _mark[i] != GC#_inqueue_flag then
	    call _destroy(i)
	endif
	set i = i +1
    endloop
endfunction
