// scope Table
// REQUIRES List Print

globals
    #include "alloc-globals.j"

    hashtable _tbl = InitHashtable()
    integer array _head


    // indexed by list
    // for GC (and other) purposes
    integer array _key
    integer array _val
endglobals

#include "alloc.j"

function _set takes integer tbl, integer reg, integer val returns nothing
    local integer ls = LoadInteger(_tbl, tbl, reg)

    if ls != 0 then
        set _val[ls] = val
    else
        set ls = List#_cons(_head[tbl])
        set _val[ls] = val
	set _key[ls] = reg
        set _head[tbl] = ls
        call SaveInteger(_tbl, tbl, reg, ls)
    endif
endfunction

function _internal_get_list_entry takes integer tbl, integer reg returns integer
    return LoadInteger(_tbl, tbl, reg)
endfunction

function _get takes integer tbl, integer reg returns integer
    return _val[ LoadInteger(_tbl, tbl, reg) ]
endfunction

function _has takes integer tbl, integer reg returns boolean
    return HaveSavedInteger(_tbl, tbl, reg)
endfunction

function _remove takes integer tbl, integer reg returns nothing
    local integer ls = LoadInteger(_tbl, tbl, reg)
    local integer head = _head[tbl]

    if ls == 0 then
	return
    endif

    if head == ls then
	set _head[tbl] = List#_next[head]
    else
	loop
	exitwhen List#_next[head] == ls
	    set head = List#_next[head]
	endloop
	set List#_next[head] = List#_next[ls]
    endif

    call RemoveSavedInteger(_tbl, tbl, reg)
endfunction

function _append takes integer target, integer source, integer offset returns nothing
    local integer k = 1

    loop
	if Table#_has( source, k ) then
	    call Table#_set( target, k + offset, _get( source, k ))
	else
	    exitwhen true
	endif
	set k = k +1
    endloop

endfunction

function _getlist takes integer target, integer source, integer offset returns nothing
    local integer k = offset

    loop
	if _has( source, k ) then
	    call _set( target, k - offset +1, _get( source, k ))
	else
	    exitwhen true
	endif
	set k = k +1
    endloop
endfunction

function _len takes integer tbl returns integer
    local integer k = 1
    loop
	if Table#_has( tbl, k ) then
	    set k = k +1
	else
	    return k - 1
	endif
    endloop
    return 0
endfunction
