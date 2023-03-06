// scope Context
// REQUIRES StringTable Print

globals
    #include "alloc-globals.j"

    integer array _ip

    // @type StringTable
    integer array _locals

    // @type Table
    integer array _tmps

    // @type Context
    integer array _parent

    // @type integer
    integer array _ret_behaviour

    string array  _chunk_name


    integer _hasrec_last_value = 0
endglobals

#include "alloc.j"

function _init takes integer ctx returns nothing
    set _locals[ctx] = Table#_alloc() // StringTable
    //set _params[ctx] = Table#_alloc()
    set _tmps[ctx] = Table#_alloc()
    set _ret_behaviour[ctx] = 0
endfunction


function _clone takes integer ctx returns integer
    local integer new_ctx = _alloc()
    call _init(new_ctx)
    set _ip[new_ctx] = _ip[ctx]
    set _chunk_name[new_ctx] = _chunk_name[ctx]
    set _parent[new_ctx] = _parent[ctx]
    return new_ctx
endfunction

function _has_rec takes integer ctx, string name returns integer
    loop
        exitwhen ctx == 0
	if StringTable#_has(_locals[ctx], name) then
	    return ctx
        endif
        set ctx = _parent[ctx]
    endloop
    return 0
endfunction

function _set takes integer ctx, string name, integer value returns nothing
    local integer parent_ctx = _has_rec(ctx, name)
    if parent_ctx == 0 then
        call StringTable#_set(_locals[ctx], name, value)
    else
        call StringTable#_set(_locals[parent_ctx], name, value)
    endif
endfunction

function _get takes integer ctx, string name returns integer
    local integer parent_ctx = _has_rec(ctx, name)
    local integer v
    if parent_ctx == 0 then
	return 0
    else
	set v = StringTable#_get(_locals[parent_ctx], name)
	return v
    endif
endfunction


