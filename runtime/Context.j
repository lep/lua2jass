// scope Context
// REQUIRES StringTable Print

globals
    #include "alloc-globals.j"

    integer array _ip
    // @type StringTable
    integer array _locals
    // @type LuaTable
    integer array _params
    // @type Table
    integer array _tmps
    // @type Context
    integer array _parent_call
    // @type Context
    integer array _parent

    string array  _chunk_name


    integer _hasrec_last_value = 0
endglobals

#include "alloc.j"

function _init takes integer ctx returns nothing
    set _locals[ctx] = Table#_alloc() // StringTable
    set _params[ctx] = Table#_alloc()
    set _tmps[ctx] = Table#_alloc()
endfunction


function _clone takes integer ctx returns integer
    local integer new_ctx = _alloc()
    call _init(new_ctx)
    set _ip[new_ctx] = _ip[ctx]
    set _chunk_name[new_ctx] = _chunk_name[ctx]
    set _parent[new_ctx] = ctx
    set _parent_call[new_ctx] = _parent_call[ctx]
    return new_ctx
endfunction

function _has_rec takes integer ctx, string name returns integer
    //call Print#_print("Context#_has_rec("+I2S(ctx)+","+name+")")
    loop
        exitwhen ctx == 0
	//call Print#_print("  - checking if "+I2S(ctx)+" has "+name)
	if StringTable#_has(_locals[ctx], name) then
	    //call Print#_print("  - yes")
	    return ctx
        endif
	//call Print#_print("  - no")
        set ctx = _parent[ctx]
    endloop
    //call Print#_print("  - not found at all. retuning 0")
    return 0
endfunction

function _set takes integer ctx, string name, integer value returns nothing
    local integer t = Print#_print("Context#_set("+I2S(ctx)+","+name+","+I2S(value)+")")
    local integer parent_ctx = _has_rec(ctx, name)
    //call Print#_print("  - parent_ctx is "+I2S(parent_ctx))
    if parent_ctx == 0 then
        call StringTable#_set(_locals[ctx], name, value)
	//call Print#_print("  - Reading again "+I2S(StringTable#_get(_locals[ctx], name)))
    else
        call StringTable#_set(_locals[parent_ctx], name, value)
	//call Print#_print("  - Reading again "+I2S(StringTable#_get(_locals[parent_ctx], name)))
    endif
endfunction

function _get takes integer ctx, string name returns integer
    //local integer t = Print#_print("Context#_get("+I2S(ctx)+","+name+")")
    local integer parent_ctx = _has_rec(ctx, name)
    local integer v
    //call Print#_print("  - parent_ctx = "+I2S(parent_ctx))
    if parent_ctx == 0 then
	//call Print#_print("  - returning 0")
	return 0
    else
	set v = StringTable#_get(_locals[parent_ctx], name)
	//call Print#_print("  - returning "+I2S(v))
	return v
    endif
endfunction


