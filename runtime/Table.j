// scope Table
// REQUIRES List Print

globals
    #include "alloc-globals.j"

    hashtable _tbl = InitHashtable()
    integer array _head


    // indexed by list
    // for GC purposes
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
        set _head[tbl] = ls
        call SaveInteger(_tbl, tbl, reg, ls)
    endif
endfunction

function _get takes integer tbl, integer reg returns integer
    return _val[ LoadInteger(_tbl, tbl, reg) ]
endfunction

function _has takes integer tbl, integer reg returns boolean
    return HaveSavedInteger(_tbl, tbl, reg)
endfunction
