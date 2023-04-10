// scope List
// REQUIRES Print Alloc

globals
    integer array _next
endglobals

function _cons takes integer tl returns integer
    local integer new = _alloc()
    set _next[new] = tl
    return new
endfunction

function _destroy takes integer l returns nothing
    loop
    exitwhen l == 0
        call _free(l)
        set l = _next[l]
    endloop
endfunction

