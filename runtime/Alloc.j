globals
    integer _F = 0
    integer array _V
    integer _I = 0
endglobals

function _alloc takes nothing returns integer
    local integer this = _F
    if this != 0 then
        set _F = _V[this]
    else
        set _I = _I+1
        set this = _I
    endif
    
    if this >= JASS_MAX_ARRAY_SIZE then
        call Print#_warn("No more free instances ")
        return 0
    endif

    set _V[this] = -1
    return this
endfunction

function _free takes integer this returns nothing
    if this == 0 then
        call Print#_warn("Attempt to free nullptr ")
        return
    elseif _V[this] != -1 then
        call Print#_warn("Attempt to double free ")
        return
    endif
    set _V[this] = _F
    set _F = this
endfunction
