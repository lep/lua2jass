globals
    integer _F = 0
    integer array _V
    integer _I = 0

    integer _stats_high = 0
    integer _stats_live = 0
    integer _stats_count_alloc = 0
    integer _stats_count_free = 0
endglobals

function _alloc takes nothing returns integer
    local integer this = _F
    set _stats_live = _stats_live + 1
    set _stats_count_alloc = _stats_count_alloc + 1
    if this != 0 then
        set _F = _V[this]
    else
        set _I = _I+1
        set this = _I
	if this > _stats_high then
	    set _stats_high = this
	endif
    endif
    
    if this >= JASS_MAX_ARRAY_SIZE then
        call Print#_warn("No more free instances " + _alloc_name )
        return 0
    endif

    set _V[this] = -1
    return this
endfunction

function _free takes integer this returns nothing
    set _stats_count_free = _stats_count_free + 1
    if this == 0 then
        call Print#_warn("Attempt to free nullptr " + _alloc_name)
        return
    elseif _V[this] != -1 then
        call Print#_warn("Attempt to double free " + _alloc_name)
        return
    endif
    set _stats_live = _stats_live - 1
    set _V[this] = _F
    set _F = this
endfunction
