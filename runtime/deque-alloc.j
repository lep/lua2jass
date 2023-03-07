
#include "alloc.j"

function _D_pop_or_alloc takes integer deque returns integer
    local integer cell = _D_head[deque]
    local integer cell_last
    //call Print#_print("_D_pop_or_alloc")
    if cell == 0 then
        //call Print#_print("  - cell is 0 ")
        set _D_count = _D_count + 1
        set cell = _D_count
    elseif _D_next[cell] == cell then
        set _D_head[deque] = 0
    else
        set cell_last = _D_prev[cell]
        set _D_head[deque] = _D_next[cell]

        set _D_prev[_D_head[deque]] = cell_last
        set _D_next[cell_last] = _D_head[deque]

    endif
    if cell > JASS_MAX_ARRAY_SIZE then
        call Print#_error( "deque cell max array size" )
    endif
    set _D_next[cell] = cell
    set _D_prev[cell] = cell
    return cell
endfunction

function _D_move takes integer cell, integer deque returns nothing
    local integer head_cell = _D_head[deque]
    //call Print#_print("_D_move")
    if head_cell != 0 then
        //call Print#_print("  - head_cell is not 0")
        set _D_next[cell] = head_cell
        set _D_prev[cell] = _D_prev[head_cell]
        set _D_next[_D_prev[head_cell]] = cell
        set _D_prev[head_cell] = cell
    endif
    set _D_head[deque] = cell
endfunction

function _fresh takes integer deque, integer recycler returns integer
    //local integer t = Print#_print("_fresh")
    local integer cell = _D_pop_or_alloc( recycler )
    //call Print#_print("  - cell: "+I2S(cell))
    call _D_move( cell, deque )
    return cell
endfunction

function _D_move_all takes integer source_deque, integer target_deque returns nothing
    local integer source_cell = _D_head[source_deque]
    local integer target_cell = _D_head[target_deque]
    local integer source_last_cell = _D_prev[source_cell]
    local integer target_last_cell = _D_prev[target_cell]
    //call Print#_print("_D_move_all")
    
    if source_cell != 0 and target_cell != 0 then
        //call Print#_print("  - both are not 0")
        set _D_next[source_last_cell] = target_cell
        set _D_prev[target_cell] = source_last_cell

        set _D_prev[source_cell] = target_last_cell
        set _D_next[target_last_cell] = source_cell

        set _D_head[source_deque] = 0
        set _D_head[target_deque] = source_cell

    elseif target_cell == 0  and source_cell != 0 then
        //call Print#_print("  - target_cell = 0")
        set _D_head[target_deque] = source_cell
        set _D_head[source_deque] = 0
    elseif target_cell != 0 and source_cell == 0 then
        call Print#_error("_D_move_all both cells are 0")
    endif
endfunction

