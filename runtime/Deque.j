// scope Deque
// REQUIRES Print Alloc

globals
    // global for fresh cells
    //integer _count = 0

    // struct cell
    integer array _next
    integer array _prev



    // struct deque
    integer array _head
endglobals


// seems to work
function _push takes integer queue returns integer
    local integer new = _alloc()
    local integer head = _head[queue]
    local integer tmp = _prev[head]

    if head == 0 then
	set _prev[new] = new
	set _next[new] = new
	set _head[queue] = new
    else
	set _prev[head] = new
	set _next[tmp] = new
	set _prev[new] = tmp
	set _next[new] = head
    endif

    return new
endfunction

// seems to work
function _shift takes integer queue returns integer
    local integer head = _head[queue]
    local integer last = _prev[head]

    if head == 0 then	    // 0 elements
	return 0
    elseif last == head then // 1 element
	set _head[queue] = 0
	return last
    else		    // n elements
	set _head[queue] = _next[head]
	set _next[last] = _head[queue]
	set _prev[_head[queue]] = last
    endif


    call _free(head)
    return head
endfunction

function _isEmpty takes integer queue returns boolean
    return queue == 0 or _head[queue] == 0
endfunction

//function _pop_or_alloc takes integer deque returns integer
//    local integer cell = _head[deque]
//    local integer cell_last
//    //call Print#_print("_pop_or_alloc")
//    if cell == 0 then
//        //call Print#_print("  - cell is 0 ")
//        set _count = _count + 1
//        set cell = _count
//    elseif _next[cell] == cell then
//        set _head[deque] = 0
//    else
//        set cell_last = _prev[cell]
//        set _head[deque] = _next[cell]
//
//        set _prev[_head[deque]] = cell_last
//        set _next[cell_last] = _head[deque]
//
//    endif
//    if cell > JASS_MAX_ARRAY_SIZE then
//        call Print#_error( "deque cell max array size" )
//    endif
//    set _next[cell] = cell
//    set _prev[cell] = cell
//    return cell
//endfunction
//
//function _move takes integer cell, integer deque returns nothing
//    local integer head_cell = _head[deque]
//    //call Print#_print("_move")
//    if head_cell != 0 then
//        //call Print#_print("  - head_cell is not 0")
//        set _next[cell] = head_cell
//        set _prev[cell] = _prev[head_cell]
//        set _next[_prev[head_cell]] = cell
//        set _prev[head_cell] = cell
//    endif
//    set _head[deque] = cell
//endfunction
//
//function _fresh takes integer deque, integer recycler returns integer
//    //local integer t = Print#_print("_fresh")
//    local integer cell = _pop_or_alloc( recycler )
//    //call Print#_print("  - cell: "+I2S(cell))
//    call _move( cell, deque )
//    return cell
//endfunction
//
//function _move_all takes integer source_deque, integer target_deque returns nothing
//    local integer source_cell = _head[source_deque]
//    local integer target_cell = _head[target_deque]
//    local integer source_last_cell = _prev[source_cell]
//    local integer target_last_cell = _prev[target_cell]
//    //call Print#_print("_move_all")
//    
//    if source_cell != 0 and target_cell != 0 then
//        //call Print#_print("  - both are not 0")
//        set _next[source_last_cell] = target_cell
//        set _prev[target_cell] = source_last_cell
//
//        set _prev[source_cell] = target_last_cell
//        set _next[target_last_cell] = source_cell
//
//        set _head[source_deque] = 0
//        set _head[target_deque] = source_cell
//
//    elseif target_cell == 0  and source_cell != 0 then
//        //call Print#_print("  - target_cell = 0")
//        set _head[target_deque] = source_cell
//        set _head[source_deque] = 0
//    elseif target_cell != 0 and source_cell == 0 then
//        call Print#_error("_move_all both cells are 0")
//    endif
//endfunction
