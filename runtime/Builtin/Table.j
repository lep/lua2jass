// scope Builtin/Table
// REQUIRES Print Value Context

globals
    integer array _heap

    integer _interpreter
    integer _compfunc
endglobals

function _swap takes integer i, integer j returns nothing
    local integer tmp = _heap[i]
    set _heap[i] = _heap[j]
    set _heap[j] = tmp
endfunction

function _iParent takes integer i returns integer
    return (i-1) / 2
endfunction

function _iLeftChild takes integer i returns integer
    return 2*i +1
endfunction

function _comp takes integer a, integer b returns boolean
    local integer r
    if _compfunc == 0 then
        return Value#_lt_numeric_noalloc(a, b)
    else
        set r = Value#_table()
        call Call#_call2( _compfunc, a, b, r, _interpreter )
        return Value#_truthy(Value#_gettable(r, Value#_litint(1)))
    endif
endfunction

function _siftDown takes integer start, integer end returns nothing
    local integer root = start
    local integer child
    local integer swap
    loop
    exitwhen _iLeftChild(root) > end
        set child = _iLeftChild(root)
        set swap = root

        if _comp(_heap[swap], _heap[child]) then
            set swap = child
        endif

        if child +1 <= end and _comp(_heap[swap], _heap[child+1]) then
            set swap = child + 1
        endif

        if swap == root then
            return
        else
            call _swap(root, swap)
            set root = swap
        endif
    endloop
endfunction

function _heapify takes integer count returns nothing
    local integer start = _iParent(count-1)
    loop
    exitwhen start < 0
        call _siftDown(start, count-1)
        set start = start -1
    endloop
endfunction

// copy'n'paste heapsort
// taken from wikipedia
function _sort takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer list = Table#_get( tbl, 1 )

    local integer i = 1
    local integer count = 0
    local integer end

    set _compfunc = Table#_get( tbl, 2 )
    set _interpreter = interpreter


    loop
        if Table#_has( Value#_Int[list], i ) then
            set _heap[count] = Table#_get( Value#_Int[list], i )
        else
            exitwhen true
        endif
        set i = i + 1
        set count = count +1
    endloop

    call _heapify(count)
    set end = count -1
    loop
    exitwhen end <= 0
        call _swap(end, 0)
        set end = end -1
        call _siftDown(0, end)
    endloop

    set i = 0
    loop
    exitwhen i == count
        call Table#_set( Value#_Int[list], i+1, _heap[i] )
        set i = i +1
    endloop

    call Table#_set( Value#_Int[r], 1, Value#_litnil() )
endfunction

function _concat takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer list = Table#_get( tbl, 1 )
    local integer sep = Table#_get( tbl, 2 )
    local integer i = Table#_get( tbl, 3 )
    local integer j = Table#_get( tbl, 4 )

    local string sep_s = ""
    local string r_s = ""


    local boolean not_first = false

    if sep != 0 then
        set sep_s = Value#_tostring_concat(sep)
    endif

    if i == 0 then
        set i = 1
    else
        set i = Value#_integercontext(i)
        set i = Value#_Int[i]
    endif

    if j == 0 then
        set j = Table#_len( Value#_Int[list] )
    else
        set j = Value#_integercontext(j)
        set j = Value#_Int[j]
    endif

    if i > j then
        call Table#_set( Value#_Int[r], 1, Value#_litstring(r_s) )
        return
    endif

    loop
        exitwhen i > j
        if Table#_has( Value#_Int[list], i ) then
            if not_first then
                set r_s = r_s + sep_s
            endif
            set r_s = r_s + Value#_tostring_concat(Table#_get(Value#_Int[list], i))
        else
            call Value#_error_str("invalid value (nil) at index "+I2S(i)+" in table for 'concat'")
            return
        endif
        set not_first = true
        set i = i +1
    endloop

    call Table#_set( Value#_Int[r], 1, Value#_litstring(r_s) )
endfunction

function _register takes integer ctx returns nothing
    local integer table_table = Value#_table()
    call Value#_settable( table_table, Value#_litstring("sort"), Context#_get(ctx, "$table.sort") )
    call Value#_settable( table_table, Value#_litstring("concat"), Context#_get(ctx, "$table.concat") )

    call Context#_set( ctx, "table", table_table )
endfunction
