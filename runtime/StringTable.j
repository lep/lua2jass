// scope StringTable
// REQUIRES Table List Print

globals
    string array _key
    integer array _value

    integer array _head
    integer array _ls
endglobals

function _set takes integer _tbl, string _k, integer _v returns nothing
    local integer _lst = Table#_get(_tbl, StringHash(_k))
    local integer _tmp = _lst
    loop
    exitwhen _tmp == 0
        if _key[_tmp] == _k then
            set _value[_tmp] = _v
            return
        endif
        set _tmp = List#_next[_tmp]
    endloop
    // either _lst was 0 in the first place or no element was found in the list
    set _lst = List#_cons(_lst)

    set _head[_tbl] = List#_cons(_head[_tbl])
    set _ls[_head[_tbl]] = _lst

    set _key[_lst] = _k
    set _value[_lst] = _v
    call Table#_set(_tbl, StringHash(_k), _lst)
endfunction

function _get takes integer _tbl, string _k returns integer
    local integer _lst = Table#_get(_tbl, StringHash(_k))
    loop
    exitwhen _lst == 0
        if _key[_lst] == _k then
            return _value[_lst]
        endif
        set _lst = List#_next[_lst]
    endloop
    return 0
endfunction

function _has takes integer tbl, string k returns boolean
    local integer lst = Table#_get(tbl, StringHash(k))
    loop
    exitwhen lst == 0
	if _key[lst] == k then
	    return true
	endif
	set lst = List#_next[lst]
    endloop

    return false
endfunction

function _dealloc takes integer tbl returns nothing
    local integer ls = _head[tbl]
    loop
    exitwhen ls == 0
        call List#_free(_ls[ls])
        set ls = List#_next[ls]
    endloop


    call Table#_free(tbl)
    set _head[tbl] = 0
endfunction
