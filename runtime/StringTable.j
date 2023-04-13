// scope StringTable
// REQUIRES Table List Print

globals
    // both indexed by _ls
    string array _key
    integer array _value

    integer array _ls
endglobals

function _set takes integer tbl, string k, integer v returns nothing
    local integer lst = Table#_get(tbl, StringHash(k))
    local integer tmp = lst

    loop
    exitwhen tmp == 0
        if _key[tmp] == k then
            set _value[tmp] = v
            return
        endif
        set tmp = List#_next[tmp]
    endloop
    // either _lst was 0 in the first place or no element was found in the list
    set lst = List#_cons(lst)

    set _key[lst] = k
    set _value[lst] = v
    call Table#_set(tbl, StringHash(k), lst)
endfunction

function _get takes integer tbl, string k returns integer
    local integer _lst = Table#_get(tbl, StringHash(k))
    loop
    exitwhen _lst == 0
        if _key[_lst] == k then
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
