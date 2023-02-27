// scope Builtins
// REQUIRES Print Value Table Context

function _print takes integer tbl, integer reg_res, integer ctx returns nothing
    local string r = ""
    local integer k = 1
    local integer v

    loop
        if Table#_has( tbl, k ) then
            set v = Table#_get(tbl, k)
            set r = r + Value#_tostring(v) + "    "
            set k = k +1
        else
            exitwhen true
        endif
    endloop
    call Print#_print(r)
endfunction

function _dispatch_builtin takes integer value, integer params, integer ctx, integer reg_res returns nothing
    local integer tbl = Value#_Int[params]
    local string name = Value#_String[value]
    if name == "print" then
        call _print(tbl, reg_res, ctx)
    else
        call Print#_print("Unknown builtin function "+name)
    endif
endfunction


function _register_builtin takes integer ctx, string name, integer id returns nothing
    call Context#_set( ctx, name, Value#_builtin(name) )
endfunction

