// scope Builtin/Math
// REQUIRES Print Context Value

// TODO: log, max, min
// TODO: type, tointeger

function _abs takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer x = Table#_get( tbl, 1 )

    set x = Value#_numbercontext(x)
    if Value#_Type[x] == Jass#_integer then
        if Value#_Int[x] < 0 then
            call Table#_set( Value#_Int[r], 1, Value#_litint(-Value#_Int[x]))
        else
            call Table#_set( Value#_Int[r], 1, x)
        endif
    else
        if Value#_Real[x] < 0 then
            call Table#_set( Value#_Int[r], 1, Value#_litfloat(-Value#_Real[x]))
        else
            call Table#_set( Value#_Int[r], 1, x)
        endif
    endif
endfunction

function _atan takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    local integer n = Table#_get( tbl, 2 )

    if m != 0 and n != 0 then
        set m = Value#_numbercontext(m)
        set n = Value#_numbercontext(n)
        call Table#_set( Value#_Int[r], 1, Value#_litfloat( Atan2( Value#_2real(m, interpreter), Value#_2real(n, interpreter) ) ) )
    else
        set m = Value#_numbercontext(m)
        call Table#_set( Value#_Int[r], 1, Value#_litfloat( Atan( Value#_2real(m, interpreter) ) ) )
    endif
endfunction

function _ceil takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer x = Table#_get( tbl, 1 )
    set x = Value#_numbercontext(x)
    call Table#_set( Value#_Int[r], 1, Value#_ceil(Value#_2real(x, interpreter) ))
endfunction

function _floor takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer x = Table#_get( tbl, 1 )
    set x = Value#_numbercontext(x)
    set x = Value#_idiv( x, Value#_litint(1) )
    call Table#_set( Value#_Int[r], 1, x )
endfunction

function _fmod takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    local integer n = Table#_get( tbl, 2 )

    call Table#_set( Value#_Int[r], 1, Value#_mod( Value#_numbercontext(m), Value#_numbercontext(n) ) )

endfunction

function _log takes integer tbl, integer ctx, integer interpreter returns nothing
endfunction

function _max takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    local integer n
    local integer i = 2
    if m == 0 then
        call Value#_error_str("bad argument #1 to 'max' (value expected)")
        return
    endif

    loop
        set n = Table#_get( tbl, i )
        exitwhen n == 0
        if Value#_lt_numeric_noalloc(m, n) then
            set m = n
        endif
        set i = i +1
    endloop

    call Table#_set( Value#_Int[r], 1, m )
endfunction

function _min takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    local integer n
    local integer i = 2
    call Print#_print("_min")
    if m == 0 then
        call Value#_error_str("bad argument #1 to 'min' (value expected)")
        return
    endif

    loop
        set n = Table#_get( tbl, i )
        exitwhen n == 0
        if Value#_lt_numeric_noalloc(n, m) then
            set m = n
        endif
        set i = i +1
    endloop

    call Table#_set( Value#_Int[r], 1, m )
endfunction

function _modf takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer x = Table#_get( tbl, 1 )
    local real xr
    set x = Value#_numbercontext(x)
    set xr = Value#_2real(x, interpreter)
    set x = R2I(xr)
    call Table#_set( Value#_Int[r], 1, Value#_litint(x))
    call Table#_set( Value#_Int[r], 2, Value#_litfloat(xr - x))

endfunction

function _random takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    local integer n = Table#_get( tbl, 2 )

    // according to <https://www.hiveworkshop.com/threads/reforgeds-luahelper-lua-and-broken-fourcc.337280/>
    // math.random("123", "456") *should* work in contrast to other natives,
    // which only take *real* numbers.

    // Do note, that m and n come from `Table`s and not `Value#_table` so
    // we have to check against `0` and not `nil`.
    if m != 0 and n != 0 then
        set m = Value#_idiv( Value#_numbercontext(m), Value#_litint(1) )
        set n = Value#_idiv( Value#_numbercontext(n), Value#_litint(1) )
        set m = Value#_Int[m]
        set n = Value#_Int[n]
        call Table#_set( Value#_Int[r], 1, Value#_litint( GetRandomInt(m, n) ) )
    elseif m != 0 then
        set m = Value#_numbercontext(m)
        set m = Value#_idiv( m, Value#_litint(1) )
        set m = Value#_Int[m]
        call Table#_set( Value#_Int[r], 1, Value#_litint( GetRandomInt(1, m) ) )
    else
        call Table#_set( Value#_Int[r], 1, Value#_litfloat( GetRandomReal(0, 1) ) )
    endif

endfunction

function _randomseed takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get( tbl, 0 )
    local integer m = Table#_get( tbl, 1 )
    set m = Value#_idiv( Value#_numbercontext(m), Value#_litint(1) )
    set m = Value#_Int[m]
    call SetRandomSeed( m )
    call Table#_set( Value#_Int[r], 1, Value#_litnil() )
endfunction

function _type takes integer tbl, integer ctx, integer interpreter returns nothing
endfunction

function _tointeger takes integer tbl, integer ctx, integer interpreter returns nothing
endfunction

function _register takes integer ctx returns nothing
    local integer math_table = Value#_table()
    call Value#_settable( math_table, Value#_litstring("acos"), Context#_get(ctx, "Acos") )
    call Value#_settable( math_table, Value#_litstring("asin"), Context#_get(ctx, "Asin") )
    call Value#_settable( math_table, Value#_litstring("cos"), Context#_get(ctx, "Cos") )
    call Value#_settable( math_table, Value#_litstring("deg"), Context#_get(ctx, "Rad2Deg") )
    call Value#_settable( math_table, Value#_litstring("exp"), Context#_get(ctx, "Pow") )
    call Value#_settable( math_table, Value#_litstring("rad"), Context#_get(ctx, "Deg2Rad") )
    call Value#_settable( math_table, Value#_litstring("sin"), Context#_get(ctx, "Sin") )
    call Value#_settable( math_table, Value#_litstring("sqrt"), Context#_get(ctx, "SquareRoot") )
    call Value#_settable( math_table, Value#_litstring("tan"), Context#_get(ctx, "Tan") )

    call Value#_settable( math_table, Value#_litstring("pi"), Value#_litfloat(3.141592653) )

    call Value#_settable( math_table, Value#_litstring("random"), Context#_get(ctx, "$math.random") )
    call Value#_settable( math_table, Value#_litstring("randomseed"), Context#_get(ctx, "$math.randomseed") )
    call Value#_settable( math_table, Value#_litstring("fmod"), Context#_get(ctx, "$math.fmod") )
    call Value#_settable( math_table, Value#_litstring("modf"), Context#_get(ctx, "$math.modf") )
    call Value#_settable( math_table, Value#_litstring("atan"), Context#_get(ctx, "$math.atan") )
    call Value#_settable( math_table, Value#_litstring("floor"), Context#_get(ctx, "$math.floor") )
    call Value#_settable( math_table, Value#_litstring("ceil"), Context#_get(ctx, "$math.ceil") )
    call Value#_settable( math_table, Value#_litstring("abs"), Context#_get(ctx, "$math.abs") )
    call Value#_settable( math_table, Value#_litstring("min"), Context#_get(ctx, "$math.min") )
    call Value#_settable( math_table, Value#_litstring("max"), Context#_get(ctx, "$math.max") )

    call Context#_set( ctx, "math", math_table )
endfunction

