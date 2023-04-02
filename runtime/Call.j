// scope Call
// REQUIRES Wrap Table

function _call0  takes integer fn, integer ret, integer interpreter returns nothing
    local integer params = Table#_alloc()

    call Table#_set( params, 0, ret ) // return value
    //call Table#_set( params, 1, p1 )

    call Wrap#_call_function( fn, params, interpreter)
endfunction


function _call1  takes integer fn, integer p1, integer ret, integer interpreter returns nothing
    local integer params = Table#_alloc()

    call Table#_set( params, 0, ret ) // return value
    call Table#_set( params, 1, p1 )

    call Wrap#_call_function( fn, params, interpreter)
endfunction

function _call2  takes integer fn, integer p1, integer p2, integer ret, integer interpreter returns nothing
    local integer params = Table#_alloc()

    call Table#_set( params, 0, ret ) // return value
    call Table#_set( params, 1, p1 )
    call Table#_set( params, 2, p2 )

    call Wrap#_call_function( fn, params, interpreter)
endfunction

function _call3  takes integer fn, integer p1, integer p2, integer p3, integer ret, integer interpreter returns nothing
    local integer params = Table#_alloc()

    call Table#_set( params, 0, ret ) // return value
    call Table#_set( params, 1, p1 )
    call Table#_set( params, 2, p2 )
    call Table#_set( params, 3, p3 )

    call Wrap#_call_function( fn, params, interpreter)
endfunction
