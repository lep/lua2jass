// scope Builtin/Boolexpr
// REQUIRES Print Value Table Jass Call Natives

globals
    integer _v
    integer _r
    integer _i

    integer _code_v
    integer _code_r
    integer _code_i

    boolexpr _filter
endglobals

function _cb takes nothing returns nothing
    call Call#_call0( _code_v, _code_r, _code_i )
endfunction

function _eval takes integer v, integer r, integer i returns boolean
    local integer t = Table#_get( Value#_Int[v], 'type' )
    local integer sub_type = Table#_get( Value#_Int[v], 'subt' )
    local integer f1
    local integer f2

    if Value#_Type[v] == Types#_Nil then
	return true
    endif

    if t != Jass#_boolexpr then
	return false
    endif

    if sub_type == 0 then
	set f1 = Table#_get( Value#_Int[v], 'func' )
	call Call#_call0( f1, r, i )
	return Value#_truthy( Table#_get( Value#_Int[r], 1 ) )
    elseif sub_type == 'Op/A' then
	set f1 = Table#_get( Value#_Int[v], 'arg1' )
	set f2 = Table#_get( Value#_Int[v], 'arg2' )

	return _eval(f1, Value#_table(), i) and _eval(f2, Value#_table(), i)

    elseif sub_type == 'Op/O' then
	set f1 = Table#_get( Value#_Int[v], 'arg1' )
	set f2 = Table#_get( Value#_Int[v], 'arg2' )

	return _eval(f1, Value#_table(), i) or _eval(f2, Value#_table(), i)
    elseif sub_type == 'Op/N' then
	set f1 = Table#_get( Value#_Int[v], 'arg1' )
	return not _eval(f1, Value#_table(), i)
    endif

    return false
endfunction

function _boolexpr takes nothing returns boolean
    return _eval( _v, _r, _i )
endfunction

function _Filter takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer obj = Value#_table()

    // this doesn't solve our problems as we cannot (afair) get the conditions
    // from the trigger anyways. so we have to implement all the trigger-functions
    // by hand anyways
    //local boolexpr f = And(function _filter, null) // TODO: check if this has the correct properties
    //call Table#_set( _boolexpr2value, GetHandleId(f), obj )

    call Table#_set( Value#_Int[obj], 'type', Jass#_boolexpr )
    call Table#_set( Value#_Int[obj], 'intp', interpreter )
    call Table#_set( Value#_Int[obj], 'func', arg1 )

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _Condition takes integer tbl, integer ctx, integer interpreter returns nothing
    call _Filter( tbl, ctx, interpreter )
endfunction

function _And takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer obj = Value#_table()

    call Table#_set( Value#_Int[obj], 'type', Jass#_boolexpr )
    call Table#_set( Value#_Int[obj], 'intp', interpreter )
    call Table#_set( Value#_Int[obj], 'arg1', arg1 )
    call Table#_set( Value#_Int[obj], 'arg2', arg2 )
    call Table#_set( Value#_Int[obj], 'subt', 'Op/A' )

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _Or takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer obj = Value#_table()

    call Table#_set( Value#_Int[obj], 'type', Jass#_boolexpr )
    call Table#_set( Value#_Int[obj], 'intp', interpreter )
    call Table#_set( Value#_Int[obj], 'arg1', arg1 )
    call Table#_set( Value#_Int[obj], 'arg2', arg2 )
    call Table#_set( Value#_Int[obj], 'subt', 'Op/O' )

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _Not takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer obj = Value#_table()

    call Table#_set( Value#_Int[obj], 'type', Jass#_boolexpr )
    call Table#_set( Value#_Int[obj], 'intp', interpreter )
    call Table#_set( Value#_Int[obj], 'arg1', arg1 )
    call Table#_set( Value#_Int[obj], 'subt', 'Op/N' )

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _EnumItemsInRect takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer arg3 = Table#_get(tbl, 3)

    set _code_v = arg3
    set _code_i = interpreter
    set _code_r = Value#_table()

    call EnumItemsInRect( Natives#_convert2rect(arg1, interpreter), Natives#_convert2boolexpr(arg2, interpreter), function _cb )

endfunction

function _EnumDestructablesInRect takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer arg3 = Table#_get(tbl, 3)

    set _code_v = arg3
    set _code_i = interpreter
    set _code_r = Value#_table()

    call EnumDestructablesInRect( Natives#_convert2rect(arg1, interpreter), Natives#_convert2boolexpr(arg2, interpreter), function _cb )

endfunction

function _init takes nothing returns nothing
    set _filter = Filter( function _boolexpr )
endfunction
