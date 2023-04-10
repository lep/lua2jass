// scope Builtin::Boolexpr
// REQUIRES Print Value Table Jass Call Natives

globals
    integer _v
    integer _r
    integer _i

    integer _code_v
    integer _code_r
    integer _code_i

    boolexpr _filter

    constant integer _NORMAL = 1
    constant integer _AND = 2
    constant integer _OR = 3
    constant integer _NOT = 4

    integer array _Fun1
    integer array _Fun2
endglobals

function _cb takes nothing returns nothing
    call Call#_call0( _code_v, _code_r, _code_i )
endfunction

function _eval takes integer v, integer r, integer i returns boolean
    local integer t = Value#_Int[v]
    local integer sub_type = Value#_Int3[v]

    if Value#_Type[v] == Types#_Nil then
	return true
    endif

    if t != Jass#_boolexpr then
	return false
    endif

    if sub_type == _NORMAL then
	call Call#_call0( _Fun1[v], r, i )
	return Value#_truthy( Table#_get( Value#_Int[r], 1 ) )
    elseif sub_type == _AND then
	return _eval(_Fun1[v], Value#_table(), i) and _eval(_Fun2[v], Value#_table(), i)
    elseif sub_type == _OR then
	return _eval(_Fun1[v], Value#_table(), i) or _eval(_Fun2[v], Value#_table(), i)
    elseif sub_type == _NOT then
	return not _eval(_Fun1[v], Value#_table(), i)
    endif

    return false
endfunction

function _boolexpr takes nothing returns boolean
    return _eval( _v, _r, _i )
endfunction

function _Filter takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer obj = Value#_foreign( Jass#_boolexpr )

    // this doesn't solve our problems as we cannot (afair) get the conditions
    // from the trigger anyways. so we have to implement all the trigger-functions
    // by hand anyways
    //local boolexpr f = And(function _filter, null) // TODO: check if this has the correct properties
    //call Table#_set( _boolexpr2value, GetHandleId(f), obj )

    set Value#_Int2[obj] = interpreter
    set Value#_Int3[obj] = _NORMAL
    set _Fun1[obj] = arg1

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _And takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer obj = Value#_foreign(Jass#_boolexpr)

    set Value#_Int2[obj] = interpreter
    set Value#_Int3[obj] = _AND
    set _Fun1[obj] = arg1
    set _Fun2[obj] = arg2

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _Or takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)
    local integer obj = Value#_foreign(Jass#_boolexpr)

    set Value#_Int2[obj] = interpreter
    set Value#_Int3[obj] = _OR
    set _Fun1[obj] = arg1
    set _Fun2[obj] = arg2

    call Table#_set( Value#_Int[r], 1, obj )
    
endfunction

function _Not takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer obj = Value#_foreign(Jass#_boolexpr)

    set Value#_Int2[obj] = interpreter
    set Value#_Int3[obj] = _NOT
    set _Fun1[obj] = arg1
    set _Fun2[obj] = 0

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
