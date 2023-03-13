// scope Builtin/Player
// REQUIRES Print Value Table Jass


globals
    player array _value2player
endglobals

// TODO: autogenerate this stuff
function _Player takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer integer_value = Table#_get( tbl, 1 )
    local integer return_table = Table#_get( tbl, 0 )
    local integer player_value = Value#_table()

    set _value2player[player_value] = Player(Value#_Int[integer_value])
    call Table#_set( Value#_Int[player_value], 'type', Jass#_Player )
    call Table#_set( Value#_Int[return_table], 1, player_value )
endfunction

