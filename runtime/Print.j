// scope Print
// REQUIRES

function _print takes string s returns integer
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, s)
    return 0
endfunction
