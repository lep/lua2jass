// scope Print
// REQUIRES

function _print takes string s returns integer
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, s)
    return 0
endfunction

function _error takes string s returns integer
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, "|c00ff0000"+s+"|r")
    call I2S(1 / 0)
    return 0
endfunction

function _warn takes string s returns integer
    call DisplayTimedTextToPlayer(Player(0), 0, 0, 60, "|c00ffff00"+s+"|r")
    return 0
endfunction

