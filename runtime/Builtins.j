// scope Builtins
// REQUIRES Print Value Table Context Natives Builtin/Boolexpr GC Error


globals
    integer array _ascii_i
    integer array _ascii_h
    integer array _ascii_y
    string array _ascii_c
endglobals

// taken from Bribe (https://www.hiveworkshop.com/threads/lua-vjass-ascii.190746/)
function _Char2Ascii takes string p returns integer
    local integer z = _ascii_i[StringHash(p)/0x1F0748+0x40D]
    if (_ascii_c[z] != p) then
	if (_ascii_c[z - 32] != p) then
	    if (_ascii_c[_ascii_h[z]] != p) then
		if (_ascii_c[_ascii_y[z]] != p) then
		    if (_ascii_c[83] != p) then
			return 0
		    endif
		    return 83
		endif
		return _ascii_y[z]
	    endif
	    return _ascii_h[z]
	endif
	return z - 32
    endif
    return z
endfunction

function _S2A takes string s returns integer
    local integer a=0
    local integer l=StringLength(s)
    local integer j=0
    local string m
    local integer h
    loop
	exitwhen j==l
	set a = a*256 + _Char2Ascii(SubString(s,j,j+1))
	set j=j+1
    endloop
    return a
endfunction

function _FourCC takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local integer str = Table#_get( tbl, 1 )
    call Table#_set( Value#_Int[return_table], 1, Value#_litint(_S2A(Value#_2string(str, interpreter))) )
endfunction

function _collectgarbage takes integer tbl, integer ctx, integer interpreter returns nothing
    call GC#_full_mark_and_sweep()
endfunction

function _rawset takes integer ptbl, integer ctx, integer interpreter returns nothing
    local integer tbl = Table#_get( ptbl, 1 )
    local integer key = Table#_get( ptbl, 2 )
    local integer val = Table#_get( ptbl, 3 )

    call Value#_settable( tbl, key, val )
endfunction

function _rawget takes integer ptbl, integer ctx, integer interpreter returns nothing
    local integer ret = Table#_get( ptbl, 0 )
    local integer tbl = Table#_get( ptbl, 1 )
    local integer key = Table#_get( ptbl, 2 )

    call Table#_set( Value#_Int[ret], 1, Value#_gettable(tbl, key) )
endfunction

function _print takes integer tbl, integer ctx, integer interpreter returns nothing
    local string r = ""
    local integer k = 1
    local integer v
    //call Print#_print("_print("+I2S(tbl)+")")


    //if ctx == 0 then
    //    set k = 0
    //endif

    //call Print#_print("  - starting at k = "+I2S(k))

    loop
        if Table#_has( tbl, k ) then
            set v = Table#_get(tbl, k)

	    //if interpreter == 0 then
	    //    if Value#_Type[v] == Types#_Int then
	    //        set r = r + I2S(Value#_Int[v])+".   "
	    //    elseif Value#_Type[v] == Types#_Real then
	    //        set r = r + R2S(Value#_Real[v])+".   "
	    //    elseif Value#_Type[v] == Types#_String then
	    //        set r = r + (Value#_String[v])+".   "
	    //    else
	    //        set r = r + "(type "+I2S(Value#_Type[v])+") .  "
	    //    endif
	    //else
		set r = r + Value#_tostring(v, interpreter) + "   "
	    //endif
            set k = k +1
        else
            exitwhen true
        endif
    endloop
    call Print#_print("|c00aaaaff"+r+"|r")
endfunction

// https://www.lua.org/pil/13.3.html
// function setmetatable(table, metatable)
function _setmetatable takes integer params_tbl, integer ctx, integer interpreter returns nothing
    local integer table = Table#_get( params_tbl, 1 )
    local integer metatable = Table#_get( params_tbl, 2 )
    local integer return_table = Table#_get( params_tbl, 0 )
    local integer current_metatable = Value#_Int3[table]

    if Value#_Type[table] != Types#_Table then
	call Error#_error_str("Bad argument #1 to 'setmetatable' (table expected)")
	return
    endif

    if Value#_Type[metatable] != Types#_Table and Value#_Type[metatable] != Types#_Nil  then
	call Error#_error_str("Bad argument #2 to 'setmetatable' (table or nil expected)")
	return
    endif

    if Value#_gettable(metatable, Value#_litstring("__metatable")) != Value#_Nil then
	call Error#_error_str("Cannot change protected metatable")
	return
    endif

    if metatable == Value#_Nil then
	set Value#_Int3[table] = 0
    else
	set Value#_Int3[table] = metatable
    endif

    call Table#_set( Value#_Int[return_table], 1, table )
endfunction

function _getmetatable takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer return_table = Table#_get( tbl, 0 )
    local integer table = Table#_get( tbl, 1 )
    local integer metatable
    if Value#_Type[table] != Types#_Table then
	call Table#_set( Value#_Int[return_table], 1, Value#_Nil )
    elseif Value#_Int3[table] == 0 then
	call Table#_set( Value#_Int[return_table], 1, Value#_Nil )
    else
	set metatable = Value#_gettable( Value#_Int3[table], Value#_litstring("__metatable"))
	if metatable != Value#_Nil then
	    call Table#_set( Value#_Int[return_table], 1, metatable )
	else
	    call Table#_set( Value#_Int[return_table], 1, Value#_Int3[table] )
	endif
    endif
endfunction


function _ForForce takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)

    set Builtin/Boolexpr#_code_r = r
    set Builtin/Boolexpr#_code_i = interpreter
    set Builtin/Boolexpr#_code_v = arg2

    call ForForce( Natives#_convert2force(arg1, interpreter), function Builtin/Boolexpr#_cb)
endfunction

function _ForGroup takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r = Table#_get(tbl, 0)
    local integer arg1 = Table#_get(tbl, 1)
    local integer arg2 = Table#_get(tbl, 2)

    set Builtin/Boolexpr#_code_r = r
    set Builtin/Boolexpr#_code_i = interpreter
    set Builtin/Boolexpr#_code_v = arg2

    call ForGroup( Natives#_convert2group(arg1, interpreter), function Builtin/Boolexpr#_cb)
endfunction


function _pcall takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r0 = Table#_get(tbl, 0)
    local integer f = Table#_get(tbl, 1)
    local integer args = Table#_alloc()
    local integer r1 = Value#_table()
    local integer frame = Interpreter#_stack_top[interpreter]
    local boolean result

    call Table#_getlist( args, tbl, 2 )
    call Table#_set( args, 0, r1 )

    set Error#_inProtectedCall = Error#_inProtectedCall + 1
    set result = Wrap#_call_function( f, args, interpreter )
    set Error#_inProtectedCall = Error#_inProtectedCall - 1

    call Table#_set( Value#_Int[r0], 1, Value#_litbool(result) )

    if not result then
	call Table#_set( Value#_Int[r0], 2, Error#_getLastErrorValue() )
	set Interpreter#_stack_top[interpreter] = frame
    else
	call Table#_append( Value#_Int[r0], Value#_Int[r1], 1 )
    endif
endfunction

function _xpcall takes integer tbl, integer ctx, integer interpreter returns nothing
    local integer r0 = Table#_get(tbl, 0)
    local integer f = Table#_get(tbl, 1)
    local integer msgh = Table#_get(tbl, 2)
    local integer args = Table#_alloc()
    local integer r1 = Value#_table()
    local integer r2 = Value#_table()
    local integer frame = Interpreter#_stack_top[interpreter]
    local boolean result

    call Table#_getlist( args, tbl, 3 )
    call Table#_set( args, 0, r1 )

    set Error#_inProtectedCall = Error#_inProtectedCall + 1
    set result = Wrap#_call_function( f, args, interpreter )
    set Error#_inProtectedCall = Error#_inProtectedCall - 1

    call Table#_set( Value#_Int[r0], 1, Value#_litbool(result) )

    call Call#_call1( msgh, Error#_getLastErrorValue(), r2, interpreter )


    if not result then
	call Table#_set( Value#_Int[r0], 2, Table#_get( Value#_Int[r2], 1 ) )
	set Interpreter#_stack_top[interpreter] = frame
    else
	call Table#_append( Value#_Int[r0], Value#_Int[r1], 1 )
    endif
endfunction

function _error takes integer tbl, integer ctx, integer interpreter returns nothing
    call Error#_error( Table#_get(tbl, 1) )
endfunction

function _init takes nothing returns nothing
    set _ascii_i[966] = 8
    set _ascii_i[1110] = 9
    set _ascii_i[1621] = 10
    set _ascii_i[1375] = 12
    set _ascii_i[447] = 13
    set _ascii_i[233] = 32
    set _ascii_i[2014] = 33
    set _ascii_i[1348] = 34
    set _ascii_i[1038] = 35
    set _ascii_i[1299] = 36
    set _ascii_i[1018] = 37
    set _ascii_i[1312] = 38
    set _ascii_i[341] = 39
    set _ascii_i[939] = 40
    set _ascii_i[969] = 41
    set _ascii_i[952] = 42
    set _ascii_i[2007] = 43
    set _ascii_i[1415] = 44
    set _ascii_i[2020] = 45
    set _ascii_i[904] = 46
    set _ascii_i[1941] = 47
    set _ascii_i[918] = 48
    set _ascii_i[1593] = 49
    set _ascii_i[719] = 50
    set _ascii_i[617] = 51
    set _ascii_i[703] = 52
    set _ascii_i[573] = 53
    set _ascii_i[707] = 54
    set _ascii_i[1208] = 55
    set _ascii_i[106] = 56
    set _ascii_i[312] = 57
    set _ascii_i[124] = 58
    set _ascii_i[1176] = 59
    set _ascii_i[74] = 60
    set _ascii_i[1206] = 61
    set _ascii_i[86] = 62
    set _ascii_i[340] = 63
    set _ascii_i[35] = 64
    set _ascii_i[257] = 65
    set _ascii_i[213] = 66
    set _ascii_i[271] = 67
    set _ascii_i[219] = 68
    set _ascii_i[1330] = 69
    set _ascii_i[1425] = 70
    set _ascii_i[1311] = 71
    set _ascii_i[238] = 72
    set _ascii_i[1349] = 73
    set _ascii_i[244] = 74
    set _ascii_i[1350] = 75
    set _ascii_i[205] = 76
    set _ascii_i[1392] = 77
    set _ascii_i[1378] = 78
    set _ascii_i[1432] = 79
    set _ascii_i[1455] = 80
    set _ascii_i[1454] = 81
    set _ascii_i[1431] = 82
    set _ascii_i[1409] = 83
    set _ascii_i[1442] = 84
    set _ascii_i[534] = 85
    set _ascii_i[1500] = 86
    set _ascii_i[771] = 87
    set _ascii_i[324] = 88
    set _ascii_i[1021] = 89
    set _ascii_i[73] = 90
    set _ascii_i[1265] = 91
    set _ascii_i[1941] = 92
    set _ascii_i[1671] = 93
    set _ascii_i[1451] = 94
    set _ascii_i[1952] = 95
    set _ascii_i[252] = 96
    set _ascii_i[257] = 97
    set _ascii_i[213] = 98
    set _ascii_i[271] = 99
    set _ascii_i[219] = 100
    set _ascii_i[1330] = 101
    set _ascii_i[1425] = 102
    set _ascii_i[1311] = 103
    set _ascii_i[238] = 104
    set _ascii_i[1349] = 105
    set _ascii_i[244] = 106
    set _ascii_i[1350] = 107
    set _ascii_i[205] = 108
    set _ascii_i[1392] = 109
    set _ascii_i[1378] = 110
    set _ascii_i[1432] = 111
    set _ascii_i[1455] = 112
    set _ascii_i[1454] = 113
    set _ascii_i[1431] = 114
    set _ascii_i[1409] = 115
    set _ascii_i[1442] = 116
    set _ascii_i[534] = 117
    set _ascii_i[1500] = 118
    set _ascii_i[771] = 119
    set _ascii_i[324] = 120
    set _ascii_i[1021] = 121
    set _ascii_i[73] = 122
    set _ascii_i[868] = 123
    set _ascii_i[1254] = 124
    set _ascii_i[588] = 125
    set _ascii_i[93] = 126
    set _ascii_i[316] = 161
    set _ascii_i[779] = 162
    set _ascii_i[725] = 163
    set _ascii_i[287] = 164
    set _ascii_i[212] = 165
    set _ascii_i[7] = 166
    set _ascii_i[29] = 167
    set _ascii_i[1958] = 168
    set _ascii_i[1009] = 169
    set _ascii_i[1580] = 170
    set _ascii_i[1778] = 171
    set _ascii_i[103] = 172
    set _ascii_i[400] = 174
    set _ascii_i[1904] = 175
    set _ascii_i[135] = 176
    set _ascii_i[1283] = 177
    set _ascii_i[469] = 178
    set _ascii_i[363] = 179
    set _ascii_i[550] = 180
    set _ascii_i[1831] = 181
    set _ascii_i[1308] = 182
    set _ascii_i[1234] = 183
    set _ascii_i[1017] = 184
    set _ascii_i[1093] = 185
    set _ascii_i[1577] = 186
    set _ascii_i[606] = 187
    set _ascii_i[1585] = 188
    set _ascii_i[1318] = 189
    set _ascii_i[980] = 190
    set _ascii_i[1699] = 191
    set _ascii_i[1292] = 192
    set _ascii_i[477] = 193
    set _ascii_i[709] = 194
    set _ascii_i[1600] = 195
    set _ascii_i[2092] = 196
    set _ascii_i[50] = 197
    set _ascii_i[546] = 198
    set _ascii_i[408] = 199
    set _ascii_i[853] = 200
    set _ascii_i[205] = 201
    set _ascii_i[411] = 202
    set _ascii_i[1311] = 203
    set _ascii_i[1422] = 204
    set _ascii_i[1808] = 205
    set _ascii_i[457] = 206
    set _ascii_i[1280] = 207
    set _ascii_i[614] = 208
    set _ascii_i[1037] = 209
    set _ascii_i[237] = 210
    set _ascii_i[1409] = 211
    set _ascii_i[1023] = 212
    set _ascii_i[1361] = 213
    set _ascii_i[695] = 214
    set _ascii_i[161] = 215
    set _ascii_i[1645] = 216
    set _ascii_i[1822] = 217
    set _ascii_i[644] = 218
    set _ascii_i[1395] = 219
    set _ascii_i[677] = 220
    set _ascii_i[1677] = 221
    set _ascii_i[881] = 222
    set _ascii_i[861] = 223
    set _ascii_i[1408] = 224
    set _ascii_i[1864] = 225
    set _ascii_i[1467] = 226
    set _ascii_i[1819] = 227
    set _ascii_i[1971] = 228
    set _ascii_i[949] = 229
    set _ascii_i[774] = 230
    set _ascii_i[1828] = 231
    set _ascii_i[865] = 232
    set _ascii_i[699] = 233
    set _ascii_i[786] = 234
    set _ascii_i[1806] = 235
    set _ascii_i[1286] = 236
    set _ascii_i[1128] = 237
    set _ascii_i[1490] = 238
    set _ascii_i[1720] = 239
    set _ascii_i[1817] = 240
    set _ascii_i[729] = 241
    set _ascii_i[1191] = 242
    set _ascii_i[1164] = 243
    set _ascii_i[413] = 244
    set _ascii_i[349] = 245
    set _ascii_i[1409] = 246
    set _ascii_i[660] = 247
    set _ascii_i[2016] = 248
    set _ascii_i[1087] = 249
    set _ascii_i[1497] = 250
    set _ascii_i[753] = 251
    set _ascii_i[1579] = 252
    set _ascii_i[1456] = 253
    set _ascii_i[606] = 254
    set _ascii_i[1625] = 255
    set _ascii_h[92] = 47
    set _ascii_h[201] = 108
    set _ascii_h[201] = 76
    set _ascii_h[203] = 103
    set _ascii_h[203] = 71
    set _ascii_h[246] = 115
    set _ascii_h[246] = 83
    set _ascii_h[246] = 211
    set _ascii_h[254] = 187
    set _ascii_y[201] = 108
    set _ascii_y[203] = 103
    set _ascii_y[246] = 115

    set _ascii_c[8]="\b"
    set _ascii_c[9]="\t"
    set _ascii_c[10]="\n"
    set _ascii_c[12]="\f"
    set _ascii_c[13]="\r"
    set _ascii_c[32]=" "
    set _ascii_c[33]="!"
    set _ascii_c[34]="\""
    set _ascii_c[35]="#"
    set _ascii_c[36]="$"
    set _ascii_c[37]="%"
    set _ascii_c[38]="&"
    set _ascii_c[39]="'"
    set _ascii_c[40]="("
    set _ascii_c[41]=")"
    set _ascii_c[42]="*"
    set _ascii_c[43]="+"
    set _ascii_c[44]=","
    set _ascii_c[45]="-"
    set _ascii_c[46]="."
    set _ascii_c[47]="/"
    set _ascii_c[48]="0"
    set _ascii_c[49]="1"
    set _ascii_c[50]="2"
    set _ascii_c[51]="3"
    set _ascii_c[52]="4"
    set _ascii_c[53]="5"
    set _ascii_c[54]="6"
    set _ascii_c[55]="7"
    set _ascii_c[56]="8"
    set _ascii_c[57]="9"
    set _ascii_c[58]=":"
    set _ascii_c[59]=";"
    set _ascii_c[60]="<"
    set _ascii_c[61]="="
    set _ascii_c[62]=">"
    set _ascii_c[63]="?"
    set _ascii_c[64]="@"
    set _ascii_c[65]="A"
    set _ascii_c[66]="B"
    set _ascii_c[67]="C"
    set _ascii_c[68]="D"
    set _ascii_c[69]="E"
    set _ascii_c[70]="F"
    set _ascii_c[71]="G"
    set _ascii_c[72]="H"
    set _ascii_c[73]="I"
    set _ascii_c[74]="J"
    set _ascii_c[75]="K"
    set _ascii_c[76]="L"
    set _ascii_c[77]="M"
    set _ascii_c[78]="N"
    set _ascii_c[79]="O"
    set _ascii_c[80]="P"
    set _ascii_c[81]="Q"
    set _ascii_c[82]="R"
    set _ascii_c[83]="S"
    set _ascii_c[84]="T"
    set _ascii_c[85]="U"
    set _ascii_c[86]="V"
    set _ascii_c[87]="W"
    set _ascii_c[88]="X"
    set _ascii_c[89]="Y"
    set _ascii_c[90]="Z"
    set _ascii_c[91]="["
    set _ascii_c[92]="\\"
    set _ascii_c[93]="]"
    set _ascii_c[94]="^"
    set _ascii_c[95]="_"
    set _ascii_c[96]="`"
    set _ascii_c[97]="a"
    set _ascii_c[98]="b"
    set _ascii_c[99]="c"
    set _ascii_c[100]="d"
    set _ascii_c[101]="e"
    set _ascii_c[102]="f"
    set _ascii_c[103]="g"
    set _ascii_c[104]="h"
    set _ascii_c[105]="i"
    set _ascii_c[106]="j"
    set _ascii_c[107]="k"
    set _ascii_c[108]="l"
    set _ascii_c[109]="m"
    set _ascii_c[110]="n"
    set _ascii_c[111]="o"
    set _ascii_c[112]="p"
    set _ascii_c[113]="q"
    set _ascii_c[114]="r"
    set _ascii_c[115]="s"
    set _ascii_c[116]="t"
    set _ascii_c[117]="u"
    set _ascii_c[118]="v"
    set _ascii_c[119]="w"
    set _ascii_c[120]="x"
    set _ascii_c[121]="y"
    set _ascii_c[122]="z"
    set _ascii_c[123]="{"
    set _ascii_c[124]="|"
    set _ascii_c[125]="}"
    set _ascii_c[126]="~"
    set _ascii_c[128] = "€"
    set _ascii_c[130] = "‚"
    set _ascii_c[131] = "ƒ"
    set _ascii_c[132] = "„"
    set _ascii_c[133] = "…"
    set _ascii_c[134] = "†"
    set _ascii_c[135] = "‡"
    set _ascii_c[136] = "ˆ"
    set _ascii_c[137] = "‰"
    set _ascii_c[138] = "Š"
    set _ascii_c[139] = "‹"
    set _ascii_c[140] = "Œ"
    set _ascii_c[142] = "Ž"
    set _ascii_c[145] = "‘"
    set _ascii_c[146] = "’"
    set _ascii_c[147] = "“"
    set _ascii_c[148] = "”"
    set _ascii_c[149] = "•"
    set _ascii_c[150] = "–"
    set _ascii_c[151] = "—"
    set _ascii_c[152] = "˜"
    set _ascii_c[153] = "™"
    set _ascii_c[154] = "š"
    set _ascii_c[155] = "›"
    set _ascii_c[156] = "œ"
    set _ascii_c[158] = "ž"
    set _ascii_c[159] = "Ÿ"
    set _ascii_c[160] = " "
    set _ascii_c[161] = "¡"
    set _ascii_c[162] = "¢"
    set _ascii_c[163] = "£"
    set _ascii_c[164] = "¤"
    set _ascii_c[165] = "¥"
    set _ascii_c[166] = "¦"
    set _ascii_c[167] = "§"
    set _ascii_c[168] = "¨"
    set _ascii_c[169] = "©"
    set _ascii_c[170] = "ª"
    set _ascii_c[171] = "«"
    set _ascii_c[172] = "¬"
    set _ascii_c[174] = "®"
    set _ascii_c[175] = "¯"
    set _ascii_c[176] = "°"
    set _ascii_c[177] = "±"
    set _ascii_c[178] = "²"
    set _ascii_c[179] = "³"
    set _ascii_c[180] = "´"
    set _ascii_c[181] = "µ"
    set _ascii_c[182] = "¶"
    set _ascii_c[183] = "·"
    set _ascii_c[184] = "¸"
    set _ascii_c[185] = "¹"
    set _ascii_c[186] = "º"
    set _ascii_c[187] = "»"
    set _ascii_c[188] = "¼"
    set _ascii_c[189] = "½"
    set _ascii_c[190] = "¾"
    set _ascii_c[191] = "¿"
    set _ascii_c[192] = "À"
    set _ascii_c[193] = "Á"
    set _ascii_c[194] = "Â"
    set _ascii_c[195] = "Ã"
    set _ascii_c[196] = "Ä"
    set _ascii_c[197] = "Å"
    set _ascii_c[198] = "Æ"
    set _ascii_c[199] = "Ç"
    set _ascii_c[200] = "È"
    set _ascii_c[201] = "É"
    set _ascii_c[202] = "Ê"
    set _ascii_c[203] = "Ë"
    set _ascii_c[204] = "Ì"
    set _ascii_c[205] = "Í"
    set _ascii_c[206] = "Î"
    set _ascii_c[207] = "Ï"
    set _ascii_c[208] = "Ð"
    set _ascii_c[209] = "Ñ"
    set _ascii_c[210] = "Ò"
    set _ascii_c[211] = "Ó"
    set _ascii_c[212] = "Ô"
    set _ascii_c[213] = "Õ"
    set _ascii_c[214] = "Ö"
    set _ascii_c[215] = "×"
    set _ascii_c[216] = "Ø"
    set _ascii_c[217] = "Ù"
    set _ascii_c[218] = "Ú"
    set _ascii_c[219] = "Û"
    set _ascii_c[220] = "Ü"
    set _ascii_c[221] = "Ý"
    set _ascii_c[222] = "Þ"
    set _ascii_c[223] = "ß"
    set _ascii_c[224] = "à"
    set _ascii_c[225] = "á"
    set _ascii_c[226] = "â"
    set _ascii_c[227] = "ã"
    set _ascii_c[228] = "ä"
    set _ascii_c[229] = "å"
    set _ascii_c[230] = "æ"
    set _ascii_c[231] = "ç"
    set _ascii_c[232] = "è"
    set _ascii_c[233] = "é"
    set _ascii_c[234] = "ê"
    set _ascii_c[235] = "ë"
    set _ascii_c[236] = "ì"
    set _ascii_c[237] = "í"
    set _ascii_c[238] = "î"
    set _ascii_c[239] = "ï"
    set _ascii_c[240] = "ð"
    set _ascii_c[241] = "ñ"
    set _ascii_c[242] = "ò"
    set _ascii_c[243] = "ó"
    set _ascii_c[244] = "ô"
    set _ascii_c[245] = "õ"
    set _ascii_c[246] = "ö"
    set _ascii_c[247] = "÷"
    set _ascii_c[248] = "ø"
    set _ascii_c[249] = "ù"
    set _ascii_c[250] = "ú"
    set _ascii_c[251] = "û"
    set _ascii_c[252] = "ü"
    set _ascii_c[253] = "ý"
    set _ascii_c[254] = "þ"
    set _ascii_c[255] = "ÿ"
endfunction
