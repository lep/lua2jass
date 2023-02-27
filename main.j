function InitCustomPlayerSlots takes nothing returns nothing
    call SetPlayerStartLocation(Player(0),0)
    call SetPlayerColor(Player(0),ConvertPlayerColor(0))
    call SetPlayerRacePreference(Player(0),RACE_PREF_HUMAN)
    call SetPlayerRaceSelectable(Player(0),true)
    call SetPlayerController(Player(0),MAP_CONTROL_USER)
endfunction


function InitCustomTeams takes nothing returns nothing
    call SetPlayerTeam(Player(0),0)
endfunction

function InitAllyPriorities takes nothing returns nothing
    call SetStartLocPrioCount(0,1)
    call SetStartLocPrio(0,0,1,MAP_LOC_PRIO_HIGH)
endfunction

function main takes nothing returns nothing
    call SetCameraBounds(-5376.0+GetCameraMargin(CAMERA_MARGIN_LEFT),-5632.0+GetCameraMargin(CAMERA_MARGIN_BOTTOM),5376.0-GetCameraMargin(CAMERA_MARGIN_RIGHT),5120.0-GetCameraMargin(CAMERA_MARGIN_TOP),-5376.0+GetCameraMargin(CAMERA_MARGIN_LEFT),5120.0-GetCameraMargin(CAMERA_MARGIN_TOP),5376.0-GetCameraMargin(CAMERA_MARGIN_RIGHT),-5632.0+GetCameraMargin(CAMERA_MARGIN_BOTTOM))
    call InitBlizzard()
    call InitCustomTriggers()
endfunction

function config takes nothing returns nothing
    call SetMapName("TRIGSTR_001")
    call SetMapDescription("TRIGSTR_003")
    call SetPlayers(1)
    call SetTeams(1)
    call SetGamePlacement(MAP_PLACEMENT_TEAMS_TOGETHER)
    call DefineStartLocation(0, 0, 0)
    call InitCustomPlayerSlots()
    call SetPlayerSlotAvailable(Player(0),MAP_CONTROL_USER)
    call InitGenericPlayerSlots()
    call InitAllyPriorities()
endfunction

