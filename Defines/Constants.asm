;controller input constants
Input_A = $80
Input_B = $40
Input_Select = $20
Input_Start = $10
Input_Up = $08
Input_Down = $04
Input_Left = $02
Input_Right = $01

;tile stuff
BGTile_Empty = $24
BGTile_Cursor = $26
BGTile_ClayPigeon = $D3                                     ;uses two consecutive tiles, first is filled (indicating that it's been shot), and empty
BGTile_Duck = $D5                                           ;same deal as above
BGTile_PassLineTile = $D7                                   ;this measures required hits

BGTile_Bullet = $D9
BGTile_SHOT = $DA                                           ;3 consecutive tiles to form a "SHOT" indicator

StripeImageCommand_Repeat = $40                             ;bit 6 will make repeat writes of one value
StripeImageWriteCommand_DrawVert = $80                      ;bit 7 - change drawing from horizontal line to vertical
StripeImageWriteCommand_Stop = $00                          ;command to stop VRAM write and return from routine.

;sound stuff. all sound effects are stored in bitwice format
SFX_Queue1_Silence = %00000001
SFX_Queue1_ZapperShot = %00000010
SFX_Queue1_ClayPigeonHit = %00000100
SFX_Queue1_WingFlap = %00001000
SFX_Queue1_DuckFall = %00010000
SFX_Queue1_ClayPigeonFall = %00100000
SFX_Queue1_DogLaugh = %01000000
;SFX_Queue1_SilenceSortOf = %10000000 ;silences square, something else...

SFX_Queue2_ClayShootingStartTheme = %00000001
SFX_Queue2_TitleTheme = %00000010
SFX_Queue2_GameOverTheme = %00000100
SFX_Queue2_GotDuckJingle = %00001000
SFX_Queue2_DuckGameStartTheme = %00010000
SFX_Queue2_HitTableShuffle = %00100000
SFX_Queue2_BeatRoundTheme = %01000000
SFX_Queue2_PauseJingle = %10000000

SFX_Queue3_Bark = %00000001
SFX_Queue3_Quack = %00000010
;bit 2 is unused
SFX_Queue3_GroundThud = %00001000
SFX_Queue3_PerfectJingle = %00010000
SFX_Queue3_ClayPigeonThrow = %00100000
;bit 6 is unused
SFX_Queue3_GameOverJingle = %10000000

;OAM stuff
Score_OAM_Slot = 60                                         ;each score pop-up takes 2 OAM slots. there can only be 2 at any time.

;easy OAM props, don't change these
OAMProp_YFlip = %10000000
OAMProp_XFlip = %01000000
OAMProp_BGPriority = %00100000
OAMProp_Palette0 = %00000000
OAMProp_Palette1 = %00000001
OAMProp_Palette2 = %00000010
OAMProp_Palette3 = %00000011