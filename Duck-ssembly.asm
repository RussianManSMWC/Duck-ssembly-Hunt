;Duck Hunt disassembly
;To be compiled with ASM6N - a fork of ASM6 (https://forums.nesdev.org/viewtopic.php?t=18324)

incsrc Defines/Registers.asm
incsrc Defines/Constants.asm
incsrc Defines/Enums.asm
incsrc Defines/RAM.asm
incsrc iNES_Header.asm
incsrc Macros.asm

org $C000
FILLVALUE $FF                                               ;freespace is marked with a bunch of $FFs.
RESET_C000:
CLD                                                         ;standard initialization. decimal mode
SEI                                                         ;interrupt

LOOP_C002:
LDA HardwareStatus                                          ;
BPL LOOP_C002                                               ;wait for NES power on...

LDX #$00                                                    ;
STX ControlBits                                             ;init NES registers
STX RenderBits                                              ;

DEX                                                         ;
TXS                                                         ;init stack

LDX RNG_Value                                               ;RNG seed

LDY #$06                                                    ;only clear $0000-$06FF
STY $01                                                     ;

LDY #$00                                                    ;
STY $00                                                     ;

LDA #$00                                                    ;

LOOP_C01E:
STA ($00),Y                                                 ;standard RAM clearing loop
DEY                                                         ;
BNE LOOP_C01E                                               ;

DEC $01                                                     ;
BPL LOOP_C01E                                               ;

TXA                                                         ;
BNE CODE_C02C                                               ;if random number is not 0, keep it

LDX #$5F                                                    ;initialize it to this value

CODE_C02C:
STX RNG_Value                                               ;

JSR ClearScreenAndAttributesInit_C22B                       ;
JSR ClearOAM_C23B                                           ;

LDY #$00                                                    ;
STA VRAMRenderAreaReg                                       ;
STA VRAMRenderAreaReg                                       ;

LDY #$01
STY TitleScreenFlag                                         ;we're at the title screen

LDA #$0F                                                    ;
STA APU_SoundChannels                                       ;enable all channels, excluding DMC

JSR CODE_F581                                               ;sound related

LDA #$90                                                    ;
STA ControlBits                                             ;
STA ControlMirror                                           ;

LDA #$02                                                    ;
STA RenderMirror                                            ;

LDX #$0B                                                    ;load 12 bytes of data

LOOP_C056:
LDA DefaultTopScores_E5DF,X                                 ;
STA TopScoreTable,X                                         ;initialize top scores
DEX                                                         ;
BPL LOOP_C056                                               ;
BMI LOOP_C07C                                               ;

CODE_C061:
LDA TitleScreenFlag                                         ;check if we're at the title screen
BEQ CODE_C06B                                               ;

LDA TitleScreenMode                                         ;dont play sound if currently loading title screen
CMP #$01
BNE CODE_C06E

CODE_C06B:
JSR SoundEngine_F56C

CODE_C06E:
JSR HandleGlobalTimers_C348                                 ;timers
JSR ReadControllers_C284                                    ;get controller inputs
JSR ExecuteGameCode_C0B3                                    ;make the game act as a game

INC FrameCounter                                            ;frame counter +=1
JSR PrepareForNMIWait_C61E                                  ;

;game loop
LOOP_C07C:
LDA FrameFlag                                               ;wait for the whole frame to pass
BNE CODE_C061                                               ;

JSR RNG_C588                                                ;update "random" numbers in the meantime
JMP LOOP_C07C                                               ;wee!

NMI_C086:
PHP                                                         ;doesn't NMI hit already preserve processor bits on its own? regardless, this is a new addition since Mario Bros. (NES)
PHA                                                         ;pretty standard NMI entry...
TXA                                                         ;
PHA                                                         ;
TYA                                                         ;
PHA                                                         ;

LDA #$00                                                    ;
STA OAMAddress                                              ;pretty standard OAM update (still don't really know what this register is for in particular)

LDA #>OAM_Y                                                 ;update sprite tiles
STA OAMDMAReg                                               ;

LDA NMIFunctionsDisableFlag                                 ;don't do anything in case 
BNE CODE_C0A6                                               ;

JSR BufferedDraw_C2BF                                       ;draw from the buffer
JSR UpdatePalette_C249                                      ;change palette if needed
JSR UpdateDuckPalette_C263                                  ;change palette once more when it comes to feathery fiends
JSR UpdateCameraPosition_C3C1                               ;keep camera where it belongs after messing with VRAM

CODE_C0A6:
LDY #$01
STY NMIFunctionsDisableFlag                                 ;done with shenanigans
STY FrameFlag                                               ;hit the frame

PLA                                                         ;pretty standard NMI bowing and then leaving respectfully
TAY                                                         ;
PLA                                                         ;
TAX                                                         ;
PLA                                                         ;
PLP                                                         ;
RTI                                                         ;

ExecuteGameCode_C0B3:
LDA TitleScreenFlag                                         ;check if at title screen
BEQ CODE_C0BA                                               ;not at title screen
JMP TitleScreenCode_C14E                                    ;yes at title screen

CODE_C0BA:
LDA Controller1Input_Hold                                   ;check if holding...
AND #Input_Start                                            ;start (player 1 only)
BEQ CODE_C11E                                               ;do not begin video game just yet

LDY StartHeldFlag                                           ;check if we just pressed the start button or held it
BNE CODE_C123                                               ;nothing special happens if we already activated start things
INY                                                         ;
STY StartHeldFlag                                           ;we definitely pressed start this frame, if we check start on the next frame and it's pressed again, assume it's held.

LDY GameplayMode                                            ;check which gameplay mode we're at
CPY #GameplayMode_Pause                                     ;check if we were already in paused state
BEQ CODE_C0FF                                               ;unpause
CPY #GameplayMode_DuckGameMain                              ;shooting ducks
BEQ CODE_C0DF                                               ;can pause
CPY #GameplayMode_RoundEnd_DuckGame                         ;and at the end of a round
BEQ CODE_C0DF                                               ;can pause
CPY #GameplayMode_ClayShootingMain                          ;shooting clay pigeons
BEQ CODE_C0DF                                               ;can pause
CPY #GameplayMode_RoundEnd_ClayShooting                     ;and at the end of a round
BNE CODE_C123                                               ;can pause

CODE_C0DF:
LDX #$05                                                    ;

CODE_C0E1:
LDA Timer_Base,X                                            ;back up timers
STA Timer_Backup,X                                          ;
DEX                                                         ;
BPL CODE_C0E1                                               ;

LDA RenderMirror                                            ;disable sprite display
AND #$0E                                                    ;
STA RenderBits                                              ;
STA RenderMirror                                            ;

LDA GameplayMode                                            ;
STA GameplayMode_Backup                                     ;

LDA #GameplayMode_Pause                                     ;
STA GameplayMode                                            ;pause game mode

LDA #Message_PAUSE                                          ;
JMP CODE_C112                                               ;will display pause message

CODE_C0FF:
LDA RenderMirror                                            ;enable sprites
ORA #$10                                                    ;
STA RenderBits                                              ;
STA RenderMirror                                            ;

LDA #GameplayMode_Unpause                                   ;
STA GameplayMode                                            ;

LDA #48                                                     ;wait 
STA UnpauseTimer                                            ;

LDA #Message_None                                           ;remove pause message

CODE_C112:
JSR DrawMessage_D464                                        ;

JSR QueueSFX_Silence_D4E2                                   ;
JSR WaitForNMI_C5D9                                         ;
JMP QueueSFX_PauseJingle_D521                               ;

CODE_C11E:
LDA #$00                                                    ;
STA StartHeldFlag                                           ;

CODE_C123:
LDA GameplayMode                                            ;
JSR ExecutePointers_C35E                                    ;

dw InitGameplay_C81D
dw LoadDuckGame_C839
dw InitDuckGame_C854
dw ExecuteDuckGame_C86E
dw RoundEnd_D2C9                                            ;execute round end related shenanigans
dw DoNothing_C5E0                                           ;unused
dw InitGameOver_C779
dw GameOver_C72C
dw LoadClayShooting_D54B                                    ;
dw InitClayShooting_D56A
dw InitClayShootingPart2_D593
dw CODE_D5D2
dw RoundEnd_D2C9
dw ClayPigeonIntro_C7EF                                     ;
dw DoNothing_C5E0                                           ;unused...
dw DoNothing_C5E0                                           ;unused...
dw DoNothing_C5E0                                           ;pause state (nothing special happens)
dw UnpauseGame_C271                                         ;wait a little bit then unpause for real
dw WaitForGameplayModeChange_C625                           ;generic wait then change state... state

;title screen code
TitleScreenCode_C14E:
LDA #$00
STA TitleScreenZapperShotState

LDA #$01                                                    ;
STA ZapperFunctionsEnabledFlag                              ;
STA ShotCount                                               ;we have 1 shot at the title screen. this is to be able to activate a selected option

LDA TitleScreenMode                                         ;let the game initialize title screen through a pointer
BEQ CODE_C18D                                               ;

JSR HandleZapperInput_D131
JSR HandleZapperFunctions_D160

LDA TitleScreenZapperShotState                              ;
CMP #$01                                                    ;
BEQ CODE_C176                                               ;check if shot directly at the screen. acts as start button.
CMP #$02                                                    ;
BEQ CODE_C199                                               ;check if shot at anything but the screen. acts as select button.

LDA Controller1Input_Hold                                   ;
AND #Input_Start|Input_Select                               ;check if held...
CMP #Input_Start                                            ;start
BNE CODE_C189                                               ;

CODE_C176:
LDA #GameplayMode_InitGameplay                              ;not on title screen
STA TitleScreenFlag                                         ;
STA GameplayMode                                            ;

JSR QueueSFX_Silence_D4E2
JSR DisableRender_C5E1

LDA #$02
STA Timer_Timing                                            ;wait 2+(2*10) frames
STA Timer_2D
RTS                                                         ;

CODE_C189:
LDX TitleScreenMode                                         ;check if title screen is being loaded in, or we're already in
BNE CODE_C190                                               ;

CODE_C18D:
JMP CODE_C217

CODE_C190:
CMP #Input_Select                                           ;check if held start
BNE CODE_C1A9

LDA SelectHeldFlag                                          ;
BNE CODE_C1B7                                               ;

CODE_C199:
LDY TitleScreenChoice                                       ;
INY                                                         ;go to next choice
CPY #$03                                                    ;handle wrap around
BNE CODE_C1A3                                               ;

LDY #$00                                                    ;

CODE_C1A3:
STY TitleScreenChoice                                       ;update title screen choice
JMP CODE_C1B2

CODE_C1A9:
CMP #$00                                                    ;holding anything else?
BNE CODE_C1B2                                               ;
STA SelectHeldFlag                                          ;clear this flag.
BEQ CODE_C1BB

CODE_C1B2:
LDA #$01                                                    ;
STA SelectHeldFlag                                          ;pressed select don't change option again until repress.

CODE_C1B7:
LDA #$FF                                                    ;255*10 frames
STA Timer_2D

CODE_C1BB:
CPX #$01
BNE CODE_C217

LDA #$51                                                    ;5 rows with 1 tile each
STA BGTileBuffer_Transfer

LDA #BGTile_Empty                                           ;empty tile
LDX #$04                                                    ;

CODE_C1C8:
STA BGTileBuffer_Transfer+1,X
DEX
BPL CODE_C1C8

LDA TitleScreenChoice                                       ;draw cursor where it belongs
ASL A                                                       ;
TAX                                                         ;
LDA #BGTile_Cursor                                          ;
STA BGTileBuffer_Transfer+1,X                               ;

LDX #$26                                                    ;
LDY #$22                                                    ;
JSR TransferBGWriteToBuffer_C71D                            ;

LDA TitleScreenChoice                                       ;show the top score for the respective game choice
ASL A                                                       ;
ASL A                                                       ;
CLC                                                         ;
ADC #$03                                                    ;
TAX                                                         ;
LDY #$05                                                    ;

CODE_C1EA:
LDA TopScoreTable,X
AND #$0F
STA BGTileBuffer_Transfer+1,Y

DEY
LDA TopScoreTable,X
LSR A
LSR A
LSR A
LSR A
STA BGTileBuffer_Transfer+1,Y
DEX
DEY
BPL CODE_C1EA

LDA BGTileBuffer_Transfer+1                                 ;if hundred thousands is a 0, omit it
BNE CODE_C20B                                               ;

LDA #BGTile_Empty                                           ;
STA BGTileBuffer_Transfer+1                                 ;

CODE_C20B:
LDA #$16                                                    ;6 tiles on a single row
STA BGTileBuffer_Transfer                                   ;

LDX #$12                                                    ;
LDY #$23                                                    ;
JSR TransferBGWriteToBuffer_C71D                            ;

CODE_C217:
LDA Timer_2D
BNE CODE_C222                                               ;will play title theme again after a while

JSR QueueSFX_TitleTheme_D509

LDA #$FF
STA Timer_2D

CODE_C222:
LDA TitleScreenMode                                         ;
JSR ExecutePointers_C35E                                    ;

dw InitTitleScreen_C5AA                                     ;
dw DoNothing_C5E0                                           ;

;screen clearing stuff
ClearScreenAndAttributesInit_C22B:
LDA #$02                                                    ;
JSR CODE_C232                                               ;

ClearScreenInit_C230:
LDA #$01                                                    ;

CODE_C232:
STA $01                                                     ;

LDA #BGTile_Empty                                           ;
STA $00                                                     ;
JMP ClearScreen_C303                                        ;

ClearOAM_C23B:
LDY #$00                                                    ;
LDA #$F4                                                    ;

CODE_C23F:
STA OAM_Y,Y                                                 ;
INY                                                         ;
INY                                                         ;
INY                                                         ;
INY                                                         ;
BNE CODE_C23F                                               ;
RTS                                                         ;

UpdatePalette_C249:
LDY PaletteToLoad                                           ;check if we should alter the palette
BEQ RETURN_C262                                             ;if not updating palette... not updating palette. i think.
DEY                                                         ;
TYA                                                         ;
ASL A                                                       ;calculate pointer
TAY                                                         ;
LDA POINTERS_E532,Y                                         ;get a pointer for what palette we're going to apply
LDX POINTERS_E532+1,Y                                       ;

LDY #LoadPalette_None                                       ;
STY PaletteToLoad                                           ;reset the flag

CODE_C25B:
STA $00                                                     ;
STX $01                                                     ;
JSR DrawStripeImage_C3B8                                    ;update palette

RETURN_C262:
RTS

;using stripe format, uploads palette for ducks
UpdateDuckPalette_C263:
LDY DuckPaletteLoadFlag                                     ;should we update duck palettes?
BEQ RETURN_C262                                             ;return if not

LDY #$00                                                    ;
STY DuckPaletteLoadFlag                                     ;we're doing it          

LDA #<DuckPaletteStorage                                    ;
LDX #>DuckPaletteStorage                                    ;
BNE CODE_C25B                                               ;let's upload them

UnpauseGame_C271:
DEC UnpauseTimer                                            ;check if we should stop
BNE RETURN_C283                                             ;

LDX #$05                                                    ;

LOOP_C277:
LDA Timer_Backup,X                                          ;restore timers
STA Timer_Base,X                                            ;
DEX                                                         ;
BPL LOOP_C277                                               ;

LDA GameplayMode_Backup                                     ;
STA GameplayMode                                            ;

RETURN_C283:
RTS                                                         ;

ReadControllers_C284:
LDX #$01                                                    ;\prepare controller 2 for reading
STX ControllerReg                                           ;/
DEX                                                         ;\prepare controller 1 for reading
STX ControllerReg                                           ;/

JSR CODE_C291                                               ;get inputs from controller 1
INX                                                         ;then second one

CODE_C291:
LDY #$08                                                    ;8 bits, of course

LOOP_C293:
PHA                                                         ;
LDA ControllerReg,X                                         ;load whatever bit
STA $00                                                     ;store here
LSR A                                                       ;get rid of the first bit
ORA $00                                                     ;get rid of all bits but bit zero (that happens)
LSR A                                                       ;
PLA                                                         ;PLA doesn't make much sense in this context, because we are not loading A. Mario Bros. does DEX : TXA to get A
ROL A                                                       ;"sum" active bits... I think
DEY                                                         ;
BNE LOOP_C293

CMP ControllerInput,X                                       ;if not the same input...
BNE CODE_C2B6                                               ;store it

INC ControllerInput_Counter,X                               ;some kinda timer...

LDY ControllerInput_Counter,X                               ;
CPY #$05                                                    ;check if this counter had reached this value...
BCC RETURN_C2BE                                             ;
STA ControllerInput_Hold,X                                  ;updates every 6th frame

JMP CODE_C2B9                                               ;

CODE_C2B6:
STA ControllerInput,X                                       ;store controller inputs

CODE_C2B9:
LDA #$00                                                    ;
STA ControllerInput_Counter,X                               ;timer zero

RETURN_C2BE:
RTS                                                         ;

BufferedDraw_C2BF:
LDA BGTileBuffer_WriteFlag                                  ;flag for tile update
BEQ RETURN_C302                                             ;obviously don't do that if not set

Macro_SetWord BGTileBuffer, $00                             ;set buffer address as indirect address

LDA ControlMirror                                           ;
AND #$FB                                                    ;enable any of bits except for bits 0 and 1
STA ControlBits                                             ;(which are related with nametables)
STA ControlMirror                                           ;back them up

LDX HardwareStatus                                          ;prepare for drawing
LDY #$00                                                    ;initialize Y register
BEQ CODE_C2F4                                               ;jump ahead

LOOP_C2DB:
STA VRAMPointerReg                                          ;set tile drawing position, high byte

INY                                                         ;
LDA ($00),Y                                                 ;
STA VRAMPointerReg                                          ;

INY                                                         ;
LDA ($00),Y                                                 ;
AND #$3F                                                    ;
TAX                                                         ;set how many tiles to draw on a single line

LOOP_C2EA:
INY                                                         ;
LDA ($00),Y                                                 ;now, tiles
STA VRAMUpdateRegister                                      ;
DEX                                                         ;
BNE LOOP_C2EA                                               ;draw untill the end

INY                                                         ;

CODE_C2F4:
LDA ($00),Y                                                 ;if it transferred all tile data from buffer addresses by hitting address with 0, return
BNE LOOP_C2DB                                               ;

LDA #$00                                                    ;
STA BGTilebuffer_Offset                                     ;
STA BGTileBuffer                                            ;
STA BGTileBuffer_WriteFlag                                  ;end draw, reset flag

RETURN_C302:
RTS                                                         ;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Fill screen routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Clears screen, fill it with one time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;USED RAM ADRESSES:
;$00 - Tile value, used to fill screen
;$01 - VRAM tile write placement, used to determine starting position for tile drawing
;$02 - used as position for attribute clearing, VRAM's position, high byte
;ControlMirror - uses previous stored PPU bits to enable almost everything, except bit 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ClearScreen_C303:
LDA HardwareStatus                                          ;ready to draw
LDA ControlMirror                                           ;\
AND #$FB                                                    ;|all bits except for nametable
STA ControlBits                                             ;|
STA ControlMirror                                           ;/

LDA #$1C                                                    ;
CLC                                                         ;

CODE_C312:
ADC #$04                                                    ;

DEC $01                                                     ;calculate high byte of tile drawing starting point
BNE CODE_C312                                               ;20, 24, 28, 2C
STA $02                                                     ;VRAM location high byte
STA VRAMPointerReg                                          ;

LDA #$00                                                    ;tile drawing's position, low byte
STA VRAMPointerReg                                          ;

LDX #$04                                                    ;to effectively clear full screen, we need to go from 0 to 255 (dec) 4 times! which is 8 horizontal tile lines from the top right to the bottom left tile. that's how many 8x8 tiles to clear
LDY #$00                                                    ;(technically not, as this also affects attributes that start after 2xBF, but they get cleared afterwards anyway)
LDA $00                                                     ;load whatever valus is in there.

LOOP_C328:
STA VRAMUpdateRegister                                      ;\fill screen(s) with tiles
DEY                                                         ;|
BNE LOOP_C328                                               ;|
DEX                                                         ;|
BNE LOOP_C328                                               ;/

LDA $02                                                     ;\calculate position of tile attribute data.
ADC #$03                                                    ;|
STA VRAMPointerReg                                          ;/

LDA #$C0                                                    ;\attributes location, low byte
STA VRAMPointerReg                                          ;/

LDY #$40                                                    ;64 bytes of attributes
LDA #$00                                                    ;zero 'em out

LOOP_C341:
STA VRAMUpdateRegister                                      ;\this loop clears tile attributes (y'know, 32x32 areas that contain palette data for each individual 16x16 in it tile)
DEY                                                         ;|
BNE LOOP_C341                                               ;|
RTS                                                         ;/

;timer handler
HandleGlobalTimers_C348:
LDX #$01                                                    ;a couple of timers decrease every frame
DEC Timer_Timing                                            ;
BPL LOOP_C354                                               ;

LDA #10                                                     ;some timers tick every 10 frames
STA Timer_Timing                                            ;

LDX #$03                                                    ;a couple more timers decrease every time Timer_Timing

LOOP_C354:
LDA Timer_Base+1,X                                          ;
BEQ CODE_C35A                                               ;can only count down if non-zero

DEC Timer_Base+1,X                                          ;tick tock

CODE_C35A:
DEX                                                         ;next timer
BPL LOOP_C354                                               ;
RTS                                                         ;

ExecutePointers_C35E:
STX $28                                                     ;\save X and Y (new addition since Mario Bros. (NES))
STY $29                                                     ;/

ASL A                                                       ;loaded value multiply by 2
TAY                                                         ;turn into y
INY                                                         ;and add 1 (to jump over jsr's bytes and load table values correctly)
PLA                                                         ;pull our previous location that JSR pushed for us
STA $14                                                     ;low byte
PLA                                                         ;
STA $15                                                     ;high byte

LDA ($14),Y                                                 ;load new location from the table, low byte
TAX                                                         ;
INY                                                         ;increase Y for high byte
LDA ($14),Y                                                 ;
STA $15                                                     ;
STX $14                                                     ;

LDX $28                                                     ;reload X and Y
LDY $29                                                     ;
JMP ($0014)                                                 ;jump wherever

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Layout building routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This routine draws tiles from table, accessed via indirect addressing, using NES Stripe Image RLE format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;USED RAM ADRESSES:
;$00 - Table location in ROM, low byte
;$01 - Table location in ROM, high byte
;ControlMirror - contains previously enabled bits of $2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LOOP_C37C:
STA VRAMPointerReg                                          ;store VRAM position to write to (high byte)

INY                                                         ;
LDA ($00),Y                                                 ;
STA VRAMPointerReg                                          ;Low byte

INY                                                         ;
LDA ($00),Y                                                 ;
ASL A                                                       ;bit 7 - vertical draw bit, test for it
PHA                                                         ;
LDA ControlMirror                                           ;
ORA #$04                                                    ;enable drawing in a verical line
BCS CODE_C392                                               ;if bit 7 enabled, will draw vertically
AND #$FB                                                    ;do not enable it if vertical draw bit not set

CODE_C392:
STA ControlBits                                             ;
STA ControlMirror                                           ;
PLA                                                         ;
ASL A                                                       ;now test for bit 6 - repeated draw (draw the same tile multiple times)
BCC CODE_C39E                                               ;
ORA #$02                                                    ;this will trigger the same tile draw loop, remember - we have shifted A left two times. if we return it back to where it belongs, this will set carry
INY                                                         ;move on from drawing property to a tile array or value to repeat

CODE_C39E:
LSR A                                                       ;bits 5 through 0 - amount of data to process (unique tiles or tile repeats)
LSR A                                                       ;
TAX                                                         ;

LOOP_C3A1:
BCS CODE_C3A4                                               ;if the repeat draw bit was set, will draw the same tile

INY                                                         ;draw the next tile from an array

CODE_C3A4:
LDA ($00),Y                                                 ;
STA VRAMUpdateRegister                                      ;
DEX                                                         ;
BNE LOOP_C3A1                                               ;

SEC                                                         ;
TYA                                                         ;
ADC $00                                                     ;update table pointer
STA $00                                                     ;

LDA #$00                                                    ;
ADC $01                                                     ;high byte ofc
STA $01                                                     ;

DrawStripeImage_C3B8:
LDX HardwareStatus                                          ;warm our arm up before drawing on canvas
LDY #$00                                                    ;
LDA ($00),Y                                                 ;check for stop command, which is 0
BNE LOOP_C37C                                               ;non-zero = draw

UpdateCameraPosition_C3C1:
PHA                                                         ;
LDA CameraPositionX                                         ;restore camera position
STA VRAMRenderAreaReg                                       ;

LDA CameraPositionY                                         ;
STA VRAMRenderAreaReg                                       ;
PLA                                                         ;
RTS                                                         ;fin

;Set up buffered write.
;$00-$01 - VRAM location to write to
;$02 - row amount and length. high nibble is the amount of rows, while low nibble is the amount of tiles on a single row.
WriteScreenUpdateToBuffer_C3CE:
LDA #$01                                                    ;
STA BGTileBuffer_WriteFlag                                  ;ready to draw. though it would've made sense to set after, you know, in case the game lags at an unfortunate time

LDY #$00                                                    ;
LDA ($02),Y                                                 ;number of bytes to update on a single row
AND #$0F                                                    ;
STA $05                                                     ;

LDA ($02),Y                                                 ;number of rows to update
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
STA $04                                                     ;

LDX BGTilebuffer_Offset                                     ;continue where left off

LOOP_C3E5:
LDA $01                                                     ;VRAM location
STA BGTileBuffer,X                                          ;

JSR MoveToNextBufferByte_C426                               ;

LDA $00                                                     ;VRAM location part 2
STA BGTileBuffer,X

JSR MoveToNextBufferByte_C426                               ;

LDA $05                                                     ;number of tiles to draw
STA $06                                                     ;set up a loop
STA BGTileBuffer,X                                          ;and save that information in the buffer.

LOOP_C3FC:
JSR MoveToNextBufferByte_C426                               ;
INY                                                         ;
LDA ($02),Y                                                 ;
STA BGTileBuffer,X                                          ;

DEC $06                                                     ;keep looping untill all bytes are in the buffer  
BNE LOOP_C3FC                                               ;

JSR MoveToNextBufferByte_C426                               ;
STX BGTilebuffer_Offset                                     ;store current buffer offset
CLC                                                         ;
LDA #$20                                                    ;offset by 32 (move down 1 tile for the next row)
ADC $00                                                     ;
STA $00                                                     ;

LDA #$00                                                    ;
ADC $01                                                     ;
STA $01                                                     ;

DEC $04                                                     ;draw until all rows are done
BNE LOOP_C3E5                                               ;

LDA #$00                                                    ;
STA BGTileBuffer,X                                          ;stop command
RTS                                                         ;

MoveToNextBufferByte_C426:
INX                                                         ;next byte
TXA                                                         ;
CMP #$4F                                                    ;check if there's too much to update (compared to Donkey Kong (NES), this limit is increased!)
BCC RETURN_C436                                             ;

LDX BGTilebuffer_Offset                                     ;

LDA #$00                                                    ;
STA BGTileBuffer,X                                          ;put a stop to it!

PLA                                                         ;have a panic attack and stop buffer write!!
PLA                                                         ;

RETURN_C436:
RTS                                                         ;

;score related
;Input:
;$04 - substraction flag, clear - addition, set - substraction (substraction is unused in this game)
;$05 - Hundreds and tens thousands to add
;$06 - Thousands and hundreds to add
;$07 - Tens and ones to add
;A - score address offset, where 0 - mario and 1 - luigi
UpdateScore_C437:
CLC                                                         ;
ADC #$01                                                    ;
AND #$0F                                                    ;does this mean that theoratically you can have more than 2 player scores? i don't know what ninty game allows for more than 2 and uses this same routine
ASL A                                                       ;
ASL A                                                       ;x4 to get correct score address
TAX                                                         ;

LDA $04                                                     ;0 - addition, non-zero - substraction
BEQ CODE_C46A                                               ;

LDA $C3,X                                                   ;whatever profanity this is.
BEQ CODE_C46E                                               ;

CODE_C447:
CLC                                                         ;
LDA CurrentScore+2,X                                        ;store original value before calculation into $03
STA $03                                                     ;

LDA $07                                                     ;
JSR CounterAddition_C4BF                                    ;calculate tens and ones
STA CurrentScore+2,X                                        ;

LDA CurrentScore+1,X                                        ;
STA $03                                                     ;

LDA $06                                                     ;
JSR CounterAddition_C4BF                                    ;now calculate hundreds and thousands
STA CurrentScore+1,X                                        ;

LDA CurrentScore,X                                          ;
STA $03                                                     ;

LDA $05                                                     ;
JSR CounterAddition_C4BF                                    ;calculate tens and hundreds of thousands
STA CurrentScore,X                                          ;
RTS                                                         ;you guessed it, calculated result

;the game never takes score away from you
CODE_C46A:
LDA CurrentScore-1,X                                        ;
BEQ CODE_C447                                               ;will substract instead if this is set.

CODE_C46E:
SEC                                                         ;
LDA CurrentScore+2,X                                        ;
STA $03                                                     ;

LDA $07                                                     ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore+2,X                                        ;

LDA CurrentScore+1,X                                        ;
STA $03                                                     ;

LDA $06                                                     ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore+1,X                                        ;

LDA CurrentScore,X                                          ;
STA $03                                                     ;

LDA $05                                                     ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore,X                                          ;

LDA CurrentScore,X                                          ;this LDA isn't necessary?
BNE CODE_C49C                                               ;

LDA CurrentScore+1,X                                        ;
BNE CODE_C49C                                               ;

LDA CurrentScore+2,X                                        ;
BEQ CODE_C4A2                                               ;

CODE_C49C:
BCS RETURN_C4BE                                             ;

LDA CurrentScore-1,X                                        ;
EOR #$FF                                                    ;

CODE_C4A2:
STA CurrentScore-1,X                                        ;
SEC                                                         ;
LDA #$00                                                    ;
STA $03                                                     ;

LDA CurrentScore+2,X                                        ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore+2,X                                        ;

LDA CurrentScore+1,X                                        ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore+1,X                                        ;

LDA CurrentScore,X                                          ;
JSR CounterSubstraction_C4E0                                ;
STA CurrentScore,X                                          ;

RETURN_C4BE:
RTS                                                         ;

;Calculate counter value, like score, addition
;Input:
;$03 - original value to add to
;A - value to add
;Output:
;A - result
;Carry - if next calculation in this routine should have a +1 to the low digit
CounterAddition_C4BF:
JSR ExtractDigits_C502                                      ;
ADC $01                                                     ;
CMP #$0A                                                    ;if value less than A
BCC CODE_C4CA                                               ;don't round
ADC #$05                                                    ;by adding 6 (incluing carry)

CODE_C4CA:
CLC                                                         ;
ADC $02                                                     ;
STA $02                                                     ;

LDA $03                                                     ;tens/thousands/ten thousands
AND #$F0                                                    ;only care about left digit that can overflow
ADC $02                                                     ;and additional ten/whatev
BCC CODE_C4DB                                               ;overflow?

CODE_C4D7:
ADC #$5F                                                    ;+$60 because of the carry (need carry set to get there)
SEC                                                         ;and +1 to the next digit calculation (so for example 90+20=110, which results in +1 to the hundreds address)
RTS                                                         ;

CODE_C4DB:
CMP #$A0                                                    ;hundreds and etc?
BCS CODE_C4D7                                               ;if so, round it to 0
RTS                                                         ;otherwise return

;Calculate counter value, like score, substraction
;$03 - original value to substract from
;A - value to substract
;Output:
;A - result
;Carry - if next calculation in this routine should have a -1 to the low digit
CounterSubstraction_C4E0:
JSR ExtractDigits_C502                                      ;
SBC $01                                                     ;do some calculation for carry
STA $01                                                     ;
BCS CODE_C4F3                                               ;
ADC #$0A                                                    ;decimal-ize, 10-1 = 9, not F
STA $01                                                     ;

LDA $02                                                     ;
ADC #$0F                                                    ;
STA $02                                                     ;

CODE_C4F3:
LDA $03                                                     ;
AND #$F0                                                    ;
SEC                                                         ;
SBC $02                                                     ;
BCS CODE_C4FF                                               ;

ADC #$A0                                                    ;same decimal operation with high nibble
CLC                                                         ;

CODE_C4FF:
ORA $01                                                     ;
RTS                                                         ;

;extract two digits into two bytes
;Input:
;A - Value to get digits from
;
;Output:
;$01 - right digit (00-0F)
;$02 - left digit (00-F0)
ExtractDigits_C502:
PHA                                                         ;
AND #$0F                                                    ;
STA $01                                                     ;save low digit
PLA                                                         ;
AND #$F0                                                    ;
STA $02                                                     ;and high digit

LDA $03                                                     ;calculate low digit of value we're about to calculate
AND #$0F                                                    ;
RTS                                                         ;

;this code is used to calculate difference between player score(s) and TOP score to potentially replace it
;Input $00 - low nibble is player score offset (bits 0-2 ONLY), high nibble - TOP score offset (#$10 is added to it, so if it's #$F0 it'll result in #$00)
;low nibble bit 3 - run through 2 player scores (if not set, can be used for single player games)
UpdateTOPScore_C511:
LDA #$00                                                    ;
STA $04                                                     ;not quite sure what this is for yet

CLC                                                         ;
LDA $00                                                     ;the amount of high scores?
ADC #$10                                                    ;
AND #$F0                                                    ;$F0+$10 = $00, indicating 1 TOP score on screen
LSR A                                                       ;
LSR A                                                       ;
TAY                                                         ;Y - TOP score offset (can support multiple TOP scores?)

LDA $00                                                     ;X - players score offset
AND #$07                                                    ;
ASL A                                                       ;
ASL A                                                       ;
TAX                                                         ;

LOOP_C526:
LDA CurrentScore-1,Y                                        ;those unknown flags that are as unknown as ever
BEQ CODE_C57C                                               ;

LDA TopScore-1,X                                            ;those unknown flags 2: electric boogaloo
BEQ CODE_C555                                               ;

CODE_C52F:
SEC                                                         ;
LDA CurrentScore+2,Y                                        ;
STA $03                                                     ;

LDA TopScore+2,X                                            ;
JSR CounterSubstraction_C4E0                                ;will compare score with top score by substracting TOP from current player score

LDA CurrentScore+1,Y                                        ;
STA $03                                                     ;

LDA TopScore+1,X                                            ;
JSR CounterSubstraction_C4E0                                ;

LDA CurrentScore,Y                                          ;
STA $03                                                     ;

LDA TopScore,X                                              ;
JSR CounterSubstraction_C4E0                                ;
BCS CODE_C580                                               ;

LDA CurrentScore-1,Y                                        ;
BNE CODE_C585                                               ;

CODE_C555:
LDA #$FF                                                    ;
STA $04                                                     ;
SEC                                                         ;

CODE_C55A:
TYA                                                         ;
BNE RETURN_C57B                                             ;
BCC CODE_C56F                                               ;

LDA TopScore-1,X                                            ;unknown flags that I still have no clue what they are. seriously, what are these??
STA CurrentScore-1                                          ;

LDA TopScore,X                                              ;now this doesn't make any sense. in other games, this is done the other way around.
STA CurrentScore                                            ;but in this game, current score gets stored to top score and then vice versa??? why is it so convoluted?

LDA TopScore+1,X                                            ;
STA CurrentScore+1                                          ;

LDA TopScore+2,X                                            ;
STA CurrentScore+2                                          ;

CODE_C56F:
LDA $00                                                     ;
AND #$08                                                    ;
BEQ RETURN_C57B                                             ;check if want to update top score by comparing with other score (multiplayer)

DEX                                                         ;
DEX                                                         ;
DEX                                                         ;
DEX                                                         ;
BPL LOOP_C526                                               ;next score to check. naturally, this functionality is unused in this game

RETURN_C57B:
RTS                                                         ;

CODE_C57C:
LDA TopScore-1,X                                            ;
BEQ CODE_C52F                                               ;

CODE_C580:
LDA CurrentScore-1,Y                                        ;
BNE CODE_C555                                               ;

CODE_C585:
CLC                                                         ;
BCC CODE_C55A                                               ;

RNG_C588:
LDA RNG_Value                                               ;
AND #$02                                                    ;
STA $07                                                     ;something to make it a little more "random"

LDA RNG_Value+1                                             ;
AND #$02                                                    ;
EOR $07                                                     ;
CLC                                                         ;
BEQ CODE_C59A                                               ;if value isn't zero, don't set carry
SEC                                                         ;

CODE_C59A:
ROR RNG_Value                                               ;
ROR RNG_Value+1                                             ;
ROR RNG_Value+2                                             ;
ROR RNG_Value+3                                             ;

LDA RNG_Value                                               ;output for some calls
RTS                                                         ;

InitTitleScreen_C5AA:
JSR DisableRender_C5E1                                      ;hide any evidence of us messing with the screen

JSR ClearScreenInit_C230                                    ;
JSR WaitForNMI_C5D9                                         ;after clearing screen, take a break

LDA #$02                                                    ;clear $2400 for some reason.
STA $01

LDA #$50                                                    ;tile $50
STA $00                                                     ;
JSR ClearScreen_C303                                        ;

JSR ClearOAM_C23B                                           ;remove all sprite tiles
JSR WaitForNMI_C5D9                                         ;a deserved vacation after the hard work

DrawStripeImage Layout_TitleScreen_E404                     ;draw title screen

LDA #LoadPalette_TitleScreen                                ;
STA PaletteToLoad                                           ;load title screen

LDA #$01                                                    ;
STA SelectHeldFlag                                          ;pretend like we held select so it doesn't trigger immediately after loading in

INC TitleScreenMode                                         ;do nothing else of importance

JMP EnableRender_C5EE                                       ;make you look at the title screen

;a simple loop to take a break in
WaitForNMI_C5D9:
JSR PrepareForNMIWait_C61E                                  ;

LOOP_C5DC:
LDA FrameFlag                                               ;wait
BEQ LOOP_C5DC                                               ;

DoNothing_C5E0:
RTS                                                         ;do nothing. this comment is utterly redundant.

DisableRender_C5E1:
JSR WaitForNMI_C5D9                                         ;wait before we can do it

LDA RenderMirror                                            ;
AND #$E7                                                    ;

CODE_C5E8:
STA RenderMirror                                            ;
STA RenderBits                                              ;
RTS                                                         ;

EnableRender_C5EE:
JSR WaitForNMI_C5D9                                         ;

LDA RenderMirror                                            ;
ORA #$18                                                    ;
BNE CODE_C5E8                                               ;enable these bits

;input Y - entity variable offset for Entity_Variables
GetCurrentEntityVariables_C5F7:
STY CurrentEntity_MemoryOffset

LDX #$00                                                    ;

LOOP_C5FB:
LDA Entity_Variables,Y                                      ;
STA CurrentEntity_Variables,X                               ;
INY                                                         ;
INX                                                         ;
CPX #Entity_Variables_Size                                  ;whopping 80 bytes of RAM for each entity.
BNE LOOP_C5FB                                               ;
RTS                                                         ;

;store from current entity variables to... entity variables. can move onto next entity or something.
UpdateEntityVariables_C607:
LDY CurrentEntity_MemoryOffset                              ;
LDX #$00                                                    ;

LOOP_C60B:
LDA CurrentEntity_Variables,X                               ;
STA Entity_Variables,Y                                      ;
INY                                                         ;
INX                                                         ;
CPX #Entity_Variables_Size                                  ;
BNE LOOP_C60B                                               ;
RTS                                                         ;

;X - table location, low byte
;Y - table location, high byte
DrawStripeImageInit_C617
STX $00                                                     ;
STY $01                                                     ;
JMP DrawStripeImage_C3B8                                    ;

;clear NMI-related variables to make it so that NMI can run its code and alert us when its done
PrepareForNMIWait_C61E:
LDA #$00                                                    ;
STA FrameFlag                                               ;
STA NMIFunctionsDisableFlag                                 ;
RTS                                                         ;

;change gameplay mode when the timer runs out
WaitForGameplayModeChange_C625:
LDA Timer_2D                                                ;
BNE RETURN_C62D                                             ;wait...

LDA GameplayMode_Delayed                                    ;
STA GameplayMode                                            ;

RETURN_C62D:
RTS                                                         ;

;TimeGameplayModeChange
;X - gameplay mode to store after timer runs out
;A - amount of time to wait * 10
StartTimedGameplayModeChange_C62E:
STX GameplayMode_Delayed                                    ;
STA Timer_2D                                                ;

LDA #GameplayMode_WaitForGameplayModeChange                 ;
STA GameplayMode                                            ;
RTS                                                         ;

;clears RAM in range 0300-03FF (which includes entity variables)
Clear0300Page_C637:
LDY #$00                                                    ;
TYA                                                         ;

LOOP_C63A:
STA $0300,Y
INY                                                         ;
BNE LOOP_C63A                                               ;
RTS                                                         ;

;clears RAM $30-$FF
ClearZeroPageVariables_C641:
LDA #$00
LDX #$30

LOOP_C645:
STA $00,X
INX
BNE LOOP_C645
RTS

;clear RAM 40-BF
ClearZeroPageVariablesLimited_C64B:
LDA #$00
LDX #$7F

LOOP_C64F:
STA $40,X
DEX
BPL LOOP_C64F
RTS

;Puts each individual digit into buffer to update it later
PutScoreIntoBuffer_C655:
LDX #$03                                                    ;
LDY #$05                                                    ;6 digits

LOOP_C659:
LDA CurrentScore-1,X                                        ;
AND #$0F                                                    ;low digit
STA BGTileBuffer_Transfer+1,Y                               ;

DEY                                                         ;
LDA CurrentScore-1,X                                        ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
STA BGTileBuffer_Transfer+1,Y                               ;other digit
DEX                                                         ;
DEY                                                         ;
BPL LOOP_C659                                               ;

LDA #$16                                                    ;6 tiles on a single row
STA BGTileBuffer_Transfer                                   ;

LDX ScorePointerVRAMLoc_E5DB+1                              ;someone forgot about turning this table into an immediate value
LDY ScorePointerVRAMLoc_E5DB                                ;
JMP TransferBGWriteToBuffer_C71D                            ;

;$0C - shape
;$B1 - vertical position
;$B2 - horizontal position
DrawZapperShape_C67C:
LDA $0C                                                     ;
ASL A                                                       ;
TAY                                                         ;
LDA ZapperShapeSpriteGFXPointers_EB30,Y                     ;get shape pointer
STA $10

LDA ZapperShapeSpriteGFXPointers_EB30+1,Y                   ;
STA $11                                                     ;

LDY #$00                                                    ;
LDX #$00                                                    ;

LOOP_C68E:
LDA ($10),Y                                                 ;
CMP #$AA                                                    ;$AA is terminator for this routine
BNE CODE_C695                                               ;

RETURN_C694:
RTS                                                         ;umm, I'm outta here!

CODE_C695:
CLC
ADC $B1
CMP #$A8
BCC CODE_C69E

LDA #$F4                                                    ;offscreen

CODE_C69E:
STA $0D

INY
LDA ($10),Y
CLC
ADC $B2
STA $0E

INY
LDA $0D                                                     ;vertical position
STA OAM_Y,X

INX                                                         ;
LDA #$56                                                    ;square tile
STA OAM_Tile-1,X                                            ;

INX                                                         ;
LDA #OAMProp_Palette0                                       ;
STA OAM_Prop-2,X                                            ;

INX
LDA $0E
STA OAM_X-3,X

INX
JMP LOOP_C68E

HandleHitTableVisualUpdate_C6C4:
LDA HitCounterUpdateFlag                                    ;
BEQ RETURN_C694                                             ;

LDA #$00                                                    ;
STA HitCounterUpdateFlag                                    ;

DrawHitTable_C6CC:
LDA #BGTile_Duck+1                                          ;
LDY SelectedGame                                            ;check if we're playing with clay pigeons or real pigeons
CPY #$02                                                    ;
BNE CODE_C6D6                                               ;

LDA #BGTile_ClayPigeon+1                                    ;

CODE_C6D6:
STA $0C                                                     ;remember the tile

LDX #$09                                                    ;

LOOP_C6DA:
LDY $0C                                                     ;
LDA HitTable,X                                              ;
BEQ CODE_C6E2                                               ;
DEY                                                         ;use filled tile if shot

CODE_C6E2:
TYA                                                         ;
STA BGTileBuffer_Transfer+1,X                               ;

DEX                                                         ;
BPL LOOP_C6DA                                               ;

CODE_C6E9:
LDA #$1A                                                    ;a single row with 10 tiles
STA BGTileBuffer_Transfer                                   ;

LDX #$4C                                                    ;
LDY #$23                                                    ;hit table is at $234C
JMP TransferBGWriteToBuffer_C71D                            ;

HandleBulletVisualUpdate_C6F5:
LDA ShotCounterUpdateFlag                                   ;
BEQ RETURN_C694                                             ;

LDA #$00                                                    ;
STA ShotCounterUpdateFlag                                   ;

LDX ShotCount                                               ;
LDY #$00                                                    ;

LOOP_C701:
CPX #$00                                                    ;
BEQ CODE_C70A                                               ;check if ran out of bullets to draw. will draw spaces
DEX                                                         ;
LDA #BGTile_Bullet                                          ;draw a bullet
BNE CODE_C70C                                               ;

CODE_C70A:
LDA #$B7                                                    ;blank space

CODE_C70C:
STA BGTileBuffer_Transfer+1,Y                               ;
INY                                                         ;
CPY #$03                                                    ;
BNE LOOP_C701                                               ;

LDA #$13                                                    ;
STA BGTileBuffer_Transfer                                   ;

LDX #$43                                                    ;
LDY #$23                                                    ;

;X - VRAM location low byte
;Y - VRAM location high byte
TransferBGWriteToBuffer_C71D:
STX $00                                                     ;
STY $01                                                     ;

LDX #<BGTileBuffer_Transfer                                 ;
LDY #>BGTileBuffer_Transfer                                 ;
STX $02                                                     ;
STY $03                                                     ;
JMP WriteScreenUpdateToBuffer_C3CE                          ;

GameOver_C72C:
LDA Timer_2D
BEQ CODE_C73A                                               ;just wait.

LDA SelectedGame                                            ;check if current game is NOT clay pigeon game
CMP #$02                                                    ;
BEQ RETURN_C739                                             ;
JMP HandleDogEntity_CC94                                    ;what the dog doin?

RETURN_C739:
RTS                                                         ;

CODE_C73A:
LDX #$03                                                    ;

CODE_C73C:
LDA CurrentScore-1,X                                        ;
STA TopScore-1,X                                            ;
DEX                                                         ;
BPL CODE_C73C                                               ;

LDA SelectedGame                                            ;
ASL A                                                       ;
ASL A                                                       ;
TAY                                                         ;
STY $0F                                                     ;

LDX #$00                                                    ;

LOOP_C74C:
LDA TopScoreTable,Y                                         ;
STA CurrentScore-1,X                                        ;in this context, current score is top score.
INY                                                         ;
INX                                                         ;
CPX #$04                                                    ;
BNE LOOP_C74C                                               ;

LDA #$F0                                                    ;
STA $00                                                     ;
JSR UpdateTOPScore_C511                                     ;update top score for real

LDY $0F
LDX #$00

LOOP_C762:
LDA CurrentScore-1,X
STA TopScoreTable,Y                                         ;update our table of scores
INY                                                         ;
INX                                                         ;
CPX #$04                                                    ;
BNE LOOP_C762                                               ;

LDY #$00                                                    ;
STY ZapperEnabledFlag                                       ;disable zapper for now
STY TitleScreenMode                                         ;gonna load the title screen
INY                                                         ;
STY TitleScreenFlag                                         ;return to the title screen

JMP DisableRender_C5E1                                      ;go to darkness

InitGameOver_C779:
JSR QueueSFX_GameOverTheme_D50D
JSR WaitForNMI_C5D9                                         ;

JSR QueueSFX_DogLaugh_D4FA                                  ;the game attempts to play dog laughter during game over. what happens next will shock you! nothing, because of sound priority.

LDA #$01
STA Entity_Variables_ActiveFlag+(Entity_Variables_Size*2)   ;
STA Entity_Variables_CurrentState+(Entity_Variables_Size*2) ;

LDA #GameplayMode_GameOver                                  ;
STA GameplayMode                                            ;

LDA #$15
STA Timer_2D
RTS

;fill the bottom bar with required hits
UpdateRequiredHits_C793:
LDX #$0A                                                    ;

LOOP_C795:
CPX HitCountToWin                                           ;
BCC CODE_C79F                                               ;if less or equal to the amount of kills necessary...
BEQ CODE_C79F                                               ;will draw the stripe tile indicating a required hit

LDA #$B7                                                    ;empty tile for the rest
BNE CODE_C7A1                                               ;

CODE_C79F:
LDA #BGTile_PassLineTile                                    ;

CODE_C7A1:
STA BGTileBuffer_Transfer,X                                 ;
DEX                                                         ;
BNE LOOP_C795                                               ;

LDA #$1A                                                    ;
STA BGTileBuffer_Transfer                                   ;

LDX #$6C                                                    ;
LDY #$23                                                    ;
JMP TransferBGWriteToBuffer_C71D                            ;

;add +1 to the round counter and queue it for drawing
UpdateRoundNumber_C7B3:
LDA CurrentRound                                            ;store current round number here for the calculation
STA $03                                                     ;

LDA #$01                                                    ;add 1
CLC                                                         ;
JSR CounterAddition_C4BF                                    ;
STA CurrentRound                                            ;update

LDA #$12                                                    ;1 row, 2 tiles
STA BGTileBuffer_Transfer

LDX #$DD                                                    ;which tile to use instead of the missing digit (ones)
LDA SelectedGame                                            ;
CMP #$02                                                    ;
BEQ CODE_C7CE                                               ;use above tile for clay shooting

LDX #$50                                                    ;use this tile for duck game

CODE_C7CE:
STX BGTileBuffer_Transfer+2

LDX #$00                                                    ;
LDA CurrentRound                                            ;
AND #$F0                                                    ;
BEQ CODE_C7E1                                               ;if there are no tens, only one digit to draw
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
STA BGTileBuffer_Transfer+1,X                               ;tens
INX                                                         ;

CODE_C7E1:
LDA CurrentRound                                            ;
AND #$0F                                                    ;
STA BGTileBuffer_Transfer+1,X                               ;ones

LDX #$05
LDY #$23
JMP TransferBGWriteToBuffer_C71D                            ;

ClayPigeonIntro_C7EF:
LDA $9D
BEQ CODE_C7F6
JMP CODE_C7FB

CODE_C7F6:
LDA #GameplayMode_StartClayShooting
STA GameplayMode
RTS



CODE_C7FB:
LDA Timer_2B
BNE RETURN_C81C

LDY $9D
BEQ RETURN_C81C

LDA DATA_EC1F,Y
CMP #$AA
BEQ CODE_C818
PHA
INY
LDA DATA_EC1F,Y
STA Timer_2B
INY
STY $9D
PLA
JMP CODE_D46C

CODE_C818:
LDA #$00
STA $9D

RETURN_C81C:
RTS

InitGameplay_C81D:
JSR ClearZeroPageVariables_C641                             ;clear some essentials
JSR WaitForNMI_C5D9
JSR ClearScreenAndAttributesInit_C22B                       ;prepare for transition...
JSR InitVariablesAndOAM_D4D9

LDX #GameplayMode_LoadDuckGame                              ;go to duck game...
LDA TitleScreenChoice                                       ;
STA SelectedGame                                            ;
CMP #$02                                                    ;check if our choice was clay shooting or duck game
BCC CODE_C836

LDX #GameplayMode_LoadClayShooting                          ;...or go clay pigeon

CODE_C836:
STX GameplayMode
RTS

LoadDuckGame_C839:
JSR WaitForNMI_C5D9

DrawStripeImage Layout_DuckGame_E042

LDA #LoadPalette_DuckGame
STA PaletteToLoad

LDA #$02
STA GameplayMode

JSR UpdateCameraPosition_C3C1
JSR EnableRender_C5EE
JMP QueueSFX_DuckGameStartTheme_D515

InitDuckGame_C854:
JSR InitVariablesAndOAM_D4D9                                ;
JSR SetRequiredAmountOfHitsAndSummonDog_D2A4                ;summon god. i mean, dog.
JSR UpdateRequiredHits_C793                                 ;
JSR UpdateRoundNumber_C7B3                                  ;

NOP                                                         ;the three sussy nopes... dummied JSR?
NOP                                                         ;and yes, sussy. haha, amogus, OMEGALUL i am so funny.
NOP                                                         ;

LDA #$00                                                    ;
STA ZapperEnabledFlag                                       ;
STA $9D

LDA #GameplayMode_DuckGameMain                              ;
STA GameplayMode                                            ;
RTS                                                         ;

ExecuteDuckGame_C86E:
JSR HandleZapperInput_D131                                  ;
JSR HandleZapperFunctions_D160                              ;
JSR HandleDogEntity_CC94                                    ;handle dog behavior. is it normal that the dog is laughing at me?
JSR HandleDuckEntities_C890
JSR HandleBulletVisualUpdate_C6F5
JSR HandleHitTableIconAnimation_D3D2
JSR HandleHitTableVisualUpdate_C6C4
JSR HandleScoreSprites_CFBA
JSR HandleShotCounterBlinkAnimation_D428
JSR CODE_C7FB

NOP                                                         ;you can't tell me there wasn't something there!
NOP                                                         ;
NOP                                                         ;
RTS                                                         ;

HandleDuckEntities_C890:
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*0)   ;check if duck 1 is enabled
BEQ HandleSecondDuck_C8A2

LDY #Entity_Variables_Size*0
STY CurrentEntity_Slot                                      ;first entity
JSR GetCurrentEntityVariables_C5F7
JSR ExecuteDuckEntityCode_C902
JSR UpdateEntityVariables_C607

HandleSecondDuck_C8A2:
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)   ;check if duck 2 is enabled
BEQ CODE_C8B4

LDY #Entity_Variables_Size*1                                ;
INC CurrentEntity_Slot                                      ;second entity
JSR GetCurrentEntityVariables_C5F7                          ;
JSR ExecuteDuckEntityCode_C902                              ;
JSR UpdateEntityVariables_C607                              ;

CODE_C8B4:
LDA ZapperFunctionsEnabledFlag                              ;
BEQ RETURN_C901                                             ;

LDY $9F
BNE CODE_C8C4

LDY #$41
LDA ShotCount
BNE CODE_C8C4

LDY #$15

CODE_C8C4:
DEY
STY $9F
LDA $BC
BNE CODE_C8FB

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*0)
BNE RETURN_C901                                             ;check if duck 1 exists

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*1)
BNE RETURN_C901                                             ;check if duck 2 exists

LDA Timer_2B
BNE RETURN_C901                                             ;timer check

LDA #$0A                                                    ;maniacal laughter incoming
LDY #LoadPalette_FlyAwayDog                                 ;
LDX CurrentShotDuckCount                                    ;check if shot any ducks, or they escaped
BEQ CODE_C8E5                                               ;

LDA #$0C                                                    ;happy dog is a good dog. i think.
LDY #LoadPalette_DuckGame                                   ;

CODE_C8E5:
STA Entity_Variables_CurrentState+(Entity_Variables_Size*2) ;
STY PaletteToLoad                                           ;

JSR WaitForNMI_C5D9
JSR DrawSHOT_D446                                           ;restore shot word if it blinked out of existence

LDA #$40
STA Timer_2B

LDA #Message_None                                           ;remove message
STA ZapperFunctionsEnabledFlag                              ;also disable zapper along the way
JMP DrawMessage_D464                                        ;

CODE_C8FB:
LDA $A6
BEQ RETURN_C901

DEC $A6

RETURN_C901:
RTS

ExecuteDuckEntityCode_C902:
LDA CurrentEntity_State                                     ;duck state
JSR ExecutePointers_C35E

dw DoNothing_C5E0                                           ;do nothing (fled/lying on the ground)
dw DuckEntity_Init_C919                                     ;
dw CODE_C932
dw CODE_CA19

dw CODE_CC1F
dw CODE_CC3D
dw DuckEntity_StartFalling_CC4F
dw DuckEntity_Falling_CC7C
dw CODE_CC8B

DuckEntity_Init_C919:
LDX #31                                                     ;

LOOP_C91B:
LDA DuckEntity_InitialVariables_E5EB,X                      ;set some initial values
STA CurrentEntity_Variables,X
DEX
BPL LOOP_C91B

LDA CurrentEntity_Slot                                      ;check if first or second duck
BEQ CODE_C92F

LDA #16*4
STA CurrentEntity_OAMStartPoint                             ;adjust second duck's OAM offset

LDA #(32*4)-1                                               ;where the OAM ceiling is, which dictates how many OAM slots does it take max, unused ones will be cleared.
STA CurrentEntity_OAMEndPoint                               ;

CODE_C92F:
INC CurrentEntity_State                                     ;initialized, moving on

RETURN_C931:
RTS                                                         ;

CODE_C932:
LDA ZapperFunctionsEnabledFlag                              ;zapper disabled for some reason?
BEQ RETURN_C931                                             ;nothing happens

LDA $A6
BNE RETURN_C931

JSR RNG_C588                                                ;shake the proverbial box filled with numbers
AND #$0F
LDY SelectedGame                                            ;
BNE CODE_C946                                               ;check if 1 duck game or 2 duck game
CLC
ADC #$10

CODE_C946:
CMP $9C
BNE CODE_C958
CLC
ADC #$01
CMP #$10
BEQ CODE_C955
CMP #$20
BNE CODE_C958

CODE_C955:
SEC
SBC #$10

CODE_C958:
STA $9C
ASL A
TAX
LDA POINTERS_E60B,X
STA $44

LDA POINTERS_E60B+1,X
STA $45

JSR RNG_C588
AND #$3F
TAY
INY
STY $A6

LDY #$00
LDA ($44),Y
INY
STY $47
STA CurrentEntity_XPos

LDY #OAMProp_Palette0                                       ;first duck uses palettes 1 and 2
LDX $99
BEQ CODE_C980

LDY #OAMProp_Palette2                                       ;second duck uses palettes 2 and 3

CODE_C980:
STY CurrentEntity_OAMProp

INC $99

LDX CurrentDuckConfiguration
LDA DuckColorConfigurations_E766,X
CPY #$00
BEQ CODE_C992
AND #$0F
JMP CODE_C996

CODE_C992:
LSR A
LSR A
LSR A
LSR A

CODE_C996:
STA $3F
LDX CurrentRound
LDA SelectedGame
BNE CODE_C9B9

LDA #$0B
CPX #$12
BCS CODE_C9B2

LDA #$0A
CPX #$11
BCS CODE_C9B2

LDA #$09
CPX #$10
BCS CODE_C9B2
DEX
TXA

CODE_C9B2:
TAX
LDA DATA_E75A,X
JMP CODE_C9D0

CODE_C9B9:
LDA #$1B
LDX CurrentRound
CPX #$10
BCS CODE_C9C9
DEX
TXA
STA $0C
ASL A
CLC
ADC $0C

CODE_C9C9:
CLC
ADC $3F
TAX
LDA DATA_E73C,X

CODE_C9D0:
STA $5E

LDX CurrentRound                                            ;
LDA #$0B                                                    ;duck's zapper shape becomes slightly the further you're in, making it harder to hit
CPX #$27                                                    ;smallest hitbox
BCS CODE_C9E2

LDA #$0A
CPX #$24                                                    ;rounds 24-26 will use this hitbox
BCS CODE_C9E2

LDA #$09                                                    ;standard collision otherwise (32x32)

CODE_C9E2:
STA CurrentEntity_ZapperShape                               ;

LDA #$00
STA $46
STA $42
STA $4E
STA $4F

LDA #$01
STA $4B
STA $AE

LDA SelectedGame
BNE CODE_CA0E

LDX CurrentRound
LDA #$7D
CPX #$11
BCC CODE_CA08

LDA #$5D
CPX #$20
BCC CODE_CA08

LDA #$3E

CODE_CA08:
STA $4C
LDA #$00
STA $AF

CODE_CA0E:
INC TargetEntityCount
LDA TargetEntityCount
STA $3B

DEC $BC
INC CurrentEntity_State
RTS

CODE_CA19:
LDA FrameCounter                                            ;flap sound effect every 8 frames
AND #$07                                                    ;
BNE CODE_CA22                                               ;

JSR QueueSFX_WingFlag_D4EE                                  ;

CODE_CA22:
LDA $9F
BNE CODE_CA29

JSR QueueSFX_Quack_D52C                                     ;make a weird noise.

CODE_CA29:
LDA ShotCount                                               ;check if shots remain
BNE CODE_CA3E

LDA $4E
BNE CODE_CA3E

LDA #$01
STA $4E

LDA #$F0
STA $46
LDA #$00
JMP CODE_CB77

CODE_CA3E:
LDA SelectedGame                                            ;check if 1 duck game or two
BEQ CODE_CA45

CODE_CA42:
JMP CODE_CB59

CODE_CA45:
LDA $4B
BEQ CODE_CA53

LDA CurrentEntity_YPos
CMP #$88
BCS CODE_CA42

LDA #$00
STA $4B

CODE_CA53:
LDX #$01
LDA $4C
BEQ CODE_CA70

LDA ShotCount                                               ;
BNE CODE_CA66                                               ;
STA $4C

CODE_CA5F:
JSR ShowFlyAwayMessage_CF96

LDX #$01
BNE CODE_CA70

CODE_CA66:
DEX
LDA FrameCounter
LSR A
BCC CODE_CA70

DEC $4C
BEQ CODE_CA5F

CODE_CA70:
STX $AE
CPX #$01
BEQ CODE_CA8A

LDA Controller1Input_Hold                                   ;see if controller 1 had pressed any direction
AND #$0F
BNE CODE_CAA8

LDA Controller2Input_Hold                                   ;controller 2 d-pad check
AND #$0F
BNE CODE_CAA8

LDA #$00
STA $AF
LDA $AE
BEQ CODE_CA8D

CODE_CA8A:
JMP CODE_CF16

CODE_CA8D:
LDA CurrentEntity_YPos
CMP #$20
BCC CODE_CAD1
CMP #$90
BCS CODE_CAD7

LDX #$00
LDA CurrentEntity_XPos
CMP #$10
BCC CODE_CAE3
LDX #$08
CMP #$F0
BCS CODE_CAE9

CODE_CAA5:
JMP CODE_CBC3

;this code is responsible for manipulating duck movement with D-pad
CODE_CAA8:
CMP $AF
BEQ CODE_CA8A 
STA $AF

LDX #$35                 
LSR A                    
BCS CODE_CABF

LDX #$DB
LSR A
BCS CODE_CABF

LDX #$79                 
LSR A                    
BCS CODE_CABF
 
LDX #$1F

CODE_CABF:                   
LDA FrameCounter
LSR A
TXA
BCC CODE_CACA
AND #$0F
JMP CODE_CB56

CODE_CACA:                   
LSR A                    
LSR A                    
LSR A                    
LSR A                    
JMP CODE_CB56                

CODE_CAD1:
INC CurrentEntity_YPos

LDY #$00
BEQ CODE_CADB

CODE_CAD7:
DEC CurrentEntity_YPos

LDY #$02

CODE_CADB:
LDA $48
AND #$08
TAX
JMP CODE_CAED

CODE_CAE3:
INC CurrentEntity_XPos

LDY #$04
BNE CODE_CAED

CODE_CAE9:
DEC CurrentEntity_XPos

LDY #$02

CODE_CAED:
STX $0C
STY $0F

LDA $48
AND #$07
EOR #$07
ORA $0C
STA $0C
JSR RNG_C588
AND #$03
CMP #$03
BNE CODE_CB06
LDA #$02

CODE_CB06:
CLC
ADC $0C
STA $0E
LDY $0F
CMP #$00
BNE CODE_CB1B
CPY #$01
BEQ CODE_CB4B
CPY #$02
BEQ CODE_CB45
BNE CODE_CB50

CODE_CB1B:
CMP #$04
BNE CODE_CB29
CPY #$00
BEQ CODE_CB50
CPY #$02
BEQ CODE_CB4B
BNE CODE_CB45

CODE_CB29:
CMP #$08
BNE CODE_CB37
CPY #$00
BEQ CODE_CB45
CPY #$01
BEQ CODE_CB50
BNE CODE_CB4B

CODE_CB37:
CMP #$0C
BNE CODE_CB52
CPY #$00
BEQ CODE_CB4B
CPY #$01
BEQ CODE_CB45
BNE CODE_CB50

CODE_CB45:
JSR RNG_C588
ASL A
BCS CODE_CB50

CODE_CB4B:
DEC $0E
JMP CODE_CB52

CODE_CB50:
INC $0E

CODE_CB52:
LDA $0E
AND #$0F

CODE_CB56:
JMP CODE_CB77

CODE_CB59:
LDA $46
BNE CODE_CBC3

LDY $47
LDA ($44),Y
INY
CMP #$FF
BNE CODE_CB70

LDA ($44),Y
INY
STA CurrentEntity_State

LDA ($44),Y
STA $42
RTS


CODE_CB70:
STA $46
LDA ($44),Y
INY
STY $47

CODE_CB77:
STA $48
TAX
LDA DATA_E64B,X
STA $49
STA $4A

LDA #$00
STA CurrentEntity_HorzDir

LDA $48
CMP #$11
BCC CODE_CBA2
TAX
LDA #$00
CPX #$15
BEQ CODE_CBBC
CPX #$14
BEQ CODE_CBB0
CPX #$13
BEQ CODE_CBA8

INC CurrentEntity_HorzDir
CPX #$12
BEQ CODE_CBB0
BNE CODE_CBBC

CODE_CBA2:
CMP #$08
BCC CODE_CBA8

INC CurrentEntity_HorzDir

CODE_CBA8:
LDX #<DATA_E70C
LDY #>DATA_E70C
AND #$07
BEQ CODE_CBC0

CODE_CBB0:
LDX #<DATA_E705
LDY #>DATA_E705
CMP #$03
BCC CODE_CBC0
CMP #$06
BCS CODE_CBC0

CODE_CBBC:
LDX #<DATA_E71D
LDY #>DATA_E71D

CODE_CBC0:
JSR CODE_CF0B

CODE_CBC3:
JSR CODE_D81B

CODE_CBC6:
PHA
LDA CurrentEntity_YPos
CMP #$F0
BCC CODE_CBD1
PLA
JMP CODE_CF2A

CODE_CBD1:
LDA $46
BEQ CODE_CBD7
DEC $46

CODE_CBD7:
LDY $49

CODE_CBD9:
LDA DATA_E661,Y
INY
CMP #$AA
BNE CODE_CBE6
LDY $4A
JMP CODE_CBD9

CODE_CBE6:
CLC
ADC CurrentEntity_YPos
LDX $4B
BNE CODE_CBF9

LDX $AE
BNE CODE_CBF9

CMP #$1E
BCC CODE_CBFB
CMP #$92
BCS CODE_CBFB

CODE_CBF9:
STA $32

CODE_CBFB:
LDA DATA_E661,Y
INY
STY $49
CLC
ADC CurrentEntity_XPos

LDX $4B
BNE CODE_CC14

LDX $AE
BNE CODE_CC14

CMP #$0E
BCC CODE_CC16
CMP #$F2
BCS CODE_CC16

CODE_CC14:
STA CurrentEntity_XPos

CODE_CC16:
PLA
SEC
SBC #$01
BNE CODE_CBC6
JMP CODE_CE4D

CODE_CC1F:
LDX $3B
LDA #$01
STA HitTable-1,X

LDA #$01
STA $AD
STA $5E

LDA #$12
STA $4D
JSR CODE_CF68

LDX #<DATA_E713
LDY #>DATA_E713

CODE_CC37:
JSR CODE_CF0B

INC CurrentEntity_State
RTS

CODE_CC3D:
LDA $4D
BEQ CODE_CC4C

DEC $4D
AND #$0F
CMP #$0A
BNE CODE_CC4C

JSR QueueSFX_Quack_D52C                                     ;quack attack

CODE_CC4C:
JMP CODE_CE4D

DuckEntity_StartFalling_CC4F:
JSR SpawnScoreSprite_CFF9                                   ;show score

JSR QueueSFX_DuckFall_D4F2                                  ;sound effect

LDA CurrentEntity_XPos
LDX #$48                                                    ;left boundary
CMP #$48                                                    ;check where it is horizontally
BCC CODE_CC64                                               ;

LDX #$A0                                                    ;right boundary
CMP #$A0
BCS CODE_CC64
TAX

CODE_CC64:
STX Entity_Variables_XPos+(Entity_Variables_Size*2)         ;the dog will show up where the duck fell, or close to

LDA #$01
STA $4B

LDX #$10
STX $48

LDA DATA_E64B,X
STA $49
STA $4A

LDX #<DATA_E718
LDY #>DATA_E718
BNE CODE_CC37

DuckEntity_Falling_CC7C:
LDA CurrentEntity_YPos                                      ;check if low enough on screen
CMP #$B0
BCS CODE_CC85                                               ;if that is the case, it landed somewhere on the ground.
JMP CODE_CBC3

CODE_CC85:
JSR QueueSFX_GroundThud_D534                                ;hit ground sound

INC CurrentEntity_State                                     ;
RTS                                                         ;

CODE_CC8B:
LDA #$00
STA CurrentEntity_State

LDX CurrentEntity_OAMStartPoint
JMP ClearEntityOAM_D042

HandleDogEntity_CC94:
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*2)   ;check if dog exist
BEQ RETURN_CCA8                                             ;dog no exist

LDA #2                                                      ;dog slot
STA CurrentEntity_Slot                                      ;

LDY #Entity_Variables_Size*2                                ;dog variables
JSR GetCurrentEntityVariables_C5F7                          ;

JSR ExecuteDogEntityCode_CCA9                               ;execute code
JSR UpdateEntityVariables_C607                              ;store variables

RETURN_CCA8:
RTS                                                         ;bow and quit

;dog state pointers
ExecuteDogEntityCode_CCA9:
LDA CurrentEntity_State
JSR ExecutePointers_C35E

dw DoNothing_C5E0
dw DogEntity_Init_CCD4
dw CODE_CD16
dw CODE_CD1D
dw CODE_CD3C
dw CODE_CE4D
dw CODE_CD4C
dw CODE_CE4D
dw CODE_CD52
dw CODE_CD5C
dw CODE_CD6C ;laugh?
dw CODE_CE4D
dw CODE_CD80 ;got ducks
dw CODE_CE4D
dw SpawnDucks_CD9A ;re-enable ducks
dw CODE_CE18
dw CODE_CE2E ;game over laugh
dw CODE_CE4D
dw CODE_CE40

DogEntity_Init_CCD4:
LDX #$1F                                                    ;

CODE_CCD6:
LDA DogEntity_InitialVariables_E76E,X                       ;
STA CurrentEntity_Variables,X                               ;
DEX                                                         ;
BPL CODE_CCD6                                               ;

LDA #$03                                                    ;that's right, the dog itself gives you those bullets, apparently.
STA ShotCount                                               ;

LDA #$01
STA ShotCounterUpdateFlag                                   ;draw said bullets
STA HitCounterUpdateFlag                                    ;draw hit counter

LDA #LoadPalette_DuckGame
STA PaletteToLoad                                           ;

LDA GameplayMode                                            ;check if we game overed
CMP #GameplayMode_GameOver                                  ;
BNE CODE_CCF7                                               ;

LDA #$10                                                    ;game over related..
STA CurrentEntity_State
RTS

CODE_CCF7:
LDA CurrentRound
LDY #$18
LDX #$00
CMP #$01                                                    ;check if at round 1 (just started the game)
BEQ CODE_CD05                                               ;

LDY #$50                                                    ;spawn closer to the center
LDX #$18

CODE_CD05:
STY CurrentEntity_XPos
STX $44

LDA #$01
STA $9D
JSR WaitForNMI_C5D9
JSR SoundEngine_F56C

INC CurrentEntity_State                                     ;
RTS                                                         ;

CODE_CD16:
LDX #<DATA_E78E
LDY #>DATA_E78E
JMP CODE_CD44

CODE_CD1D:
LDA CurrentEntity_Image
CMP #$0C
BNE CODE_CD2E

INC $44
LDA $44
CMP #$20
BCC CODE_CD2E

INC CurrentEntity_State

RETURN_CD2D:
RTS

CODE_CD2E:
JSR CODE_CE4D

LDA CurrentEntity_XPos
CMP $45
BNE RETURN_CD2D

LDA #$06
STA CurrentEntity_State
RTS

CODE_CD3C:
LDA #$00
STA $44

LDX #<DATA_E798
LDY #>DATA_E798

CODE_CD44:
JSR CODE_CF0B

INC CurrentEntity_State
JMP CODE_CE4D

CODE_CD4C:
LDX #<DATA_E7A7
LDY #>DATA_E7A7
BNE CODE_CD44

CODE_CD52:
LDA #$04
STA $9E

LDX #<DATA_E7AC
LDY #>DATA_E7AC
BNE CODE_CD44

CODE_CD5C:
DEC $9E
LDA $9E
BNE CODE_CD69

LDA #$10
STA $9E

JSR QueueSFX_Bark_D528

CODE_CD69:
JMP CODE_CE4D

CODE_CD6C:
LDA Timer_2B
BNE RETURN_CD2D                                             ;timer...

JSR QueueSFX_DogLaugh_D4FA                                  ;hahaha! you suck!

LDA #$80                                                    ;show up at the center of the screen
STA CurrentEntity_XPos
JSR CODE_CF39

LDX #<DATA_E846
LDY #>DATA_E846
BNE CODE_CD44

CODE_CD80:
LDA Timer_2B
BNE RETURN_CD2D

JSR QueueSFX_GotDuckJingle_D511                                               ;happy jingle for getting ducks
JSR CODE_CF39

LDX #<DATA_E894
LDY #>DATA_E894

LDA CurrentShotDuckCount                                    ;check if shot one duck
CMP #$01
BEQ CODE_CD44

LDX #<DATA_E8A3
LDY #>DATA_E8A3
BNE CODE_CD44                                               ;two ducks

SpawnDucks_CD9A:
LDA #$30
STA $A6

LDA #$01                                                    ;
STA ZapperFunctionsEnabledFlag                              ;can use zapper

LDA #$03                                                    ;restore shots
STA ShotCount                                               ;

LDA #$01                                                    ;
STA ShotCounterUpdateFlag                                   ;will draw them all

LDA #$00                                                    ;
STA CurrentShotDuckCount                                    ;clear shot duck counter

LDA #$02                                                    ;
LDY #$01                                                    ;second duck may be active depending on selected game
LDX SelectedGame                                            ;
BNE CODE_CDBA                                               ;

LDA #$01                                                    ;
LDY #$00                                                    ;second duck not active

CODE_CDBA:
STA $BC
STY Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)
STY Entity_Variables_CurrentState+(Entity_Variables_Size*1)

LDA #$01                                                    ;first duck is already enabled
STA Entity_Variables_ActiveFlag+(Entity_Variables_Size*0)   ;
STA Entity_Variables_CurrentState+(Entity_Variables_Size*0) ;

JSR RNG_C588                                                ;randomize
STA $0C                                                     ;

LDA #$03                                                    ;
LDY CurrentRound                                            ;check if in round 1
CPY #$01                                                    ;
BEQ CODE_CDD9                                               ;will have a smaller range of duck configurations on first round only.

LDA #$07                                                    ;can release ducks at any possible configuration

CODE_CDD9:
AND $0C
STA CurrentDuckConfiguration

LDY #$00
STY $99
TAX
LDA DuckColorConfigurations_E766,X
STA $0C
AND #$F0                                                    ;
LSR A                                                       ;
LDY #$00                                                    ;first duck's palette
JSR LoadDuckPalette_CF44

LDA $0C                                                     ;
AND #$0F                                                    ;
ASL A                                                       ;times 8
ASL A                                                       ;
ASL A                                                       ;second duck's palette
JSR LoadDuckPalette_CF44

LDA #$3F                                                    ;VRAM offset
STA DuckPaletteStorage                                      ;

LDA #$10                                                    ;
STA DuckPaletteStorage+1                                    ;
STA DuckPaletteStorage+2                                    ;and size, 16 bytes

LDA #StripeImageWriteCommand_Stop                           ;stop command
STA DuckPaletteStorage+19                                   ;

LDA #$01                                                    ;
STA DuckPaletteLoadFlag                                     ;Paint Me! This is another reference to Color a Dinosaur. Because that's what it was called in the prototype.

LDA #$12
STA CurrentEntity_State

LDA #$00
STA $42
RTS

CODE_CE18:
LDA TargetEntityCount
CMP #$0A
BNE CODE_CE29

LDA #$00
STA RoundEndState

LDA #GameplayMode_RoundEnd_DuckGame
STA GameplayMode
JMP ClearOAM_C23B

CODE_CE29:
LDA #$0E
STA CurrentEntity_State
RTS

CODE_CE2E:
LDA #$80                                                    ;spawn at the center
STA CurrentEntity_XPos                                      ;

LDA #$01
STA CurrentEntity_BehindBGFlag                              ;go behind background
JSR CODE_CF39

LDX #<DATA_E8B2
LDY #>DATA_E8B2
JMP CODE_CD44

CODE_CE40:
LDA $42
BNE CODE_CE4D

LDA #$00
STA CurrentEntity_State

LDX CurrentEntity_OAMStartPoint
JMP ClearEntityOAM_D042

;a drawing routine of sorts
CODE_CE4D:
LDA $42
BEQ CODE_CE56

DEC $42
JMP SpriteDrawRoutine_D01B

CODE_CE56:
LDY $43
LDA ($40),Y
CMP #$FA
BCC CODE_CE72

INC $43
SEC
SBC #$FA
JSR ExecutePointers_C35E

dw DoNothing_C5E0                                           ;unused
dw DoNothing_C5E0                                           ;unused
dw CODE_CF07
dw CODE_CF00
dw CODE_CEF9
dw CODE_CEED

CODE_CE72:
LDY $43
LDX CurrentEntity_Slot                                      ;check if we're drawing a dog
CPX #$02                                                    ;
BNE CODE_CEE6                                               ;

LDX CurrentEntity_State
CPX #$09
BEQ CODE_CEAD
CPX #$0B
BEQ CODE_CE8C
CPX #$0D
BEQ CODE_CE8C
CPX #$11
BNE CODE_CED4

CODE_CE8C:
LDX $46
BEQ CODE_CE9C

DEC $46

LDA $47
CLC
ADC CurrentEntity_YPos
STA CurrentEntity_YPos
JMP SpriteDrawRoutine_D01B

CODE_CE9C:
STA $46
INY
LDA ($40),Y
BPL CODE_CEA8
AND #$7F
JSR InvertValue_D12B

CODE_CEA8:
STA $47
JMP CODE_CEC9

CODE_CEAD:
TAX
BPL CODE_CEB5
AND #$7F
JSR InvertValue_D12B

CODE_CEB5:
CLC
ADC CurrentEntity_YPos
STA CurrentEntity_YPos

INY
LDA ($40),Y
LDX CurrentEntity_HorzDir
BEQ CODE_CEC4

JSR InvertValue_D12B

CODE_CEC4:
CLC
ADC CurrentEntity_XPos
STA CurrentEntity_XPos

CODE_CEC9:
INY
LDA ($40),Y
STA CurrentEntity_Image
INY
STY $43
JMP SpriteDrawRoutine_D01B

CODE_CED4:
CPX #$03                                                    ;????
BNE CODE_CEE6

LDA #$02
LDX CurrentEntity_HorzDir
BEQ CODE_CEE1

JSR InvertValue_D12B

CODE_CEE1:
CLC
ADC CurrentEntity_XPos
STA CurrentEntity_XPos

CODE_CEE6:
LDA ($40),Y
STA $42
JMP CODE_CEC9

CODE_CEED:
LDY $43
LDA ($40),Y
STA CurrentEntity_State

INY
LDA ($40),Y
STA $42
RTS


CODE_CEF9:
LDA #$00

CODE_CEFB:
STA $43
JMP CODE_CE56

CODE_CF00:
LDA #$01
STA CurrentEntity_BehindBGFlag
JMP CODE_CE56

CODE_CF07:
LDA #$1E
BNE CODE_CEFB

CODE_CF0B:
STX $40
STY $41

LDA #$00
STA $43
STA $42
RTS

;keep ducks in bounds?
CODE_CF16:
LDA CurrentEntity_YPos
CMP #$08
BCC CODE_CF2A
CMP #$90
BCS CODE_CF33

LDA CurrentEntity_XPos
CMP #$0C
BCC CODE_CF2A
CMP #$F4
BCC CODE_CF36

CODE_CF2A:
LDA #$08
STA CurrentEntity_State

LDA #$00
STA $42
RTS

CODE_CF33:
JMP CODE_CAD7

CODE_CF36:
JMP CODE_CAA5

CODE_CF39:
LDA #$00
STA $46
STA $47

LDA #$AC
STA CurrentEntity_YPos
RTS

;input A - palette offset
LoadDuckPalette_CF44:
TAX                                                         ;
LDA #$08                                                    ;amount of colors to load
STA $0D                                                     ;

CODE_CF49:
LDA DuckPalettes_E724,X
STA DuckPaletteStorage+3,Y
INX
INY
DEC $0D
BNE CODE_CF49
RTS

;clay pigeon score
CODE_CF56:
LDA CurrentRound
LDY #$02
CMP #$06
BCC CODE_CF7F

LDY #$03
CMP #$11
BCC CODE_CF7F

LDY #$05
BNE CODE_CF7F

;duck score i think...
CODE_CF68:
LDX CurrentRound
LDA #$00
CPX #$06
BCC CODE_CF78

LDA #$03
CPX #$11
BCC CODE_CF78

LDA #$06                                                    ;higher score from round 11 onwards

CODE_CF78:
CLC
ADC $3F                                                     ;duck's color
TAX
LDY DuckScoreRewardIndexes_EBF1,X

CODE_CF7F:
STY CurrentEntity_ScoreReward                               ;will spawn this score sprite

LDA ScoreSpriteRewards_EBFA,Y
STA $06

LDA #$00
STA $04
STA $05
STA $07

LDA #$0F
JSR UpdateScore_C437                                        ;GIVE ME SCORE
JMP PutScoreIntoBuffer_C655

ShowFlyAwayMessage_CF96:
LDA CurrentShotDuckCount                                    ;if shot a duck, fly away message will not be displayed
BNE RETURN_CFB9

LDA #Message_FLY_AWAY                                       ;fly away
JSR DrawMessage_D464                                        ;

LDA #$30
STA Timer_2B

LDA #LoadPalette_FlyAwayBackgroundRestore                   ;
STA PaletteToLoad                                           ;

LDA #$01                                                    ;
STA DuckPaletteLoadFlag                                     ;

LDA #$36                                                    ;this will set back area color to a different color.
STA DuckPaletteStorage+3
STA DuckPaletteStorage+7
STA DuckPaletteStorage+11
STA DuckPaletteStorage+15

RETURN_CFB9:
RTS                                                         ;

HandleScoreSprites_CFBA:
LDX #$00                                                    ;first score sprite
LDY #$00                                                    ;
JSR CODE_CFC5

LDX #$04                                                    ;second score sprite
LDY #$08                                                    ;

CODE_CFC5:
LDA ScoreSprite_Timer,X                                     ;
BEQ RETURN_CFF8                                             ;

DEC ScoreSprite_Timer,X                                     ;check if timer just ran out
BNE CODE_CFD3                                               ;

LDA #$F4                                                    ;remove this score sprite
STA ScoreSprite_YPos,X                                      ;
STA ScoreSprite_XPos,X                                      ;

CODE_CFD3:
LDA ScoreSprite_YPos,X                                      ;
STA Score_OAM_Y,Y                                           ;
STA Score_OAM_Y+4,Y                                         ;

LDA ScoreSprite_Tile,X                                      ;
STA Score_OAM_Tile,Y                                        ;

LDA #$FF                                                    ;second tile is always 00
STA Score_OAM_Tile+4,Y                                      ;

LDA #$00                                                    ;
STA Score_OAM_Prop,Y                                        ;properties
STA Score_OAM_Prop+4,Y                                      ;

LDA ScoreSprite_XPos,X
STA Score_OAM_X,Y                                           ;
CLC                                                         ;
ADC #$08                                                    ;second tile is 8 pixels to the right
STA Score_OAM_X+4,Y                                         ;

RETURN_CFF8:
RTS                                                         ;

SpawnScoreSprite_CFF9:
LDX #$00                                                    ;
LDA ScoreSprite_Timer,X                                     ;check if the first score sprite is currently on-screen
BEQ CODE_D001                                               ;

LDX #$04                                                    ;spawn second one

CODE_D001:
LDA CurrentEntity_YPos                                      ;
SEC                                                         ;
SBC #$04                                                    ;
STA ScoreSprite_YPos,X                                      ;

LDA CurrentEntity_XPos                                      ;
SEC                                                         ;
SBC #$08                                                    ;
STA ScoreSprite_XPos,X                                      ;

LDA CurrentEntity_ScoreReward                               ;
CLC                                                         ;
ADC #$F7                                                    ;
STA ScoreSprite_Tile,X                                      ;

LDA #$30                                                    ;
STA ScoreSprite_Timer,X                                     ;
RTS                                                         ;

SpriteDrawRoutine_D01B:
LDA CurrentEntity_OAMStartPoint
STA CurrentOAMOffset

LDA CurrentEntity_Image                                     ;what sprite image to display
ASL A
TAY

Macro_SetWord POINTERS_DAC5, $12

LDA ($12),Y
STA $10                                                     ;get graphics pointer

INY
LDA ($12),Y
STA $11

LDY #$00
STY $1A

LOOP_D038:
LDY $1A
LDA ($10),Y
CMP #$00                                                    ;go through the subpointer until we hit the break command
BNE CODE_D050

LDX CurrentOAMOffset                                        ;will clear any unused tiles

ClearEntityOAM_D042:
LDA #$F4                                                    ;

LOOP_D044:
CPX CurrentEntity_OAMEndPoint                               ;
BEQ RETURN_D04F                                             ;
STA OAM_Y,X                                                 ;
INX                                                         ;
JMP LOOP_D044                                               ;

RETURN_D04F:
RTS                                                         ;

CODE_D050:
STA $13                                                     ;set up subpointer

INY
LDA ($10),Y
INY
STA $12                                                     ;subpointer

LDA ($10),Y
INY
CLC
ADC CurrentEntity_YPos
STA $B1                                                     ;entire image y-offset

LDA ($10),Y
INY
LDX CurrentEntity_HorzDir
BEQ CODE_D06D                                               ;check if horizontally flipped?

JSR InvertValue_D12B
SEC
SBC #$08

CODE_D06D:
CLC
ADC CurrentEntity_XPos
STA $B2

STY $1A

LDY #$00
LDA ($12),Y                                                 ;first byte is image's shape
LDX #$00
CMP #$00
BEQ CODE_D08C

LDX #$10
CMP #$01
BEQ CODE_D08C

LDX #$38
CMP #$02
BEQ CODE_D08C

LDX #$44

CODE_D08C:
STX $1B

LOOP_D08E:
INY

LOOP_D08F:
LDA ($12),Y                                                 ;
CMP #$FF                                                    ;end subpointer command
BEQ LOOP_D038                                               ;
CMP #$FE                                                    ;skip tile command?
BNE CODE_D0A0                                               ;

INC $1B                                                     ;skip this tile
INC $1B                                                     ;
JMP LOOP_D08E                                               ;

CODE_D0A0:
CMP #$FD                                                    ;check if we're setting a tile
BNE CODE_D0B9

INY                                                         ;set property
LDA ($12),Y                                                 ;
CLC                                                         ;note: gets added instead of doing logical or
ADC CurrentEntity_OAMProp                                   ;
STA CurrentOAMProperty                                      ;

LDA CurrentEntity_BehindBGFlag                              ;if the entity is behind BG, will enable background priority bit
BEQ LOOP_D08E                                               ;

LDA CurrentOAMProperty                                      ;
ORA #OAMProp_BGPriority                                     ;
STA CurrentOAMProperty                                      ;
JMP LOOP_D08E                                               ;

CODE_D0B9:
LDX $1B
LDA GeneralSpriteTileOffsets_DCC0,X
INC $1B
CLC
ADC $B1
JSR CODE_D10E

LDX CurrentOAMOffset
STA OAM_Y,X

INC CurrentOAMOffset

LDA ($12),Y
INY
LDX CurrentOAMOffset
STA OAM_Tile-1,X

INC CurrentOAMOffset

LDA CurrentOAMProperty
LDX CurrentEntity_HorzDir
BEQ CODE_D0E9
AND #$BF
STA $0C

LDA CurrentOAMProperty
AND #OAMProp_XFlip
EOR #OAMProp_XFlip
ORA $0C

CODE_D0E9:
LDX CurrentOAMOffset
STA OAM_Prop-2,X

INC CurrentOAMOffset
LDX $1B
LDA GeneralSpriteTileOffsets_DCC0,X
INC $1B

LDX CurrentEntity_HorzDir
BEQ CODE_D0FE
JSR InvertValue_D12B

CODE_D0FE:
CLC
ADC $B2
JSR CODE_D114

LDX CurrentOAMOffset
STA OAM_X-3,X
INC CurrentOAMOffset
JMP LOOP_D08F

CODE_D10E:
PHA
LDA CurrentEntity_YPos
JMP CODE_D117

CODE_D114:
PHA
LDA CurrentEntity_XPos

CODE_D117:
STA $0F
PLA                                                         ;
PHA                                                         ;
SEC
SBC $0F
BCS CODE_D123

JSR InvertValue_D12B

CODE_D123:
CMP #$30
PLA
BCC RETURN_D12A

LDA #$FF                                                    ;tile offscreen

RETURN_D12A:
RTS

;input A
InvertValue_D12B:
EOR #$FF                                                    ;
CLC                                                         ;
ADC #$01                                                    ;
RTS                                                         ;

HandleZapperInput_D131:
LDA Zapper_OutputBits                                       ;
AND #$10                                                    ;check if squeezed the trigger
CMP ZapperTriggerState                                      ;check if the trigger state is the same
BEQ CODE_D141                                               ;it same
STA ZapperTriggerState                                      ;update

CODE_D13C:
LDA #$00                                                    ;some counter.
STA ZapperCounter                                           ;

RETURN_D140:
RTS                                                         ;

CODE_D141:
INC ZapperCounter                                           ;increment this timer, counter, or whatever

LDY ZapperCounter                                           ;
CPY #$01                                                    ;
BNE RETURN_D140                                             ;it cannot be anything other than $01, because it gets cleared.
AND #$10                                                    ;check trigger state again, if it's released/fully in
BEQ CODE_D153                                               ;

LDA #$01                                                    ;half-pressed trigger state. there's a half A press joke in there somewhere.
STA ZapperPulledTriggerFlag                                 ;
BNE CODE_D13C                                               ;

CODE_D153:
LDA ZapperPulledTriggerFlag                                 ;
BEQ CODE_D13C                                               ;

LDY #$00                                                    ;
STY ZapperPulledTriggerFlag                                 ;clear this flag
INY                                                         ;
STY ZapperEnabledFlag                                       ;re-enable zapper
BNE CODE_D13C                                               ;

HandleZapperFunctions_D160:
LDA ZapperFunctionsEnabledFlag                              ;
BNE CODE_D169                                               ;

DisableZapper_D164:
LDA #$00                                                    ;
STA ZapperEnabledFlag                                       ;

RETURN_D168:
RTS                                                         ;

CODE_D169:
LDA ZapperEnabledFlag                                       ;if zapper functionality isn't enabled...
BEQ RETURN_D168                                             ;return

LDA ShotCount                                               ;check if out of shots
BEQ DisableZapper_D164                                      ;

DEC ShotCount                                               ;-1 shot

LDA #$01                                                    ;
STA ShotCounterUpdateFlag                                   ;update shot counter because we took one bullet away

LDA #$00                                                    ;
STA ZapperEnabledFlag                                       ;momentarily disable zapper

JSR QueueSFX_ZapperShot_D4E6                                ;the weird noise from your gun.

LDA #LoadPalette_Zapper1                                    ;
STA PaletteToLoad                                           ;
JSR ClearOAM_C23B                                           ;
JSR DisableRender_C5E1                                      ;

LDA TitleScreenFlag                                         ;check if at title screen
BEQ CODE_D19F                                               ;

JSR QueueSFX_Silence_D4E2                                   ;shut up, title screen!
JSR SoundEngine_F56C                                        ;handle sound while we're waiting for transition

JSR QueueSFX_ZapperShot_D4E6                                ;the game attempts to play the zapper shot sound after the title screen...?

LDA #$02                                                    ;shooting at the title screen
STA ZapperShotConfiguration

LDA #$FF                                                    ;
STA CameraPositionX                                         ;the camera is set... somewhere?
BNE CODE_D1CA

CODE_D19F:
LDA #LoadPalette_Zapper2                                    ;
STA PaletteToLoad                                           ;

LDA #$00                                                    ;targetting duck 1
STA ZapperShotConfiguration

LDY #Entity_Variables_Size*0
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*0)   ;duck real?
BEQ CODE_D1B5

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*0)
CMP #$03
BEQ CODE_D1C7                                               ;check if can be shot

CODE_D1B5:
LDA #$01                                                    ;targetting duck 2, duck 1 does not exist
STA ZapperShotConfiguration

LDY #Entity_Variables_Size*1                                ;
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)   ;how many different ways can I ask if the duck is real?
BEQ CODE_D1DD                                               ;

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*1) ;
CMP #$03
BNE CODE_D1DD                                               ;

CODE_D1C7:
JSR DrawZapperShapePrep_D292

CODE_D1CA:
JSR PrepareForNMIWait_C61E                                  ;

LOOP_D1CD:
LDA FrameFlag                                               ;wait for the NMI hit...
BNE CODE_D1F4                                               ;

LDA Zapper_OutputBits                                       ;
AND #$08                                                    ;I am guessing this checks if the gun is pointed at the screen
BNE LOOP_D1CD                                               ;

LDA ZapperShotConfiguration
JMP CODE_D28A

CODE_D1DD:
LDA SelectedGame                                            ;
CMP #$02                                                    ;
BNE CODE_D1E6                                               ;check if we're in duck game
JMP CODE_D266                                               ;

CODE_D1E6:
LDY #LoadPalette_DuckGame                                   ;
STY PaletteToLoad                                           ;

JSR WaitForNMI_C5D9                                         ;

LDY #$01                                                    ;
STY DuckPaletteLoadFlag                                     ;restore duck colors
JMP CODE_D26A

CODE_D1F4:
JSR PrepareForNMIWait_C61E                                  ;

LDA RenderMirror                                            ;
AND #$E7                                                    ;will disable sprites and background
STA RenderMirror                                            ;

LDA #$08                                                    ;enable background
LDY ZapperShotConfiguration                                 ;
CPY #$02                                                    ;check if shooting at title screen
BEQ CODE_D207

LDA #$10                                                    ;enable sprites, they should be white shapes

CODE_D207:
ORA RenderMirror
STA RenderMirror
STA RenderBits

JSR ClearOAM_C23B                                           ;remove all sprites

LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)   ;check the presence of the second duck
BEQ CODE_D222                                               ;

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*1) ;
CMP #$03                                                    ;
BNE CODE_D222                                               ;can it be targeted?

LDY #Entity_Variables_Size*1                                ;
JSR DrawZapperShapePrep_D292                                ;draw second duck's shape

CODE_D222:
LDA FrameFlag
BNE CODE_D270

LDA Zapper_OutputBits                                       ;check if detected light
AND #$08
BNE CODE_D222

LDA ZapperShotConfiguration
CMP #$02
BNE CODE_D246

LDA #$01                                                    ;

CODE_D235:
STA TitleScreenZapperShotState                              ;

LDA #LoadPalette_TitleScreen                                ;
STA PaletteToLoad                                           ;

LDA #$00
STA CameraPositionX                                         ;set camera where it belongs
JSR WaitForNMI_C5D9
JMP EnableRender_C5EE

CODE_D246:
INC CurrentHitCount                                         ;
INC CurrentShotDuckCount                                    ;shot a duck

LDX #$04                                                    ;shot state
LDA ZapperShotConfiguration
BNE CODE_D256                                               ;check if we hit first or second duck
STX Entity_Variables_CurrentState+(Entity_Variables_Size*0) ;
JMP CODE_D259

CODE_D256:
STX Entity_Variables_CurrentState+(Entity_Variables_Size*1) ;

CODE_D259:
LDA SelectedGame                                            ;check if in clay pigeon game
CMP #$02                                                    ;
BEQ CODE_D266                                               ;

LDY #$01                                                    ;Color a Duck. Not as cool as Color a Dinosaur. 
STY DuckPaletteLoadFlag                                     ;
INY                                                         ;LDY #LoadPalette_DuckGame
BNE CODE_D268                                               ;

CODE_D266:
LDY #LoadPalette_ClayShooting                               ;

CODE_D268:
STY PaletteToLoad                                           ;

CODE_D26A:
JSR ClearOAM_C23B                                           ;
JMP EnableRender_C5EE                                       ;

CODE_D270:
LDA ZapperShotConfiguration
BNE CODE_D28A

LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)   ;check for second duck... again?
BEQ CODE_D259

LDA Entity_Variables_CurrentState+(Entity_Variables_Size*1)
CMP #$03
BNE CODE_D259

LDA #$01
STA ZapperShotConfiguration
JSR PrepareForNMIWait_C61E
JMP CODE_D222

CODE_D28A:
CMP #$02                                                    ;check if we're shooting at the title screen
BNE CODE_D259

LDA #$02                                                    ;acts as select
BNE CODE_D235                                               ;

DrawZapperShapePrep_D292:
LDA Entity_Variables_YPos,Y
STA $B1

LDA Entity_Variables_XPos,Y
STA $B2

LDA $030A,Y
STA $0C
JMP DrawZapperShape_C67C

;note: only dog only shows up in duck games, but it's there in spirit at clay shooting. watching over you from the skies.
SetRequiredAmountOfHitsAndSummonDog_D2A4
LDY RequiredHitCountIndex                                   ;

CODE_D2A6:
LDA RequiredHitAmountPerRound_EC02,Y                        ;
CMP #$FF                                                    ;check if reached the end of this table
BNE CODE_D2B1                                               ;

LDY #$13                                                    ;hardcoded index (coulda just DEY honestly)
BNE CODE_D2A6                                               ;

CODE_D2B1:
INY                                                         ;
STY RequiredHitCountIndex                                   ;
STA HitCountToWin                                           ;set an amount of hits we need to beat this round

LDA #$00                                                    ;
STA TargetEntityCount                                       ;
STA CurrentHitCount                                         ;reset hit counter

LDA #$01                                                    ;enable dog
STA Entity_Variables_ActiveFlag+(Entity_Variables_Size*2)   ;
STA Entity_Variables_CurrentState+(Entity_Variables_Size*2) ;sniff sniff

LDA #$FF                                                    ;
STA $9C
RTS                                                         ;

;victory state
RoundEnd_D2C9:
LDA RoundEndState                                           ;
JSR ExecutePointers_C35E                                    ;

dw HitTableShuffleDelay_D2D6
dw ShuffleHitTable_D2DD
dw RoundBeatenAnimation_D33D
dw WaitAfterPerfect_D3C9

HitTableShuffleDelay_D2D6:
LDA #$10                                                    ;hit table shuffles every 16 frames
STA Timer_2B

INC RoundEndState
RTS

ShuffleHitTable_D2DD:
LDA Timer_2B
BNE RETURN_D32A                                             ;wait for the timer to end

LDY #$00
LDX #$01

LOOP_D2E5:
LDA HitTable,X
BEQ CODE_D2F7

LDA HitTable-1,X
BNE CODE_D2F7

INC HitTable-1,X
DEC HitTable,X

LDY #$01

CODE_D2F7:
INX
CPX #$0A
BNE LOOP_D2E5
DEY
BNE CODE_D309

LDA #$00                                                    ;reset round end state, will shuffle hit table again
STA RoundEndState
JSR QueueSFX_DuckGameStartTheme_D519
JMP DrawHitTable_C6CC

CODE_D309:
LDA CurrentHitCount                                         ;check if we hit less than a required amount
CMP HitCountToWin                                           ;
BCC CODE_D32B

JSR QueueSFX_BeatRoundTheme_D51D

LDA #$00                                                    ;
STA ShotCounterUpdateFlag                                   ;

LDA #$10
STA Timer_2B

INC RoundEndState                                           ;

LDA CurrentRound                                            ;display GOOD every 10 rounds
AND #$0F                                                    ;
BNE RETURN_D32A                                             ;

JSR ClearOAM_C23B                                           ;

LDA #Message_GOOD                                           ;
JMP DrawMessage_D464                                        ;

RETURN_D32A:
RTS                                                         ;

CODE_D32B:
JSR ClearOAM_C23B                                           ;

LDA #Message_GAME_OVER                                      ;inform the player that the game is, in fact, over
JSR DrawMessage_D464                                        ;

JSR QueueSFX_GameOverJingle_D544                            ;the game over noise before actual game over theme

LDX #GameplayMode_InitGameOver                              ;
LDA #$0C                                                    ;
JMP StartTimedGameplayModeChange_C62E                       ;

RoundBeatenAnimation_D33D:
LDA Timer_2B
BNE RETURN_D32A

LDA #$10                                                    ;
STA Timer_2B

INC ShotCounterUpdateFlag                                   ;animate shot counter
LDA ShotCounterUpdateFlag                                   ;
CMP #$0F                                                    ;
BNE CODE_D3AD                                               ;

LDA CurrentHitCount                                         ;check if got perfect score 
CMP #$0A                                                    ;
BNE CODE_D399                                               ;if not, moving on

JSR QueueSFX_PerfectJingle_D538                             ;hooray!

LDA CurrentRound                                            ;what round are we in?
LDX #$00                                                    ;
CMP #$11                                                    ;10 and less?
BCC CODE_D369                                               ;

INX                                                         ;
CMP #$16                                                    ;round 15 and less
BCC CODE_D369                                               ;

INX                                                         ;
CMP #$21                                                    ;round 20 and less
BCC CODE_D369                                               ;

INX                                                         ;the highest reward

CODE_D369:
LDA PerfectScore_TensThousands_EBE9,X                       ;
STA $05                                                     ;
STA $0C                                                     ;

LDA PerfectScore_Thousands_EBED,X                           ;
STA $06                                                     ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
STA $0D                                                     ;

LDA #$00                                                    ;
STA $04                                                     ;
STA $07                                                     ;

LDA #$0F                                                    ;
JSR UpdateScore_C437                                        ;
JSR PutScoreIntoBuffer_C655                                 ;

LDA #$03
STA RoundEndState

LDA #$A0
STA Timer_2B

JSR ClearOAM_C23B                                           ;

LDA #Message_PERFECT                                        ;
JMP DrawMessage_D464                                        ;

CODE_D399:
LDA #$00

CODE_D39B:
LDX #GameplayMode_InitDuckGame                              ;
LDY SelectedGame                                            ;
CPY #$02                                                    ;
BNE CODE_D3A5                                               ;

LDX #GameplayMode_InitClayShooting                          ;

CODE_D3A5:
JSR StartTimedGameplayModeChange_C62E                       ;

LDA #Message_None                                           ;remove the message
JMP DrawMessage_D464                                        ;

CODE_D3AD:
AND #$01
BNE CODE_D3B4

JMP DrawHitTable_C6CC

CODE_D3B4:
LDX #$09

LDA #BGTile_Duck+1
LDY SelectedGame
CPY #$02
BNE CODE_D3C0

LDA #BGTile_ClayPigeon+1

CODE_D3C0:
STA BGTileBuffer_Transfer+1,X
DEX
BPL CODE_D3C0
JMP CODE_C6E9

WaitAfterPerfect_D3C9:
LDA Timer_2B
BNE RETURN_D3D1                                             ;

LDA #$04
BNE CODE_D39B

RETURN_D3D1:
RTS                                                         ;

;Animate current duck/clay pigeon icon on the hit table. the icon will blink until target is shot
HandleHitTableIconAnimation_D3D2:
LDA DuckPaletteLoadFlag                                     ;currently loading palette for ducks...
BNE RETURN_D427                                             ;nothing happens.

INC HitTableIconBlinkTimer                                  ;

LDY #Entity_Variables_Size*0                                ;first entity
JSR CODE_D3DF

LDY #Entity_Variables_Size*1                                ;second entity

CODE_D3DF:
LDA Entity_Variables_ActiveFlag,Y                           ;
BEQ RETURN_D427                                             ;if duck/clay pigeon does not exist, quit

LDA Entity_Variables_CurrentState,Y
CMP #$03
BEQ CODE_D3FB                                               ;if flying, blink hit icon
CMP #$08
BNE RETURN_D427

LDX $030B,Y
LDA HitTable-1,X
BEQ CODE_D409

LDX #BGTile_ClayPigeon
BNE CODE_D40B

CODE_D3FB:
LDA HitTableIconBlinkTimer
AND #$0F
BNE RETURN_D427

LDX #$B7
LDA HitTableIconBlinkTimer
AND #$10
BEQ CODE_D413

CODE_D409:
LDX #BGTile_ClayPigeon+1

CODE_D40B:
LDA SelectedGame                                            ;check if we're in clay pigeon game
CMP #$02                                                    ;
BEQ CODE_D413                                               ;
INX                                                         ;offset tiles so that they are duck icons
INX                                                         ;

CODE_D413:
STX BGTileBuffer_Transfer+1

LDX #$11                                                    ;a single tile update
STX BGTileBuffer_Transfer

LDA $030B,Y
CLC
ADC #$4B
TAX
LDY #$23
JMP TransferBGWriteToBuffer_C71D

RETURN_D427:
RTS

;this code is used to blink the shot counter when all bullets have been used
HandleShotCounterBlinkAnimation_D428:
LDA ZapperFunctionsEnabledFlag                              ;
BEQ RETURN_D427                                             ;

LDA ShotCount                                               ;
BNE RETURN_D427                                             ;

LDA ShotCounterBlinkTimer                                   ;
AND #$07                                                    ;
BNE CODE_D461                                               ;

LDA DuckPaletteLoadFlag                                     ;if we're set to load duck colors, don't update shot counter image
BNE RETURN_D427

LDA ShotCounterBlinkTimer                                   ;
AND #$08                                                    ;every 8 frames alternate between showing shot counter and not showing it.
BEQ DrawSHOT_D446                                           ;

LDA #$B7                                                    ;this removes shot count
TAX                                                         ;
TAY                                                         ;
BNE CODE_D44C                                               ;

DrawSHOT_D446:
LDA #BGTile_SHOT                                            ;this plasters shot counter
LDX #BGTile_SHOT+1                                          ;
LDY #BGTile_SHOT+2                                          ;

CODE_D44C:
STA BGTileBuffer_Transfer+1                                 ;
STX BGTileBuffer_Transfer+2                                 ;
STY BGTileBuffer_Transfer+3                                 ;

LDA #$13                                                    ;3 tiles on a single row
STA BGTileBuffer_Transfer                                   ;

LDX #$63                                                    ;
LDY #$23                                                    ;$2363 is where shot counter's at
JSR TransferBGWriteToBuffer_C71D

CODE_D461:
INC ShotCounterBlinkTimer                                   ;
RTS                                                         ;

;update the screen with a message or something
DrawMessage_D464:
PHA                                                         ;
JSR WaitForNMI_C5D9                                         ;
JSR SoundEngine_F56C                                        ;make sure sound plays properly because we just stalled
PLA                                                         ;

CODE_D46C:
STA $1B                                                     ;
BEQ CODE_D4BC                                               ;if message box value is 0, remove it.
ASL A                                                       ;
TAX                                                         ;
LDA MessageTilemapPointers_EC3A-2,X                         ;
STA $14                                                     ;

LDA MessageTilemapPointers_EC3A-1,X                         ;
STA $15                                                     ;

LDY #$01                                                    ;
LDA ($14),Y                                                 ;
STA $0F                                                     ;amount of tiles to transfer

LDX #$00                                                    ;
INY                                                         ;

CODE_D485:
LDA ($14),Y                                                 ;
STA BGTileBuffer_Transfer,X                                 ;
INY                                                         ;
INX                                                         ;
DEC $0F                                                     ;
BNE CODE_D485                                               ;

LDA $1B                                                     ;check what message we just displayed.
CMP #$01                                                    ;is it ROUND?
BNE CODE_D4AB                                               ;

LDA CurrentRound                                            ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
LSR A                                                       ;
BEQ CODE_D4A1                                               ;
STA BGTileBuffer_Transfer+24                                ;will draw round number, tens

CODE_D4A1:
LDA CurrentRound                                            ;
AND #$0F                                                    ;
STA BGTileBuffer_Transfer+25                                ;round number, ones
JMP CODE_D4CF                                               ;

CODE_D4AB:
CMP #$05                                                    ;check if we're drawing "perfect!!"
BNE CODE_D4CF

LDA $0C                                                     ;score we're giving for that
STA BGTileBuffer_Transfer+33                                ;

LDA $0D                                                     ;
STA BGTileBuffer_Transfer+34                                ;
JMP CODE_D4CF                                               ;

CODE_D4BC:
LDX #68                                                     ;69 tiles (0 inclusive)
LDA #$24                                                    ;blank tile

CODE_D4C0:
STA BGTileBuffer_Transfer+1,X                               ;
DEX                                                         ;
BPL CODE_D4C0                                               ;

LDA #$6B                                                    ;6 rows with 11 tiles on each
STA BGTileBuffer_Transfer                                   ;

LDX #$CB                                                    ;
BNE CODE_D4D4                                               ;

CODE_D4CF:
LDY #$00                                                    ;
LDA ($14),Y                                                 ;VRAM location low byte
TAX                                                         ;

CODE_D4D4:
LDY #$20                                                    ;the message will always be in the range $2000-$20FF
JMP TransferBGWriteToBuffer_C71D                            ;

InitVariablesAndOAM_D4D9:
JSR ClearOAM_C23B
JSR ClearZeroPageVariablesLimited_C64B
JMP Clear0300Page_C637                                      ;honestly, could've just pasted the routine right after, seeing as it's only called once

QueueSFX_Silence_D4E2:
LDA #SFX_Queue1_Silence
BNE CODE_D500

QueueSFX_ZapperShot_D4E6:
LDA #SFX_Queue1_ZapperShot
BNE CODE_D500

QueueSFX_ClayPigeonHit_D4EA:
LDA #SFX_Queue1_ClayPigeonHit
BNE CODE_D500

QueueSFX_WingFlag_D4EE:
LDA #SFX_Queue1_WingFlap
BNE CODE_D500

QueueSFX_DuckFall_D4F2:
LDA #SFX_Queue1_DuckFall
BNE CODE_D500

QueueSFX_ClayPigeonFall_D4F6:
LDA #SFX_Queue1_ClayPigeonFall
BNE CODE_D500

QueueSFX_DogLaugh_D4FA:
LDA #SFX_Queue1_DogLaugh
BNE CODE_D500

CODE_D4FE:
LDA #%10000000

CODE_D500:
ORA Sound_SFX1Queue
STA Sound_SFX1Queue
RTS

QueueSFX_ClayShootingStartTheme_D505:
LDA #SFX_Queue2_ClayShootingStartTheme
BNE CODE_D523

QueueSFX_TitleTheme_D509:
LDA #SFX_Queue2_TitleTheme
BNE CODE_D523

;game over song
QueueSFX_GameOverTheme_D50D:
LDA #SFX_Queue2_GameOverTheme
BNE CODE_D523

;happy jingle when got a duck or two
QueueSFX_GotDuckJingle_D511:
LDA #SFX_Queue2_GotDuckJingle
BNE CODE_D523

QueueSFX_DuckGameStartTheme_D515:
LDA #SFX_Queue2_DuckGameStartTheme
BNE CODE_D523

;shuffle sfx
QueueSFX_DuckGameStartTheme_D519:
LDA #SFX_Queue2_HitTableShuffle
BNE CODE_D523

QueueSFX_BeatRoundTheme_D51D:
LDA #SFX_Queue2_BeatRoundTheme
BNE CODE_D523

QueueSFX_PauseJingle_D521:
LDA #SFX_Queue2_PauseJingle

CODE_D523:
ORA Sound_SFX2Queue
STA Sound_SFX2Queue
RTS

QueueSFX_Bark_D528:
LDA #SFX_Queue3_Bark
BNE CODE_D546

QueueSFX_Quack_D52C:
LDA #SFX_Queue3_Quack
BNE CODE_D546

LDA #%00000100                                              ;unused
BNE CODE_D546

QueueSFX_GroundThud_D534:
LDA #SFX_Queue3_GroundThud
BNE CODE_D546

QueueSFX_PerfectJingle_D538:
LDA #SFX_Queue3_PerfectJingle
BNE CODE_D546

QueueSFX_ClayPigeonThrow_D53C:
LDA #SFX_Queue3_ClayPigeonThrow
BNE CODE_D546

LDA #%01000000                                              ;unused
BNE CODE_D546

QueueSFX_GameOverJingle_D544:
LDA #SFX_Queue3_GameOverJingle

CODE_D546:
ORA Sound_SFX3Queue
STA Sound_SFX3Queue
RTS

LoadClayShooting_D54B:
JSR WaitForNMI_C5D9

DrawStripeImage Layout_ClayShooting_E26C

LDA #LoadPalette_ClayShooting
STA PaletteToLoad

LDA #$00
STA $9D

LDA #GameplayMode_InitClayShooting                          ;
STA GameplayMode                                            ;

JSR UpdateCameraPosition_C3C1
JSR EnableRender_C5EE
JMP QueueSFX_ClayShootingStartTheme_D505

InitClayShooting_D56A
JSR InitVariablesAndOAM_D4D9
JSR SetRequiredAmountOfHitsAndSummonDog_D2A4
JSR UpdateRequiredHits_C793
JSR UpdateRoundNumber_C7B3
JSR WaitForNMI_C5D9
JSR SoundEngine_F56C

LDA #$01                                                    ;draw
STA ShotCounterUpdateFlag

LDA #$03                                                    ;three bullets
STA ShotCount                                               ;

JSR HandleBulletVisualUpdate_C6F5                           ;gotta draw 'em all
JSR DrawHitTable_C6CC

LDA #$06
STA $9D

LDA #GameplayMode_ClayShootingIntro                         ;
STA GameplayMode                                            ;
RTS                                                         ;

;even MORE init!!
InitClayShootingPart2_D593:
LDA #$01                                                    ;
STA Entity_Variables_ActiveFlag+(Entity_Variables_Size*0)   ;enable pigeons made out of clay
STA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)   ;
STA Entity_Variables_CurrentState+(Entity_Variables_Size*0) ;
STA Entity_Variables_CurrentState+(Entity_Variables_Size*1) ;

LDA #$30

LDX CurrentRound
CPX #$10
BCS CODE_D5AD
DEX

LDA DATA_EC17,X

CODE_D5AD:
STA $A8                                                     ;I am guessing this is the speed of clay pigeons

LDA #$80
STA $A6

LDA #$01                                                    ;
STA ShotCounterUpdateFlag                                   ;you guessed it, will draw the bullets

LDA #$03                                                    ;
STA ShotCount                                               ;

LDA #$02
STA $BC

LDA #$00                                                    ;
STA ZapperEnabledFlag                                       ;
STA ZapperFunctionsEnabledFlag                              ;
STA CurrentShotDuckCount                                    ;sure...

JSR HandleBulletVisualUpdate_C6F5
JSR DrawHitTable_C6CC

LDA #GameplayMode_ClayShootingMain                          ;
STA GameplayMode                                            ;
RTS                                                         ;

CODE_D5D2:
JSR HandleZapperInput_D131
JSR HandleZapperFunctions_D160
JSR HandleClayPigeons_D5EE
JSR HandleBulletVisualUpdate_C6F5
JSR HandleHitTableIconAnimation_D3D2
JSR HandleHitTableVisualUpdate_C6C4
JSR HandleScoreSprites_CFBA
JSR HandleShotCounterBlinkAnimation_D428

NOP                                                         ;three NOPs again! very curious...
NOP                                                         ;
NOP                                                         ;
RTS                                                         ;


HandleClayPigeons_D5EE:
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*0)
BEQ CODE_D600

LDY #Entity_Variables_Size*0
STY CurrentEntity_Slot
JSR GetCurrentEntityVariables_C5F7
JSR ExecuteClayPigeonCode_D652
JSR UpdateEntityVariables_C607

CODE_D600:
LDA Entity_Variables_ActiveFlag+(Entity_Variables_Size*1)
BEQ CODE_D612

LDY #Entity_Variables_Size*1
INC CurrentEntity_Slot
JSR GetCurrentEntityVariables_C5F7
JSR ExecuteClayPigeonCode_D652
JSR UpdateEntityVariables_C607

CODE_D612:
LDA $BC
BNE CODE_D64B

LDX Entity_Variables_CurrentState+(Entity_Variables_Size*0)
LDY Entity_Variables_CurrentState+(Entity_Variables_Size*1)
CPX #$03
BEQ CODE_D627
CPY #$03
BEQ CODE_D627
JSR CODE_D4FE

CODE_D627:
CPX #$00
BNE RETURN_D651
CPY #$00
BNE RETURN_D651

JSR DrawSHOT_D446

LDA #$00
STA ZapperFunctionsEnabledFlag

LDA TargetEntityCount
CMP #$0A
BEQ CODE_D640

LDA #GameplayMode_StartClayShooting
BNE CODE_D646

CODE_D640:
LDA #$00                                                    ;
STA RoundEndState                                           ;

LDA #GameplayMode_RoundEnd_ClayShooting                     ;

CODE_D646:
STA GameplayMode
JMP ClearOAM_C23B

CODE_D64B:
LDA $A6
BEQ RETURN_D651

DEC $A6

RETURN_D651:
RTS


ExecuteClayPigeonCode_D652
LDA CurrentEntity_State
JSR ExecutePointers_C35E

dw DoNothing_C5E0
dw ClayPigeonEntity_Init_D669
dw CODE_D678
dw CODE_D6D5
dw CODE_D752
dw CODE_D78F
dw DoNothing_C5E0                                           ;unused
dw DoNothing_C5E0                                           ;unused
dw CODE_D7C5

ClayPigeonEntity_Init_D669:
LDX #$4F

LOOP_D66B:
LDA ClayPigeonEntity_InitialVariables_E8DA,X
STA CurrentEntity_Variables,X
DEX
BPL LOOP_D66B

LDA #$01
STA $A7
RTS

CODE_D678:
LDA $A6
BNE CODE_D6D4

LDA #$01                                                    ;
STA ZapperFunctionsEnabledFlag                              ;can't shoot yet

JSR RNG_C588
AND #$3F
TAY
INY
STY $A6

JSR RNG_C588
AND #$0F
CMP $9C
BNE CODE_D697
CLC
ADC #$01
AND #$0F

CODE_D697:
STA $9C
ASL A
STA $0C
ASL A
CLC
ADC $0C                                                     ;times 6
TAX
LDA DATA_E968,X
STA $44

LDA DATA_E968+1,X
STA $43

LDA DATA_E968+2,X
STA $4A

LDA DATA_E968+3,X
STA $42

LDA DATA_E968+4,X
STA $47

LDA DATA_E968+5,X
STA $4C

INC TargetEntityCount
LDA TargetEntityCount
STA $3B

DEC $BC

LDA #$FF
STA $5F

JSR QueueSFX_ClayPigeonThrow_D53C

LDA #$05
STA $58

INC CurrentEntity_State

CODE_D6D4:
RTS

CODE_D6D5:
LDA $58
BEQ CODE_D6E0

DEC $58
BNE CODE_D6E0

JSR QueueSFX_ClayPigeonFall_D4F6

CODE_D6E0:
LDA $4C
BMI CODE_D6EC

LDA $49
LSR A
LSR A
CMP #$07
BCC CODE_D6EE

CODE_D6EC:
LDA #$07

CODE_D6EE:
CMP $5F
BEQ CODE_D6FD
STA $5F
CLC
ADC $A8
TAX
LDA DATA_E9C8,X
STA $5E

CODE_D6FD:
JSR CODE_D81B

CODE_D700:
PHA
JSR CODE_D8AE
PLA
SEC
SBC #$01
BNE CODE_D700

LDA $4E
BMI CODE_D74D

LDA $49
CMP #$30
BCS CODE_D74D
JSR CODE_D82F

LDA CurrentEntity_YPos
SEC
SBC #$40
STA CurrentEntity_YPos

LDA $49
LSR A
TAX
LDA DATA_E92A,X
STA CurrentEntity_Image
STX $0C
SEC
SBC #$18
STA $56

LDX CurrentRound
LDA #$00
CPX #$12
BCC CODE_D73E

LDA #$03
CPX #$23
BCC CODE_D73E

LDA #$06

CODE_D73E:
CLC
ADC $0C
TAX
LDA ClayPigeonZapperShapes_E944,X
STA CurrentEntity_ZapperShape

JSR CODE_D805
JMP SpriteDrawRoutine_D01B

CODE_D74D:
LDA #$08
STA CurrentEntity_State
RTS


CODE_D752:
LDX $3B
LDA #$01
STA HitTable-1,X
STA $AD

LDA $56
ASL A
ASL A
TAX
LDY #$00

CODE_D762:
LDA DATA_EAF2,X
STA $0050,Y
INX
INY
CPY #$04
BNE CODE_D762
LDA #$00
STA $57
LDX #$00

CODE_D774:
LDA CurrentEntity_YPos
JSR CODE_DAA5

LDA CurrentEntity_XPos
JSR CODE_DAB5
INX
INX
CPX #$20
BNE CODE_D774
JSR CODE_CF56

LDX CurrentEntity_OAMStartPoint
JSR ClearEntityOAM_D042

INC CurrentEntity_State
RTS

CODE_D78F:
JSR CODE_D805
JSR CODE_D93A

LDA CurrentEntity_OAMStartPoint
STA CurrentOAMOffset

LDY $50
INY
BEQ CODE_D7A7

LDX #$00
LDY #$0C
LDA $54
JSR CODE_D7CE

CODE_D7A7:
LDY $53
INY
BEQ CODE_D7B5

LDX #$10
LDY #$20
LDA $55
JSR CODE_D7CE

CODE_D7B5:
LDA $57
CMP #$02
BNE CODE_D7BE

JSR QueueSFX_ClayPigeonHit_D4EA

CODE_D7BE:
LDA #$00
STA $54
STA $55
RTS

CODE_D7C5:
LDA #$00
STA CurrentEntity_State

LDX CurrentEntity_OAMStartPoint
JMP ClearEntityOAM_D042

;clay pigeon particle related
CODE_D7CE:
STA $0D
STY $0C
LDY CurrentOAMOffset

CODE_D7D4:
LDA $0D
BNE CODE_D7DC

LDA $60,X
BNE CODE_D7E0

CODE_D7DC:
INX
JMP CODE_D7F9

CODE_D7E0:
STA OAM_Y,Y

INY
LDA DATA_EA30,X
STA OAM_Tile-1,Y

INY
INX
LDA DATA_EA30,X
STA OAM_Prop-2,Y

INY
LDA $60,X
STA OAM_X-3,Y
INY

CODE_D7F9:
INX
CPX $0C
BNE CODE_D7D4
STY CurrentOAMOffset
TYA
TAX
JMP ClearEntityOAM_D042


CODE_D805:
LDA $A7
BEQ RETURN_D81A

LDA #$00
STA $A7

LDX CurrentEntity_OAMStartPoint
JSR ClearEntityOAM_D042

LDA #$50
STA CurrentEntity_OAMStartPoint

LDA #$9F
STA CurrentEntity_OAMEndPoint

RETURN_D81A:
RTS

CODE_D81B:
LDX $5E
LDA DATA_EA00,X
PHA
INX
TXA
AND #$03
BNE CODE_D82B

DEX
DEX
DEX
DEX

CODE_D82B:
STX $5E
PLA
RTS

CODE_D82F:
LDA $4D
SEC
SBC #$00
TAY

LDA $4E
SBC #$10
TAX

LDA #$3B
JSR CODE_D8DB
STY $14
STX $15

LDA $4D
SEC
SBC #$00
TAY

LDA $4E
SBC #$10
TAX

LDA #$E7
JSR CODE_D8DB
STY $12
STX $13

LDA $48
SEC
SBC #$00
TAY

LDA $49
SBC #$18
TAX

LDA #$7F
JSR CODE_D8DB
CLC
TYA
ADC #$4F
TAY

TXA
ADC #$23
TAX
TYA
SEC
SBC $14
STA $93

TXA
SBC $15
CMP #$50
ROR A
STA $94

ROR $93
LDY $91
LDX $92
LDA #$6C
JSR CODE_D8DB
CLC
TYA
ADC $12
TAY

TXA
ADC $13
TAX
JSR CODE_D907
SBC #$00
EOR #$7F
STA CurrentEntity_YPos

LDA $43
SEC
SBC #$00
TAY

LDA $44
SBC #$09
TAX
JSR CODE_D907
EOR #$80
STA CurrentEntity_XPos
RTS

CODE_D8AE:
LDA #$0F

LOOP_D8B0:
SEC
SBC #$05
TAX
LDY #$00
LDA $40,X
BPL CODE_D8BB
DEY

CODE_D8BB:
CLC
ADC $41,X
STA $41,X
STA $15
TYA
LDY #$00
ADC $42,X
STA $42,X
BPL CODE_D8CC
DEY

CODE_D8CC:
ASL $15
ADC $43,X
STA $43,X
TYA
ADC $44,X
STA $44,X
TXA
BNE LOOP_D8B0
RTS


CODE_D8DB:
STY $10
STX $11
STA $95

LDA #$00
STA $96
STA $97

LDX #$08

LOOP_D8E9:
ROR $95
BCC CODE_D8FA
CLC
LDA $96
ADC $10
STA $96
LDA $97
ADC $11
STA $97

CODE_D8FA:
ASL A
ROR $97
ROR $96
DEX
BNE LOOP_D8E9

LDY $96
LDX $97
RTS

CODE_D907:
STY $96
STX $97

LDA #$00
STA $95

LDX #$07
CLC
LDA $96
ADC $93
TAY

LDA $97
ADC $94

LOOP_D91B:
BCC CODE_D921
STY $96
STA $97

CODE_D921:
ROL $95
ROL $96
ROL $97
SEC
LDA $96
SBC $93
TAY
LDA $97
SBC $94
DEX
BPL LOOP_D91B
BCS CODE_D937
DEX

CODE_D937:
LDA $95
RTS

CODE_D93A:
INC $57
LDY $50
CPY #$FF
BEQ CODE_D977

INC $50
LDA DATA_EA50,Y
STA $0C
STA $0F
CMP #$AA
BEQ CODE_D9B3
CMP #$BB
BEQ CODE_D9BA

LDA $57
CMP $51
BEQ CODE_D9C7

CODE_D959:
LDX #$00

LOOP_D95B:
STX $0D
LDA DATA_EB22,X
STA $0E
TXA
ASL A
TAX
LDA $60,X
BEQ CODE_D970
LDA $61,X
BEQ CODE_D970
JSR CODE_D9F6

CODE_D970:
LDX $0D
INX
CPX #$06
BNE LOOP_D95B

CODE_D977:
LDY $53
CPY #$FF
BEQ CODE_D9E2

INC $53

LDA DATA_EA8F,Y
STA $0C
CMP #$AA
BEQ CODE_D9CE
CMP #$BB
BEQ CODE_D9D5

LDA DATA_EAC2,Y
STA $0F

CODE_D991:
LDX #$00

LOOP_D993:
STX $0D
LDA DATA_EB28,X
STA $0E
TXA
ASL A
CLC
ADC #$10
TAX
LDA $60,X
BEQ CODE_D9AB

LDA $61,X
BEQ CODE_D9AB
JSR CODE_D9F6

CODE_D9AB:
LDX $0D
INX
CPX #$08
BNE LOOP_D993
RTS


CODE_D9B3:
LDA #$FF
STA $50
JMP CODE_D977

CODE_D9BA:
LDA #$01
STA $54

LDA #$00
STA $0C
STA $0F
JMP CODE_D959

CODE_D9C7:
LDA $52
STA $53
JMP CODE_D959

CODE_D9CE:
LDA #$FF
STA $53
JMP CODE_D9E2

CODE_D9D5:
LDA #$01
STA $55

LDA #$00
STA $0C
STA $0F
JMP CODE_D991

CODE_D9E2:
LDA $50
CMP #$FF
BNE RETURN_D9F5

LDA $53
CMP #$FF
BNE RETURN_D9F5

JSR SpawnScoreSprite_CFF9

LDA #$08
STA CurrentEntity_State

RETURN_D9F5:
RTS

CODE_D9F6:
LDA $57
AND #$03
STA $1B

LDA $0E
JSR ExecutePointers_C35E

dw DoNothing_C5E0                                           ;unused
dw CODE_DA21
dw CODE_DA2E
dw CODE_DA38
dw CODE_DA40
dw CODE_DA45
dw CODE_DA4D
dw CODE_DA57
dw DoNothing_C5E0                                           ;unused
dw CODE_DA61
dw CODE_DA6C
dw CODE_DA7D
dw CODE_DA75
dw CODE_DA85
dw CODE_DA90
dw CODE_DA9A

CODE_DA21:
LDA $0F
JSR CODE_DAB5

CODE_DA26:
LDA $0C
JSR InvertValue_D12B
JMP CODE_DAA5

CODE_DA2E:
LDA $1B
BEQ CODE_DA26

JSR CODE_DA26
JMP CODE_DA40

CODE_DA38:
LDA $0F
JSR InvertValue_D12B
JSR CODE_DAA5

CODE_DA40:
LDA $0C
JMP CODE_DAB5

CODE_DA45:
JSR CODE_DA40

CODE_DA48:
LDA $0F
JMP CODE_DAA5

CODE_DA4D:
LDA $1B
BEQ CODE_DA48
JSR CODE_DA5C
JMP CODE_DA40

CODE_DA57:
LDA $0F
JSR CODE_DAB5

CODE_DA5C:
LDA $0C
JMP CODE_DAA5

CODE_DA61:
LDA $0F
JSR InvertValue_D12B
JSR CODE_DAB5
JMP CODE_DA5C

CODE_DA6C:
LDA $1B
BEQ CODE_DA48

LDA $0C
JSR CODE_DAA5

CODE_DA75:
LDA $0C
JSR InvertValue_D12B
JMP CODE_DAB5

CODE_DA7D:
LDA $0F
JSR CODE_DAA5
JMP CODE_DA75

CODE_DA85:
JSR CODE_DA75

CODE_DA88:
LDA $0F
JSR InvertValue_D12B
JMP CODE_DAA5

CODE_DA90:
LDA $1B
BEQ CODE_DA88
JSR CODE_DA75
JMP CODE_DA26

CODE_DA9A:
LDA $0F
JSR InvertValue_D12B
JSR CODE_DAB5
JMP CODE_DA26

CODE_DAA5:
CLC
ADC $60,X
CMP #$07
BCC CODE_DAB0
CMP #$C7
BCC CODE_DAB2

CODE_DAB0:
LDA #$00

CODE_DAB2:
STA $60,X
RTS

CODE_DAB5:
CLC
ADC $61,X
CMP #$07
BCC CODE_DAC0
CMP #$F8
BCC CODE_DAC2

CODE_DAC0:
LDA #$00

CODE_DAC2:
STA $61,X
RTS

;pointers to sprite images
POINTERS_DAC5:
dw DATA_DB0F
dw DATA_DB14
dw DATA_DB1D
dw DATA_DB26
dw DATA_DB2F
dw DATA_DB38
dw DATA_DB41
dw DATA_DB4A
dw DATA_DB5B
dw DATA_DB86
dw DATA_DB6C
dw DATA_DB79
dw DATA_DB8F
dw DATA_DB9C
dw DATA_DBA9
dw DATA_DBB6
dw DATA_DBC3
dw DATA_DC3A
dw DATA_DBFA
dw DATA_DC07
dw DATA_DC0C
dw DATA_DC25
dw DATA_DBD0
dw DATA_DBE5
dw DATA_DC4B
dw DATA_DC50
dw DATA_DC61
dw DATA_DC72
dw DATA_DC7B
dw DATA_DC88
dw DATA_DC91
dw DATA_DC9A
dw DATA_DC9F
dw DATA_DCA8
dw DATA_DCAD
dw DATA_DCB6
dw UNUSED_DCBB                                                ;unused?

;format: pointer for the sprite image, followed by y-offset and x-offset. repeat until hitting break command $00
DATA_DB0F:
dwb DATA_DD0C
db $F0,$F0
db $00

DATA_DB14:
dwb DATA_DD24
db $F0,$F0

dwb DATA_DDE1
db $EF,$08
db $00

DATA_DB1D:
dwb DATA_DD3A
db $F0,$F0

dwb DATA_DDE1
db $EE,$08
db $00

DATA_DB26:
dwb DATA_DD53
db $F0,$F8

dwb DATA_DD65
db $F4,$F0
db $00

DATA_DB2F:
dwb DATA_DD6F
db $F0,$F8

dwb DATA_DD81
db $FC,$F0
db $00

DATA_DB38:
dwb DATA_DD8A
db $F0,$F8

dwb DATA_DD9C
db $04,$F0
db $00

DATA_DB41:
dwb DATA_DDC3
db $F0,$F0

dwb DATA_DDD7
db $FC,$10
db $00

DATA_DB4A:
dwb DATA_DDE6
db $F8,$F8

dwb DATA_DDEF
db $FF,$F0

dwb DATA_DDF4
db $F7,$08

dwb DATA_DDDC
db $FB,$10
db $00

DATA_DB5B:
dwb DATA_DDFA
db $F8,$F8

dwb DATA_DDEF
db $FE,$F0

dwb DATA_DDF4
db $F6,$08

dwb DATA_DDD7
db $FA,$10
db $00

DATA_DB6C:
dwb DATA_DE04
db $F0,$F4

dwb DATA_DE12
db $F4,$04

dwb DATA_DE17
db $08,$04
db $00

DATA_DB79:
dwb DATA_DE1C
db $F0,$FC

dwb DATA_DE2A
db $F4,$F4

dwb DATA_DE2F
db $08,$F4
db $00

DATA_DB86:
dwb DATA_DDA5
db $F0,$F0

dwb DATA_DDBD
db $08,$FA
db $00

DATA_DB8F:
dwb DATA_DE90
db $F8,$10

dwb DATA_DE34
db $F0,$E8

dwb DATA_DFED
db $E8,$EA
db $00

DATA_DB9C:
dwb DATA_DE90
db $F8,$10

dwb DATA_DE4B
db $F0,$E8

dwb DATA_DFF2
db $E0,$F0
db $00

DATA_DBA9:
dwb DATA_DE90
db $F8,$10

dwb DATA_DE62
db $F0,$E8

dwb DATA_DFED
db $E8,$EA
db $00

DATA_DBB6:
dwb DATA_DE90
db $F8,$10

dwb DATA_DE79
db $F0,$E8

dwb DATA_DFF2
db $E0,$F0
db $00

DATA_DBC3:
dwb DATA_DE9A
db $F8,$10

dwb DATA_DE34
db $F0,$E8

dwb DATA_DFED
db $E8,$EA
db $00

DATA_DBD0:
dwb DATA_DEA4
db $F0,$F8

dwb DATA_DEB8
db $10,$F8

dwb DATA_DEC0
db $ED,$F0

dwb DATA_DEC6
db $F0,$08

dwb DATA_DECB
db $FE,$F0
db $00

DATA_DBE5:
dwb DATA_DED3
db $F0,$F8

dwb DATA_DEE7
db $10,$F8

dwb DATA_DEEF
db $ED,$F0

dwb DATA_DEF5
db $F0,$08

dwb DATA_DEFA
db $FE,$F0
db $00

DATA_DBFA:
dwb DATA_DF02
db $E0,$00

dwb DATA_DF0B
db $F0,$10

dwb DATA_DF10
db $F0,$F0
db $00

DATA_DC07:
dwb DATA_DF29
db $F0,$F0
db $00

DATA_DC0C:
dwb DATA_DF3F
db $ED,$F0

dwb DATA_DF45
db $03,$F0

dwb DATA_DF4B
db $F0,$F8

dwb DATA_DF55
db $10,$F8

dwb DATA_DF5D
db $F0,$00

dwb DATA_DF80
db $04,$18
db $00

DATA_DC25:
dwb DATA_DF85
db $F0,$E0

dwb DATA_DF5D
db $F0,$00

dwb DATA_DFAB
db $04,$E0

dwb DATA_DFB0
db $04,$18

dwb DATA_DF55
db $10,$F8
db $00

DATA_DC3A:
dwb DATA_DFB5
db $E0,$00

dwb DATA_DFCB
db $00,$00

dwb DATA_DFDA
db $F0,$E8

dwb DATA_DFF8
db $E0,$F0
db $00

DATA_DC4B:
dwb DATA_DFFE
db $F8,$F4
db $00

DATA_DC50:
dwb DATA_E00F
db $FC,$F5

dwb DATA_E014
db $FC,$03

dwb DATA_E019
db $F9,$FC

dwb DATA_E01E
db $FE,$FC
db $00

DATA_DC61:
dwb DATA_E00F
db $FC,$F7

dwb DATA_E014
db $FC,$01

dwb DATA_E019
db $FA,$FC

dwb DATA_E01E
db $FD,$FC
db $00

DATA_DC72:
dwb DATA_E00F
db $FC,$F8

dwb DATA_E014
db $FC,$00
db $00

DATA_DC7B:
dwb DATA_E023
db $FC,$F7

dwb DATA_E023
db $FC,$F9

dwb DATA_E028
db $FC,$01
db $00

DATA_DC88:
dwb DATA_E023
db $FC,$F8

dwb DATA_E028
db $FC,$00
db $00

DATA_DC91:
dwb DATA_E023
db $FC,$F9

dwb DATA_E028
db $FC,$FF
db $00

DATA_DC9A:
dwb DATA_E02D
db $FC,$FC
db $00

DATA_DC9F:
dwb DATA_E032
db $FC,$FC

dwb DATA_E032
db $FC,$FB
db $00

DATA_DCA8:
dwb DATA_E032
db $FC,$FC
db $00

DATA_DCAD:
dwb DATA_E037
db $FC,$FC

dwb DATA_E037
db $FC,$FD
db $00

DATA_DCB6:
dwb DATA_E037
db $FC,$FC
db $00

UNUSED_DCBB:
dwb UNUSED_E03C
db $FC,$F8
db $00

;general sprite tile offsets for drawing a sprite image, depending on the image size
;y-offset, x-offset
;16x32
GeneralSpriteTileOffsets_DCC0:
db $00,$00
db $08,$00
db $10,$00
db $18,$00
db $00,$08
db $08,$08
db $10,$08
db $18,$08

;"duck size". not a 32x32
db $00,$00
db $00,$08
db $00,$10
db $00,$18
db $08,$00
db $08,$08
db $08,$10
db $08,$18
db $10,$00
db $10,$08
db $10,$10
db $10,$18
db $18,$00
db $18,$08
db $18,$10
db $18,$18
db $08,$20                                                  ;looks like these were added later...
db $10,$20
db $18,$20

db $18,$20                                                  ;unused (placeholder?)

;16x24
db $00,$00
db $08,$00
db $10,$00
db $00,$08
db $08,$08
db $10,$08

;size 3
db $00,$00
db $08,$00
db $00,$18
db $08,$18

;format:
;first byte is the image size, using general tile offset table. available sizes are 32x16, 32x32, 24x24 and 16x16
;then follows image data.
;$FD - set OAM tile property, using the immediate next byte
;$FE - skip tile
DATA_DD0C:
db $01

db $FD,OAMProp_Palette0
db $00,$01,$02

db $FD,OAMProp_Palette1
db $03

db $FD,OAMProp_Palette0
db $04,$05,$06,$07
db $08,$09,$0A,$FE

db $FD,OAMProp_Palette1
db $0B,$0C

db $FF

DATA_DD24:
db $01
db $FD,OAMProp_Palette0,$FE,$FE,$0D,$FE
db $FD,OAMProp_Palette0,$0F,$10,$11,$FE,$12,$13,$14,$15
db $FD,OAMProp_Palette1,$16,$17
db $FF

DATA_DD3A:
db $01
db $FD,OAMProp_Palette0,$FE,$FE,$18,$FE
db $FD,OAMProp_Palette0,$1A,$1B,$1C,$FE,$1D,$1E,$1F,$FE
db $FD,OAMProp_Palette1,$20,$21
db $FD,OAMProp_Palette0,$22,$FF

DATA_DD53:
db $00
db $FD,OAMProp_Palette0,$25,$26,$27
db $FD,OAMProp_Palette1,$28
db $FD,OAMProp_Palette0|OAMProp_XFlip,$25,$26,$27
db $FD,OAMProp_Palette1|OAMProp_XFlip,$28
db $FF

DATA_DD65:
db $03
db $FD,OAMProp_Palette0,$23,$24
db $FD,OAMProp_Palette0|OAMProp_XFlip,$23,$24
db $FF

DATA_DD6F:
db $00
db $FD,OAMProp_Palette0,$2A,$2B,$2C
db $FD,OAMProp_Palette1,$2D
db $FD,OAMProp_Palette0|OAMProp_XFlip,$2A,$2B,$2C
db $FD,OAMProp_Palette1|OAMProp_XFlip,$2D
db $FF

DATA_DD81:
db $03
db $FD,OAMProp_Palette0,$29,$FE
db $FD,OAMProp_Palette0|OAMProp_XFlip,$29
db $FF

DATA_DD8A:
db $00
db $FD,OAMProp_Palette0
db $2F,$30,$31
db $FD,OAMProp_Palette1
db $32
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $2F,$30,$31
db $FD,OAMProp_Palette1|OAMProp_XFlip
db $32
db $FF

DATA_DD9C:
db $03

db $FD,OAMProp_Palette0
db $2E,$FE
db $FD,OAMProp_Palette0|OAMProp_XFlip,$2E

db $FF

DATA_DDA5:
db $01

db $FD,OAMProp_Palette0
db $33,$34,$35

db $FD,OAMProp_Palette1
db $36

db $FD,OAMProp_Palette0
db $37,$38,$39

db $FD,OAMProp_Palette1
db $3A

db $FD,OAMProp_Palette0
db $FE,$3B,$3C,$3D

db $FF

DATA_DDBD:
db $01
db $FD,OAMProp_Palette1
db $3E,$3F
db $FF

DATA_DDC3:
db $01

db $FD,OAMProp_Palette0
db $FE,$40,$41,$FE
db $FE,$42,$43,$44
db $45,$46,$47,$48
db $FE

db $FD,OAMProp_Palette1
db $49
db $FF

DATA_DDD7:
db $00
db $FD,OAMProp_Palette1
db $4A
db $FF

DATA_DDDC:
db $00
db $FD,OAMProp_Palette1
db $F6
db $FF

DATA_DDE1:
db $00
db $FD,OAMProp_Palette1
db $03
db $FF

DATA_DDE6:
db $02
db $FD,OAMProp_Palette1
db $9F,$A0,$A1,$A2,$A3
db $FF

DATA_DDEF:
db $00
db $FD,OAMProp_Palette1
db $45
db $FF

DATA_DDF4:
db $00
db $FD,OAMProp_Palette0
db $44,$48
db $FF

DATA_DDFA:
db $02
db $FD,OAMProp_Palette1
db $A4,$A5,$A6,$A7
db $A8,$A9
db $FF

DATA_DE04:
db $00

db $FD,OAMProp_Palette1
db $AB,$AC,$FE,$FE,$0E,$19,$AA

db $FD,OAMProp_Palette0|OAMProp_YFlip
db $E5

db $FF

DATA_DE12:
db $00
db $FD,OAMProp_Palette1|OAMProp_YFlip
db $EA
db $FF

DATA_DE17:
db $00
db $FD,OAMProp_Palette1|OAMProp_YFlip
db $E9
db $FF

DATA_DE1C:
db $00

db $FD,OAMProp_Palette1|OAMProp_XFlip
db $0E,$19,$AA

db $FD,OAMProp_Palette0|OAMProp_XFlip|OAMProp_YFlip
db $E5

db $FD,OAMProp_Palette1|OAMProp_XFlip
db $AB,$AC

db $FF

DATA_DE2A:
db $00
db $FD,OAMProp_Palette1|OAMProp_XFlip|OAMProp_YFlip
db $EA
db $FF

DATA_DE2F:
db $00
db $FD,OAMProp_Palette1|OAMProp_XFlip|OAMProp_YFlip
db $E9
db $FF

DATA_DE34:
db $01
db $FD,OAMProp_Palette3
db $52,$53,$54,$FE
db $55,$56,$56,$57
db $5A,$5B,$5C,$5D
db $61,$62,$63,$64
db $58,$5E,$65
db $FF

DATA_DE4B:
db $01
db $FD,OAMProp_Palette3
db $52,$6A
db $54,$FE,$55,$6B,$56,$57,$6C,$6D
db $6E,$6F,$70,$71,$72,$73,$58,$5E
db $74
db $FF

DATA_DE62:
db $01
db $FD,OAMProp_Palette3
db $75,$53,$54
db $FE,$76,$56,$56,$57,$77,$78,$79
db $7A,$7B,$7C,$7D,$7E,$58,$5E,$7F
db $FF

DATA_DE79:
db $01
db $FD,OAMProp_Palette3
db $52,$6A,$54,$FE
db $76,$6B,$56,$57,$80,$81,$82,$83
db $85,$86,$87,$88,$58,$84,$89
db $FF

DATA_DE90:
db $02
db $FD,OAMProp_Palette2
db $59,$5F,$66,$FE,$60,$67
db $FF

DATA_DE9A:
db $02
db $FD,OAMProp_Palette2
db $4B,$4C,$4D
db $4E,$4F,$50
db $FF

DATA_DEA4:
db $00
db $FD,OAMProp_Palette2
db $CD,$CE,$CF
db $FD,OAMProp_Palette3
db $D0
db $FD,OAMProp_Palette2|OAMProp_XFlip
db $CD,$CE
db $FD,OAMProp_Palette2
db $D2
db $FD,OAMProp_Palette3
db $D3
db $FF

DATA_DEB8:
db $01
db $FD,OAMProp_Palette3
db $D1
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $D1
db $FF

DATA_DEC0:
db $03
db $FD,OAMProp_Palette2
db $C9,$CA
db $FF

DATA_DEC6:
db $00
db $FD,OAMProp_Palette2
db $D4
db $FF

DATA_DECB:
db $03
db $FD,OAMProp_Palette3
db $CB,$CC,$D5,$D6
db $FF

DATA_DED3:
db $00
db $FD,OAMProp_Palette2
db $D7,$D8,$D9
db $FD,OAMProp_Palette3
db $D0
db $FD,OAMProp_Palette2|OAMProp_XFlip
db $D7,$D8
db $FD,OAMProp_Palette2
db $DA
db $FD,OAMProp_Palette3
db $D3
db $FF

DATA_DEE7:
db $01
db $FD,OAMProp_Palette3
db $D1
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $D1
db $FF

DATA_DEEF:
db $03
db $FD,OAMProp_Palette2
db $C9,$CA
db $FF

DATA_DEF5:
db $00
db $FD,OAMProp_Palette2
db $D4
db $FF

DATA_DEFA:
db $03
db $FD,OAMProp_Palette3
db $CB,$CC,$D5,$D6
db $FF

DATA_DF02:
db $02
db $FD,OAMProp_Palette3
db $B3,$B4,$FE,$B9,$BA
db $FF

DATA_DF0B:
db $00
db $FD,OAMProp_Palette2
db $BF
db $FF

DATA_DF10:
db $01
db $FD,OAMProp_Palette3
db $FE,$AF,$B5
db $FD,OAMProp_Palette2
db $BB
db $FD,OAMProp_Palette3
db $FE,$B0,$B6,$BC,$AD
db $B1,$B7,$BD,$AE,$B2
db $B8,$BE,$C0
db $FF

DATA_DF29:
db $01
db $FD,OAMProp_Palette3
db $FE,$FE,$C1,$C2
db $C3,$C4,$C5,$C6
db $AD,$B1,$B7,$C7
db $AE,$B2,$B8,$BE
db $FE,$C8
db $FF

DATA_DF3F:
db $00
db $FD,OAMProp_Palette2
db $C9,$CA
db $FF

DATA_DF45:
db $00
db $FD,OAMProp_Palette3
db $DB,$DC
db $FF

DATA_DF4B:
db $00
db $FD,OAMProp_Palette2
db $DD,$DE
db $FD,OAMProp_Palette3
db $DF,$E0
db $FF

DATA_DF55:
db $01
db $FD,OAMProp_Palette3
db $D1
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $D1
db $FF

DATA_DF5D:
db $01
db $FD,OAMProp_Palette2|OAMProp_XFlip
db $DD
db $FD,OAMProp_Palette2
db $D4
db $FD,OAMProp_Palette0
db $E5
db $FD,OAMProp_Palette1
db $E9
db $FD,OAMProp_Palette2|OAMProp_XFlip
db $DE
db $FD,OAMProp_Palette3
db $E2
db $E6,$FE,$E1,$E3
db $FD,OAMProp_Palette1
db $E7,$FE
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $E0
db $FD,OAMProp_Palette1
db $E4,$E8
db $FF

DATA_DF80:
db $00
db $FD,OAMProp_Palette1
db $EA
db $FF

DATA_DF85:
db $01
db $FD,OAMProp_Palette1|OAMProp_XFlip
db $E9
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $E5
db $FD,OAMProp_Palette2|OAMProp_XFlip
db $D4
db $FD,OAMProp_Palette2
db $DD,$FE
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $E6,$E2
db $FD,OAMProp_Palette2
db $DE,$FE
db $FD,OAMProp_Palette1|OAMProp_XFlip
db $E7
db $FD,OAMProp_Palette3|OAMProp_XFlip
db $E3,$E1,$FE
db $FD,OAMProp_Palette1|OAMProp_XFlip
db $E8,$E4
db $FD,OAMProp_Palette3
db $E0
db $FF

DATA_DFAB:
db $00
db $FD,OAMProp_Palette1|OAMProp_XFlip
db $EA
db $FF

DATA_DFB0:
db $00
db $FD,OAMProp_Palette1
db $EA
db $FF

DATA_DFB5:
db $01
db $FD,OAMProp_Palette3
db $8A,$8B,$FE,$FE,$8C
db $FD,OAMProp_Palette2
db $8D,$8E,$FE,$8F,$90
db $91,$92,$93,$94,$95,$96
db $FF

DATA_DFCB:
db $01
db $FD,OAMProp_Palette3
db $97
db $FD,OAMProp_Palette2
db $98
db $FD,OAMProp_Palette3
db $99,$FE,$9A,$9B
db $9C
db $FF

DATA_DFDA:
db $01
db $FD,OAMProp_Palette3
db $52,$53,$54
db $FE,$55,$56,$56
db $FE,$5A,$5B,$5C
db $FE,$61,$62,$63
db $FF

DATA_DFED:
db $01
db $FD,OAMProp_Palette3
db $51
db $FF

DATA_DFF2:
db $02
db $FD,OAMProp_Palette3
db $68,$69
db $FF

DATA_DFF8:
db $00
db $FD,OAMProp_Palette3
db $9D,$9E
db $FF

DATA_DFFE:
db $01
db $FD,OAMProp_Palette0
db $F2,$F3
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $F2,$FE
db $FD,OAMProp_Palette0
db $F4,$F5
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $F4
db $FF

DATA_E00F:
db $00
db $FD,OAMProp_Palette0
db $EB
db $FF

DATA_E014:
db $00
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $EB
db $FF

DATA_E019:
db $00
db $FD,OAMProp_Palette0
db $F3
db $FF

DATA_E01E:
db $00
db $FD,OAMProp_Palette0
db $F5
db $FF

DATA_E023:
db $01
db $FD
db $00,$EC
db $FF

DATA_E028:
db $01
db $FD,OAMProp_Palette0|OAMProp_XFlip
db $EC
db $FF

DATA_E02D:
db $01
db $FD,OAMProp_Palette0
db $EF
db $FF

DATA_E032:
db $00
db $FD,OAMProp_Palette0
db $F0
db $FF

DATA_E037:
db $00
db $FD,OAMProp_Palette0
db $F1
db $FF

;unknown
UNUSED_E03C:
db $01
db $FD,OAMProp_Palette0
db $ED,$EE
db $FF

;Game A/B - Ducks
Layout_DuckGame_E042
dwb $2300
db 32|StripeImageCommand_Repeat
db $50

dwb $2320
db 32|StripeImageCommand_Repeat
db $50

dwb $2340
db 32|StripeImageCommand_Repeat 
db $50

dwb $2360
db 32|StripeImageCommand_Repeat 
db $50

dwb $2380
db 32|StripeImageCommand_Repeat 
db $50

dwb $23A0
db 32|StripeImageCommand_Repeat 
db $50

dwb $2240
db 32
db $3D,$3E,$3F,$40,$56,$57,$D1,$D2
db $3A,$3B,$3C,$3D,$3E,$3F,$40,$39
db $3A,$3B,$3C,$3D,$3E,$3F,$40,$D2
db $30,$31,$32,$33,$39,$3A,$3B,$3C

dwb $2260
db 32
db $95,$96,$97,$98,$58,$59,$5A,$5B
db $92,$93,$94,$95,$96,$97,$98,$91
db $92,$93,$94,$95,$96,$97,$98,$5B
db $34,$35,$36,$37,$91,$92,$93,$94

dwb $2280
db 32
db $46,$45,$48,$46,$46,$45,$46,$48
db $42,$43,$44,$46,$45,$48,$46,$41
db $42,$43,$44,$46,$45,$48,$46,$48
db $38,$45,$46,$46,$41,$42,$43,$44

dwb $22A0
db 32|StripeImageCommand_Repeat
db $46

;add some grass detail
dwb $22A4
db 1
db $48

dwb $22A8
db 1
db $47

dwb $22AA
db 1
db $48

dwb $22B0
db 1
db $47

dwb $22B2
db 1
db $48

dwb $22BA
db 1
db $48

dwb $22C0
db 32
db $4C,$4D,$4E,$4F,$49,$4A,$4B,$49
db $4C,$4D,$4E,$4F,$4D,$4E,$4F,$49
db $4A,$4B,$49,$4C,$4D,$4E,$4F,$49
db $49,$4A,$4B,$49,$4C,$4D,$4E,$4F

dwb $22E0
db 32
db $52,$50,$53,$50,$50,$51,$50,$50
db $52,$50,$53,$50,$50,$53,$50,$50
db $51,$50,$50,$52,$50,$53,$50,$50
db $50,$51,$50,$50,$52,$50,$53,$50

dwb $2300
db 1
db $52

dwb $2302
db 2
db $54,$52

dwb $230A
db 2
db $54,$52

dwb $230D
db 2
db $54,$52

dwb $230F                                                   ;could've combined this with the previous write to save 3 bytes (by drawing 4 at once)
db 2
db $54,$52

dwb $2315
db 2
db $54,$52

dwb $231E
db 2
db $54,$52

dwb $21F9
db 3
db $28,$29,$2A

dwb $2219
db 3
db $2B,$46,$2C

dwb $2238
db 4
db $2D,$2E,$46,$2F

dwb $2081
db 4
db $5C,$5D,$5E,$5F

dwb $20A1
db 5
db $60,$61,$46,$62,$63

dwb $20C1
db 5
db $64,$65,$66,$67,$68

dwb $20E3
db 5
db $69,$74,$75,$76,$6A

dwb $2103
db 5
db $6B,$6C,$7C,$6D,$6E

dwb $2123
db 5
db $6F,$70,$71,$72,$73

dwb $2140
db 7
db $74,$75,$76,$77,$78,$79,$7A

dwb $2160
db 9
db $7B,$7C,$7D,$7E,$7F,$80,$5D,$5E,$5F

dwb $2181
db 9
db $81,$82,$83,$84,$60,$61,$46,$62,$63

dwb $21A3
db 7
db $85,$86,$64,$87,$88,$67,$68

dwb $21C3
db 4
db $89,$8A,$8B,$8C

dwb $21E4
db 2
db $8D,$8E

dwb $2204
db 2
db $8F,$90

dwb $2224
db 2
db $8F,$55

dwb $2303
db 2
db $1B,$27

dwb $2323
db 27|StripeImageCommand_Repeat
db $E3

dwb $2322
db $06
db $E2,$E3,$E3,$E3,$E4,$E2

dwb $2336
db 2
db $E4,$E2

dwb $233E
db 1
db $E4

dwb $2348
db 22|StripeImageCommand_Repeat
db $B7

dwb $2342
db 6
db $E5,$D9,$D9,$D9,$E7,$E5

dwb $234C
db 10|StripeImageCommand_Repeat
db $D6

dwb $2356
db 2
db $E7,$E5

dwb $235E
db 1
db $E7

dwb $2358
db 6|StripeImageCommand_Repeat
db $00

dwb $2368
db 22|StripeImageCommand_Repeat
db $B7

dwb $2362
db 6
db $E5,$DA,$DB,$DC,$E7,$E5

dwb $2376
db 2
db $E7,$E5

dwb $237E
db 1
db $E7

dwb $2379
db 5
db $1C,$0C,$18,$1B,$0E

dwb $2383
db 27|StripeImageCommand_Repeat
db $E9

dwb $2382
db 1
db $E8

dwb $2386
db 2
db $EA,$E8

dwb $2396
db 2
db $EA,$E8

dwb $239E
db 1
db $EA

dwb $23C0
db 32|StripeImageCommand_Repeat
db $00

dwb $23CA
db 4
db $CC,$FF,$FF,$FF

dwb $23D2
db 4
db $CC,$FF,$FF,$FF

dwb $23E0
db 8|StripeImageCommand_Repeat
db $50

dwb $23E1
db 1
db $40

dwb $23E8
db 8|StripeImageCommand_Repeat
db $A5

dwb $23F0
db 8
db $AA,$AA,$AA,$FA,$FA,$BA,$FA,$BA

dwb $23F8
db 8|StripeImageCommand_Repeat
db $0A

dwb $2348
db 3
db $11,$12,$1D

db StripeImageWriteCommand_Stop

Layout_ClayShooting_E26C:
dwb $2220
db 32|StripeImageCommand_Repeat
db $B6

dwb $2240
db 32|StripeImageCommand_Repeat
db $B5

dwb $2260
db 32|StripeImageCommand_Repeat
db $B5

dwb $2280
db 32|StripeImageCommand_Repeat
db $B5

dwb $22A0
db 32|StripeImageCommand_Repeat
db $B5

dwb $22C0
db 32|StripeImageCommand_Repeat
db $B5

dwb $22E0
db 32|StripeImageCommand_Repeat
db $B5

dwb $2300
db 32|StripeImageCommand_Repeat
db $B5

dwb $2185
db 5
db $AC,$AD,$AE,$C1,$C2

dwb $21A4
db 7
db $AF,$B0,$B5,$B5,$C3,$C4,$C5

dwb $21B0
db 4
db $EC,$B4,$B8,$B9

dwb $21C2
db 11
db $B1,$B2,$B5,$B5,$B5,$B5,$B5,$C6
db $C7,$C8,$C9

dwb $21CE
db 15
db $CA,$B4,$B5,$B5,$B5,$ED,$BA,$BB
db $BC,$B3,$EB,$B0,$BE,$BB,$BF

dwb $21E2
db 25|StripeImageCommand_Repeat
db $B5

dwb $21E0
db 2
db $B3,$B4

dwb $21EB
db 4
db $CB,$C4,$CC,$CD

dwb $21F5
db 1
db $BD

dwb $21FB
db 5
db $BD,$CB,$C4,$C8,$BB

dwb $2200
db 29|StripeImageCommand_Repeat
db $B5

dwb $220D
db 3
db $CE,$CF,$D0

dwb $221D
db 3
db $C3,$C0,$C0

dwb $2260
db 6
db $99,$9A,$9B,$9C,$99,$9A

dwb $2280
db 6
db $9D,$9E,$9F,$A0,$9D,$9E

dwb $22A0
db 6
db $A1,$A2,$A3,$A4,$A1,$A2

dwb $22C0
db 6
db $A5,$A6,$A5,$A7,$A5,$A6

dwb $2247
db 4|StripeImageWriteCommand_DrawVert
db $9B,$9F,$A3,$A5

dwb $2248
db 4|StripeImageWriteCommand_DrawVert
db $9C,$A0,$A4,$A6

dwb $224A
db 2
db $A8,$A9

dwb $226A
db 2
db $AA,$AB

dwb $2258
db 5
db $A8,$A9,$A8,$A8,$A8

dwb $2277
db 6
db $A8,$AA,$AB,$AA,$AA,$AA

dwb $2274
db 2|StripeImageWriteCommand_DrawVert
db $A9,$AB

dwb $2297
db 1
db $AA

dwb $22BA
db 4
db $99,$9A,$9B,$9C

dwb $22DA
db 4
db $9D,$9E,$9F,$A0

dwb $22FA
db 4
db $A1,$A2,$A3,$A4

dwb $2320
db 32|StripeImageCommand_Repeat
db $C0

dwb $2340
db 32|StripeImageCommand_Repeat
db $C0

dwb $2360
db 32|StripeImageCommand_Repeat
db $C0

dwb $2380
db 32|StripeImageCommand_Repeat
db $C0

dwb $23A0
db 32|StripeImageCommand_Repeat
db $C0

dwb $2300
db 32|StripeImageCommand_Repeat
db $DD

dwb $2308
db 16|StripeImageCommand_Repeat
db $DF

dwb $2307
db 1
db $DE

dwb $2318
db 1
db $E0

dwb $2328
db 22|StripeImageCommand_Repeat
db $E3

dwb $2348
db 22|StripeImageCommand_Repeat
db $B7

dwb $2368
db 22|StripeImageCommand_Repeat
db $B7

dwb $2388
db 22|StripeImageCommand_Repeat
db $E9

dwb $2358
db 6|StripeImageCommand_Repeat
db $00

dwb $2322
db 6
db $E2,$E3,$E3,$E3,$E4,$E2

dwb $2342
db 6
db $E5,$D9,$D9,$D9,$E7,$E5

dwb $2362
db $06
db $E5,$DA,$DB,$DC,$E7,$E5

dwb $2382
db $06
db $E8,$E9,$E9,$E9,$EA,$E8

dwb $234C
db 10|StripeImageCommand_Repeat
db $D4

dwb $2303
db 2
db $1B,$27

dwb $2336
db 4|StripeImageWriteCommand_DrawVert
db $E4,$E7,$E7,$EA

dwb $2337
db 4|StripeImageWriteCommand_DrawVert
db $E2,$E5,$E5,$E8

dwb $233E
db 4|StripeImageWriteCommand_DrawVert
db $E4,$E7,$E7,$EA

dwb $2379
db 5
db $1C,$0C,$18,$1B,$0E

dwb $23C0
db 32|StripeImageCommand_Repeat
db $00

dwb $23C8
db 16|StripeImageCommand_Repeat
db $FF

dwb $23E0
db 8|StripeImageCommand_Repeat
db $50

dwb $23E8
db 8|StripeImageCommand_Repeat
db $55

dwb $23F0
db 8
db $AA,$AA,$AA,$FA,$FA,$BA,$FA,$BA

dwb $23F8
db 8|StripeImageCommand_Repeat
db $0A

dwb $2348
db 3
db $11,$12,$1D

db StripeImageWriteCommand_Stop

Layout_TitleScreen_E404:
dwb $2064
db 19|StripeImageCommand_Repeat 
db $B7

dwb $2084
db 19|StripeImageCommand_Repeat
db $B7

dwb $20A4
db 19|StripeImageCommand_Repeat
db $B7

dwb $20C4
db 19|StripeImageCommand_Repeat
db $B7

dwb $20E4
db 19|StripeImageCommand_Repeat
db $B7

dwb $2086
db 2|StripeImageWriteCommand_DrawVert
db $F5,$F4

dwb $2067
db 5|StripeImageWriteCommand_DrawVert
db $F0,$B7,$B7,$B7,$F1

dwb $2068
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $206B
db 3|StripeImageWriteCommand_DrawVert
db $24,$24,$F4

dwb $20E9
db 4
db $F3,$B7,$B7,$F1

dwb $206D
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $206E
db 4
db $F2,$B7,$B7,$F0

dwb $20EE
db 4
db $F3,$B7,$B7,$F1

dwb $2090
db 2
db $F5,$F6

dwb $20B0
db 2
db $F4,$F7

dwb $2072
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $2075
db 5|StripeImageWriteCommand_DrawVert
db $24,$F8,$B7,$F9,$24

dwb $2096
db 2|StripeImageWriteCommand_DrawVert
db $F1,$F0

dwb $2124
db 24|StripeImageCommand_Repeat
db $FB

dwb $2149
db 19|StripeImageCommand_Repeat
db $B7

dwb $2169
db 18|StripeImageCommand_Repeat 
db $B7

dwb $2189
db 18|StripeImageCommand_Repeat
db $B7

dwb $21A9
db 18|StripeImageCommand_Repeat
db $B7

dwb $21C9
db 18|StripeImageCommand_Repeat
db $B7

dwb $214B
db $85
db $24,$FA,$B7,$24,$24

dwb $214D
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $21CE
db 4
db $F3,$B7,$B7,$F1

dwb $2150
db 3|StripeImageWriteCommand_DrawVert
db $24,$24,$F4

dwb $2152
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $2155
db $85
db $24,$F0,$B7,$F9,$24

dwb $2157
db 5|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $2178
db 4|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $217B
db 4|StripeImageWriteCommand_DrawVert|StripeImageCommand_Repeat
db $24

dwb $2228
db 15
db $10,$0A,$16,$0E,$24,$0A,$24,$24
db $24,$01,$24,$0D,$1E,$0C,$14

dwb $2268
db 16
db $10,$0A,$16,$0E,$24,$0B,$24,$24
db $24,$02,$24,$0D,$1E,$0C,$14,$1C

dwb $22A8
db $16
db $10,$0A,$16,$0E,$24,$0C,$24,$24
db $24,$0C,$15,$0A,$22,$24,$1C,$11
db $18,$18,$1D,$12,$17,$10

dwb $2307
db 11
db $1D,$18,$19,$24,$1C,$0C,$18,$1B
db $0E,$24,$27

dwb $2345
db 22
db $25,$01,$09,$08,$04,$24,$17,$12
db $17,$1D,$0E,$17,$0D,$18,$24,$0C
db $18,$EE,$15,$1D,$0D,$EF

dwb $23C0
db 32|StripeImageCommand_Repeat
db $00

dwb $23E0
db 2|StripeImageCommand_Repeat
db $55

dwb $23E2
db 6|StripeImageCommand_Repeat
db $FF

dwb $23EA
db 6|StripeImageCommand_Repeat
db $FF

dwb $23F0
db 8|StripeImageCommand_Repeat
db $5A

dwb $23F8
db 8|StripeImageCommand_Repeat
db $00

dwb $23E1
db 1
db $55

dwb $23E9
db 1
db $55

db StripeImageWriteCommand_Stop

;palettes! i think
POINTERS_E532:
dw Palette_TitleScreen_E540
dw Palette_DuckGame_E558
dw Palette_ClayShooting_E57C
dw Palette_ZapperShot1_E5A0
dw Palette_ZapperShot2_E5A9
dw Palette_FlyAwayBackgroundRestore_E5C7
dw Palette_FlyAwayDog_E5B4

Palette_TitleScreen_E540:
dwb $3F00
db 16
db $0F,$2C,$27,$0F,$0F,$0F,$30,$30
db $0F,$0F,$2A,$2A,$0F,$0F,$27,$27

dwb $3F10
db 16|StripeImageCommand_Repeat
db $0F

db StripeImageWriteCommand_Stop

Palette_DuckGame_E558:
dwb $3F00
db 32
db $21,$07,$29,$0A,$21,$30,$29,$0A
db $21,$0F,$29,$18,$21,$0F,$30,$16

db $21,$30,$0A,$0F,$21,$30,$26,$0F
db $21,$30,$17,$0F,$21,$07,$17,$0F

db StripeImageWriteCommand_Stop

Palette_ClayShooting_E57C:
dwb $3F00
db 32
db $2C,$29,$19,$09,$2C,$07,$29,$1A
db $2C,$0F,$29,$10,$2C,$0F,$30,$16

db $2C,$30,$0F,$0F,$2C,$30,$16,$07
db $2C,$30,$17,$0F,$2C,$07,$17,$0F

db StripeImageWriteCommand_Stop

;shroud everything in dark. this is only really used for the title screen, to make zapper shot activate selected game
Palette_ZapperShot1_E5A0:
dwb $3F00
db 32|StripeImageCommand_Repeat
db $0F

dwb $3F03
db 1                                                        ;and set BG palette 0 color 4 as white. everything will be blank at this point, which means shooting anywhere on-screen will trigger zapper detection
db $30

db StripeImageWriteCommand_Stop

;keep everything in dark. used to detect sprites with zapper
Palette_ZapperShot2_E5A9:
dwb $3F00
db 32|StripeImageCommand_Repeat
db $0F

dwb $3F11
db 3                                                        ;and set sprite palette 0 colors 2-4 as white for an understandable reason (zapper detection)
db $30,$30,$30

db StripeImageWriteCommand_Stop

;used for when the dog pops up, only to laugh at you.
Palette_FlyAwayDog_E5B4:
dwb $3F10
db $10
db $21,$30,$0A,$0F,$21,$30,$26,$0F
db $21,$30,$17,$0F,$21,$07,$17,$0F

;used to restore background palette after it changes for "FLY AWAY"
Palette_FlyAwayBackgroundRestore_E5C7:
dwb $3F00
db $10
db $21,$07,$29,$0A,$21,$30,$29,$0A
db $21,$0F,$29,$18,$21,$0F,$30,$16

db StripeImageWriteCommand_Stop

;VRAM location of the score counter
ScorePointerVRAMLoc_E5DB:
dwb $2358

;unknown
db $06,$00

;first byte is unknown. the rest are score values
;by default all top scores are the same 
DefaultTopScores_E5DF:
db $00,$01,$20,$00
db $00,$01,$20,$00
db $00,$01,$20,$00

;initial duck values. (to be broken down in the same way as in Mario Bros. NES)
DuckEntity_InitialVariables_E5EB:
db $01,$01,$A8,$80,$00,$00,$00,$3F
db $01,$00,$03,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$03,$00,$00,$00,$00

POINTERS_E60B:
dw DATA_ED76
dw DATA_ED98
dw DATA_EDB0
dw DATA_EDBC
dw DATA_EDD4
dw DATA_EDFA
dw DATA_EE1A
dw DATA_EE34
dw DATA_EE4C
dw DATA_EE68
dw DATA_EE82
dw DATA_EE96
dw DATA_EEB8
dw DATA_EED0
dw DATA_EEF4
dw DATA_EF0E
dw DATA_EF2A
dw DATA_EF2D
dw DATA_EF30
dw DATA_EF33
dw DATA_EF36
dw DATA_EF39
dw DATA_EF3C
dw DATA_EF3F
dw DATA_EF42
dw DATA_EF45
dw DATA_EF48
dw DATA_EF4B
dw DATA_EF4E
dw DATA_EF51
dw DATA_EF54
dw DATA_EF57

;offsets for tables!!
DATA_E64B:
db DATA_E661-DATA_E661
db DATA_E664-DATA_E661
db DATA_E671-DATA_E661
db DATA_E678-DATA_E661
db DATA_E685-DATA_E661
db DATA_E688-DATA_E661
db DATA_E695-DATA_E661
db DATA_E69C-DATA_E661
db UNUSED_E6A9-DATA_E661
db DATA_E6AC-DATA_E661
db DATA_E6B9-DATA_E661
db DATA_E6C0-DATA_E661
db DATA_E6CD-DATA_E661
db DATA_E6D0-DATA_E661
db DATA_E6DD-DATA_E661
db DATA_E6E4-DATA_E661
db DATA_E6F1-DATA_E661
db UNUSED_E6F6-DATA_E661
db DATA_E6F9-DATA_E661
db DATA_E6FC-DATA_E661
db DATA_E6FF-DATA_E661
db UNUSED_E702-DATA_E661

DATA_E661:
db $FF,$00
db $AA

DATA_E664:
db $FF,$01
db $FF,$00
db $FF,$00
db $FF,$01
db $FF,$00
db $00,$00
db $AA

DATA_E671
db $FF,$01
db $FF,$01
db $00,$00
db $AA

DATA_E678
db $00,$01
db $FF,$01
db $00,$01
db $FF,$01
db $00,$01
db $00,$00
db $AA

DATA_E685:
db $00,$01
db $AA

DATA_E688:
db $00,$01
db $01,$01
db $00,$01
db $01,$01
db $00,$01
db $00,$00
db $AA

DATA_E695:
db $01,$01
db $01,$01
db $00,$00
db $AA

DATA_E69C:
db $01,$01
db $01,$00
db $01,$00
db $01,$01
db $01,$00
db $00,$00
db $AA

UNUSED_E6A9:
db $01,$00
db $AA

DATA_E6AC:
db $01,$FF
db $01,$00
db $01,$00
db $01,$FF
db $01,$00
db $00,$00
db $AA

DATA_E6B9:
db $01,$FF
db $01,$FF
db $00,$00
db $AA

DATA_E6C0:
db $00,$FF
db $01,$FF
db $00,$FF
db $01,$FF
db $00,$FF
db $00,$00
db $AA

DATA_E6CD:
db $00,$FF
db $AA

DATA_E6D0:
db $00,$FF
db $FF,$FF
db $00,$FF
db $FF,$FF
db $00,$FF
db $00,$00
db $AA

DATA_E6DD:
db $FF,$FF
db $FF,$FF
db $00,$00
db $AA

DATA_E6E4:
db $FF,$FF
db $FF,$00
db $FF,$00
db $FF,$FF
db $FF,$00
db $00,$00
db $AA

DATA_E6F1:
db $02,$01
db $02,$FF
db $AA

UNUSED_E6F6:
db $FF,$FE
db $AA

DATA_E6F9:
db $FE,$FF
db $AA

DATA_E6FC:
db $FE,$00
db $AA

DATA_E6FF
db $FE,$01
db $AA

UNUSED_E702:
db $FF,$02
db $AA

DATA_E705:
db $02,$00,$02,$01,$04,$02,$FE

DATA_E70C:
db $02,$03,$02,$04,$04,$05,$FE

DATA_E713:
db $14,$09
db $FF,$06,$01

DATA_E718:
db $04,$0A,$04,$0B,$FE

DATA_E71D:
db $02,$06,$02,$07,$04,$08,$FE

;duck palettes
DuckPalettes_E724:
;black/green duck
db $21,$30,$0A,$0F
db $21,$30,$26,$0F

;blue/purple duck
db $21,$30,$14,$02
db $21,$30,$26,$02

;red
db $21,$30,$0F,$05
db $21,$30,$26,$05

DATA_E73C:
db $04,$08

;unknown
db $0C

db $08,$0C,$10,$08,$0C,$10,$0C,$10
db $14,$10,$14,$18,$10,$14,$18
db $14,$18,$1C,$14,$1C,$1C,$18,$1C
db $1C,$1C,$1C,$20

DATA_E75A:
db $10,$14,$14,$18,$18,$1C,$1C,$20
db $20,$24,$24,$28

;Duck Color configurations. Low nibble is the first duck's palette index, high nibble is for the second one
DuckColorConfigurations_E766:
db $00                                                      ;both green
db $01                                                      ;first green, second purple
db $10                                                      ;first purple, second green
db $11                                                      ;both purple
db $12                                                      ;first purple, second red
db $20                                                      ;first red, second green
db $21                                                      ;first red, second purple
db $22                                                      ;both red

;some initial variables for the dog.
DogEntity_InitialVariables_E76E:
db $01,$01,$AC,$18,$00,$00,$00,$BF
db $00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$60,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00

DATA_E78E:
db $06,$0C,$06,$0D,$06,$0E,$06,$0F,$FE

;unknown
db $00

DATA_E798:
db $08,$0C,$08,$10,$08,$0C,$08,$10
db $08,$0C,$08,$10,$FF,$02,$01

DATA_E7A7:
db $10
db $11,$FF,$08,$01

DATA_E7AC:
db $90,$03,$12,$83
db $01,$12,$83,$01,$12,$83,$01,$12
db $83,$01,$12,$82,$01,$12,$82,$01
db $12,$82,$01,$12,$82,$01,$12,$81
db $01,$12,$82,$01,$12,$81,$01,$12
db $81,$01,$12,$81,$01,$12,$81,$01
db $12,$81,$01,$12,$81,$01,$12,$84
db $01,$13,$00,$01,$13,$81,$01,$13
db $00,$01,$13,$81,$01,$13,$00,$01
db $13,$00,$01,$13,$00,$01,$13,$81
db $01,$13,$FD,$00,$01,$13,$00,$01
db $13,$00,$01,$13,$00,$01,$13,$01
db $01,$13,$00,$01,$13,$01,$01,$13
db $00,$01,$13,$01,$01,$13,$01,$01
db $13,$01,$01,$13,$02,$01,$13,$02
db $01,$13,$02,$01,$13,$03,$01,$13
db $03,$01,$13,$04,$00,$13,$04,$00
db $13,$05,$00,$13,$05,$00,$13,$05
db $00,$13,$05,$00,$13,$05,$00,$13
db $05,$00,$13,$FF,$0E,$00

DATA_E846:
db $04,$81
db $16,$04,$81,$17,$04,$81,$16,$04
db $81,$17,$04,$81,$16,$04,$81,$17
db $04,$81,$16,$04,$81,$17,$04,$81
db $16,$04,$81,$17,$04,$80,$16,$04
db $80,$17,$04,$80,$16,$04,$80,$17
db $04,$80,$16,$04,$80,$17,$04,$80
db $16,$04,$80,$17,$04,$02,$16,$04
db $02,$17,$04,$02,$16,$04,$02,$17
db $04,$02,$16,$04,$02,$17,$04,$02
db $16,$FF,$0F,$20

DATA_E894:
db $14,$82,$14,$10
db $00,$14,$14,$02,$14,$14,$02,$14
db $FF,$0F,$20

DATA_E8A3:
db $14,$82,$15,$10,$00,$15,$14,$02
db $15,$14,$02,$15,$FF,$0F,$20

DATA_E8B2:
db $04,$81,$16,$04,$81,$17
db $04,$81,$16,$04,$81,$17,$04,$81
db $16,$04,$81,$17,$04,$81,$16,$04
db $81,$17,$04,$81,$16,$04,$81,$17
db $04,$80,$16,$04,$80,$17,$04,$80
db $16,$FC

ClayPigeonEntity_InitialVariables_E8DA:
db $01,$02,$00,$00,$00,$00,$00,$4F
db $00,$00,$00,$00,$00,$00,$00,$01
db $00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$02,$00
db $00,$00,$00,$00,$00,$00,$00,$00
db $00,$00,$00,$00,$00,$00,$00,$00
db $F9,$FE,$FB,$FE,$FF,$FE,$FF,$FA
db $FB,$FA,$F9,$FA,$00,$00,$00,$00
db $FA,$FD,$FB,$FD,$FB,$FD,$FC,$FD
db $FC,$FB,$FB,$FB,$FB,$FB,$FA,$FB

DATA_E92A:
db $18,$18,$19,$1A,$1A,$1B,$1C,$1D
db $1E,$1E,$1F,$1F,$20,$20,$21,$21
db $22,$22,$22,$22,$23,$23,$23,$23

;unknown
db $23,$23

;clay pigeon's zapper shape, depending on its distance
ClayPigeonZapperShapes_E944:
db $00,$00,$00,$01,$01,$01,$02,$02
db $02,$02,$03,$03,$03,$03,$04,$04
db $04,$04,$04,$05,$05,$05,$05,$05
db $05,$05,$05,$05,$05,$05

;unknown
db $05,$05,$05,$05,$05,$05

DATA_E968:
db $00,$00,$C0,$15,$1E,$2C
db $01,$00,$C0,$0A,$1E,$30
db $03,$00,$D0,$10,$1E,$25
db $04,$00,$C0,$00,$1C,$30
db $06,$00,$C0,$0F,$1C,$30
db $08,$00,$C0,$F6,$1C,$30
db $09,$00,$C0,$0C,$1C,$30
db $0A,$00,$C0,$F4,$1C,$30
db $0C,$00,$C0,$F4,$1E,$2D
db $0E,$00,$C0,$08,$1C,$30
db $0A,$00,$C0,$00,$1D,$30
db $11,$00,$B8,$EE,$20,$33
db $13,$00,$C8,$F0,$1C,$2A
db $04,$80,$C8,$F7,$1C,$2D
db $0A,$80,$C8,$0B,$1C,$2D
db $0F,$80,$C6,$F3,$21,$26

;note: the seventh byte of each entry is unused for some reason.
DATA_E9C8:
db $14,$14,$10,$10,$10,$08,$04,$00 
db $18,$14,$14,$10,$0C,$08,$04,$00
db $1C,$18,$18,$14,$10,$0C,$08,$04
db $20,$1C,$18,$14,$10,$0C,$08,$04
db $24,$20,$1C,$18,$14,$0C,$08,$04
db $28,$24,$20,$1C,$14,$10,$0C,$08
db $2C,$28,$24,$20,$18,$10,$0C,$08

DATA_EA00:
db $01,$01,$01,$01,$02,$01,$01,$01
db $02,$01,$02,$01,$02,$02,$02,$01
db $02,$02,$02,$02,$03,$02,$03,$02
db $03,$03,$03,$03,$04,$03,$04,$03
db $04,$04,$04,$04,$05,$05,$05,$05
db $06,$06,$06,$06,$07,$07,$07,$07

DATA_EA30:
db $ED,OAMProp_Palette0|OAMProp_XFlip
db $ED,OAMProp_Palette0|OAMProp_XFlip
db $ED,OAMProp_Palette0|OAMProp_XFlip|OAMProp_YFlip
db $ED,OAMProp_Palette0|OAMProp_YFlip
db $ED,OAMProp_Palette0
db $ED,OAMProp_Palette0

db $ED,OAMProp_Palette0                                     ;\unused?
db $ED,OAMProp_Palette0                                     ;/

db $EE,OAMProp_Palette0|OAMProp_XFlip
db $EE,OAMProp_Palette0|OAMProp_XFlip
db $EE,OAMProp_Palette0|OAMProp_XFlip
db $EE,OAMProp_Palette0|OAMProp_XFlip
db $EE,OAMProp_Palette0
db $EE,OAMProp_Palette0
db $EE,OAMProp_Palette0
db $EE,OAMProp_Palette0

DATA_EA50:
db $06,$05,$04,$03,$03,$02,$03,$03
db $02,$02,$02,$02,$02,$01,$01,$02
db $01,$01,$01,$01,$01,$01,$01,$00
db $01,$01,$01,$01,$00,$01,$01,$01
db $00,$01,$01,$00,$01,$00,$01,$00
db $BB,$01,$00,$01,$00,$BB,$01,$00
db $01,$00,$BB,$01,$00,$01,$BB,$00
db $01,$BB,$00,$00,$BB,$00,$AA

DATA_EA8F:
db $02
db $02,$01,$02,$02,$01,$01,$01,$01
db $01,$00,$01,$00,$01,$00,$01,$00
db $01,$BB,$00,$00,$01,$BB,$00,$00
db $01,$BB,$00,$00,$BB,$01,$00,$00
db $BB,$01,$00,$BB,$00,$00,$BB,$BB
db $BB,$BB,$BB,$BB,$BB,$BB,$BB,$BB
db $BB,$AA

DATA_EAC2:
db $01,$01,$00,$01,$01,$01,$00,$01
db $00,$01,$00,$00,$01,$00,$00,$00
db $00,$01

;unknown
db $00

;known
db $00,$00,$00

;unknown
db $00

;known
db $01,$00,$00

;unknown
db $00

;known
db $00,$00

;unknown
db $00

;known
db $01,$00,$00

;unknown
db $00

;known
db $00,$00

;unknown
db $00

;known
db $00,$00

;unknown
db $00,$00,$00,$00,$00,$00,$00,$00
db $00

DATA_EAF2:
db $00,$06,$00,$FF

;unknown
db $02,$05,$02,$FF


db $08,$04,$06,$FF,$0E,$04,$07,$FF
db $12,$05,$08,$FF,$14,$04,$0A,$FF
db $FF,$00,$00,$02,$FF,$00,$00,$04
db $FF,$00,$00,$06,$FF,$00,$00,$0A
db $FF,$00,$00,$10,$FF,$00,$00,$14

DATA_EB22:
db $02,$04,$06,$0A,$0C,$0E

DATA_EB28:
db $01,$03,$05,$07,$09,$0B,$0D,$0F

;this is used to draw different sprite shapes for zapper detection (typically rectangles)
ZapperShapeSpriteGFXPointers_EB30:
dw DATA_EB4A
dw DATA_EB5D
dw DATA_EB70
dw DATA_EB7D
dw DATA_EB86
dw DATA_EB8F
dw UNUSED_EB92                                              ;unused
dw UNUSED_EB92                                              ;unused
dw UNUSED_EB92                                              ;unused
dw DATA_EB95
dw DATA_EBB6
dw DATA_EBC9
dw UNUSED_EBDC                                              ;unused

;x-offset, y-offset
;$AA - terminator command
DATA_EB4A:
db $F4,$F4
db $F4,$FC
db $F4,$04
db $FC,$F4
db $FC,$FC
db $FC,$04
db $04,$F4
db $04,$FC
db $04,$04
db $AA

DATA_EB5D:
db $F7,$F5
db $F7,$FD
db $F7,$03
db $FF,$F5
db $FF,$FD
db $FF,$03
db $01,$F5
db $01,$FD
db $01,$03
db $AA

DATA_EB70:
db $F9,$F7
db $F9,$FF
db $F9,$01
db $FF,$F7
db $FF,$FF
db $FF,$01
db $AA

DATA_EB7D:
db $FA,$F8
db $FA,$00
db $FE,$F8
db $FE,$00
db $AA

DATA_EB86:
db $FA,$FA
db $FA,$FE
db $FE,$FA
db $FE,$FE
db $AA

DATA_EB8F:
db $FC,$FC
db $AA

;unknown. duplicate of the previous entry?
UNUSED_EB92:
db $FC,$FC
db $AA

DATA_EB95:
db $F0,$F0
db $F0,$F8
db $F0,$00
db $F0,$08
db $F8,$F0
db $F8,$F8
db $F8,$00
db $F8,$08
db $00,$F0
db $00,$F8
db $00,$00
db $00,$08
db $08,$F0
db $08,$F8
db $08,$00
db $08,$08
db $AA

DATA_EBB6:
db $F4,$F5,$F4,$FD,$F4,$03,$FC,$F5
db $FC,$FD,$FC,$03,$04,$F5,$04,$FD
db $04,$03,$AA

DATA_EBC9:
db $F5,$F7
db $F5,$FF
db $F5,$01
db $FD,$F7
db $FD,$FF
db $FD,$01
db $03,$F7
db $03,$FF
db $03,$01
db $AA

UNUSED_EBDC:
db $F6,$F8
db $F6,$00
db $FE,$F8
db $FE,$00
db $02,$F8
db $02,$00
db $AA

;How much score is awarded for getting perfect hit amount. This is tens thousands and hundreds thousands 
PerfectScore_TensThousands_EBE9:
db $01,$01,$02,$03

;same as above, but hundreds and thousands
PerfectScore_Thousands_EBED:
db $00,$50,$00,$00

;what kind of score reward to give, depending on the duck color and difficulty
DuckScoreRewardIndexes_EBF1:
db ScoreReward_500,ScoreReward_1000,ScoreReward_1500
db ScoreReward_800,ScoreReward_1600,ScoreReward_2400
db ScoreReward_1000,ScoreReward_2000,ScoreReward_3000

;score values from sprite score pop-ups.
ScoreSpriteRewards_EBFA:
db $05,$08,$10,$15,$16,$20,$24,$30

;amount of hits required to clear the stage
;$FF - infinitely repeat previous value
RequiredHitAmountPerRound_EC02:
db $06,$06,$06,$06,$06,$06,$06,$06                          ;rounds 1-8
db $06,$06,$07,$07,$08,$08,$09,$09                          ;rounds 9-16
db $09,$09,$09,$0A,$FF                                      ;rounds 17-20 and beyond

DATA_EC17:
db $00,$08,$10,$18,$18,$20,$20,$28

DATA_EC1F:
db $28,$01,$80,$00,$00,$AA,$01,$40
db $02,$10,$03,$08,$02,$10,$03,$08
db $02,$10,$03,$08,$02,$10,$03,$08
db $00,$00,$AA

;DATA POINTERS, POG!!!!!
;messages
MessageTilemapPointers_EC3A:
dw Message_ROUND_EC4A
dw Message_GO_EC70
dw Message_GO_Blank_EC82
dw Message_GOOD_EC8B
dw Message_PERFECT_ECB1
dw Message_FLY_AWAY_ECE6
dw Message_PAUSE_ED07
dw Message_GAME_OVER_ED3C

;format:
;first byte - VRAM location, low byte (the message's high byte is hardcoded to $20)
;second byte - amount of tile to load into buffer transfer
;the rest are buffer transfer tile data:
;first byte - amount of rows to draw and amount of tiles on each row. high nibble - rows, low nibble - tile amount
;the rest are tiles to draw
;then amount of rows and tiles on said rows, repeat until done.
Message_ROUND_EC4A:
db $CD
db @End-@Message

@Message
db $57
db $FF,$13,$13,$13,$13,$13,$1A
db $D8,"ROUND"-$37,$21
db $D8,$B7,$B7,$B7,$B7,$B7,$21
db $D8,$B7,$B7,$B7,$B7,$B7,$21
db $23,$FC,$FC,$FC,$FC,$FC,$FD

@End

Message_GO_EC70:
db $EE
db @End-@Message

@Message
db $35
db $B7,$B7,$B7,$B7,$B7
db $B7,"GO"-$37,$FE,$B7
db $B7,$B7,$B7,$B7,$B7

@End

;used for GO!! blinking
Message_GO_Blank_EC82:
db $EF
db @End-@Message-3

@Message
db $23
db $B7,$B7,$B7
db $B7,$B7,$B7                                              ;second row is unused for some reason, despite setting two rows with 3 tiles each. it doesn't seem to lead to any glitches at least.

@End

Message_GOOD_EC8B:
db $CD
db @End-@Message

@Message
db $57
db $FF,$13,$13,$13,$13,$13,$1A
db $D8,$B7,$B7,$B7,$B7,$B7,$21
db $D8,"GOOD"-$37,$FE,$21
db $D8,$B7,$B7,$B7,$B7,$B7,$21
db $23,$FC,$FC,$FC,$FC,$FC,$FD

@End

Message_PERFECT_ECB1:
db $CC
db @End-@Message

@Message
db $5A
db $FF,$13,$13,$13,$13,$13,$13,$13,$13,$1A
db $D8,"PERFECT"-$37,$FE,$21
db $D8,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$21
db $D8,$B7,$B7,$B7,$00,$00,$00,$B7,$B7,$21
db $23,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FD

@End

Message_FLY_AWAY_ECE6:
db $EC
db @End-@Message

@Message
db $3A
db $FF,$13,$13,$13,$13,$13,$13,$13,$13,$1A
db $D8,"FLY"-$37,$B7,"AWAY"-$37,$21
db $23,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FD

@End

;for some reason, the tilemap for the pause message is wider than its actual appearance, there are blank tiles on the right and left. was it wider before?
Message_PAUSE_ED07:
db $CC
db @End-@Message

@Message
db $5A
db $24,$FF,$13,$13,$13,$13,$13,$1A,$24,$24
db $24,$D8,$B7,$B7,$B7,$B7,$B7,$21,$24,$24
db $24,$D8,"PAUSE"-$37,$21,$24,$24
db $24,$D8,$B7,$B7,$B7,$B7,$B7,$21,$24,$24
db $24,$23,$FC,$FC,$FC,$FC,$FC,$FD,$24,$24

@End

Message_GAME_OVER_ED3C:
db $AB
db @End-@Message

@Message
db $5B
db $FF,$13,$13,$13,$13,$13,$13,$13,$13,$13,$1A
db $D8,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$21
db $D8,"GAME"-$37,$B7,"OVER"-$37,$21
db $D8,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$B7,$21
db $23,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FD

@End

DATA_ED76:
db $9C,$18,$14,$10,$01
db $10,$02,$20,$03,$08,$02,$08,$01
db $18,$00,$10,$0E,$08,$0D,$20,$0C
db $08,$0D,$08,$0C,$B0,$0B,$80,$03
db $40,$02,$FF,$08,$00

DATA_ED98:
db $88,$10,$12,$28,$0F,$08,$00,$10
db $01,$08,$02,$48,$03,$20,$04,$70
db $0C,$30,$0D,$30,$0E,$FF,$08,$00

DATA_EDB0:
db $90,$20,$12,$60,$0D,$90,$03,$78
db $02

;unknown
db $FF,$08,$00

DATA_EDBC:
db $78,$20,$13,$40,$0F,$10,$0E,$20
db $0D,$18,$0B,$20,$0A,$50,$06,$28
db $04,$38,$03,$78,$02,$FF,$08,$00

DATA_EDD4:
db $80,$10,$12,$20,$0D,$08,$0E,$08
db $0F,$20,$03,$10,$02,$08,$0E,$20
db $0D,$08,$0E,$08,$0F,$08,$01,$08
db $03,$30,$04,$40,$05,$80,$0B,$30
db $0E,$60,$0F,$FF,$08,$00

DATA_EDFA:
db $B8,$08,$14,$10,$04,$08,$03,$08
db $02,$08,$01,$08,$00,$08,$0E,$08
db $0D,$08,$0C,$50,$0B,$60,$0C,$10
db $0D,$40,$02,$70,$01,$FF,$08,$00

DATA_EE1A:
db $70,$18,$12,$40,$0D,$10,$0F,$10
db $02,$30,$04,$20,$05,$40,$03,$20
db $04,$20,$03,$18,$02,$20,$01,$FF
db $08,$00

DATA_EE34:
db $B0,$10,$14,$28,$02,$18,$01,$30
db $00,$10,$0F,$50,$0B,$60,$0C,$20
db $0B,$70,$04,$80,$05,$FF,$08,$00

DATA_EE4C:
db $5A,$08,$13,$10,$02,$20,$0D,$10
db $03,$10,$04,$10,$03,$58,$0D,$28
db $03,$48,$04,$10,$03,$48,$0D,$80
db $03

;unknown
db $FF,$08,$00

DATA_EE68:
db $46,$20,$12,$20,$0E,$38,$04,$18
db $02,$10,$04,$20,$06,$08,$05,$18
db $04,$18,$02,$40,$01,$40,$00

;unknown
db $FF,$08,$00

DATA_EE82:
db $D0,$2A,$14,$30,$0C,$20,$0B,$20
db $0C,$38,$0B,$48,$0C,$40,$02,$30
db $0C,$FF,$08,$00

DATA_EE96:
db $70,$10,$12,$10,$0D,$20,$0E,$10
db $0F,$10,$00,$30,$02,$10,$04,$10
db $05,$60,$05,$20,$0B,$40,$0C,$08
db $0D,$70,$03,$30,$02,$18,$00

;unknown
db $FF,$08,$00

DATA_EEB8:
db $C0,$18,$12,$10,$0D,$40,$0B,$38
db $0D,$08,$0B,$08,$0C,$10,$0D,$70
db $03,$40,$02,$40,$01

;unknown
db $FF,$08,$00

DATA_EED0:
db $90,$18,$14,$20,$02,$28,$0D,$20
db $0B,$20,$0B,$08,$0C,$10,$0D,$20
db $03,$20,$05,$20,$04,$10,$02,$10
db $03,$70,$0D,$40,$04,$50,$03,$20
db $02

;unknown
db $FF,$08,$00

DATA_EEF4:
db $58,$1C,$14,$24,$02,$08,$00,$48
db $0C,$38,$0E,$50,$07,$30,$05,$30
db $04,$28,$0D,$20,$02,$80,$01

;unknown
db $FF,$08,$00

DATA_EF0E:
db $50,$20,$12,$20,$0D,$40,$03,$30
db $0C,$40,$03,$30,$04,$38,$05,$08
db $0B,$28,$0C,$08,$0D,$40,$03,$4A
db $04,$FF,$08,$00

DATA_EF2A:
db $40,$E0,$01

DATA_EF2D:
db $80,$E0,$02

DATA_EF30:
db $D0,$E0,$0E

DATA_EF33:
db $38,$E0,$01

DATA_EF36:
db $A0,$E0,$0D

DATA_EF39:
db $30,$E0,$03

DATA_EF3C:
db $B0,$E0,$0E

DATA_EF3F:
db $90,$E0,$0F

DATA_EF42:
db $40,$E0,$03

DATA_EF45:
db $80,$E0,$03

DATA_EF48:
db $C8,$E0,$0E

DATA_EF4B:
db $50,$E0,$0F

DATA_EF4E:
db $B8,$E0,$0D

DATA_EF51:
db $20,$E0,$03

DATA_EF54:
db $48,$E0,$01

DATA_EF57:
db $B8,$E0,$0F

;free space after this

org $F300                                                   ;this is where sound related matters are
ChannelFrequencyLookup_F300:
db $07,$F0,$00                                              ;first 3 bytes are unused

db $00,$00,$D4,$00,$C8,$00,$BD,$00
db $B2,$00,$A8,$00,$9F,$00,$96,$00
db $8D,$00,$85,$00,$7E,$00,$70,$01
db $AB,$01,$93,$01,$7C,$01,$67,$01
db $52,$01,$3F,$01,$2D,$01,$1C,$01
db $0C,$00,$FD,$00,$EE,$00,$E1,$03
db $57,$02,$F9,$02,$CF,$02,$A6,$02
db $80,$02,$5C,$02,$3A,$02,$1A,$01
db $FC,$01,$DF,$01,$C4,$00,$69,$00
db $63,$00,$53,$00,$4A

ChannelLengthLookup_F350:
db $03,$06
db $0C,$18,$30                                              ;unused
db $12                                                      ;used
db $24                                                      ;unused
db $04,$08,$10,$20,$40,$18                                  ;used
db $30                                                      ;unused

;store to all sound registers related to a single sound channel from a pointer.
;A - sound register offset
;X,Y - pointer to get sound data from, low and high bytes respectively
SetupSoundChannel_Square1_F35E:
LDA #<APU_Square1DutyAndVolume                              ;
BEQ SetupSoundChannel_F36C                                  ;

SetupSoundChannel_Triangle_F362:
LDA #<APU_TriangleLinearCounter                             ;
BNE SetupSoundChannel_F36C                                  ;

SetupSoundChannel_Noise_F366:
LDA #<APU_NoiseVolume                                       ;
BNE SetupSoundChannel_F36C                                  ;

SetupSoundChannel_Square2_F36A:
LDA #<APU_Square2DutyAndVolume                              ;

SetupSoundChannel_F36C:
STA $F4

LDA #>APU_ChannelRegister1
STA $F5
STX $F6
STY $F7

LDY #$00                                                    ;

LOOP_F378:
LDA ($F6),Y                                                 ;get sound data
STA ($F4),Y                                                 ;store to sound register
INY                                                         ;
TYA                                                         ;
CMP #$04                                                    ;stop after all 4 registers have been stored to
BNE LOOP_F378                                               ;
RTS                                                         ;

;init something, sample?
InitSpecialSound_F383:
STA Sound_CurrentSFX2ID

LDA FanfareHeaders_F846,Y                                   ;headers!!
TAY

LDA FanfareHeaders_F846,Y
STA Sound_NoteLengthOffset

LDA FanfareHeaders_F846+1,Y
STA Sound_SFX2DataPointer

LDA FanfareHeaders_F846+2,Y
STA Sound_SFX2DataPointer+1

LDA FanfareHeaders_F846+3,Y                                 ;another pointer
STA $FA

LDA FanfareHeaders_F846+4,Y
STA $FB

LDA FanfareHeaders_F846+5,Y                                 ;another pointer
STA $FC

LDA FanfareHeaders_F846+6,Y
STA $FD

LDA FanfareHeaders_F846+7,Y                                 ;one. more. pointer.
STA $FE

LDA FanfareHeaders_F846+8,Y
STA $FF

LDA #$01
STA $D8
STA $D9
STA $DA
STA $DB

LDA #$00
STA $F3
STA $D0
STA $D1
STA $D2
STA $D3
STA APU_DMCLoadCounter
RTS

CODE_F3D0:
LDA #$7F
STA APU_ChannelRegister2
STA APU_Square2Sweep
STX $E9
STY $EA
RTS

DATA_F3DD:
db $13,$7F,$3E,$F0

DATA_F3E1:
db $4F,$53,$58,$5E,$63,$69,$70,$76
db $7E,$85,$8D,$90,$92,$93

DATA_F3EF:
db $92,$D5,$40,$C0

DATA_F3F3:
db $B2,$DF,$FF,$80

DATA_F3F7:
db $02,$7F,$0F,$08

DATA_F3FB:
db $00,$7F,$03,$08

;unknown
db $FF

DATA_F400:
db $10,$00,$18                                  ;unused

db $10,$01,$18,$00,$01,$88,$06,$02
db $40,$05,$02,$C0


CODE_F40F:
LDA $F9
BEQ CODE_F451

DEC $D8
BNE CODE_F451

LDX #$00

LDA #<CODE_F423
STA $0612

LDA #<CODE_F42E
STA $0610

CODE_F423:
LDY $D0
INC $D0
LDA (Sound_SFX2DataPointer),Y
BEQ CODE_F443                                               ;check if reached the end
JMP CODE_F51C

CODE_F42E:
TYA
BPL CODE_F43A

JSR ProcessLengthData_F544
LDY $D0
INC $D0
LDA (Sound_SFX2DataPointer),Y

CODE_F43A:
JSR SetSquare1Frequency_F550
BNE CODE_F448
LDY #$10
BNE CODE_F44A

CODE_F443:
LDA #$00
JMP SilenceSoundChannels_F68C

CODE_F448:
LDY $E9

CODE_F44A:
STY APU_ChannelRegister1
LDA $DC
STA $D8

CODE_F451:
LDA $FB
BEQ CODE_F47E
DEC $D9
BNE CODE_F47E
LDX #$01
LDY $D1
INC $D1
LDA ($FA),Y
BPL CODE_F46C
JSR ProcessLengthData_F544
LDY $D1
INC $D1
LDA ($FA),Y

CODE_F46C:
JSR SetSquare2Frequency_F564
BNE CODE_F475
LDY #$10
BNE CODE_F477

CODE_F475:
LDY $EA

CODE_F477:
STY APU_Square2DutyAndVolume
LDA $DD
STA $D9

CODE_F47E:
LDA $FD
BEQ CODE_F4D1
DEC $DA
BNE CODE_F4D1

LDX #$02
LDA #<CODE_F49B
STA $0610

LDA #<CODE_F492
STA $0612

CODE_F492:
LDY $D2
INC $D2
LDA ($FC),Y
JMP CODE_F51C

CODE_F49B:
TYA
BPL CODE_F4BF
JSR ProcessLengthData_F544
LDY $E4
BEQ CODE_F4A9
LDA #$FF
BNE CODE_F4B4

CODE_F4A9:
CLC
ADC #$FE
ASL A
ASL A
CMP #$3C
BCC CODE_F4B4
LDA #$3C

CODE_F4B4:
STA APU_TriangleLinearCounter
STA $E7

LDY $D2
INC $D2
LDA ($FC),Y

CODE_F4BF:
JSR SetTriangleFrequency_F568
BNE CODE_F4C8

LDY #$00
BEQ CODE_F4CA

CODE_F4C8:
LDY $E7

CODE_F4CA:
STY APU_TriangleLinearCounter
LDA $DE
STA $DA

CODE_F4D1:
LDA $FF
BEQ RETURN_F501
DEC $DB
BNE RETURN_F501

LDX #$03
LDA #<CODE_F4EE
STA $0610

LDA #<CODE_F4E5
STA $0612

CODE_F4E5:
LDY $D3
INC $D3
LDA ($FE),Y
JMP CODE_F51C

CODE_F4EE:
TYA
BPL CODE_F4FA
JSR ProcessLengthData_F544
LDY $D3
INC $D3
LDA ($FE),Y

CODE_F4FA:
JSR CODE_F502
LDA $DF
STA $DB

RETURN_F501:
RTS

CODE_F502:
TAY
LDA $F3
CMP #$02
BEQ RETURN_F51B

LDA DATA_F400,Y
STA APU_NoiseVolume

LDA DATA_F400+1,Y
STA APU_NoiseLoop

LDA DATA_F400+2,Y
STA APU_NoiseLength

RETURN_F51B:
RTS

CODE_F51C:
TAY
CMP #$FF
BEQ CODE_F52A
AND #$C0
CMP #$C0
BEQ CODE_F536
JMP ($0610)

CODE_F52A:
LDA $E0,X
BEQ CODE_F541

DEC $E0,X

LDA $D4,X
STA $D0,X
BNE CODE_F541

CODE_F536:
TYA
AND #$3F
STA $E0,X
DEC $E0,X
LDA $D0,X
STA $D4,X

CODE_F541:
JMP ($0612)

ProcessLengthData_F544:
AND #$07
CLC
ADC Sound_NoteLengthOffset
TAY
LDA ChannelLengthLookup_F350,Y
STA $DC,X
RTS

SetSquare1Frequency_F550:
LDX #<APU_Square1DutyAndVolume

;Input A - frequency lookup table indx
;X - channel offset
SetChannelFrequency_F552:
TAY                                                         ;
LDA ChannelFrequencyLookup_F300+1,Y                         ;
BEQ RETURN_F563                                             ;
STA APU_ChannelFrequency,X                                  ;

LDA ChannelFrequencyLookup_F300,Y                           ;
ORA #$08                                                    ;
STA APU_ChannelFrequencyHigh,X                              ;

RETURN_F563:
RTS                                                         ;

SetSquare2Frequency_F564:
LDX #<APU_Square2DutyAndVolume                              ;
BNE SetChannelFrequency_F552                                ;

SetTriangleFrequency_F568:
LDX #<APU_TriangleLinearCounter                             ;
BNE SetChannelFrequency_F552                                ;

;sound engine
SoundEngine_F56C:
LDA #$C0                                                    ;don't generate IRQs
STA APU_FrameCounter                                        ;

JSR CODE_F6CF
JSR CODE_F621
JSR CODE_F7CC
JSR PlayDMCSample_F591

LDA #$00
BEQ CODE_F58A

CODE_F581:
LDA #>CODE_F423                                             ;set high byte for RAM pointers. note that this assumes all codes we're pointing to are in ROM betwenn $F400-$F4FF
STA $0613                                                   ;this high byte remains unchanged while low byte is modified
STA $0611
RTS

;purge queues.
CODE_F58A:
STA Sound_SFX1Queue                                         ;
STA Sound_SFX2Queue                                         ;
STA Sound_SFX3Queue                                         ;
RTS                                                         ;

PlayDMCSample_F591:
LDA Sound_SFX3Queue                                         ;
LSR A                                                       ;
BCS PlayBarkSample_F5A2                                     ;
LSR A                                                       ;
BCS PlayQuackSample_F59A                                    ;only two samples to comb through
RTS                                                         ;

PlayQuackSample_F59A:
LDA #$20                                                    ;
GetDMCSampleLocation DMCSample_DuckQuack_FD00               ;
LDY #$0F                                                    ;
BNE LoadDMCSample_F5A8                                      ;

PlayBarkSample_F5A2:
LDA #$20                                                    ;same size
GetDMCSampleLocation DMCSample_DogBark_FB00                 ;
LDY #$0E                                                    ;

;A - sample size
;X - sample location
;Y - sample frequency
LoadDMCSample_F5A8
STA APU_DMCSampleLength                                     ;
STX APU_DMCSampleAddress                                    ;
STY APU_DMCFrequency                                        ;

LDA #$1F                                                    ;enable all channels, including DMC
STA APU_SoundChannels                                       ;
RTS                                                         ;

DATA_F5B7:
db $9F,$7F,$60,$00

DATA_F5BB:
db $9F,$7F,$61,$00

PlaySFX_DogLaugh_F5BF:
LDX #<DATA_F3DD
LDY #>DATA_F3DD
JSR SetupSoundChannel_Triangle_F362

LDA #$00
STA $EB
STA $ED
STA $EE

LDA #$06
STA $EF

LDA #$3E
STA $EC

LDA #$40
STA $F3

RETURN_F5DA:
RTS

CODE_F5DB:
INC $EE
LDA $EE
CMP #$01
BNE RETURN_F5DA

LDA #$00
STA $EE

INC $EB
LDA $EB
CMP $EF
BEQ CODE_F5FB

DEC $EC
DEC $EC
DEC $EC
LDA $EC
STA APU_TriangleFrequency
RTS


CODE_F5FB:
LDA $EF
CLC
ADC #$06
STA $EF

LDY $ED
LDA DATA_F3E1,Y
STA APU_TriangleFrequency

STA $EC
LDA #$F0
STA APU_TriangleFrequencyHigh

INC $ED
LDA $ED
CMP #$0E
BNE RETURN_F5DA

LDA #$00
STA APU_TriangleLinearCounter
STA $F3
RTS

;last 4 bit checks
CODE_F621:
LDA Sound_SFX1Queue
ASL A
BCS CODE_F669
ASL A
BCS PlaySFX_DogLaugh_F5BF
ASL A
BCS PlaySFX_ClayPigeonFall_F641
ASL A
BCS PlaySFX_DuckFall_F639

LDA $F3
ASL A
ASL A
BCS CODE_F5DB
ASL A
BCS CODE_F655
RTS

PlaySFX_DuckFall_F639:
LDX #<DATA_F3EF
LDY #>DATA_F3EF
JSR SetupSoundChannel_Square1_F35E
RTS

PlaySFX_ClayPigeonFall_F641:
LDX #<DATA_F3F3
LDY #>DATA_F3F3
JSR SetupSoundChannel_Square2_F36A

LDA $F3
ORA #$20
STA $F3

LDA #$00
STA $EF
STA $EE

RETURN_F654:
RTS

CODE_F655:
INC $EE
LDA $EE
CMP #$02
BNE RETURN_F654

LDA #$00
STA $EE

INC $EF
LDA $EF
CMP #$A0
BNE RETURN_F654

CODE_F669:
LDA #$10
STA APU_Square2DutyAndVolume

LDA $F3
AND #$DF
STA $F3
RTS

PlaySFX_ClayPigeonHit_F675:
LDX #<DATA_F3F7
LDY #>DATA_F3F7
JSR SetupSoundChannel_Noise_F366

LDA #$10
STA APU_ChannelRegister1

LDA $F3
AND #$F0
STA $F3
RTS

;silence all sound channels, disable special sound from playing
PlaySFX_Silence_F688:
CODE_F688:
LDA #$00                                                    ;
STA $F3

SilenceSoundChannels_F68C:
STA APU_TriangleLinearCounter                               ;no triangle
STA APU_DMCLoadCounter                                      ;no DMC
STA Sound_CurrentSFX2ID
STA $E4

LDA #$10                                                    ;
STA APU_Square1DutyAndVolume                                ;no square 1
STA APU_Square2DutyAndVolume                                ;no square 2
STA APU_NoiseVolume                                         ;no noise
RTS                                                         ;

InitSFX_WingFlag_F6A2:
LDX #<DATA_F3FB
LDY #>DATA_F3FB
JSR SetupSoundChannel_Noise_F366

LDA #$00
STA $EB

LDA $F3
ORA #$08
STA $F3

RETURN_F6B3:
RTS

PlaySFX_WingFlag_F6B4:
LDY $EB
LDA DATA_F6CB,Y
STA APU_NoiseLoop

INC $EB
LDA $EB
CMP #$04
BNE RETURN_F6B3

LDA $F3
AND #$FE
STA $F3
RTS

DATA_F6CB:
db $00,$02,$01,$00

;first 4 bits check
CODE_F6CF:
LDA Sound_SFX1Queue
LSR A
BCS PlaySFX_Silence_F688
LSR A
BCS InitSFX_ZapperShot_F6E8
LSR A
BCS PlaySFX_ClayPigeonHit_F675
LSR A
BCS InitSFX_WingFlag_F6A2

LDA $F3
LSR A
LSR A
BCS PlaySFX_ZapperShot_F703
LSR A
LSR A
BCS PlaySFX_WingFlag_F6B4
RTS

InitSFX_ZapperShot_F6E8:
LDX #<DATA_F6FF
LDY #>DATA_F6FF
JSR SetupSoundChannel_Noise_F366

LDA #$00
STA $EC
STA $ED
STA APU_DMCLoadCounter                                      ;related to noise/triangle volume

LDA $F3
ORA #$02
STA $F3

CODE_F6FE:
RTS

DATA_F6FF:
db $3F,$7F,$0F,$08

PlaySFX_ZapperShot_F703:
INC $EC
LDA $EC
CMP #$02
BNE CODE_F6FE

LDA #$00
STA $EC

LDY $ED
LDA DATA_F731,Y
STA APU_NoiseLoop

LDA DATA_F761,Y
STA APU_NoiseVolume

INC $ED
LDA $ED
CMP #$30
BNE RETURN_F730

LDA #$10
STA APU_NoiseVolume

LDA $F3
AND #$FD
STA $F3

RETURN_F730:
RTS

DATA_F731:
db $0E,$0B,$05,$05,$05,$05,$05,$05
db $05,$05,$05,$05,$04,$04,$04,$04
db $04,$04,$03,$03,$03,$03,$03,$04
db $03,$02,$02,$02,$02,$02,$02,$02
db $02,$01,$01,$01,$02,$02,$01,$01
db $00,$00,$00,$00,$00,$00,$00,$00

DATA_F761:
db $3F,$3F,$3F,$3F,$3F,$3F,$3D,$3C
db $38,$38,$38,$37,$37,$37,$36,$36
db $34,$34,$34,$34,$34,$34,$34,$34
db $33,$33,$33,$33,$33,$33,$33,$33
db $32,$32,$32,$32,$32,$32,$32,$32
db $32,$32,$32,$32,$32,$32,$32,$32

CODE_F791:
LDX #<DATA_F5B7
LDY #>DATA_F5B7
JSR SetupSoundChannel_Square1_F35E

LDX #<DATA_F5BB
LDY #>DATA_F5BB

CODE_F79C:
JSR SetupSoundChannel_Square2_F36A
RTS

DATA_F7A0:
db $84,$93,$70,$E3

DATA_F7A4:
db $84,$93,$72,$E3

CODE_F7A8:
LDA #$00                                                    ;doesn't seem necessary because we're setting this after?
STA APU_Square1DutyAndVolume                                ;

LDX #<DATA_F7A0
LDY #>DATA_F7A0
JSR SetupSoundChannel_Square1_F35E

LDX #<DATA_F7A4
LDY #>DATA_F7A4
BNE CODE_F79C

CODE_F7BA:
LDY #$06
LDA #$80
BNE CODE_F82F

CODE_F7C0:
LDY #$04
LDA #$04
BNE CODE_F826

CODE_F7C6:
LDY #$05
LDA #$08
BNE CODE_F826

CODE_F7CC:
LDA Sound_SFX2Queue
LSR A
BCS CODE_F7FD
LSR A
BCS CODE_F803
LSR A
BCS CODE_F7C0
LSR A
BCS CODE_F7C6
LSR A
BCS CODE_F809
LSR A
BCS CODE_F80F
LSR A
BCS CODE_F815
LSR A
BCS CODE_F7BA

LDA Sound_SFX3Queue
ASL A                                                       ;bit 7 check...
BCS CODE_F81B
ASL A                                                       ;skip over bit 6 because it's not used
ASL A
BCS CODE_F791
ASL A
BCS CODE_F840
ASL A
BCS CODE_F7A8

LDA Sound_CurrentSFX2ID                                     ;see if we should be playing something
BNE CODE_F7FA
RTS                                                         ;

CODE_F7FA:
JMP CODE_F40F

CODE_F7FD:
LDY #$00
LDA #$01
BNE CODE_F826

CODE_F803:
LDY #$03
LDA #$02
BNE CODE_F82F

CODE_F809:
LDY #$01
LDA #$10
BNE CODE_F826

CODE_F80F:
LDY #$07
LDA #$20
BNE CODE_F826

CODE_F815:
LDY #$02
LDA #$40
BNE CODE_F826

CODE_F81B:
LDA #$10
STA APU_NoiseVolume

LDY #$08
LDA #$EF
BNE CODE_F82F

CODE_F826:
JSR InitSpecialSound_F383

LDX #$C0
LDY #$C0
BNE CODE_F836

CODE_F82F:
JSR InitSpecialSound_F383

LDX #$86
LDY #$86

CODE_F836:
JSR CODE_F3D0

LDA #$00
STA $E4
JMP CODE_F40F

CODE_F840:
LDY #$09
LDA #$EF
BNE CODE_F826

FanfareHeaders_F846:
db FanfareHeader_1-FanfareHeaders_F846
db FanfareHeader_2-FanfareHeaders_F846
db FanfareHeader_3-FanfareHeaders_F846
db FanfareHeader_4-FanfareHeaders_F846
db FanfareHeader_5-FanfareHeaders_F846
db FanfareHeader_6-FanfareHeaders_F846
db FanfareHeader_7-FanfareHeaders_F846
db FanfareHeader_8-FanfareHeaders_F846
db FanfareHeader_9-FanfareHeaders_F846
db FanfareHeader_10-FanfareHeaders_F846

;Each fanfare has a header, with following format:
;1 byte - note length lookup offset
;2 bytes - pointer to square 1 sound data
;2 bytes - pointer to square 2 sound data
;2 bytes - pointer to triangle sound data
;2 bytes - pointer to noise sound data

FanfareHeader_1:
db $07
dw DATA_F8C3
dw DATA_F8E2
dw DATA_F908
dw DATA_F91B

FanfareHeader_2:
db $07
dw DATA_F92F
dw DATA_F960
dw DATA_F9A1
dw DATA_F9CC

FanfareHeader_3:
db $07
dw DATA_FAAF
dw DATA_FACA
dw DATA_FAE4
dw DATA_FAF5

FanfareHeader_4:
db $07
dw DATA_F9DA
dw DATA_F9F7
dw DATA_FA13
dw DATA_FA28

FanfareHeader_5:
db $07
dw DATA_FA2F
dw DATA_FA3F
dw DATA_FA64
dw DATA_FA80

FanfareHeader_6:
db $07
dw DATA_FA91
dw DATA_FA9A
dw DATA_FAA7
dw $0000

FanfareHeader_7:
db $07
dw DATA_F8BD
dw $0000
dw $0000
dw $0000

FanfareHeader_8:
db $00
dw DATA_F926
dw DATA_F92A
dw $0000
dw $0000

FanfareHeader_9:
db $00
dw DATA_F8AA
dw DATA_F8B1
dw DATA_F8B7
dw $0000

FanfareHeader_10:
db $00
dw DATA_FF00
dw DATA_FF18
dw $0000
dw $0000

DATA_F8AA:
db $85,$3E,$3C,$3A,$38,$36
db $00

DATA_F8B1:
db $85,$44,$42,$40,$3E,$3C

DATA_F8B7:
db $85,$3E,$3C,$3A,$38,$36

DATA_F8BD:
db $C3,$80,$10,$4E,$FF
db $00

DATA_F8C3:
db $84,$02,$81,$02,$2A,$2A,$02,$2C
db $02,$2C,$02,$2E,$30,$02,$22,$83
db $02,$81,$02,$08,$02,$02,$06,$02
db $06,$02,$28,$2A,$02,$22
db $00

DATA_F8E2:
db $84,$02,$81,$02,$80,$46,$46,$81
db $46,$02,$1C,$1E,$20,$22,$80,$28
db $28,$81,$2A,$02,$40,$83,$02,$81
db $02,$30,$02,$02,$80,$2C,$2C,$81
db $1E,$20,$22,$30,$83,$02

DATA_F908:
db $C6,$80,$0C,$02,$0C,$02,$18,$02
db $18,$02,$4C,$02,$4C,$02,$18,$02
db $18,$02
db $FF

DATA_F91B:
db $C6,$81,$03,$03,$06,$06,$03,$03
db $06,$03
db $FF

DATA_F926:
db $81,$16,$4A
db $00

DATA_F92A:
db $80,$02,$81,$16,$4A

DATA_F92F:
db $81,$0C,$0C,$12,$02,$0C,$02,$12
db $02,$0E,$0E,$16,$02,$0E,$02,$16
db $02,$0C,$0C,$12,$02,$0C,$02,$12
db $02,$12,$10,$0E,$0C,$08,$04,$30
db $02,$80,$04,$04,$81,$08,$0A,$0C
db $80,$0E,$0E,$81,$12,$16,$18,$48
db $00

DATA_F960:
db $80,$04,$04,$81,$04,$22,$28,$04
db $22,$24,$28,$80,$2C,$2C,$81,$2C
db $24,$2C,$04,$2C,$80,$04,$30,$2C
db $28,$80,$04,$04,$81,$22,$24,$10
db $28,$2A,$2C,$2E,$80,$30,$30,$81
db $2E,$2C,$28,$80,$24,$24,$81,$22
db $1E,$02,$80,$22,$22,$81,$24,$26
db $28,$80,$2C,$2C,$81,$30,$04,$08
db $0C

DATA_F9A1:
db $81,$1A,$1A,$02,$20,$22,$02,$3E
db $02,$3A,$3A,$02,$40,$42,$02,$1A
db $42,$1A,$32,$02,$34,$36,$38,$3A
db $02,$3E,$82,$3E,$81,$42,$44,$46
db $3E,$02,$1A,$1A,$02,$38,$3A,$3A
db $02,$3A,$1A

DATA_F9CC:
db $C8,$81,$06,$03,$06,$80,$09,$03
db $81,$03,$03,$82,$0C
db $FF

DATA_F9DA:
db $81,$2A,$2A,$28,$02,$26,$02,$24
db $02,$85,$22,$81,$24,$83,$02,$81
db $02,$0E,$06,$2A,$2A,$28,$26,$24
db $85,$22,$81,$24
db $00

DATA_F9F7:
db $81,$24,$24,$22,$02,$20,$02,$1E
db $02,$85,$3E,$81,$40,$83,$02,$81
db $02,$2A,$24,$1C,$1A,$46,$44,$40
db $85,$3E,$81,$40

DATA_FA13:
db $84,$02,$85,$1A,$81,$1C,$83,$02
db $81,$02,$4A,$14,$0E,$0A,$08,$06
db $04,$85,$1A,$81,$1C

DATA_FA28:
db $CC,$81,$06,$06,$09,$03 
db $FF

DATA_FA2F:
db $82,$08,$10,$16,$48,$18,$02,$12
db $02,$10,$0C,$08,$06,$81,$08
db $00

DATA_FA3F:
db $81,$26,$26,$82,$2C,$81,$08,$0C
db $10,$16,$80,$12,$12,$81,$28,$10
db $0C,$30,$2E,$30,$04,$80,$2C,$2C
db $81,$08,$28,$30,$80,$26,$26,$81
db $2C,$22,$2C,$82,$26

DATA_FA64:
db $85,$1E,$81,$1E,$82,$3C,$3C,$85
db $3E,$81,$3E,$85,$3E,$81,$3E,$85
db $42,$81,$42,$85,$42,$81,$42,$82
db $1E,$81,$1E,$1E                                          ;three last values seem unused?

DATA_FA80:
db $C4,$81,$06,$82,$0C,$81,$06,$80
db $09,$03,$03,$03,$09,$03,$03,$03
db $FF

DATA_FA91:
db $81,$0A,$24,$26,$28,$2E,$0A,$04
db $00

DATA_FA9A:
db $80,$12,$12,$81,$0A,$0A,$80,$20
db $20,$81,$28,$2E,$2A

DATA_FAA7:
db $81,$20,$02,$20,$28,$02,$28,$2A

DATA_FAAF:
db $82,$02,$81,$0E,$08,$04,$2C,$2A
db $28,$24,$1E,$1C,$1A,$1E,$24,$2E
db $24,$2C,$04,$28,$2E,$08,$28,$30
db $0E,$0C
db $00

DATA_FACA:
db $82,$02,$81,$2C,$2C,$02,$02,$1E
db $02,$02,$02,$02,$02,$44,$1E,$24
db $42,$1A,$2C,$44,$1E,$24,$46,$1E
db $1E,$28

DATA_FAE4:
db $82,$1E,$81,$1E,$85,$22,$24,$3C
db $82,$3E,$81,$3E,$85,$42,$44,$46
db $1A

DATA_FAF5:
db $C5,$81,$06,$06,$06,$09,$03,$06
db $FF

;free space until samples, so they are aligned properly (2 measily bytes)

include Data/DMCSamples/Bark.asm
include Data/DMCSamples/Quack.asm

DATA_FF00:
db $87,$04,$28,$04,$28,$04,$0C,$04
db $0C,$04,$0C,$12,$0C,$12,$0C,$12
db $48,$12,$48,$12,$48,$80,$02,$00

DATA_FF18:
db $80,$02,$87,$04,$28,$04,$28,$04
db $0C,$04,$0C,$04,$0C,$12,$0C,$12
db $0C,$12,$48,$12,$48,$12,$48

;freespace here

org $FFFA
dw NMI_C086
dw RESET_C000
dw RESET_C000                                   ;IRQ unused

incbin DuckHuntGFX.bin