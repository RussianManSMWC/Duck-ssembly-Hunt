;RAM Map

;$00-$07 - scratch
ControlMirror = $08
RenderMirror = $09
CameraPositionX = $0A
CameraPositionY = $0B

ControllerInput_Hold = $16                                  ;indexed. these variables are only updated if held for a few frames
Controller1Input_Hold = $16                                 ;\individual
Controller2Input_Hold = $17                                 ;/

UnpauseTimer = $1C                                          ;ticks down when pressing start after pausing the game to unpause, used to delay it for a bit

GameplayMode_Backup = $1D                                   ;backup whatever game mode we had for pausing the game
GameplayMode_Delayed = $1E                                  ;used to transition into next gameplay mode

SelectedGame = $1F                                          ;0 - 1 Duck, 1 - 2 Ducks, 3 - Clay Pigeons

FrameFlag = $20                                             ;a whole frame had passed, this is enabled in NMI
BGTileBuffer_WriteFlag = $21                                ;if set, can write to VRAM from buffer

NMIFunctionsDisableFlag = $22                               ;if set, NMI won't perform anything. needs a manual reset

PaletteToLoad = $23                                         ;0 - no palette update, the rest correspond to palette pointers
TitleScreenFlag = $24

GameplayMode = $25

TitleScreenMode = $26                                       ;execute title screen related code. 0 - load title screen, 1 - do nothing (title screen functionality is handled elsewhere)

DuckPaletteLoadFlag = $27                                   ;if set, will update palette for ducks. uses data from DuckPaletteStorage

;$28-$29 - temporary RAM used to preserve X and Y when executing a code pointer

Timer_Base = $2A                                            ;base address for all timers
Timer_Timing = $2A                                          ;used to make certain timers count down when this reaches 0

Timer_2B = $2B                                              ;decreases every frame
Timer_2C = $2C                                              ;decreases every frame. this timer appears to be unused.
Timer_2D = $2D                                              ;timed with Timer_Timing
Timer_2E = $2E                                              ;timed with Timer_Timing. this timer appears to be unused.

FrameCounter = $2F                                          ;a generic frame counter. note that it does not account for lag frames.

CurrentEntity_Variables = $30                               ;stores all variables to process current entity

CurrentEntity_ActiveFlag = CurrentEntity_Variables          ;is entity active?

CurrentEntity_State = CurrentEntity_Variables+1             ;what state the entity's in.

CurrentEntity_YPos = CurrentEntity_Variables+2              ;coordinates
CurrentEntity_XPos = CurrentEntity_Variables+3
CurrentEntity_HorzDir = CurrentEntity_Variables+4

CurrentEntity_Image = CurrentEntity_Variables+5             ;visual appearance

CurrentEntity_OAMStartPoint = CurrentEntity_Variables+6     ;from what OAM slot will it draw its graphics?
CurrentEntity_OAMEndPoint = CurrentEntity_Variables+7       ;from what OAM slot will it draw its graphics?
CurrentEntity_BehindBGFlag = CurrentEntity_Variables+8
CurrentEntity_ScoreReward = CurrentEntity_Variables+9       ;how much score is rewarded after wrecking whatever this is

;what shape does it take when using zapper
CurrentEntity_ZapperShape = CurrentEntity_Variables+$0A     ;what shape it takes when activating zapper

CurrentEntity_OAMProp = CurrentEntity_Variables+$0C

;layout is to be determined.
;$3F - difficulty related (???)

ScoreSprite_Timer = $80                                     ;ticks down until it disappears
ScoreSprite_YPos = $81
ScoreSprite_XPos = $82
ScoreSprite_Tile = $83
;the above addresses repeat for the second score sprite

CurrentDuckConfiguration = $98                              ;high nibble - first duck,low nibble - second duck. determines palette and something else.

ShotCounterBlinkTimer = $9A                                 ;increments when all shots are spent
HitTableIconBlinkTimer = $9B

;9E - bark SFX counter

CurrentEntity_Slot = $A1                                    ;what entity we're processing
CurrentEntity_MemoryOffset = $A2                            ;where current entity's variables are stored in Entity_Variables, so we can update them

RoundEndState = $A9                                         ;executes code when current round ends, either in victory or loss
CurrentHitCount = $AA
HitCountToWin = $AB

HitCounterUpdateFlag = $AD                                  ;used to visually update hit table when shooting a thing

ZapperShotConfiguration = $B0                               ;0 - shooting first duck, 1 - shooting second duck, 2 - shooting at title screen

CurrentOAMProperty = $B3
CurrentOAMOffset = $B4

ZapperEnabledFlag = $B5                                     ;used to determine if zapper can be used
ZapperPulledTriggerFlag = $B6                               ;used to determine if we squeezed the trigger
ZapperTriggerState = $B7                                    ;stores zapper trigger bit to determine if it changed (for a new shot)
ZapperCounter = $B8                                         ;this is supposed to increment every frame after changing zapper state, but it's always cleared. would've prevented the game from triggering zapper functions for a bit.
ZapperFunctionsEnabledFlag = $B9                            ;used to determine if zapper functions are enabled at all

ShotCount = $BA                                             ;how many bullets are left
TargetEntityCount = $BB                                     ;counts for every clay pigeon being thrown or a duck being released. round ends after 10 total shooting targets have been unleashed
;$BC
CurrentShotDuckCount = $BD                                  ;counts up for each duck that got hit at the moment, up to 2 for 2 ducks game
ShotCounterUpdateFlag = $BE

CurrentRound = $C1                                          ;BCD
RequiredHitCountIndex = $C2                                 ;counts up after each round, stopping at a certain point. used to get required hit amount per-round.

;$C3 - something score related...
CurrentScore = $C4                                          ;3 bytes. first byte - ten thousands and hundred thousands, second byte - hundreds and thousands, third - ones and tens
;$c7 - something top score related...
TopScore = $C8                                              ;3 bytes

;sound related things

Sound_NoteLengthOffset = $E5

Sound_CurrentSFX2ID = $E6

Sound_SFX1Queue = $F0
Sound_SFX2Queue = $F1                                       ;special sounds
Sound_SFX3Queue = $F2

Sound_SFX2DataPointer = $F8

;a large chuck of RAM dedicated to various entities. each entity reserves 80 bytes of RAM for various purposes. there can only be 3 entities at any given moment.
;as a space saving measure, these are stored to 
Entity_Variables_Size = 80                                  ;how much RAM is reserved by each entity
Entity_Variables = $0300

Entity_Variables_ActiveFlag = Entity_Variables
Entity_Variables_CurrentState = Entity_Variables+1
Entity_Variables_YPos = Entity_Variables+2
Entity_Variables_XPos = Entity_Variables+3

Entity_Variables_HitIndex = Entity_Variables+$0B

;slot 0 and 1 are ducks/clay pigeons

;slot 2 is dog

HitTable = $03F0                                            ;contains flags for every target that's been hit in current round. clear - not hit, set - hit. 10 bytes

DuckPaletteStorage = $0400                                  ;uses NES RLE stripe format to store sprite palette for ducks

Timer_Backup = $0430                                        ;same size as Timer_Base. stores all timers to restore them later

ControllerInput = $0440                                     ;indexed
Controller1Input = $0440                                    ;\individual
Controller2Input = $0441                        ;           /

ControllerInput_Counter = $0442                             ;indexed
Controller1Input_Counter = $0442                            ;\individual. counts up when holding the same input
Controller2Input_Counter = $0443                            ;/

BGTileBuffer_Transfer = $0500                               ;contains all tiles that will be transferred into actual buffer
BGTilebuffer_Offset = $0560
BGTileBuffer = $0561                                        ;64 bytes

RNG_Value = $05EC                                           ;4 bytes. last two bytes aren't actually used by anything

TopScoreTable = $05F0                                       ;contains TOP score per individual game

StartHeldFlag = $05FC
SelectHeldFlag = $05FD                                      ;used to differintiate between a select button press and hold, used for the title screen. note that this only clears after letting all inputs besides select.
TitleScreenChoice = $05FE
TitleScreenZapperShotState = $05FF                          ;0 - didn't shoot, 1 - shot directly at the screen (start the game), 2 - didn't shoot the screen (change game selection)

;OAM base ram addresses
OAM_Y = $0200
OAM_Tile = $0201
OAM_Prop = $0202
OAM_X = $0203

Score_OAM_Y = OAM_Y+(Score_OAM_Slot*4)
Score_OAM_Tile = OAM_Tile+(Score_OAM_Slot*4)
Score_OAM_Prop = OAM_Prop+(Score_OAM_Slot*4)
Score_OAM_X = OAM_X+(Score_OAM_Slot*4)