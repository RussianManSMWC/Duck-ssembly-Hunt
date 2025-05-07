;scores
ENUM 0
  ScoreReward_500 db 0
  ScoreReward_800 db 0
  ScoreReward_1000 db 0
  ScoreReward_1500 db 0
  ScoreReward_1600 db 0
  ScoreReward_2000 db 0
  ScoreReward_2400 db 0
  ScoreReward_3000 db 0
ENDE

;palettes
ENUM 0
  LoadPalette_None db 0
  LoadPalette_TitleScreen db 0
  LoadPalette_DuckGame db 0
  LoadPalette_ClayShooting db 0
  LoadPalette_Zapper1 db 0
  LoadPalette_Zapper2 db 0
  LoadPalette_FlyAwayBackgroundRestore db 0
  LoadPalette_FlyAwayDog db 0
ENDE

ENUM 0
  Message_None db 0
  Message_ROUND db 0
  Message_GO db 0
  Message_GO_Blank db 0
  Message_GOOD db 0
  Message_PERFECT db 0
  Message_FLY_AWAY db 0
  Message_PAUSE db 0
  Message_GAME_OVER db 0
ENDE

ENUM 0
  GameplayMode_InitGameplay db 0
  GameplayMode_LoadDuckGame db 0
  GameplayMode_InitDuckGame db 0
  GameplayMode_DuckGameMain db 0
  GameplayMode_RoundEnd_DuckGame db 0
  GameplayMode_Unused1 db 0
  GameplayMode_InitGameOver db 0
  GameplayMode_GameOver db 0
  GameplayMode_LoadClayShooting db 0
  GameplayMode_InitClayShooting db 0
  GameplayMode_StartClayShooting db 0
  GameplayMode_ClayShootingMain db 0
  GameplayMode_RoundEnd_ClayShooting db 0
  GameplayMode_ClayShootingIntro db 0
  GameplayMode_Unused2 db 0
  GameplayMode_Unused3 db 0
  GameplayMode_Pause db 0
  GameplayMode_Unpause db 0
  GameplayMode_WaitForGameplayModeChange db 0
ENDE