MAPPER = 0                                                  ;which mapper it's using (NROM)
MIRRORING = 1                                               ;horizontal mirroring (0 - horizontal, 1 - vertical)
REGION = 0                                                  ;NTSC

db "NES", $1A

db $01                                                      ;16KB PRG space (for code) = 1
db $01                                                      ;8KB CHR space (for GFX) = 1

db MAPPER<<4&$F0|MIRRORING                                  ;Mapper = USER VALUE and Mirroring is USER VALUE
db MAPPER&$F0                                               ;Mapper is still USER VALUE, and the system is NES (not PlayChoice-10)

db $00                                                      ;PRG RAM-Size (useless)
db REGION                                                   ;the only thing that matters - TV System (NTSC or PAL)
db $00,$00,$00,$00,$00,$00                                  ;the rest don't matter for this game