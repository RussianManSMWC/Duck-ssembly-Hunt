;insert useful macros here

;Word - the 16-bit value we store, Addr - 16-bit address we store the word to. This can be used to set up pointers or VRAM locations to draw to in RAM.
macro Macro_SetWord Word, Addr
  LDA #<Word
  STA Addr

  LDA #>Word
  STA Addr+1
endm

macro DrawStripeImage Pointer
  LDX #<Pointer
  LDY #>Pointer
  JSR DrawStripeImageInit_C617
endm

macro GetDMCSampleLocation Addr
  LDX #(Addr-$C000)/64
endm