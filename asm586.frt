/*****************************************************************************
	Intel Pentium 80586 Assembler
	Version 1.00

	Copyright (c) 1995-2006 Kuznetsov O.V. All rights reserved.
 	Licensed under the Apache License, Version 2.0
*****************************************************************************/

decimal \ warning off

vocabulary assembler                    \ Create new vocabulary
      also assembler definitions        \ And store new definitions in it

head-

variable {32BIT}

also forth definitions
+: USE32       ( --- )       \ Set 32-bit code generation, DEFAULT
               on> {32bit} ;

+: USE16       ( --- )       \ Set 16-bit code generation
               off> {32bit} ;
previous definitions

: ?use32       ( --- ? )     \ Return TRUE, if 32-bit code generation
               @> {32bit} ;
use32


\ ============================================================================
\                              Errors processing
\ ============================================================================

create   SaveTIB tibmax uallot      \ Last instruction TIB contains
variable SaveIN                     \ Last word >in value

preload string-table asm.error.table       \ Assembler's messages table
        asm.error.table

" Superfluous operand"                     +msg AE_SUPER
" Too many operands"                       +msg AE_TOOMANY
" Illegal prefix!"                         +msg AE_ILPREF
" Incompatible operand"                    +msg AE_INCOMP
" Missing jump address"                    +msg AE_MISSINGJUMP
" 16-bit immediate data required"          +msg AE_IMMED16
" 8-bit immediate data required"           +msg AE_IMMED8
" Illegal indexing mode"                   +msg AE_ILINX
" Missing operand"                         +msg AE_MISSING
" Incompatible operands size"              +msg AE_INCOMPSIZE
" Undefined operand size"                  +msg AE_UNDEFSIZE
" Constant too large"                      +msg AE_TOOLALGE
" First operand must be EAX, AX or AL"     +msg AE_AXALONLY
" Second operand must be EAX, AX or AL"    +msg AE_AXALONLY2
" Second operand must be DX or constant"   +msg AE_DXONLY2
" First operand must be DX or constant"    +msg AE_DXONLY
" Port can be 8-bit only"                  +msg AE_PORT8
" Can not use POP CS instruction"          +msg AE_POPCS
" Required 0-ffh interrupt vector"         +msg AE_INTREQ
" Can not use CALL SHORT instruction"      +msg AE_CALLSHORT
" Selector 16-bit required"                +msg AE_SEL16BIT
" Operand must be 16/32-bit size"          +msg AE_WORDDWORDONLY
" Operand must be ST(x) register"          +msg AE_FREG
" One operand must be ST(0)"               +msg AE_ST(0)
" First operand 16-bit required"           +msg AE_FIRST16BIT
" Second operand 8-bit required"           +msg AE_SECOND8BIT
" Second register must be CL"              +msg AE_REQCL
" Third  register must be CL"              +msg AE_REQCLTHIRD
" Third operand 8-bit required"            +msg AE_THIRD8BIT
" Conditionals wrong"                      +msg AE_COND
" JUMP address too far (8 bit only)"       +msg AE_OFFSET8
" JUMP address too far (16 bit only)"      +msg AE_OFFSET16
" Unresolved forward reference"            +msg AE_UNRESOLVED
" Too many forward references"             +msg AE_TOOMANYFORWARD
" Local label unused"                      +msg AE_UNUSED
" Local label was defined already"         +msg AE_ALREADYDEFLABEL
" Label && not defined yet"                +msg AE_NOTDEF&&
" Source operand not defined"              +msg AE_UNDEFSOURCE
" Destination operand not defined"         +msg AE_UNDEFDEST
\ ?

: save-error   ( --- )              \ Save Assembler insruction/operand name
               tib @ savetib tibmax cmove >in @ savein ! ;

: rest-error   ( --- )              \ Save Assembler insruction/operand name
               savetib tib @ tibmax cmove savein @ >in ! ;

: ?aerror      ( ? err --- )        \ Conditional error processing
               swap
               if{    \ Process error
                      rest-error
                      forth definitions
                      asm.error.table
                      dup ?ferror
               }      drop ;


\ ============================================================================
\                             Operands definiton
\ ============================================================================
                                    \ Address modes:
        0 constant MOD0             \ no offset
        1 constant MOD1             \ offset 8
        2 constant MOD2             \ offset 16/32
        3 constant MOD3             \ register

      01h constant REG              \ Common register
      11h constant FREG             \ Floating point register
      21h constant SREG             \ Segment register
      31h constant CREG             \ Control register CRx
      41h constant DREG             \ Debug register DRx
      51h constant TREG             \ Test register TRx
      02h constant MEM              \ Memory address mode INDIRECT
      12h constant DIRECT           \ Memory address mode DIRECT
      22h constant MEMFORWARD       \ Memory address mode LOCAL LABEL
      03h constant IMMED            \ Immediate data
      23h constant IMMFORWARD       \ Immediate data LOCAL LABEL

variable {DS}                       \ Destination operand
variable {DS.W}                     \ Destination operand size
variable {SR}                       \ Source operand
variable {SR.W}                     \ Source operand size
variable {SR2}                      \ Second source operand
variable {SR2.W}                    \ Second source operand size

variable {L}                        \ Instruction Length
variable {O}                        \ First instruction byte offset
variable {s}                        \ Immediate data 1=byte, 2=word
variable {d}                        \ Direction 0=out, 1=in
                                    \ 0 reg => reg/mem, 1 reg/mem => reg
variable {[]}                       \ JMP FAR [] mem attribyte
variable {sib}                      \ SIB exists

variable {FAR}                      \ FAR flag
variable {SHORT}                    \ SHORT flag

variable {immoff}                   \ Immediate data offset in code

defer op_source                     \ Exe last source operand

forth definitions
+variable AWarn                     \ Assembler warnings flag
          awarn on
assembler definitions

variable {we}                       \ Memory reference size

12 constant OP_MAX                  \ Number of operands
   variable OP_N                    \ Current operand

create OP_BUF op_max 3 * allot      \ Store operands for multiPUSH, multiPOP, USED
\       +0: Type of register, 1 byte
\       +1: Size, 1 byte
\       +2: Rigister, 1 byte

: +register    ( reg type size --- )     \ Set next register
               op_max op_n @ 2- = AE_TOOMANY ?aerror
               join op_n @ 2- 3 * op_buf + dup>r w!
               r> 2+ c! ;

12 constant TRS_MAX          \ Instruction max length ( without prefixs )
create TRS  trs_max allot    \ Current instruction code buffer

: field:       ( shift offset_in_trs --- ) \ Define field in TRS
               create c, c,
               does>           ( n --- )
               count trs + >r c@ lshift r@ c@ or r> c! ;

        6 1 field:   mod!
        3 1 field:   reg!
        0 1 field:   r/m!
        6 2 field: scale!
        3 2 field: index!
        0 2 field:  base!

        3 1 field:  ereg!
        3 1 field:  ekod!

    \   0 0 field:  singlereg!
    \   0 1 field:  singlefreg!

        1 0 field:     s!           \ Sign extension bit
        1 0 field:     d!           \ Direction bit
        0 0 field:    _w!           \ Size bit

: scale:       ( scale --- )        \ Define scale for indexed mode
               +create c, does> c@ scale! on> {sib} ;

               0 scale: *1
               1 scale: *2
               2 scale: *4
               3 scale: *8

: scale@       ( --- scale )        \ Get SCALE field
               trs 2+ c@ 6 3 and rshift ;

: reg@         ( --- reg )          \ Get REG field
               trs 1+ c@ 3 rshift 7 and ;

: r/m@         ( --- r/w )          \ Get R/W field
               trs 1+ c@ 7 and ;

7 constant Prefixes                 \ Max number of prefixes

create PREFIX  0 c, 0 c, 0 c,       \ Current instruction prefix buffer
               0 c, 0 c, 0 c, 0 c,
               0 c, 0 c,       \ For save LOCK and REPx prefixes
  /* pref_FWAIT pref_LOCK pref_ADDR pref_OPER pref_SEG pref_REP pref_386  */

: prefix:      ( offset_in_prefix code --- )      \ Define prefix code
                                                  \ with offset in prefix array
               +create c, c,
                does>  count swap c@ prefix + dup c@
                       AE_ILPREF ?aerror c! ;

: xprefix:     ( offset_in_prefix code --- )      \ Define prefix code
                                                  \ with offset in prefix array
               create c, c,
                does>  count swap c@ prefix + c! ;

        0 9bh  prefix: FWaitPrefix

        7 f0h  prefix: LOCK                \ LOCK prefix

        2 67h  xprefix: AddrPrefix         \ Address size prefix
        2 00h  xprefix: -AddrPrefix        \ Clear address size prefix
        3 66h  xprefix: OperPrefix         \ Operand size prefix
        3 00h  xprefix: -OperPrefix        \ Clear operand size prefix

        4 26h  prefix: ES:                 \ Segment override prefixes
        4 2eh  prefix: CS:
        4 36h  prefix: SS:
        4 3eh  prefix: DS:
        4 65h  prefix: GS:
        4 64h  prefix: FS:

        8 f3h  prefix: REP                 \ Repeat prefixes
        8 f3h  prefix: REPE
        8 f2h  prefix: REPNE
        8 f2h  prefix: REPNZ
        8 f3h  prefix: REPZ

        6 0fh  xprefix: 386Prefix          \ Prefix -1
        6 00h  xprefix: -386Prefix         \ Clear prefix -1

: ^rep         ( --- )              \ Error, if REP, REPE, REPNE
               prefix 5 + c@ AE_ILPREF ?aerror ;

: ^lock        ( --- )              \ Error, if LOCK
               prefix 1+ c@ AE_ILPREF ?aerror ;

: ^chseg       ( --- )              \ Error, if CS:, DS:, SS:, ES:, FS:, GS:
               prefix 4+ c@ AE_ILPREF ?aerror ;

: IsOperPrefix ( --- ? )     \ Return TRUE, if Prefix OPERAND
               prefix 3 + c@ 0<> ;

: IsAddrPrefix ( --- ? )     \ Return TRUE, if Prefix ADDRESS
               prefix 2 + c@ 0<> ;

: SetLOCKREP   ( --- )       \ Save prefixes LOCK and REPx
               prefix prefixes + dup w@ 0 rot w!
               split prefix 5 + c! prefix 1+ c! ;

: GetPrefixs   ( --- n )       \ Calculate number of prefixes before instruction
               0 prefix prefixes bounds
               ?do i c@ 0<> abs + loop ;

: !!           ( n addr --- )       \ Set the uninitialized variable value
               dup @ AE_SUPER ?aerror ! ;


        1 constant BYTE_SZ     4 constant PWORD_SZ
        2 constant WORD_SZ     5 constant QWORD_SZ
        3 constant DWORD_SZ    6 constant TWORD_SZ

: size:        ( size --- )         \ Define size operands
               +create c,
                does> c@ {we} !! ;

        byte_sz  size: BYTE         \ 1 byte
        word_sz  size: WORD         \ 2 bytes
       dword_sz  size: DWORD        \ 4 bytes
       pword_sz  size: PWORD        \ 6 bytes
       qword_sz  size: QWORD        \ 8 bytes
       tword_sz  size: TWORD        \ 10 bytes


\ ============================================================================
\                       Target memory reference vectors
\ ============================================================================

defer  HERE-A            ' here is  here-a
defer  C,-A              ' c,   is  c,-a
defer  W,-A              ' w,   is  w,-a
defer   ,-A              '  ,   is   ,-a
defer  C!-A              ' c!   is  c!-a
defer  W!-A              ' w!   is  w!-a
defer   !-A              '  !   is   !-a
defer  C@-A              ' c@   is  c@-a


\ ============================================================================
\                              Common procedures
\ ============================================================================

: GetAddrSize  ( --- 0/1 )   \ Calculate real address size
                             \ 0 - 32-bit, 1-16-bit
               ?use32 IsAddrprefix xor not 1 and ;

: GetOperSize  ( --- 0/1 )   \ Calculate real operand size
                             \ 0 - 32-bit, 1-16-bit
               ?use32 IsOperprefix xor not 1 and ;

: ??           ( --- n )            \ N - Assembler Stack depth
               csp @ sp@ cell+ - cell / ;

: ?8           ( n --- ? )          \ ?=true, if n - 8 bit signed
               80h + ffffff00h and 0= ;

: ?U8          ( n --- ? )          \ ?=true, if n - 8 bit unsigned
               ffffff00h and 0= ;

: ?16          ( n --- ? )          \ ?=true, if n - 16 bit with sign
               8000h + ffff0000h and 0= ;

: +{o}         ( --- )              \ Shift code generater start address
               incr> {o} ;

: +l           ( --- )              \ Add byte to instruction length
               incr> {l} ;

: sw>w         ( --- )              \ Convert fields S and W into just W
               @> {s} {ds.w} @ byte_sz <> and
               if{    \ Convert
                      {ds.w} @ dword_sz = abs 2* 1+ {l} +! {s} 0!
               } ;

: 1OP>         ( --- ds )    \ Check operands for instructon with single operand
               {sr} @ 0fh and ;

: 2OP>         ( --- ds sr ) \ Check operands for instructon with two operands
               {ds} @ 0fh and {sr} @ 0fh and ;

: offset8!     ( addr --- )         \ Calculate offset 8-bit
               here-a 2+ getprefixs + -
               dup ?8 not AE_OFFSET8 ?aerror trs 1+ c! ;

: offset16!    ( addr --- )         \ Calculate offset 16-bit
               here-a 3 + getprefixs + -
               dup ?16 not AE_OFFSET16 ?aerror trs 1+ w! ;

: offset32!    ( addr --- )         \ Calculate offset 32-bit
               here-a 5 + getprefixs + - trs 1+ ! ;

: 0-?w         ( --- )              \ Check operand size in instruction
                                    \ without operands
               0 dup {we} !! dup {ds.w} !! {sr.w} !! ;

: 1-?w         ( --- )              \ Check operand size in instruction
                                    \ with single operand
               {sr.w} @ ?dup
               ifnot{ \ Operand didn't define size
                      {we} @ dup 0= AE_UNDEFSIZE ?aerror {sr.w} !! {we} 0!
               else   \ Operand has a size
                      {we} !!
               } ;

: 2-?w         ( --- )              \ Check operand size in instruction
                                    \ with two operands
               {ds.w} @ ?dup
               if{    \ First operand has a size
                      {sr.w} @ ?dup
                      ifnot{ \ Set Second operand size
                             {sr.w} !!
                      else   \ Compare sizes
                             <> AE_INCOMPSIZE ?aerror
                      }
               else   \ First operand hasn't size
                      {sr.w} @ ?dup 0= AE_UNDEFSIZE ?aerror {ds.w} !!
               } ;

: ^far         ( --- )              \ Error, if FAR or []
               0 {far} !! 0 {[]} !! ;

: ^short       ( --- )              \ Error, if SHORT
               0 {short} !! ;

: ^^^          ( --- )              \ 3 errors
               ^far ^short ^rep ;

: ^^^^         ( --- )              \ 4 errors
               ^^^ ^lock  ;

: sr^local     ( --- )              \ Error, if local forward reference
               {sr} @ memforward over = swap
               immforward = or AE_INCOMP ?aerror ;

: ds^local     ( --- )              \ Error, if local forward reference
               {ds} @ memforward over = swap
               immforward = or AE_INCOMP ?aerror ;

: ^^^^^        ( --- )              \ Forth errors
               ^^^^ ^chseg ;

: @^controlreg ( @oper --- )         \ Error use of register CRx, DRx, TRx, STx
               @ reg over <> swap sreg <> and AE_INCOMP ?aerror ;

: @^segreg     ( @oper --- )         \ Error use of register CS, ...
               dup @ sreg = AE_INCOMP ?aerror @^controlreg ;

: ^creg        ( --- )       \ Ariphmetic and segment rigisters only
               {ds} @ 0fh and reg = if{ {ds} @^controlreg }
               {sr} @ 0fh and reg = if{ {sr} @^controlreg }
              {sr2} @ 0fh and reg = if{ {sr2} @^controlreg } ;

: ^sreg        ( --- )       \ Ariphmetic rigisters only
               {ds} @ 0fh and reg = if{ {ds} @^segreg }
               {sr} @ 0fh and reg = if{ {sr} @^segreg }
              {sr2} @ 0fh and reg = if{ {sr2} @^segreg } ;

: &^sr         ( --- )       \ ERROR: Source operand not defined
               true AE_UNDEFSOURCE ?aerror ;

: &^ds         ( --- )       \ ERROR: Destination operand not defined
               true AE_UNDEFDEST ?aerror ;

: &^oper       ( --- )       \ ERROR: Incompatible operand
               true AE_INCOMP ?aerror ;

: ///          ( --- )              \ Instruction without operands
               0-?w ^^^^^ ;

: /s           ( --- )              \ One operand instruction
               0 {ds} !! 1-?w ;

: d/s          ( --- )              \ One operand instruction
               0 {sr2} !! 2-?w ;

: >code!       ( byte --- addr )         \ Get start address for code generator
                                    \ in TRS
               trs @> {o} + c! ;

: @code!       ( addr --- )         \ Set the first byte code in instruction
               c@ >code! ;

: +op_source   ( /op/ addr --- addr ) \ Execute source operand
               >r op_source r> ;

: reg>r/m      ( --- )              \ Mode register from field REG
                                    \ to the field mod-r/m
               reg@ 0 trs 1+ c!
               r/m! mod3 mod! ;

: offset>      ( --- addr )         \ Get the memory offset address in TRS
               trs 2+ @> {sib} abs + ;


: movedirect   ( --- )
               offset> dup>r GetAddrSize
               ifnot{ \ 32-bit direct
                      @ r> cell+ !
               else   \ 16-bit direct
                      w@ r> 2+ w!
               } ;

: sr=reg       ( --- )              \ Check the source operand for REG type
               {sr} @ reg <> AE_INCOMP ?aerror ;

: sr2=reg      ( --- )              \ Check the second source operand for REG type
               {sr2} @ reg <> AE_INCOMP ?aerror ;

: ds=reg       ( --- )              \ Check the destination operand for REG type
               {ds} @ reg <> AE_INCOMP ?aerror ;

: sr=mem       ( --- )              \ Check the source operand for MEM
                                    \ or DIRECT or FORWARD type
               1op> mem <> AE_INCOMP ?aerror ;

: sr=freg      ( --- )              \
               {sr} @ freg <> AE_FREG ?aerror ;



\ ============================================================================
\                        Operand procedures definition
\ ============================================================================

: ?ControlReg  ( type --- ? )       \ Check for control register
                                    \ CRx, DRx, TRx
               sreg >= ;

: ?SamePrefix  ( size --- ? )       \ Check for Operand/Address prefix
                                    \ ?-true, if need prefix, ?-false no prefix
               ?use32
               if{    \ 32-bit instruction
                      word_sz =
               else   \ 16-bit instruction
                      dword_sz =
               } ;

: ?AddrPrefix  ( size --- )         \ Set Address prefix, if need
               ?sameprefix if{ addrprefix } ;

: ?OperPrefix  ( size --- )         \ Set Operand prefix, if need
               ?sameprefix if{ operprefix } ;

: reg,         ( reg type size --- )       \ Destination operand procedure
               dup ?operprefix
               @> op_n case{
               0  of{ \ *** First operand ***
                      {ds.w} !! {ds} !! reg! +l   }
               1  of{ \ *** Second operand ***
                      {sr2.w} !! {sr2} !!
                      {ds} @ reg =
                      if{    \ reg, reg
                             mod3 mod! r/m!
                      else   \ mem, reg
                             reg!
                      }                           }
               drop over reg over <> swap sreg <> and AE_INCOMP ?aerror
               +register
               }
               op_n incr 0 {we} !! ;

: ,reg         ( reg type size --- )       \ Source operand procedure
               dup ?operprefix
               @> op_n case{
               0  of{ \ *** Single operand
                      +l {sr.w} !! {sr} !! reg!
                      }      \                           }
               1  of{ \ *** op, reg
                      {sr.w} !! {sr} !! {ds} @ 0fh and reg =
                      if{    \ reg, reg
                             mod3 mod! r/m! 1 {d} !!
                      else   \ op, reg
                             reg!
                      }                                  }
               drop   over reg over <> swap sreg <> and AE_INCOMP ?aerror
                      3dup +register
                      {sr.w} !! {sr} !! drop  \          }
               }
               op_n incr 0 {we} !!
               ['] noop is op_source ;


: indexed16!   ( /offset/ reg --- ) \ Setup indexed addressing mode 16-bit
               6 ( [BP]) = if{ ?? ifnot{ 0 } }
               ??
               if{    \ Offset present
                      dup ?8
                      if{    \ Offset 8 bit
                             offset> c! mod1
                             incr> {immoff}
                      else   \ Offset 16 bit
                             dup ?16 not AE_OFFSET16 ?aerror
                             offset> w! +l mod2
                             2 +!> {immoff}
                      } +l
               else   \ No offset
                      mod0
               } mod! ;

: indexed32!   ( /offset/ reg --- ) \ Setup indexed addressing mode 32-bit
               dup 7 <= scale@ 0= and
               if{    \ *** [EREG]
                      dup r/m! 5 = ( [EBP] )
                      if{ ?? ifnot{ 0 } }
                      ??
                      if{    \ Offset present
                             dup ?8
                             if{    \ Offset 8 bit
                                    offset> c! mod1
                                    incr> {immoff}
                             else   \ Offset 32 bit
                                    offset> ! +l +l +l mod2
                                    4 +!> {immoff}
                             } +l
                      else   \ No offset
                             mod0
                      } mod! false {sib} !!
               else   dup 7 <=
                      if{    \ *** [EREG*2]
                          \  dup 6 = ( [ESP*2] ) AE_ILINX ?aerror
                             true !> {sib} mod0 mod! 4 r/m! 5 base!
                             index! ?? 0= if{ 0 }
                             offset> ! +l +l +l +l +l
                             4 +!> {immoff}
                      else   \ *** [EREG+EREG]   [ESP]
                          \  dup 3 rshift 7 and 4 = ( [EAX+ESP*2]) AE_ILINX ?aerror
                             dup 24h = scale@ and ( [ESP*2] ) AE_ILINX ?aerror
                             true !> {sib} dup 03fh and base! +l
                             7 and 5 = ( [EBP]) if{ ?? ifnot{ 0 } }
                             4 r/m! ??
                             if{    \ Offset present
                                    dup ?8
                                    if{    \ Offset 8 bit
                                           offset> c! mod1
                                           incr> {immoff}
                                    else   \ Offset 32 bit
                                           offset> ! +l +l +l mod2
                                           4 +!> {immoff}
                                    } +l
                             else   \ No offset
                                    mod0
                             } mod!
                      }
               }
               @> {sib} abs +!> {immoff} ;

: [reg],       ( /offset/ reg size --- )   \ Destinantion operand - INDEXED mem
               dup ?addrprefix
               @> op_n case{
               0  of{ \ *** First operand
                      mem {ds} !! {we} @ {ds.w} !!       }
               1  of{ \ *** Second operand
                      mem {sr2} !! {we} @ {sr2.w} !!     }
               true AE_ILINX ?aerror  }
               +l word_sz =
               if{    \ 16-bit operand
                      dup r/m! indexed16!
                      0 {sib} !!           \ No scale
               else   \ 32-bit operand
                      indexed32!
               }
               op_n incr {we} 0! ;

: ,[reg]       ( /offset/ reg size --- )   \ Source operand - INDEXED mem
               dup ?addrprefix 1 {d} !
               @> op_n case{
               0  of{ \ *** Single operand [REG]
                      mem {sr} !! {we} @ {sr.w} !! +l    }
               1  of{ \ *** Second last operand op, [REG]
                      mem {sr} !! {we} @ {sr.w} !!       }
               true AE_ILINX ?aerror  }
               word_sz =
               if{    \ 16-bit operand
                      dup r/m! indexed16!
                      0 {sib} !!           \ No scale
               else   \ 32-bit operand
                      indexed32!
               }
               op_n incr {we} 0!
               ['] noop is op_source ;

: Direct!      ( direct --- )       \ Setup direct memory operand 16/32-bit
               mod0 mod! offset> ?use32
               if{    \ 32/16-bit instruction
                      over 0ffff0000h and
                      if{    \ 32-bit instruction
                             ! 5 r/m! +l +l 2 +!> {immoff}
                      else   \ 16-bit instruction with prefix
                             w! 6 r/m! addrprefix
                      }
               else   \ 16/32-bit instruction
                      over 0ffff0000h and
                      if{    \ 32-bit instruction
                             ! 5 r/m! +l +l 2 +!> {immoff} addrprefix
                      else   \ 16-bit instruction with prefix
                             w! 6 r/m!
                      }
               } +l +l 2 +!> {immoff} ;

: mem,         ( offset --- )       \ Direct memory first operand
               ?? 0= AE_MISSING ?aerror
               direct! op_n @ case{
               0  of{ \ *** First operand
                      direct {ds} !! {we} @ {ds.w} !! +l    }
               1  of{ \ *** Second operand
                      direct {sr2} !! {we} @ {sr2.w} !!     }
               true AE_INCOMP ?aerror  }
               op_n incr {we} 0! ;

: ,mem         ( offset --- )       \ Direct memory source operand
               ?? 0= AE_MISSING ?aerror
               {ds} @ direct = if{ ( jmp far sel,offset) movedirect }
               direct! 1 {d} ! op_n @ case{
               0  of{ \ *** Single operand
                      direct {sr} !! {we} @ {sr.w} !! +l    }
               1  of{ \ *** Second operand
                      direct {sr} !! {we} @ {sr.w} !!       }
               true AE_INCOMP ?aerror  }
               op_n incr {we} 0! ;

: immed>       ( --- addr )         \ Address for immediate data in TRS
               {immoff} @ 2+ trs + ;

: imm          ( # --- )            \ Last operand - immediate data
               ?? 0= AE_MISSING ?aerror
               {ds} @ if{ 2-?w else 1-?w }
               dup>r immed> ! {sr.w} @ dup byte_sz =
               if{    \ Immediate data 8-bit
                      drop r> ?u8 0= AE_IMMED8 ?aerror 1
               else   \ Immediate data WORD or DWORD
                      word_sz =
                      if{    \ 16-bit operation
                             r@ 0ffff0000h and AE_IMMED16 ?aerror
                             1 r> ?u8 ifnot{ +l 1- }
                      else   \ 32-bit operation
                             1 r> ?u8 ifnot{ +l +l +l 1- }
                      }
               } {s} !! +l immed {sr} !! incr> op_n ;


\ ============================================================================
\                              Registers operand
\ ============================================================================

: reg,:        ( size type reg --- )    \ Define Destination operands
               +create c, c, c,
                does> count swap count swap c@ reg, ;

: ,reg:        ( size type reg --- )    \ Define Source operands
               +create c, c, c,
                does> count swap count swap c@ ,reg ;


        byte_sz  reg 0  ,reg: AL         byte_sz  reg 0 reg,: AL,
        byte_sz  reg 1  ,reg: CL         byte_sz  reg 1 reg,: CL,
        byte_sz  reg 2  ,reg: DL         byte_sz  reg 2 reg,: DL,
        byte_sz  reg 3  ,reg: BL         byte_sz  reg 3 reg,: BL,
        byte_sz  reg 4  ,reg: AH         byte_sz  reg 4 reg,: AH,
        byte_sz  reg 5  ,reg: CH         byte_sz  reg 5 reg,: CH,
        byte_sz  reg 6  ,reg: DH         byte_sz  reg 6 reg,: DH,
        byte_sz  reg 7  ,reg: BH         byte_sz  reg 7 reg,: BH,

        word_sz  reg 0 ,reg: AX          word_sz  reg 0 reg,: AX,
        word_sz  reg 1 ,reg: CX          word_sz  reg 1 reg,: CX,
        word_sz  reg 2 ,reg: DX          word_sz  reg 2 reg,: DX,
        word_sz  reg 3 ,reg: BX          word_sz  reg 3 reg,: BX,
        word_sz  reg 4 ,reg: SP          word_sz  reg 4 reg,: SP,
        word_sz  reg 5 ,reg: BP          word_sz  reg 5 reg,: BP,
        word_sz  reg 6 ,reg: SI          word_sz  reg 6 reg,: SI,
        word_sz  reg 7 ,reg: DI          word_sz  reg 7 reg,: DI,

       dword_sz  reg 0 ,reg: EAX        dword_sz  reg 0 reg,: EAX,
       dword_sz  reg 1 ,reg: ECX        dword_sz  reg 1 reg,: ECX,
       dword_sz  reg 2 ,reg: EDX        dword_sz  reg 2 reg,: EDX,
       dword_sz  reg 3 ,reg: EBX        dword_sz  reg 3 reg,: EBX,
       dword_sz  reg 4 ,reg: ESP        dword_sz  reg 4 reg,: ESP,
       dword_sz  reg 5 ,reg: EBP        dword_sz  reg 5 reg,: EBP,
       dword_sz  reg 6 ,reg: ESI        dword_sz  reg 6 reg,: ESI,
       dword_sz  reg 7 ,reg: EDI        dword_sz  reg 7 reg,: EDI,

        word_sz sreg 0 ,reg: ES          word_sz sreg 0 reg,: ES,
        word_sz sreg 1 ,reg: CS          word_sz sreg 1 reg,: CS,
        word_sz sreg 2 ,reg: SS          word_sz sreg 2 reg,: SS,
        word_sz sreg 3 ,reg: DS          word_sz sreg 3 reg,: DS,
        word_sz sreg 4 ,reg: FS          word_sz sreg 4 reg,: FS,
        word_sz sreg 5 ,reg: GS          word_sz sreg 5 reg,: GS,

       dword_sz creg 0 ,reg: CR0        dword_sz creg 0 reg,: CR0,
    \  dword_sz creg 1 ,reg: CR1        dword_sz creg 1 reg,: CR1,
       dword_sz creg 2 ,reg: CR2        dword_sz creg 2 reg,: CR2,
       dword_sz creg 3 ,reg: CR3        dword_sz creg 3 reg,: CR3,
       dword_sz creg 4 ,reg: CR4        dword_sz creg 4 reg,: CR4,
    \  dword_sz creg 5 ,reg: CR5        dword_sz creg 5 reg,: CR5,
    \  dword_sz creg 6 ,reg: CR6        dword_sz creg 6 reg,: CR6,
    \  dword_sz creg 7 ,reg: CR7        dword_sz creg 7 reg,: CR7,

       dword_sz dreg 0 ,reg: DR0        dword_sz dreg 0 reg,: DR0,
       dword_sz dreg 1 ,reg: DR1        dword_sz dreg 1 reg,: DR1,
       dword_sz dreg 2 ,reg: DR2        dword_sz dreg 2 reg,: DR2,
       dword_sz dreg 3 ,reg: DR3        dword_sz dreg 3 reg,: DR3,
    \  dword_sz dreg 4 ,reg: DR4        dword_sz dreg 4 reg,: DR4,
    \  dword_sz dreg 5 ,reg: DR5        dword_sz dreg 5 reg,: DR5,
       dword_sz dreg 6 ,reg: DR6        dword_sz dreg 6 reg,: DR6,
       dword_sz dreg 7 ,reg: DR7        dword_sz dreg 7 reg,: DR7,

    \  dword_sz treg 0 ,reg: TR0        dword_sz treg 0 reg,: TR0,
    \  dword_sz treg 1 ,reg: TR1        dword_sz treg 1 reg,: TR1,
    \  dword_sz treg 2 ,reg: TR2        dword_sz treg 2 reg,: TR2,
       dword_sz treg 3 ,reg: TR3        dword_sz treg 3 reg,: TR3,
       dword_sz treg 4 ,reg: TR4        dword_sz treg 4 reg,: TR4,
       dword_sz treg 5 ,reg: TR5        dword_sz treg 5 reg,: TR5,
       dword_sz treg 6 ,reg: TR6        dword_sz treg 6 reg,: TR6,
       dword_sz treg 7 ,reg: TR7        dword_sz treg 7 reg,: TR7,

       tword_sz freg 0 ,reg: ST(0)      tword_sz freg 0 reg,: ST(0),
       tword_sz freg 1 ,reg: ST(1)      tword_sz freg 1 reg,: ST(1),
       tword_sz freg 2 ,reg: ST(2)      tword_sz freg 2 reg,: ST(2),
       tword_sz freg 3 ,reg: ST(3)      tword_sz freg 3 reg,: ST(3),
       tword_sz freg 4 ,reg: ST(4)      tword_sz freg 4 reg,: ST(4),
       tword_sz freg 5 ,reg: ST(5)      tword_sz freg 5 reg,: ST(5),
       tword_sz freg 6 ,reg: ST(6)      tword_sz freg 6 reg,: ST(6),
       tword_sz freg 7 ,reg: ST(7)      tword_sz freg 7 reg,: ST(7),

\      ' st(0) alias st                 ' st(0), alias st,
\      ' st(0) alias st0                ' st(0), alias st0,
\      ' st(1) alias st1                ' st(1), alias st1,
\      ' st(2) alias st2                ' st(2), alias st2,
\      ' st(3) alias st3                ' st(3), alias st3,
\      ' st(4) alias st4                ' st(4), alias st4,
\      ' st(5) alias st5                ' st(5), alias st5,
\      ' st(6) alias st6                ' st(6), alias st6,
\      ' st(7) alias st7                ' st(7), alias st7,


\ ============================================================================
\                           Indexed memory operand
\ ============================================================================

: [reg],:      ( size reg --- )
               +create c, c,
                does> count swap c@ [reg], ;

: ,[reg]:      ( size reg --- )
               +create c, c,
                does> count swap c@ ,[reg] ;

        word_sz 0 [reg],: [BX+SI],       word_sz 0 ,[reg]: [BX+SI]
        word_sz 1 [reg],: [BX+DI],       word_sz 1 ,[reg]: [BX+DI]
        word_sz 2 [reg],: [BP+SI],       word_sz 2 ,[reg]: [BP+SI]
        word_sz 3 [reg],: [BP+DI],       word_sz 3 ,[reg]: [BP+DI]
        word_sz 4 [reg],: [SI],          word_sz 4 ,[reg]: [SI]
        word_sz 5 [reg],: [DI],          word_sz 5 ,[reg]: [DI]
        word_sz 6 [reg],: [BP],          word_sz 6 ,[reg]: [BP]
        word_sz 7 [reg],: [BX],          word_sz 7 ,[reg]: [BX]

       dword_sz 0   [reg],: [EAX],      dword_sz 0   ,[reg]: [EAX]
       dword_sz 1   [reg],: [ECX],      dword_sz 1   ,[reg]: [ECX]
       dword_sz 2   [reg],: [EDX],      dword_sz 2   ,[reg]: [EDX]
       dword_sz 3   [reg],: [EBX],      dword_sz 3   ,[reg]: [EBX]
       dword_sz 24h [reg],: [ESP],      dword_sz 24h ,[reg]: [ESP]
       dword_sz 5   [reg],: [EBP],      dword_sz 5   ,[reg]: [EBP]
       dword_sz 6   [reg],: [ESI],      dword_sz 6   ,[reg]: [ESI]
       dword_sz 7   [reg],: [EDI],      dword_sz 7   ,[reg]: [EDI]

       dword_sz 80h [reg],: [EAX+EAX],  dword_sz 80h ,[reg]: [EAX+EAX]
       dword_sz 08h [reg],: [EAX+ECX],  dword_sz 08h ,[reg]: [EAX+ECX]
       dword_sz 10h [reg],: [EAX+EDX],  dword_sz 10h ,[reg]: [EAX+EDX]
       dword_sz 18h [reg],: [EAX+EBX],  dword_sz 18h ,[reg]: [EAX+EBX]
     \ dword_sz 20h [reg],: [EAX+ESP],  dword_sz 20h ,[reg]: [EAX+ESP]
       dword_sz 28h [reg],: [EAX+EBP],  dword_sz 28h ,[reg]: [EAX+EBP]
       dword_sz 30h [reg],: [EAX+ESI],  dword_sz 30h ,[reg]: [EAX+ESI]
       dword_sz 38h [reg],: [EAX+EDI],  dword_sz 38h ,[reg]: [EAX+EDI]

       dword_sz 81h [reg],: [ECX+EAX],  dword_sz 81h ,[reg]: [ECX+EAX]
       dword_sz 09h [reg],: [ECX+ECX],  dword_sz 09h ,[reg]: [ECX+ECX]
       dword_sz 11h [reg],: [ECX+EDX],  dword_sz 11h ,[reg]: [ECX+EDX]
       dword_sz 19h [reg],: [ECX+EBX],  dword_sz 19h ,[reg]: [ECX+EBX]
     \ dword_sz 21h [reg],: [ECX+ESP],  dword_sz 21h ,[reg]: [ECX+ESP]
       dword_sz 29h [reg],: [ECX+EBP],  dword_sz 29h ,[reg]: [ECX+EBP]
       dword_sz 31h [reg],: [ECX+ESI],  dword_sz 31h ,[reg]: [ECX+ESI]
       dword_sz 39h [reg],: [ECX+EDI],  dword_sz 39h ,[reg]: [ECX+EDI]

       dword_sz 82h [reg],: [EDX+EAX],  dword_sz 82h ,[reg]: [EDX+EAX]
       dword_sz 0Ah [reg],: [EDX+ECX],  dword_sz 0Ah ,[reg]: [EDX+ECX]
       dword_sz 12h [reg],: [EDX+EDX],  dword_sz 12h ,[reg]: [EDX+EDX]
       dword_sz 1Ah [reg],: [EDX+EBX],  dword_sz 1Ah ,[reg]: [EDX+EBX]
     \ dword_sz 22h [reg],: [EDX+ESP],  dword_sz 22h ,[reg]: [EDX+ESP]
       dword_sz 2Ah [reg],: [EDX+EBP],  dword_sz 2Ah ,[reg]: [EDX+EBP]
       dword_sz 32h [reg],: [EDX+ESI],  dword_sz 32h ,[reg]: [EDX+ESI]
       dword_sz 3Ah [reg],: [EDX+EDI],  dword_sz 3Ah ,[reg]: [EDX+EDI]

       dword_sz 83h [reg],: [EBX+EAX],  dword_sz 83h ,[reg]: [EBX+EAX]
       dword_sz 0Bh [reg],: [EBX+ECX],  dword_sz 0Bh ,[reg]: [EBX+ECX]
       dword_sz 13h [reg],: [EBX+EDX],  dword_sz 13h ,[reg]: [EBX+EDX]
       dword_sz 1Bh [reg],: [EBX+EBX],  dword_sz 1Bh ,[reg]: [EBX+EBX]
     \ dword_sz 23h [reg],: [EBX+ESP],  dword_sz 23h ,[reg]: [EBX+ESP]
       dword_sz 2Bh [reg],: [EBX+EBP],  dword_sz 2Bh ,[reg]: [EBX+EBP]
       dword_sz 33h [reg],: [EBX+ESI],  dword_sz 33h ,[reg]: [EBX+ESI]
       dword_sz 3Bh [reg],: [EBX+EDI],  dword_sz 3Bh ,[reg]: [EBX+EDI]

       dword_sz 84h [reg],: [ESP+EAX],  dword_sz 84h ,[reg]: [ESP+EAX]
       dword_sz 0Ch [reg],: [ESP+ECX],  dword_sz 0Ch ,[reg]: [ESP+ECX]
       dword_sz 14h [reg],: [ESP+EDX],  dword_sz 14h ,[reg]: [ESP+EDX]
       dword_sz 1Ch [reg],: [ESP+EBX],  dword_sz 1Ch ,[reg]: [ESP+EBX]
     \ dword_sz 24h [reg],: [ESP+ESP],  dword_sz 24h ,[reg]: [ESP+ESP]
       dword_sz 2Ch [reg],: [ESP+EBP],  dword_sz 2Ch ,[reg]: [ESP+EBP]
       dword_sz 34h [reg],: [ESP+ESI],  dword_sz 34h ,[reg]: [ESP+ESI]
       dword_sz 3Ch [reg],: [ESP+EDI],  dword_sz 3Ch ,[reg]: [ESP+EDI]

       dword_sz 85h [reg],: [EBP+EAX],  dword_sz 85h ,[reg]: [EBP+EAX]
       dword_sz 0Dh [reg],: [EBP+ECX],  dword_sz 0Dh ,[reg]: [EBP+ECX]
       dword_sz 15h [reg],: [EBP+EDX],  dword_sz 15h ,[reg]: [EBP+EDX]
       dword_sz 1Dh [reg],: [EBP+EBX],  dword_sz 1Dh ,[reg]: [EBP+EBX]
     \ dword_sz 25h [reg],: [EBP+ESP],  dword_sz 25h ,[reg]: [EBP+ESP]
       dword_sz 2Dh [reg],: [EBP+EBP],  dword_sz 2Dh ,[reg]: [EBP+EBP]
       dword_sz 35h [reg],: [EBP+ESI],  dword_sz 35h ,[reg]: [EBP+ESI]
       dword_sz 3Dh [reg],: [EBP+EDI],  dword_sz 3Dh ,[reg]: [EBP+EDI]

       dword_sz 86h [reg],: [ESI+EAX],  dword_sz 86h ,[reg]: [ESI+EAX]
       dword_sz 07h [reg],: [ESI+ECX],  dword_sz 07h ,[reg]: [ESI+ECX]
       dword_sz 16h [reg],: [ESI+EDX],  dword_sz 16h ,[reg]: [ESI+EDX]
       dword_sz 17h [reg],: [ESI+EBX],  dword_sz 17h ,[reg]: [ESI+EBX]
     \ dword_sz 26h [reg],: [ESI+ESP],  dword_sz 26h ,[reg]: [ESI+ESP]
       dword_sz 27h [reg],: [ESI+EBP],  dword_sz 27h ,[reg]: [ESI+EBP]
       dword_sz 36h [reg],: [ESI+ESI],  dword_sz 36h ,[reg]: [ESI+ESI]
       dword_sz 3Eh [reg],: [ESI+EDI],  dword_sz 3Eh ,[reg]: [ESI+EDI]

       dword_sz 87h [reg],: [EDI+EAX],  dword_sz 87h ,[reg]: [EDI+EAX]
       dword_sz 0Fh [reg],: [EDI+ECX],  dword_sz 0Fh ,[reg]: [EDI+ECX]
       dword_sz 17h [reg],: [EDI+EDX],  dword_sz 17h ,[reg]: [EDI+EDX]
       dword_sz 1Fh [reg],: [EDI+EBX],  dword_sz 1Fh ,[reg]: [EDI+EBX]
     \ dword_sz 27h [reg],: [EDI+ESP],  dword_sz 27h ,[reg]: [EDI+ESP]
       dword_sz 2Fh [reg],: [EDI+EBP],  dword_sz 2Fh ,[reg]: [EDI+EBP]
       dword_sz 37h [reg],: [EDI+ESI],  dword_sz 37h ,[reg]: [EDI+ESI]
       dword_sz 3Fh [reg],: [EDI+EDI],  dword_sz 3Fh ,[reg]: [EDI+EDI]


\ ============================================================================
\                           Local label procedures
\ ============================================================================

21 constant ULABEL                  \ Label &&

21 constant lbs#                    \ Number of local label
40 constant frs#                    \ Number of unresolved forward references

create lbtable lbs# 5 * uallot      \ Local label address buffer
here constant lbtablend             \ End buffer address

/*
               ==============================
               = 4 byte              1 byte =
               ==============================
               /                       \
  Label address                         Bit 0: 1-Label defined, 0-undefined
                                        Bit 1: 1-label used , 0-unused
*/

create frtable frs# 6 * uallot      \ Unresolved forward reference buffer
here constant frtablend             \ End buffer address

/*
               ======================================
               =1 byte=1 byte=  4 byte              =
               ======================================
               /       \                             \
 Label: 1..#lbs         Bit 0: 0-Absolute resolve     Address to resolve
    &&: FFh                    1-Relative (<<8,<<9)
                        Bit 1: 0-32 bit
                               1-16 bit
*/

: lbsclear     ( --- )          \ Clear local label buffers
               lbtable lbtablend over - erase
               frtable frtablend over - erase ;

: frcheck      ( --- )          \ Check the forward references
               frtablend frtable
               \ ***** Check for resolve
               +do{ i c@ if{  \ ERROR: Unresolved forward reference
                             ?cr decimal i c@ dup ulabel =
                             if{    \ && label
                                    drop ." && - "
                             else   \ Normal label
                                    (.) type ." $ - "
                             } AE_UNRESOLVED message
                               true AE_UNRESOLVED ?aerror
                         }
               2 cell+ }
               awarn @
               if{    \ ***** Check for used
                      lbtablend lbtable
                      +do{  i 4+ c@ 3 and 1 =
                            if{  \ Unused local label
                                 ?cr i lbtable - 1 cell+ / 1+ dup ulabel =
                                 if{   \ && label
                                       drop ." && - "
                                 else  \ Common label
                                       (.) type ." $ - "
                                 } AE_UNUSED message cr
                            }
                       1 cell+ }
               } ;

: fradd        ( n --- )        \ Insert forward reference to the table
               frtablend dup frtable
               +?do{  i c@ ifnot{   \ Free space for forward reference
                                    drop i leave
                           }
               6 } dup frtablend = AE_TOOMANYFORWARD ?aerror
               dup>r c! here-a r> 2+ ! ;

: frres        ( n addr --- )        \ Resolve forward reference n
               frtablend frtable
               +?do{  i c@ pluck =
                      if{    \ It's she
                             dup i 1+ c@ 1 and
                             if{    \ Relative reference
                                    i 2+ @ dup 1- c@-a dup 0f0h and 080h =
                                    swap 0e9h 2dup 1- = -rot = or or
                                    if{    \ 16/32-bit relative
                                           i 1+ c@ 2 and
                                           ifnot{ \ 32-bit offset
                                                  dup>r cell+ - r> !-a
                                           else   \ 16-bit offset
                                                  dup>r 2+ - r> w!-a
                                           }
                                    else   \ 8-bit relative
                                           dup>r 1+ - r> c!-a
                                    }
                             else   \ Absolute reference
                                    i 2+ @ i 1+ c@ 2 and
                                    ifnot{ \ 32-bit absolute
                                           !-a
                                    else   \ 16-bit absolute
                                           w!-a
                                    }
                             } 1 i c@ 1- 5 * lbtable + 4+ bset
                               0 i c!
                      }
               6 } 2drop ;

: GetLabel     ( n --- ? value )    \ Return ?=TRUE, if label N is forward
                                    \ reference and VALUE for operand mem/imm
                                    \ Otherwise return ?=FALSE, and VALUE of label
               dup>r 1- 5 * lbtable + dup 4+ c@ 1 and
               if{    \ Label defined already
                      rdrop 1 over 4+ bset
                      @ false swap
               else   \ Forward reference
                      drop r> fradd true
                      -1 getaddrsize 0<> 16 and rshift
               } ;

: fimm         ( data --- )
               imm immforward {sr} ! ;

: llabel,      ( n --- )
               getlabel mem,
               if{    \ Forward reference
                      memforward {ds} op_n @ 1 <> if{ drop {sr2} } !
               } ;

: ,llabel      ( n --- )
               getlabel defer@> op_source ['] imm =
               if{    \ Immediate operand
                      swap
                      if{    \ Forward reference
                             drop 7fffh ?use32 2 and lshift
                             ['] fimm is op_source
                      }
               else   \ Memory operand
                      swap
                      if{    \ Forward reference
                             ,mem memforward {sr} !
                             ['] noop is op_source
                      }
               } ;

: >&&,         ( --- )
               ulabel fradd -1 mem,
               memforward {ds} op_n @ 1 <> if{ drop {sr2} } ! ;

: <&&,         ( --- )
               ulabel getlabel mem, AE_NOTDEF&& ?aerror ;

: >&&          ( --- )
               ulabel fradd defer@> op_source ['] imm =
               if{    \ Immediate operand
                      drop 7fffh ?use32 2 and lshift
                      ['] fimm is op_source
               else   \ Memory operand
                      ,mem memforward {sr} !
                      ['] noop is op_source
               } ;

: <&&          ( --- )
               getlabel drop AE_NOTDEF&& ?aerror ;

: findforward  ( --- 0/addr )       \ Find forward reference for current HERE-A
                                    \ Return 0, if no references, return addr
                                    \ of founded forward reference
               frtable 2+ frtablend frtable
               +?do{ dup @ here-a = ?leave 6 + 6 }
               dup frtablend 2+ <> abs * ;

: makeforwardabsolute ( 0/1 offset_in_trs --- )   \ 0-32bit
               >r findforward ?dup
               if{    \ Source operand is FORWARD reference
                      here-a getprefixs + r@ + over !
                      1- dup>r 0 swap breset
                      dup if{ 1 r> bset else 1 r> breset }
               } drop rdrop ;

: makeforwardrelative ( 0/1 offset_in_trs --- )
               >r findforward ?dup
               if{    \ Source operand is FORWARD reference
                      here-a getprefixs + r@ + over !
                      1- dup>r 0 swap bset
                      dup if{ 1 r> bset else 1 r> breset }
               } drop rdrop ;

: ?sr=mforward ( offset_in_trs --- )
               {sr} @ memforward =
               if{    \ Memory forward reference
                      getaddrsize over makeforwardabsolute
               } drop ;

: ?sr2=mforward ( offset_in_trs --- )
               {sr2} @ memforward =
               if{    \ Memory forward reference
                      getaddrsize over makeforwardabsolute
               } drop ;

: ?ds=mforward ( offset_in_trs --- )
               {ds} @ memforward =
               if{    \ Memory forward reference
                      getaddrsize over makeforwardabsolute
               } drop ;

: ?sr=iforward ( offset_in_trs --- )
               {sr} @ immforward =
               if{    \ Memory forward reference
                      getopersize over makeforwardabsolute
               } drop ;

: ?sr=rforward ( offset_in_trs --- )
               {sr} @ memforward =
               if{    \ Memory forward reference
                      getopersize over makeforwardrelative
               } drop ;

: off>?sr=mforward ( --- )
               offset> trs - ?sr=mforward ;

: off>?ds=mforward ( --- )
               offset> trs - ?ds=mforward ;


\ ============================================================================
\                              LABEL DEFINITIONS
\ ============================================================================

: 0reset       ( --- )         \ Clear all flags
               {ds} 0! {ds.w} 0! {sr} 0! {sr.w} 0! {sr2} 0! {sr2.w} 0!
               {l} 0! {o} 0! {s} 0! {d} 0! {[]} 0! {sib} 0! {far} 0!
               {short} 0! {we} 0! {immoff} 0! op_n 0!
               trs trs_max erase prefix prefixes erase
               ['] ,mem is op_source ?? AE_INCOMP ?aerror ;

: (reset>)     ( --- )         \ Compile instruction codes
               prefix prefixes bounds ?do{ i c@ ?dup if{ c,-a } }
               trs @> {o} + @> {l} bounds ?do{ i c@ c,-a }
               {l} 0! ;

: reset>       ( --- )         \ Prepare to new instruction
               (reset>) 0reset ;

variable buildup cell allot
here ' drop , constant boot_buildup

: resup        ( --- )                  \ Clear BUILDUP field
               boot_buildup buildup 2dup ! cell+ ! ;

: asm!         ( addr --- )             \ Save instruction in prefix notation
               buildup cell+ ! ;

: asm;         ( --- )                  \ Execute instruction in prefix notation
               buildup @ dup cell+ swap @ execute
               setlockrep
               buildup cell+ @ resup buildup ! ;

: !lbs         ( n --- )        \ Define label
               +create c,
                 does> >r asm; r> c@ dup 1- 5 * lbtable +
                       dup 4+ dup c@ 1 and AE_ALREADYDEFLABEL ?aerror
                       0 swap bset here-a swap !
                       here-a frres ;

: @lbs,        ( n --- )        \ Use label as destination
               create c,
                does> c@ llabel, ;

: @,lbs        ( n --- )        \ Use label as source
               create c,
                does> c@ ,llabel ;

             1  !lbs  1$:       1 @,lbs  1$        1 @lbs,  1$,
             2  !lbs  2$:       2 @,lbs  2$        2 @lbs,  2$,
             3  !lbs  3$:       3 @,lbs  3$        3 @lbs,  3$,
             4  !lbs  4$:       4 @,lbs  4$        4 @lbs,  4$,
             5  !lbs  5$:       5 @,lbs  5$        5 @lbs,  5$,
             6  !lbs  6$:       6 @,lbs  6$        6 @lbs,  6$,
             7  !lbs  7$:       7 @,lbs  7$        7 @lbs,  7$,
             8  !lbs  8$:       8 @,lbs  8$        8 @lbs,  8$,
             9  !lbs  9$:       9 @,lbs  9$        9 @lbs,  9$,
            10  !lbs 10$:      10 @,lbs 10$       10 @lbs, 10$,
            11  !lbs 11$:      11 @,lbs 11$       11 @lbs, 11$,
            12  !lbs 12$:      12 @,lbs 12$       12 @lbs, 12$,
            13  !lbs 13$:      13 @,lbs 13$       13 @lbs, 13$,
            14  !lbs 14$:      14 @,lbs 14$       14 @lbs, 14$,
            15  !lbs 15$:      15 @,lbs 15$       15 @lbs, 15$,
            16  !lbs 16$:      16 @,lbs 16$       16 @lbs, 16$,
            17  !lbs 17$:      17 @,lbs 17$       17 @lbs, 17$,
            18  !lbs 18$:      18 @,lbs 18$       18 @lbs, 18$,
            19  !lbs 19$:      19 @,lbs 19$       19 @lbs, 19$,
            20  !lbs 20$:      20 @,lbs 20$       20 @lbs, 20$,


\ ============================================================================
\                           Instruction definitions
\ ============================================================================

\ ----------------------------------------------------------------------------
\               Type 1 : One byte instruction without operands
\            NOP, LEAVE, XLAT/XLATB, CLC, STC, CMC, CLD, STD, CLI,
\            STI, LANF, SANF, AAA, AAS, DAA, DAS, HLT, INTO, WAIT
\                      PUSHA, POPA, PUSHF, POPF, IRET
\ ----------------------------------------------------------------------------
: <<1          ( addr --- )
               dup c@ 0d7h =
               if{    \ XLAT/XLATB instruction, can be segment prefix
                      ^^^^ 0-?w
               else   \ Common
                      ///
               } @code! +l reset> ;


\ ----------------------------------------------------------------------------
\               Type 1.1 : One byte instruction without operands
\           PUSHAD, POPAD, CBW, CWDE, CWD, CDQ, PUSHFD, POPFD, IRETD
\ ----------------------------------------------------------------------------
: <<1.1        ( addr --- )
               /// count ?operprefix @code! +l reset> ;


\ ----------------------------------------------------------------------------
\               Type 2 : Two bytes instruction without operands
\            AAM, AAD, CLTS, INVD, WBINVD, RDMSR, WRMSR, RSM, CPUID,
\            FCOMPP, FTST, FUCOMPP, FXAM, FLDZ, FLD1, FLDPI, FLDL2T,
\            FLDL2E, FLDLG2, FLDLN2, FSQRT, FSCALE, FXTRACT, FPREM,
\            FPREM1, FRNDINT, FABS, FCHS, FCOS, FPTAN, FPATAN, FSIN,
\            FSINCOS, F2XM1, FYL2X, FYL2XP1, FNINIT, FNSTSWAX, FNCLEX,
\                            FINCSTP, FDECSTP, FNOP
\ ----------------------------------------------------------------------------
: <<2          ( addr --- )
               /// @ trs w! +l +l reset> ;


\ ----------------------------------------------------------------------------
\        Type 2.F : Two bytes instruction without operands with FWAIT
\                            FINIT, FSTSWAX, FCLEX
\ ----------------------------------------------------------------------------
: <<2.float    ( addr --- )
               fwaitprefix <<2 ;


\ ----------------------------------------------------------------------------
\                             Type 3 : RET, RETF
\ ----------------------------------------------------------------------------
: <<3          ( /n/ addr --- )
               ^short ^rep ^lock ^chseg 0 {[]} !! 0-?w
               @> {far} + c@ >r
               ?? if{ \ RET n
                      dup ?16 not AE_IMMED16 ?aerror trs 1+ w! +l +l
                      r> 1- >r }
               r> >code! +l reset> ;


\ ----------------------------------------------------------------------------
\                          Type 4 : String operation
\                   CMPS, LODS, MOVS, SCAS, STOS, INS, OUTS
\ ----------------------------------------------------------------------------
: GetOperSize  ( size --- w )
               dup byte_sz =
               if{    \ BYTE size
                      drop 0
               else   \ WORD or DWORD
                      ?operprefix 1
               } ;

: <<4          ( addr --- )
               ^far ^short ^lock
               count ?dup
               ifnot{ \ Size not defined
                      @> {we} dup 0= AE_UNDEFSIZE ?aerror
               else   \ Size not defined
                      0 {we} !!
               } getopersize swap c@ dup 0ach =
               if{    \ *** LODS instruction
                      ^rep
               else   \ *** Not LODS instruction
                      ^chseg
               } or >code! +l reset> ;


\ ----------------------------------------------------------------------------
\                          Type 5 : Stack operation
\                                  PUSH, POP
\ ----------------------------------------------------------------------------
: AddPP        ( reg type size addr --- )
               >r dup byte_sz = AE_INCOMPSIZE ?aerror
               -operprefix ?operprefix
               over 1 ( CS) = over sreg = and
               r@ c@ 58h ( POP) = and AE_POPCS ?aerror
               reg =
               if{    \ Common REG
                      r@ c@ or >code! +l
               else   \ Segment SREG
                      dup 3 >
                      if{    \ SREG3: GS, FS
                             3 lshift r@ 4+ c@ or 80h or >code! +l 386prefix
                      else   \ SREG2: CS, DS, SS, ES
                             3 lshift r@ 2+ 1+ c@ or >code! +l
                      }
               } rdrop ;

variable addr
: MultiPUSH    ( addr --- )
               addr ! \ @> op_n op_l ! r/m@ r/m_l !
               ^creg ^chseg
               reg@ {ds} @ {ds.w} @ addr @ addpp (reset>)
               op_n @ 2 =
               if{    \ *** Two operands REG, REG
                      r/m@ {sr} @ {sr.w} @ addr @ addpp (reset>)
               else   \ *** Many operands
                      r/m@ {sr2} @ {sr2.w} @ addr @ addpp (reset>)
                      op_buf op_n @ 2- 3 * bounds
                      +?do{ i 2+ c@ i w@ split addr @ addpp (reset>) 3 }
               } 0reset ;

: MultiPUSHPOP ( addr --- )
               addr ! 0 {l} !
               ^creg ^chseg
               reg@ {ds} @ {ds.w} @ addr @ addpp (reset>)
               op_n @ 2 =
               if{    \ *** Two operands REG, REG
                      r/m@ {sr} @ {sr.w} @ addr @ addpp (reset>)
               else   \ *** Many operands
                      r/m@ {sr2} @ {sr2.w} @ addr @ addpp (reset>)
                      op_buf op_n @ 2- 3 * bounds
                      +?do{ i 2+ c@ i w@ split addr @ addpp (reset>) 3 }
               } 0reset ;

: &reg5        ( addr --- )
               ^creg ^chseg
               {l} 0! >r reg@ {sr} @ {sr.w} @ r> addpp ;

: &mem5        ( addr --- )
               {sr.w} @ ?operprefix
               1+ dup @code! 1+ c@ ekod! +l
               2 ?sr=mforward ;

: &imm5        ( addr --- )
               ^chseg c@ 058h = AE_INCOMP ?aerror
               +{o} @> {s} 2* 068h or >code! +l
               1 ?sr=iforward ;

: rubtab5      ( addr --- )
               exec: &^sr &reg5 &mem5 &imm5 ;

: <<5          ( addr --- )
               {we} @ {sr} @ or ifnot{ word_sz ?use32 abs + {we} ! }
               ^^^^ +op_source @> op_n 1 >
               if{    \ Multi operation
                      MultiPushPop
               else   \ Single operand
                      /s {sr.w} @ byte_sz = AE_INCOMP ?aerror
                      1op> rubtab5 reset>
               } ;


\ ----------------------------------------------------------------------------
\                              Type 6.IN : IN
\ ----------------------------------------------------------------------------
: &reg6in      ( addr --- )
               ^sreg r/m@ 2 <> {sr.w} @ word_sz <> or AE_DXONLY2 ?aerror
               @code! ;

: &mem6in      ( addr --- )
               sr^local -addrprefix 1+ @code!
               offset> @ dup 0ffffff00h and AE_PORT8 ?aerror
               trs 1+ c! 2 {l} ! ;

: &imm6in      ( addr --- )
               sr^local 1+ @code!
               immed> @ dup 0ffffff00h and AE_PORT8 ?aerror
               trs 1+ c! 2 {l} ! ;

: rubtab6in    ( addr ds --- )
               exec: &^sr &reg6in &mem6in &imm6in ;

: <<6.in       ( addr --- )
               +op_source ^^^^^ 0 {sr2} !!
               {ds} @ reg <> reg@ 0<> or
               {ds.w} @ dword_sz > or AE_AXALONLY ?aerror
               2op> nip rubtab6in
               -operprefix {ds.w} @ getopersize _w! reset> ;


\ ----------------------------------------------------------------------------
\                              Type 6.OUT : OUT
\ ----------------------------------------------------------------------------
: ?axal6out    ( field --- )
               {sr} @ reg <> or
               {sr.w} @ dword_sz > or AE_AXALONLY2 ?aerror ;

: &reg6out     ( addr --- )
               r/m@ ?axal6out
               reg@ 2 <> {ds.w} @ word_sz <> or AE_DXONLY ?aerror
               @code! ;

: &mem6out     ( addr --- )
               ds^local reg@ ?axal6out
               1+ @code! -addrprefix
               offset> @ dup 0ffffff00h and AE_PORT8 ?aerror
               trs 1+ c! 2 {l} ! ;

: rubtab6out   ( addr ds --- )
               exec: &^sr &reg6out &mem6out &^oper ;

: <<6.out      ( addr --- )
               ^^^^^ 0 {sr2} !! 2op> drop rubtab6out
               -operprefix {sr.w} @ getopersize _w! reset> ;


\ ----------------------------------------------------------------------------
\                                 Type 7: INT
\ ----------------------------------------------------------------------------
: <<7          ( addr --- )
               drop /// ?? 0= AE_INTREQ ?aerror
               dup ffffff00h and AE_INTREQ ?aerror
               cdh over 3 =
               if{    \ Exeption INT 3
                      nip 1-
               else   \ Common INT n
                      swap trs 1+ c! +l
               } >code! +l reset> ;


\ ----------------------------------------------------------------------------
\                          Type 8: Conditional Jump
\                       Jxxx, LOOP, LOOPZ/E, LOOPNZ/NE
\ ----------------------------------------------------------------------------
: ?forwardrel  ( rel --- rel/rel )
               {sr} @ memforward =
               if{    \ Forward relative
                      drop here-a
               } ;

\              >r ?? 0= AE_MISSINGJUMP ?aerror ^^^^^ 0 {ds} !!

: <<8          ( off addr --- )
               +op_source -addrprefix offset> @ swap {l} 0! ^^^^^ 0 {ds} !!
               {sr} @ direct over <> swap memforward <> and AE_INCOMP ?aerror
               dup 1+ c@ 0f0h and 070h <>
               if{    \ 8-bit offset only: LOOPx, JCXZ
                      count ?dup
                      if{    \ JECXZ, JCXZ
                             ?addrprefix
                      } @code! +l +l ?forwardrel offset8!
               else   \ Jxxx
                      1+ here-a 2+ getprefixs + pluck - ?8
                      if{    \ 8-bit offset
                             @code! +l +l offset8!
                      else   \ 16/32-bit offset
                             c@ 0fh and 080h or >code! 386prefix +l
                             ?use32
                             if{    \ 32-bit offset
                                    +l +l +l +l offset32!
                             else   \ 16-bit offset
                                    +l +l ?forwardrel offset16!
                             }
                      }
               } 1 ?sr=rforward reset> ;


\ ----------------------------------------------------------------------------
\                         Type 9: Unconditional Jump
\                        JMP, JMP FAR, CALL, CALL FAR
\              JMP short offset8       ( JMP only )
\              JMP offset16/32
\              JMP [] mem
\              JMP [reg]
\              JMP reg
\              JMPF seg , offset16/32   ( JMP FAR )
\              JMPF [] mem              ( JMP FAR )
\              JMPF [reg]               ( JMP FAR )
\ ----------------------------------------------------------------------------
: &non9        ( off addr --- )
               {l} 0! -addrprefix >r ?? 0= AE_MISSINGJUMP ?aerror
               @> {short}
               if{     \ jmp SHORT label
                       ^far ^chseg 0 {ds} !! ?forwardrel
                       r@ c@ 0e8h = AE_CALLSHORT ?aerror
                       0ebh >code! +l +l offset8!
                       1 ?sr=rforward
               else    \ Not SHORT
                       @> {far}
                       if{     \ FAR
                               {ds} @ direct =
                               if{     \ FAR SEG/SEL , OFFSET
                                       ^chseg offset> ?use32 2 and 2+ + @ dup
                                       0ffff0000h and AE_SEL16BIT ?aerror >r
                                       ?use32
                                       if{    \ 32-bit offset
                                              trs 1+ ! r> trs 5 + w! 7 {l} !
                                       else   \ 16-bit offset
                                              trs 1+ w! r> trs 3 + w! 5 {l} !
                                       } r@ 2+ @code!
                                       1 ?sr=mforward
                               else    \ FAR mem  FAR [] mem
                                       0 {ds} !! {sr} @ >r {sr} 0!
                                       op_n 0! ,mem r> {sr} !
                                       -1 >code! +l r@ 3 + c@ ekod!
                                       off>?sr=mforward
                               }
                       else    \ [] mem   label
                               @> {[]}
                               if{     \ [] mem
                                       {sr} @ >r {sr} 0! op_n 0!
                                       ,mem r> {sr} !
                                       -1 >code! +l r@ 1+ c@ ekod!
                                       off>?sr=mforward
                               else    \ label
                                       ^chseg
                                       here-a 2+ getprefixs + over - ?8
                                       r@ c@ 0e9h = and
                                       if{    \ JMP SHORT
                                              0ebh >code! +l +l offset8!
                                       else   \ 16/32-bit offset
                                              r@ @code! +l ?use32
                                              if{    \ 32-bit offset
                                                     4 {l} +! offset32!
                                              else   \ 16-bit offset
                                                     2 {l} +! offset16!
                                              }
                                       }  1 ?sr=rforward
                               }
                       }
               } rdrop ;

: &reg9        ( addr --- )
               ^^^^^ /s
               -1 >code! +l reg>r/m 1+ c@ ekod! ;

: &mem9        ( addr --- )
               {sr} @ mem <>
               if{    \ Direct memory reference
                      offset> @ swap &non9
               else   \ Indexed memory mode
                      ^short 0-?w
                      -1 >code! +l
                      1+ @> {far} 0<> 2 and + c@ ekod!
               } ;

: rubtab9      ( /off/ addr sr --- )
               exec: &non9 &reg9 &mem9 &^oper ;

: <<9          ( /off/ addr --- )
               ^rep ^lock ^sreg 0 {sr2} !!
               +op_source 1op> rubtab9 reset> ;


\ ----------------------------------------------------------------------------
\                         Type 10: Ariphmetic/Logical
\                     INC, DEC, DIV, IDEV, MUL, NEG, NOT
\ ----------------------------------------------------------------------------
: &mem10       ( addr --- )
               dup 1+ c@ 0 3 between
               ifnot{ \ Not: NOT, NEG, INC, DEC mem
                      ^lock
               }
               dup @code! 1+ c@ ekod! +l
               {sr.w} @ getopersize _w!
               off>?sr=mforward ;

: &reg10       ( addr --- )
               ^chseg ^sreg ^lock {sr.w} @ byte_sz <>
               over c@ feh = and ( INC, DEC)
               if{     \ 32/16-bit instruction - short variant
                       2+ c@ reg@ or >code!
               else    \ Full size instruction
                       reg>r/m &mem10
               } ;

: rubtab10     ( addr sr --- )
               exec: &^sr &reg10 &mem10 &^oper ;

: <<10         ( addr --- )
               +op_source ^^^ /s 1op> rubtab10 reset> ;


\ ----------------------------------------------------------------------------
\                               Type 11: BSWAP
\ ----------------------------------------------------------------------------
: <<11         ( addr --- )
               drop  ^^^^^ /s
               {sr} @ reg <> {sr.w} @ byte_sz = or AE_INCOMP ?aerror
               reg@ c8h or >code! 386prefix reset> ;


\ ----------------------------------------------------------------------------
\                                Type 12: IMUL
\              IMUL   reg/mem
\              IMUL   reg, reg/mem
\              IMUL   reg, reg/mem, imm
\ ----------------------------------------------------------------------------
: check12      ( @op1 @op2 --- )
               @ 0fh and mem over <> swap reg <> and
               swap @ reg <> or AE_INCOMP ?aerror
               {sr.w} @ byte_sz = AE_WORDDWORDONLY ?aerror ;

: &1op12       ( --- )
               0f6h >code!
               {sr} @ 0fh and mem <>
               if{    \ imul reg
                      {sr} @ reg <> AE_INCOMP ?aerror
                      reg>r/m
               } 05 ekod!
               {sr.w} @ getopersize _w!
               off>?sr=mforward ;

: &2op12       ( --- )
               {ds} {sr} check12 386prefix 0afh >code!
               off>?sr=mforward ;

: &3op12       ( --- )
               {ds} {sr2} check12 {sr} @ 0fh and immed <> AE_INCOMP ?aerror
               069h >code! {s} @ s!
               offset> trs - ?sr2=mforward
               immed> trs - ?sr=iforward ;

: rubtab12     ( addr n_operands --- )
               4 min exec: &^sr &1op12 &2op12 &3op12 &^oper ;

: <<12         ( addr --- )
               drop op_source ^^^^ @> op_n rubtab12 +l reset> ;


\ ----------------------------------------------------------------------------
\                                 Type 13.r/m
\            SETxxx, LLDT, LMSW, LTR, SLDT, SMSW, STR, VERR, VERW
\ ----------------------------------------------------------------------------
: code!+ekod!  ( addr --- )
               dup @code! 1+ c@ ekod! ;

: &reg13r/m    ( addr --- )
               ^sreg ^chseg reg>r/m c@ 0f0h and 90h = word_sz - abs
               {sr.w} @ <> AE_INCOMPSIZE ?aerror ;

: &checksize+2 ( addr --- )
               2+ c@ dup {sr.w} @ 0<> and
               if{    \ Check operand size
                      {sr.w} @ <> AE_INCOMPSIZE ?aerror
               else   \ No check
                      {sr.w} !!
               } ;

: &mem13r/m    ( addr --- )
               &checksize+2 off>?sr=mforward ;

: rubtab13r/m  ( sr --- )
               2 min exec: &^sr &reg13r/m &mem13r/m &^oper ;

: <<13.R/M     ( addr --- )
               +op_source 0 {ds} !! ^^^^ 386prefix +l
               dup 1op> rubtab13r/m code!+ekod! -operprefix reset> ;


\ ----------------------------------------------------------------------------
\                                  Type 13.r
\                  INVLPG, LGDT, LIDT, SGDT, SIDT, CMPXCHG8B
\ ----------------------------------------------------------------------------
: <<13.M       ( addr --- )
               +op_source 0 {ds} !! ^^^^ sr=mem
               386prefix +l dup &mem13r/m code!+ekod! -operprefix reset> ;


\ ----------------------------------------------------------------------------
\                                  Type 14:
\                 FBLD, FBSTP, FNSTSW, FNLDCW, FSTCW, FNSTENV
\                        FLDENV, FNSAVE, FRSTOR, FLDSW
\ ----------------------------------------------------------------------------
: <<14         ( addr --- )
               +op_source 0 {ds} !! ^^^^ sr=mem
               dup &mem13r/m code!+ekod! +l reset> ;


\ ----------------------------------------------------------------------------
\                                 Type 14.F:
\                         FSTSW, FLDCW, FSTENV, FSAVE
\ ----------------------------------------------------------------------------
: <<14.FLOAT   ( addr --- )
               fwaitprefix <<14 ;


\ ----------------------------------------------------------------------------
\                     Type 15:  Floating point operation
\       FIST, FICOM, FICOMP, FIADD, FISUB, FISUBR, FINUL, FIDIV, FIDIVR
\ ----------------------------------------------------------------------------
: <<15         ( addr --- )
               +op_source /s ^^^^ sr=mem
               {sr.w} @ case{
               word_sz  of{  \ 16-bit operand
                             code!+ekod!   }
              dword_sz  of{  \ 32-bit operand
                             dup c@ 0fbh and >code!
                             1+ c@ ekod!   }
               true AE_INCOMPSIZE ?aerror  }
               off>?sr=mforward +l reset> ;


\ ----------------------------------------------------------------------------
\                     Type 16:  Floating point operation
\                  FXCH, FUCOM, FUCOMP, FADDP, FSUBP, FSUBRP
\                         FMULP, FDIVP, FDIVRP, FFREE
\ ----------------------------------------------------------------------------

: SetFREG      ( @code --- )
               reg@ or trs 1+ c! ;

: <<16         ( addr --- )
               {sr} @ ifnot{ st(1) } ^^^^^ /s {sr} @
               if{    \ ST(i)
                      {sr} @ freg <> AE_FREG ?aerror
               }
               dup @code! +l 1+ c@ setfreg reset> ;


\ ----------------------------------------------------------------------------
\                     Type 17:  Floating point operation
\                                 FILD, FISTP
\ ----------------------------------------------------------------------------
: <<17         ( addr --- )
               +op_source /s ^^^^ sr=mem
               {sr.w} @ case{
               word_sz  of{  \ 16-bit operand
                             0dfh          }
              dword_sz  of{  \ 32-bit operand
                             1+ 0dbh       }
              qword_sz  of{  \ 64-bit operand
                             2+ 0dfh       }
               true AE_INCOMPSIZE ?aerror  }
               >code! +l c@ ekod! off>?sr=mforward reset> ;


\ ----------------------------------------------------------------------------
\                     Type 18:  Floating point operation
\                              FST, FCOM, FCOMP
\ ----------------------------------------------------------------------------
: &mem18       ( addr --- )
               {sr.w} @ qword_sz =
               if{    \ 64-bit operand
                      2+
               else   \ 32-bit only
                      {sr.w} @ dword_sz <> AE_INCOMPSIZE ?aerror
               } code!+ekod! off>?sr=mforward ;

: &reg18       ( reg d addr --- )
               ^chseg dup>r c@ swap 4* or >code!
               r> 1+ c@ or trs 1+ c! ;

: &0op18       ( addr --- )
               1 0 rot 2+ 2+ &reg18 ;

: &1op18       ( addr --- )
               {sr} @ freg =
               if{    \ FREG
                      ^chseg reg@ 0 rot 2+ 2+ &reg18
               else   \ MEM32/64
                      1op> mem <> AE_INCOMP ?aerror
                      &mem18
               } ;

: rubtab18     ( addr n_operands --- )
               2 min exec: &0op18 &1op18 &^oper ;

: default18    ( --- )
               {sr} @ ifnot{ \ Memory or default st(1)
                             ?? if{ op_source else st(1) }
                      else   op_source } ;

: <<18         ( addr --- )
               >r default18 /s ^^^^ r> @> op_n rubtab18 +l reset> ;


\ ----------------------------------------------------------------------------
\                    Type 18.1:  Floating point operation
\                    FADD, FSUB, FSUBR, FMUL, FDIV, FDIVR
\ ----------------------------------------------------------------------------
: &2op18.1     ( addr --- )
               ^chseg 2+ 2+ 2-?w
               {sr} @ {ds} @ over <> swap freg <> or AE_FREG ?aerror
               reg@ ?dup
               ifnot{ \ ST(0), ST(i)
                      r/m@ 0 pluck &reg18 1+ c@ 0f0h and 0c0h =
                      ifnot{ \ Exception FADD, FMUL
                             3 trs 1+ btoggle
                      }
               else   \ ST(i), ST(0)
                      r/m@ AE_ST(0) ?aerror
                      1 rot &reg18
               } ;

: &0op18.1     ( addr --- )  \ iP st(1), st(0)
               st(1), st(0) d/s &2op18.1 1 trs bset ;

: rubtab18.1   ( addr n_operands --- )
               3 min exec: &0op18.1 &1op18 &2op18.1 &^oper ;

: default18.1  ( --- )
               {sr} @ ifnot{ \ Memory or without operand
                             ?? if{ op_source }
                      else   op_source } ;

: <<18.1       ( addr --- )
               ^^^^^ >r default18.1 r> +l
               @> op_n rubtab18.1 reset> ;

\ ----------------------------------------------------------------------------
\                     Type 19:  Floating point operation
\                                  FLD, FSTP
\ ----------------------------------------------------------------------------
: <<19         ( addr --- )
               +op_source /s ^^^^
               {sr} @ 0fh and mem = {sr.w} @ tword_sz = and
               if{    \ mem80
                      6 + code!+ekod! off>?sr=mforward
               else   \ FREG/MEM32/MEM64
                      &1op18
               } +l reset> ;


\ ----------------------------------------------------------------------------
\                               Type 20:  ENTER
\ ----------------------------------------------------------------------------
: <<20         ( /n/ addr --- )
               drop 0-?w ^^^^^ -addrprefix sr^local ds^local
               offset> @ dup 0ffff0000h and AE_FIRST16BIT ?aerror
               trs 1+ w!
               ?? 0= AE_MISSING ?aerror dup 0ffffff00h and AE_SECOND8BIT ?aerror
               trs 2+ 1+ c! 0c8h trs c! 4 {l} ! reset> ;


\ ----------------------------------------------------------------------------
\                  Type 21:  Simple two operands instruction
\                     LEA, LDS, LES, LFS, LGS, LSS, BOUND
\ ----------------------------------------------------------------------------
: <<21         ( addr --- )
               +op_source ^^^^ ^sreg d/s
               2op> mem <> swap reg <> or AE_INCOMP ?aerror
               {sr.w} @ byte_sz <= AE_INCOMPSIZE ?aerror
               dup @code! +l c@ f0h and b0h =
               if{    \ LFS, LGS, LSS
                      386prefix
               } off>?sr=mforward reset> ;


\ ----------------------------------------------------------------------------
\                     Type 22:  Two operands instruction
\                             BSF, BSR, LAR, LSL
\ ----------------------------------------------------------------------------
: rubtab22     ( addr sr --- )
               exec: &^sr ^chseg off>?sr=mforward &^oper ;

: (22)         ( addr --- )
               +op_source d/s ^^^^ 2-?w ds=reg ^sreg
               {ds.w} @ byte_sz = AE_INCOMPSIZE ?aerror
               @code! +l ;

: <<22         ( addr --- )
               +op_source d/s ^^^^ 2-?w ds=reg ^sreg
               {ds.w} @ byte_sz = AE_INCOMPSIZE ?aerror
               @code! +l 386prefix 1op> rubtab22 reset> ;


\ ----------------------------------------------------------------------------
\                     Type 22.1:  Two operands instruction
\                                XADD, CMPXCHG
\ ----------------------------------------------------------------------------
: regswap      ( --- )
               ^chseg reg@ r/m@  c0h trs 1+ c!  reg! r/m! ;

: rubtab22.1   ( addr ds --- )
               exec: &^ds regswap off>?ds=mforward &^oper ;

: <<22.1       ( addr --- )
               +op_source d/s ^^^^ 2-?w sr=reg
               ^sreg 386prefix @code! +l {sr.w} @ getopersize _w!
               2op> drop rubtab22.1 reset> ;


\ ----------------------------------------------------------------------------
\                 Type 23:  Not good two operand instruction
\                                MOVZX, MOVSX
\ ----------------------------------------------------------------------------
: <<23         ( addr --- )
               +op_source 0 {sr2} !! ^^^^ ds=reg
               {sr.w} @ 0= AE_UNDEFSIZE ?aerror
               {sr.w} @ word_sz =
               {ds.w} @ dword_sz <> and AE_INCOMPSIZE ?aerror
               ?use32 {ds.w} @ dword_sz = and if{ -operprefix }
               386prefix 1op> rubtab22 @code! +l {sr.w} @ word_sz = abs _w!
               reset> ;


\ ----------------------------------------------------------------------------
\                               Type 24:  ARPL
\ ----------------------------------------------------------------------------
: <<24         ( addr --- )
               drop op_source d/s ^^^^ 2-?w sr=reg ^sreg -operprefix
               {sr.w} @ word_sz <> AE_INCOMPSIZE ?aerror
               063h >code! +l 2op> drop rubtab22.1 reset> ;


\ ----------------------------------------------------------------------------
\                      Type 25: Two operands instruction
\                              BT, BTS, BTR, BTC
\                                LOCK allowed
\ ----------------------------------------------------------------------------
: &mem,reg25   ( addr --- )
               ^sreg 1+ @code! off>?ds=mforward ;

: &reg,reg25   ( addr --- )
               ^chseg ^sreg regswap &mem,reg25 ;

: &mem,imm25   ( addr --- )
               c@ ekod! 0bah >code! off>?ds=mforward  ;

: &reg,imm25   ( addr --- )
               ^sreg ^chseg reg>r/m &mem,imm25 ;

: &,reg25      ( addr ds --- )
               exec: &^ds &reg,reg25 &mem,reg25 &^oper ;

: &,imm25      ( addr ds --- )
               exec: &^ds &reg,imm25 &mem,imm25 &^oper ;

: rubtab25     ( addr ds sr --- )
               exec: &^sr &,reg25 &^oper &,imm25 ;

: 25checkSRC   ( /imm8/ --- )
               {sr} @ ifnot{ \ Source operand not defined yet
                             ?? 0= AE_MISSING ?aerror
                             dup 0ffffff00h and AE_SECOND8BIT ?aerror
                             immed> c! +l immed {sr} ! } ;

: <<25         ( addr --- )
               >r sr^local 25checksrc d/s ^^^
               {ds.w} @ dup byte_sz <= AE_INCOMPSIZE ?aerror ?operprefix
               386prefix +l r> 2op> rubtab25 reset> ;


\ ----------------------------------------------------------------------------
\                      Type 26: Two operands instruction
\                    ROL, ROR, RCL, RCR, SHL/SAL, SHR, SAR
\ ----------------------------------------------------------------------------
: &mem,imm26   ( addr --- )
               c@ ekod! immed> c@ 1 =
               if{    \ mem,1
                      0d0h >code! decr> {l}
               else   \ mem,imm
                      0c0h >code!
               } off>?ds=mforward ;

: &reg,imm26   ( addr --- )
               reg>r/m &mem,imm26 ;

: &mem,reg26   ( addr --- )
               reg@ 1 ( CL ) <> {sr.w} @ byte_sz <> or AE_REQCL ?aerror
               trs 1+ c@ 0c7h and trs 1+ c!  c@ ekod! 0d2h >code!
               off>?ds=mforward ;

: &reg,reg26   ( addr --- )
               ^chseg ^sreg regswap &mem,reg26 ;

: &,reg26      ( addr ds --- )
               exec: &^ds &reg,reg26 &mem,reg26 &^oper ;

: &,imm26      ( addr ds --- )
               exec: &^ds &reg,imm26 &mem,imm26 &^oper ;

: rubtab26     ( addr ds sr --- )
               exec: &^sr &,reg26 &^oper &,imm26 ;

: <<26         ( addr --- )
               >r sr^local 25checksrc 0 {sr2} !! ^^^^ +l
               {ds.w} @ getopersize r> 2op> rubtab26 _w! reset> ;


\ ----------------------------------------------------------------------------
\                                Type 27: XCHG
\                                LOCK allowed
\ ----------------------------------------------------------------------------
: &reg,reg27   ( --- )
               ^lock ^chseg sr=reg ds=reg {ds.w} @ byte_sz =
               if{    \ REG, REG
                      087h +l
               else   \ May be short format
                      reg@
                      ifnot{ \ AX, REG
                             r/m@ 90h or
                      else   r/m@
                             ifnot{ \ REG, AX
                                    reg@ 90h or
                             else   \ REG, REG
                                    086h +l
                             }
                      }
               } >code! ;

: &reg,mem27   ( --- )
               ds=reg 086h >code! +l off>?sr=mforward ;

: &mem,reg27   ( --- )
               sr=reg 086h >code! +l off>?ds=mforward ;

: &,reg27      ( ds --- )
               exec: &^ds &reg,reg27 &mem,reg27 &^oper ;

: &,mem27      ( ds --- )
               exec: &^ds &reg,mem27 &^oper &^oper ;

: rubtab27     ( ds sr --- )
               exec: &^sr &,reg27 &,mem27 &^oper ;

: <<27         ( addr --- )
               drop op_source d/s ^^^ {ds.w} @ getopersize
               2op> rubtab27 _w! reset> ;


\ ----------------------------------------------------------------------------
\                  Type 28: Two operands common instruction
\                    ADD, ADC, AND, OR, SUB, SBB, XOR, CMP
\                                LOCK allowed
\ ----------------------------------------------------------------------------
: 28NoImmedForward ( --- )
               {ds.w} @ byte_sz = if{ sr^local } ;

: &mem,imm28   ( addr --- )
               dup 1+ @code! {s} @ s! {ds.w} @ getopersize _w!
               2+ c@ ekod! +l 28noimmedforward
               off>?ds=mforward immed> trs - ?sr=iforward ;

: &reg,imm28   ( addr --- )
               {ds.w} @ byte_sz =
               if{    \ Full format
                      reg>r/m &mem,imm28
               else   \ Check short format
                      reg@
                      ifnot{ \ AX/EAX, IMM => short format
                             +{o} sw>w 2+ 1+ c@ {ds.w} @ getopersize or >code!
                             1 ?sr=iforward
                      else   \ Full format
                             reg>r/m &mem,imm28
                      }
               } 28noimmedforward ;

: &reg,mem28   ( addr --- )
               ^sreg @code! {d} @ d! {ds.w} @ getopersize _w! +l
               off>?sr=mforward ;

: &reg,reg28   ( addr --- )
               &reg,mem28 ;

: &mem,reg28   ( addr --- )
               ^sreg @code! {d} @ d! {sr.w} @ getopersize _w! +l
               off>?ds=mforward ;

: &,reg28      ( addr ds --- )
               exec: &^ds &reg,reg28 &mem,reg28 &^oper ;

: &,mem28      ( addr ds --- )
               exec: &^ds &reg,mem28 &^oper &^oper ;

: &,imm28      ( addr ds --- )
               exec: &^ds &reg,imm28 &mem,imm28 &^oper ;

: rubtab28     ( addr ds sr --- )
               exec: &^sr &,reg28 &,mem28 &,imm28 ;

: <<28         ( addr --- )
               +op_source d/s ^^^
               2op> pluck c@ 38h ( CMP) = pluck mem <> and if{ ^lock }
               rubtab28 reset> ;


\ ----------------------------------------------------------------------------
\              Type 29: Two operands not common instruction TEST
\ ----------------------------------------------------------------------------
: <<29         ( addr --- )
               +op_source ['] noop is op_source d/s sw>w <<28 ;


\ ----------------------------------------------------------------------------
\                       Type 30: 3 operands instruction
\                                 SHLD, SHRD
\ ----------------------------------------------------------------------------
: 30-?w        ( --- )
               {sr2.w} @ dup byte_sz = AE_INCOMPSIZE ?aerror
               {ds.w} @ ?dup
               if{    \ Compare sizes
                      <> AE_INCOMPSIZE ?aerror
               else   \ Set DS size
                      {ds.w} !
               } ;

: reg/mem30    ( --- )
               {ds} @ 0fh and dup reg =
               if{    \ REG, REG,
                      drop regswap
               else   \ MEM, REG
                      mem <> AE_INCOMP ?aerror
                      off>?ds=mforward
               } ;

: <<30         ( addr --- )
               >r ^^^^ 386prefix sr2=reg 30-?w
               {sr} @ reg =
               if{    \ mem/reg, reg, CL
                      ?? AE_INCOMP ?aerror
                      sr=reg op_buf 1+ w@
                      [ byte_sz 1 join ] literal <> AE_REQCLTHIRD ?aerror
                      r> 1+ @code! +l reg/mem30
               else   \ mem/reg, reg, imm
                      immed {sr} !! sr^local ?? 0= AE_MISSING ?aerror
                      dup 0ffffff00h and AE_THIRD8BIT ?aerror
                      immed> c! +l
                      r> @code! +l reg/mem30
               }  reset> ;


\ ----------------------------------------------------------------------------
\                 Type 31: Very large 2 operands instruction
\                                     MOV
\ ----------------------------------------------------------------------------
: &_reg,reg31  ( --- )
               088h >code! {ds.w} @ getopersize _w! {d} @ d! +l
               off>?sr=mforward ;
: &sreg,reg31  ( --- )
               08ch >code! {d} @ d! +l off>?sr=mforward ;
: &creg,reg31  ( --- )
               386prefix 20h >code! {d} @ d! +l -operprefix off>?sr=mforward ;
: &dreg,reg31  ( --- )
               386prefix 21h >code! {d} @ d! +l -operprefix off>?sr=mforward ;
: &treg,reg31  ( --- )
               386prefix 24h >code! {d} @ d! +l -operprefix off>?sr=mforward ;

: rubtab31reg  ( op --- )
               4 rshift exec:
               &_reg,reg31 &^oper &sreg,reg31 &creg,reg31 &dreg,reg31 &treg,reg31 ;

: &reg,mem31   ( --- )
               reg@ 0= {sr} @ direct = and
               if{     \ AK, DIRECT
                       0a0h {ds.w} @ getopersize or
                       +{o} {d} @ 1 xor 2* or >code!
                       1 ?sr=mforward
               else    \ Other registers
                       ^creg {ds} @ sreg = if{ -operprefix }
                       {ds} @ rubtab31reg
               } ;

: &reg,reg31   ( --- )
               ^chseg {sr} @ reg <>
               if{     \ REG, xREG
                       {d} 0! regswap ds=reg {sr} @ rubtab31reg
               else    \ xREG, REG
                       -operprefix sr=reg {ds} @ rubtab31reg
               } ;

: &mem,reg31   ( --- )
               reg@ 0= {ds} @ direct = and
               if{     \ DIRECT, AK
                       0a0h {ds.w} @ getopersize or
                       +{o} {d} @ 1 xor 2* or >code!
                       1 ?ds=mforward
               else    \ Other registers
                       ^creg {sr} @ sreg = if{ -operprefix }
                       {sr} @ rubtab31reg
               } ;

: &reg,imm31   ( --- )
               ^sreg reg@ {ds.w} @ getopersize 8* or
               +{o} 0b0h or >code! 1 ?sr=iforward ;

: &mem,imm31   ( --- )
               0c6h >code! {ds.w} @ getopersize _w! +l
               off>?ds=mforward immed> trs - ?sr=iforward ;

: &,reg31      ( ds --- )
               exec: &^ds &reg,reg31 &mem,reg31 &^oper ;

: &,mem31      ( ds --- )
               exec: &^ds &reg,mem31 &^oper &^oper ;

: &,imm31      ( ds --- )
               exec: &^ds &reg,imm31 &mem,imm31 &^oper ;

: rubtab31     ( ds sr --- )
               exec: &^sr &,reg31 &,mem31 &,imm31 ;

: <<31         ( addr --- )
               drop op_source d/s sw>w ^^^^
               2op> rubtab31 reset> ;


\ ============================================================================
\                              Additional words
\ ============================================================================

+: FAR         ( --- )          \ FAR flag
               1 {far} !! ;

+: SHORT       ( --- )          \ SHORT flag
               1 {short} !! ;

+: #           ( --- )          \ Immediate data flag
               ['] imm is op_source ;

+: ,           ( --- )          \ Destination direct memory operand procedure
               mem,  ;

+: []          ( --- )          \ Indexed memory operand
               1 {[]} !! ;

+: >asm        ( --- )          \ Insert value for assembler stack
               cell csp +! ;

+: asm>        ( --- )          \ Delete value from assembler stack
               cell negate csp +! ;


\ ============================================================================
\                           Conditional structures
\ ============================================================================

: ?pairs       ( n n' --- )     \ Check pairs:
                                \ if ... else ... then
                                \ begin ... until
                                \ begin ... again
                                \ begin ... while ... repeat
               <> AE_COND ?aerror ;

: rel8>rel16/32 ( kk --- kk1 )
               0fh and 080h or ;

: a>mark       ( --- | a )          \ Reserve space for forward reference
               here-a asm> 0 w,-a
               ?use32 ifnot{ 0 w,-a } ;

: a<mark       ( --- | a )          \ Save address for backward reference
               here-a asm> ;

: a>resolve    ( a | --- )       \ Resolve forward reference
               >asm here-a over 2+ - ?use32
               if{  2- swap !-a
               else dup ?16 not AE_OFFSET16 ?aerror swap w!-a } ;

: a<resolveCond  ( a | kk --- )     \ Conditional jump backward
               >r >asm dup here-a 1+ - dup ?8
               if{    \ Short variant
                      nip r> c,-a c,-a
               else   \ Full variant
                      drop 0fh c,-a r> rel8>rel16/32 c,-a
                      here-a 2+ - ?use32
                      if{  2- ,-a
                      else dup ?16 not AE_OFFSET16 ?aerror w,-a }
               } ;

: a<resolvejmp ( a | --- )          \ Unconditional jump backward
               >asm dup here-a 1+ - dup ?8
               if{    \ Short variant
                      nip 0ebh c,-a c,-a
               else   \ Full variant
                      drop 0e9h c,-a here-a 2+ - ?use32
                      if{  2- ,-a
                      else dup ?16 not AE_OFFSET16 ?aerror w,-a }
               } ;

+: if          ( k --- | a ? )
               >r asm; 0fh c,-a r> rel8>rel16/32 c,-a a>mark 1 asm> ;

+: then        ( a ? | --- )
               asm; >asm 1 ?pairs a>resolve ;

+: else        ( a ? | --- | a' ?' )
               asm; >asm 1 ?pairs 0e9h c,-a a>mark
               swap a>resolve 1 asm> ;

+: begin       ( --- | a ? )
               asm; a<mark 2 asm> ;

+: until       ( a ? | k --- )
               >r asm; >asm 2 ?pairs r> a<resolvecond ;

+: again       ( a ? | --- )
               asm; >asm 2 ?pairs a<resolvejmp ;

+: while       ( a ? | k --- | a' a'' ? )
               >r asm; 0fh c,-a r> rel8>rel16/32 c,-a >asm
               2 ?pairs a>mark 3 asm> ;

+: repeat      ( a a' ? | --- )
               asm; >asm 3 ?pairs swap a<resolvejmp a>resolve ;

                 76h +constant u>         7eh +constant >
                 75h +constant =          74h +constant <>
                 73h +constant u<         7dh +constant <
                 77h +constant u<=        7fh +constant <=
                 72h +constant u>=        7ch +constant >=
                 75h +constant 0=         74h +constant 0<>
                 79h +constant 0<         78h +constant 0>=
              \  e3h +constant cx<>0

: 0<=          ( --- )       \ ERROR: Conditional does not exist
               true AE_INCOMP ?aerror ;

: 0> 0<= ;                   \ ERROR: Conditional does not exist



\ ============================================================================
\                              Instruction names
\ ============================================================================

: skip|       [ also forth ] bl word [ previous ] drop ;

: |           ( --- )         \ Define instruction table string
               ' skip|
               +create [ also forth ] ,  skip|
                 bl word number
                 skip|
                 base @ >r hex
                 0 ?do bl word ?uppercase dup find
                       if{ nip execute else drop number } c,
                   loop
                 r> base ! [compile] \
                 [ previous ]
               does>
                 save-error asm! asm; ;


\ ============================================================================
\ | Procedure    Instruction  N param          Codes, hex         Comment    |
\ ============================================================================
\ |              |           |         |                         |           |
  |  <<1         |  NOP      |    1    | 90                      |           |
  |  <<1         |  LEAVE    |    1    | C9                      |           |
  |  <<1         |  XLAT     |    1    | D7                      |           |
  |  <<1         |  XLATB    |    1    | D7                      |           |
  |  <<1         |  CLC      |    1    | F8                      |           |
  |  <<1         |  STC      |    1    | F9                      |           |
  |  <<1         |  CMC      |    1    | F5                      |           |
  |  <<1         |  CLD      |    1    | FC                      |           |
  |  <<1         |  STD      |    1    | FD                      |           |
  |  <<1         |  CLI      |    1    | FA                      |           |
  |  <<1         |  STI      |    1    | FB                      |           |
  |  <<1         |  LANF     |    1    | 9F                      |           |
  |  <<1         |  SANF     |    1    | 9E                      |           |
  |  <<1         |  AAA      |    1    | 37                      |           |
  |  <<1         |  AAS      |    1    | 3F                      |           |
  |  <<1         |  DAA      |    1    | 27                      |           |
  |  <<1         |  DAS      |    1    | 2F                      |           |
  |  <<1         |  HLT      |    1    | F4                      |           |
  |  <<1         |  INTO     |    1    | CE                      |           |
  |  <<1         |  WAIT     |    1    | 9B                      |           |
  |  <<1         |  PUSHA    |    1    | 60                      |           |
  |  <<1         |  POPA     |    1    | 61                      |           |
  |  <<1         |  PUSHF    |    1    | 9C                      |           |
  |  <<1         |  POPF     |    1    | 9D                      |           |
  |  <<1         |  IRET     |    1    | CF                      |           |
  |  <<1.1       |  PUSHAD   |    2    | DWORD_SZ 60             |           |
  |  <<1.1       |  POPAD    |    2    | DWORD_SZ 61             |           |
  |  <<1.1       |  CWDE     |    2    | DWORD_SZ 98             |           |
  |  <<1.1       |  CBW      |    2    |  WORD_SZ 98             |           |
  |  <<1.1       |  CDQ      |    2    | DWORD_SZ 99             |           |
  |  <<1.1       |  CWD      |    2    |  WORD_SZ 99             |           |
  |  <<1.1       |  PUSHFD   |    2    | DWORD_SZ 9C             |           |
  |  <<1.1       |  POPFD    |    2    | DWORD_SZ 9D             |           |
  |  <<1.1       |  IRETD    |    2    | DWORD_SZ CF             |           |
  |  <<2         |  AAM      |    2    | D4 0A                   |           |
  |  <<2         |  AAD      |    2    | D5 0A                   |           |
  |  <<2         |  CLTS     |    2    | 0F 06                   |           |
  |  <<2         |  INVD     |    2    | 0F 08                   |           |
  |  <<2         |  WBINVD   |    2    | 0F 09                   |           |
  |  <<2         |  RDMSR    |    2    | 0F 32                   |           |
  |  <<2         |  WRMSR    |    2    | 0F 30                   |           |
  |  <<2         |  RSM      |    2    | 0F AA                   |           |
  |  <<2         |  CPUID    |    2    | 0F A2                   |           |
  |  <<2         |  FCOMPP   |    2    | DE D9                   |           |
  |  <<2         |  FTST     |    2    | D9 E4                   |           |
  |  <<2         |  FUCOMPP  |    2    | DD E9                   |           |
  |  <<2         |  FXAM     |    2    | D9 E5                   |           |
  |  <<2         |  FLDZ     |    2    | D9 EE                   |           |
  |  <<2         |  FLD1     |    2    | D9 E8                   |           |
  |  <<2         |  FLDPI    |    2    | D9 EB                   |           |
  |  <<2         |  FLDL2T   |    2    | D9 E9                   |           |
  |  <<2         |  FLDL2E   |    2    | D9 EA                   |           |
  |  <<2         |  FLDLG2   |    2    | D9 EC                   |           |
  |  <<2         |  FLDLN2   |    2    | D9 EDh                  |           |
  |  <<2         |  FSQRT    |    2    | D9 FA                   |           |
  |  <<2         |  FSCALE   |    2    | D9 FD                   |           |
  |  <<2         |  FXTRACT  |    2    | D9 F4                   |           |
  |  <<2         |  FPREM    |    2    | D9 F8                   |           |
  |  <<2         |  FPREM1   |    2    | D9 F5                   |           |
  |  <<2         |  FRNDINT  |    2    | D9 FC                   |           |
  |  <<2         |  FABS     |    2    | D9 E1                   |           |
  |  <<2         |  FCHS     |    2    | D9 E0                   |           |
  |  <<2         |  FCOS     |    2    | D9 FF                   |           |
  |  <<2         |  FPTAN    |    2    | D9 F2                   |           |
  |  <<2         |  FPATAN   |    2    | D9 F3                   |           |
  |  <<2         |  FSIN     |    2    | D9 FE                   |           |
  |  <<2         |  FSINCOS  |    2    | D9 FB                   |           |
  |  <<2         |  F2XM1    |    2    | D9 F0                   |           |
  |  <<2         |  FYL2X    |    2    | D9 F1                   |           |
  |  <<2         |  FYL2XP1  |    2    | D9 F9                   |           |
  |  <<2         |  FNINIT   |    2    | DB E3                   |           |
  |  <<2         |  FNSTSWAX |    2    | DF E0                   |           |
  |  <<2         |  FNCLEX   |    2    | DB E2                   |           |
  |  <<2         |  FINCSTP  |    2    | D9 F7                   |           |
  |  <<2         |  FDECSTP  |    2    | D9 F6                   |           |
  |  <<2         |  FNOP     |    2    | D9 D0                   |           |
  |  <<2.FLOAT   |  FINIT    |    2    | DB E3                   |           |
  |  <<2.FLOAT   |  FSTSWAX  |    2    | DF E0                   |           |
  |  <<2.FLOAT   |  FCLEX    |    2    | DB E2                   |           |
  |  <<3         |  RET      |    2    | C3 CB                   |           |
  |  <<4         |  CMPS     |    2    |        0 A6             |           |
  |  <<4         |  CMPSB    |    2    |  BYTE_SZ A6             |           |
  |  <<4         |  CMPSW    |    2    |  WORD_SZ A6             |           |
  |  <<4         |  CMPSD    |    2    | DWORD_SZ A6             |           |
  |  <<4         |  LODS     |    2    |        0 AC             |           |
  |  <<4         |  LODSB    |    2    |  BYTE_SZ AC             |           |
  |  <<4         |  LODSW    |    2    |  WORD_SZ AC             |           |
  |  <<4         |  LODSD    |    2    | DWORD_SZ AC             |           |
  |  <<4         |  MOVS     |    2    |        0 A4             |           |
  |  <<4         |  MOVSB    |    2    |  BYTE_SZ A4             |           |
  |  <<4         |  MOVSW    |    2    |  WORD_SZ A4             |           |
  |  <<4         |  MOVSD    |    2    | DWORD_SZ A4             |           |
  |  <<4         |  SCAS     |    2    |        0 AE             |           |
  |  <<4         |  SCASB    |    2    |  BYTE_SZ AE             |           |
  |  <<4         |  SCASW    |    2    |  WORD_SZ AE             |           |
  |  <<4         |  SCASD    |    2    | DWORD_SZ AE             |           |
  |  <<4         |  STOS     |    2    |        0 AA             |           |
  |  <<4         |  STOSB    |    2    |  BYTE_SZ AA             |           |
  |  <<4         |  STOSW    |    2    |  WORD_SZ AA             |           |
  |  <<4         |  STOSD    |    2    | DWORD_SZ AA             |           |
  |  <<4         |  INS      |    2    |        0 6C             |           |
  |  <<4         |  INSB     |    2    |  BYTE_SZ 6C             |           |
  |  <<4         |  INSW     |    2    |  WORD_SZ 6C             |           |
  |  <<4         |  INSD     |    2    | DWORD_SZ 6C             |           |
  |  <<4         |  OUTS     |    2    |        0 6E             |           |
  |  <<4         |  OUTSB    |    2    |  BYTE_SZ 6E             |           |
  |  <<4         |  OUTSW    |    2    |  WORD_SZ 6E             |           |
  |  <<4         |  OUTSD    |    2    | DWORD_SZ 6E             |           |
  |  <<5         |  PUSH     |    5    | 50 FF 06 06 00          |           |
  |  <<5         |  POP      |    5    | 58 8F 00 07 01          |           |
  |  <<6.in      |  IN       |    2    | EC E4                   |           |
  |  <<6.out     |  OUT      |    2    | EE E6                   |           |
  |  <<7         |  INT      |    0    |                         |           |
  |  <<8         |  JO       |    2    | 00 70                   |           |
  |  <<8         |  JNO      |    2    | 00 71                   |           |
  |  <<8         |  JB       |    2    | 00 72                   |           |
  |  <<8         |  JNAE     |    2    | 00 72                   |           |
  |  <<8         |  JNB      |    2    | 00 73                   |           |
  |  <<8         |  JAE      |    2    | 00 73                   |           |
  |  <<8         |  JE       |    2    | 00 74                   |           |
  |  <<8         |  JZ       |    2    | 00 74                   |           |
  |  <<8         |  JNE      |    2    | 00 75                   |           |
  |  <<8         |  JNZ      |    2    | 00 75                   |           |
  |  <<8         |  JBE      |    2    | 00 76                   |           |
  |  <<8         |  JNA      |    2    | 00 76                   |           |
  |  <<8         |  JNBE     |    2    | 00 77                   |           |
  |  <<8         |  JA       |    2    | 00 77                   |           |
  |  <<8         |  JS       |    2    | 00 78                   |           |
  |  <<8         |  JNS      |    2    | 00 79                   |           |
  |  <<8         |  JP       |    2    | 00 7A                   |           |
  |  <<8         |  JPE      |    2    | 00 7A                   |           |
  |  <<8         |  JNP      |    2    | 00 7B                   |           |
  |  <<8         |  JPO      |    2    | 00 7B                   |           |
  |  <<8         |  JL       |    2    | 00 7C                   |           |
  |  <<8         |  JNGE     |    2    | 00 7C                   |           |
  |  <<8         |  JNL      |    2    | 00 7D                   |           |
  |  <<8         |  JGE      |    2    | 00 7D                   |           |
  |  <<8         |  JLE      |    2    | 00 7E                   |           |
  |  <<8         |  JNG      |    2    | 00 7E                   |           |
  |  <<8         |  JNLE     |    2    | 00 7F                   |           |
  |  <<8         |  JG       |    2    | 00 7F                   |           |
  |  <<8         |  LOOP     |    2    | 00 E2                   |           |
  |  <<8         |  LOOPZ    |    2    | 00 E1                   |           |
  |  <<8         |  LOOPE    |    2    | 00 E1                   |           |
  |  <<8         |  LOOPNZ   |    2    | 00 E0                   |           |
  |  <<8         |  LOOPNE   |    2    | 00 E0                   |           |
  |  <<8         |  JCXZ     |    2    |  WORD_SZ E3             |           |
  |  <<8         |  JECXZ    |    2    | DWORD_SZ E3             |           |
  |  <<9         |  JMP      |    4    | E9 04 EA 05             |           |
  |  <<9         |  CALL     |    4    | E8 02 9A 03             |           |
  |  <<10        |  INC      |    3    | FE 00 40                |           |
  |  <<10        |  DEC      |    3    | FE 01 48                |           |
  |  <<10        |  DIV      |    2    | F6 06                   |           |
  |  <<10        |  IDIV     |    2    | F6 07                   |           |
  |  <<10        |  NOT      |    2    | F6 02                   |           |
  |  <<10        |  NEG      |    2    | F6 03                   |           |
  |  <<11        |  BSWAP    |    0    |                         |           |
  |  <<12        |  IMUL     |    0    |                         |           |
  |  <<13.R/M    |  SETO     |    3    | 90 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNO    |    3    | 91 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETB     |    3    | 92 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNAE   |    3    | 92 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNB    |    3    | 93 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETAE    |    3    | 93 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETE     |    3    | 94 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETZ     |    3    | 94 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNE    |    3    | 95 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNZ    |    3    | 95 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETBE    |    3    | 96 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNA    |    3    | 96 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNBE   |    3    | 97 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETA     |    3    | 97 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETS     |    3    | 98 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNS    |    3    | 99 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETP     |    3    | 9A 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETPE    |    3    | 9A 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNP    |    3    | 9B 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETPO    |    3    | 9B 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETL     |    3    | 9C 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNGE   |    3    | 9C 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNL    |    3    | 9D 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETGE    |    3    | 9D 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETLE    |    3    | 9E 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNG    |    3    | 9E 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETNLE   |    3    | 9F 00 BYTE_SZ           |           |
  |  <<13.R/M    |  SETG     |    3    | 9F 00 BYTE_SZ           |           |
  |  <<13.R/M    |  LLDT     |    3    | 00 02 WORD_SZ           |           |
  |  <<13.R/M    |  LMSW     |    3    | 01 06 WORD_SZ           |           |
  |  <<13.R/M    |  LTR      |    3    | 00 03 WORD_SZ           |           |
  |  <<13.R/M    |  SLDT     |    3    | 00 00 WORD_SZ           |           |
  |  <<13.R/M    |  SMSW     |    3    | 01 04 WORD_SZ           |           |
  |  <<13.R/M    |  STR      |    3    | 00 01 WORD_SZ           |           |
  |  <<13.R/M    |  VERR     |    3    | 00 04 WORD_SZ           |           |
  |  <<13.R/M    |  VERW     |    3    | 00 05 WORD_SZ           |           |
  |  <<13.M      |  INVLPG   |    3    | 01 07 00                |           |
  |  <<13.M      |  LGDT     |    3    | 01 02 PWORD_SZ          |           |
  |  <<13.M      |  LIDT     |    3    | 01 03 PWORD_SZ          |           |
  |  <<13.M      |  SGDT     |    3    | 01 00 PWORD_SZ          |           |
  |  <<13.M      |  SIDT     |    3    | 01 01 PWORD_SZ          |           |
  |  <<13.M      | CMPXCHG8B |    3    | C7 01 QWORD_SZ          |           |
  |  <<14        |  FBLD     |    3    | DF 04 TWORD_SZ          |           |
  |  <<14        |  FBSTP    |    3    | DF 06 TWORD_SZ          |           |
  |  <<14        |  FNSTSW   |    3    | DD 07  WORD_SZ          |           |
  |  <<14        |  FNLDCW   |    3    | DD 05  WORD_SZ          |           |
  |  <<14        |  FSTCW    |    3    | D9 07  WORD_SZ          |           |
  |  <<14        |  FNSTENV  |    3    | D9 06 00                |           |
  |  <<14        |  FLDENV   |    3    | D9 04 00                |           |
  |  <<14        |  FNSAVE   |    3    | DD 06 00                |           |
  |  <<14        |  FRSTOR   |    3    | DD 04 00                |           |
  |  <<14        |  FLDSW    |    3    | D9 05  WORD_SZ          |           |
  |  <<14.FLOAT  |  FSTSW    |    3    | DD 07  WORD_SZ          |           |
  |  <<14.FLOAT  |  FLDCW    |    3    | DD 05  WORD_SZ          |           |
  |  <<14.FLOAT  |  FSTENV   |    3    | D9 06 00                |           |
  |  <<14.FLOAT  |  FSAVE    |    3    | DD 06 00                |           |
  |  <<15        |  FIST     |    2    | DF 02                   |           |
  |  <<15        |  FICOM    |    2    | DE 02                   |           |
  |  <<15        |  FICOMP   |    2    | DE 03                   |           |
  |  <<15        |  FIADD    |    2    | DE 00                   |           |
  |  <<15        |  FISUB    |    2    | DE 04                   |           |
  |  <<15        |  FISUBR   |    2    | DE 05                   |           |
  |  <<15        |  FIMUL    |    2    | DE 01                   |           |
  |  <<15        |  FIDIV    |    2    | DE 06                   |           |
  |  <<15        |  FIDIVR   |    2    | DE 07                   |           |
  |  <<16        |  FXCH     |    2    | D9 C8                   |           |
  |  <<16        |  FUCOM    |    2    | DD E0                   |           |
  |  <<16        |  FUCOMP   |    2    | DD E8                   |           |
  |  <<16        |  FADDP    |    2    | DE C0                   |           |
  |  <<16        |  FSUBP    |    2    | DE E8                   |           |
  |  <<16        |  FSUBRP   |    2    | DE E0                   |           |
  |  <<16        |  FMULP    |    2    | DE C8                   |           |
  |  <<16        |  FDIVP    |    2    | DE F8                   |           |
  |  <<16        |  FDIVRP   |    2    | DE F0                   |           |
  |  <<16        |  FFREE    |    2    | DD C0                   |           |
  |  <<17        |  FILD     |    3    | 00 00 05                |           |
  |  <<17        |  FISTP    |    3    | 03 03 07                |           |
  |  <<18        |  FST      |    6    | D9 02 DD 02 DD D0       |           |
  |  <<18        |  FCOM     |    6    | D8 02 DC 02 D8 D0       |           |
  |  <<18        |  FCOMP    |    6    | D8 03 DC 03 D8 D8       |           |
  |  <<18.1      |  FADD     |    6    | D8 00 DC 00 D8 C0       |           |
  |  <<18.1      |  FSUB     |    6    | D8 04 DC 04 D8 E8       |           |
  |  <<18.1      |  FSUBR    |    6    | D8 05 DC 05 D8 E0       |           |
  |  <<18.1      |  FMUL     |    6    | D8 01 DC 01 D8 C8       |           |
  |  <<18.1      |  FDIV     |    6    | D8 06 DC 04 D8 F8       |           |
  |  <<18.1      |  FDIVR    |    6    | D8 07 DC 07 D8 F0       |           |
  |  <<19        |  FLD      |    8    | D9 00 DD 00 D9 C0 DB 05 |           |
  |  <<19        |  FSTP     |    8    | D9 03 DD 03 DD C8 DB 07 |           |
  |  <<20        |  ENTER    |    0    |                         |           |
  |  <<21        |  LEA      |    1    | 8D                      |           |
  |  <<21        |  LDS      |    1    | C5                      |           |
  |  <<21        |  LES      |    1    | C4                      |           |
  |  <<21        |  LFS      |    1    | B4                      |           |
  |  <<21        |  LGS      |    1    | B5                      |           |
  |  <<21        |  LSS      |    1    | B2                      |           |
  |  <<21        |  BOUND    |    1    | 62                      |           |
  |  <<22        |  BSF      |    1    | BC                      |           |
  |  <<22        |  BSR      |    1    | BD                      |           |
  |  <<22        |  LAR      |    1    | 02                      |           |
  |  <<22        |  LSL      |    1    | 03                      |           |
  |  <<22.1      |  XADD     |    1    | C0                      |           |
  |  <<22.1      |  CMPXCHG  |    1    | B0                      |           |
  |  <<23        |  MOVZX    |    1    | B6                      |           |
  |  <<23        |  MOVSX    |    1    | BE                      |           |
  |  <<24        |  ARPL     |    0    |                         |           |
  |  <<25        |  BT       |    2    | 04 A3                   |           |
  |  <<25        |  BTS      |    2    | 05 AB                   |           |
  |  <<25        |  BTR      |    2    | 06 B3                   |           |
  |  <<25        |  BTC      |    2    | 07 BB                   |           |
  |  <<26        |  ROL      |    1    | 00                      |           |
  |  <<26        |  ROR      |    1    | 01                      |           |
  |  <<26        |  RCL      |    1    | 02                      |           |
  |  <<26        |  RCR      |    1    | 03                      |           |
  |  <<26        |  SHL      |    1    | 04                      |           |
  |  <<26        |  SAL      |    1    | 04                      |           |
  |  <<26        |  SHR      |    1    | 05                      |           |
  |  <<26        |  SAR      |    1    | 07                      |           |
  |  <<27        |  XCHG     |    0    |                         |           |
  |  <<28        |  ADD      |    4    | 00 80 00 04             |           |
  |  <<28        |  ADC      |    4    | 10 80 02 14             |           |
  |  <<28        |  AND      |    4    | 20 80 04 24             |           |
  |  <<28        |  OR       |    4    | 08 80 01 0C             |           |
  |  <<28        |  SUB      |    4    | 28 80 05 2C             |           |
  |  <<28        |  SBB      |    4    | 18 80 03 1C             |           |
  |  <<28        |  XOR      |    4    | 30 80 06 34             |           |
  |  <<28        |  CMP      |    4    | 38 80 07 3C             |           |
  |  <<29        |  TEST     |    4    | 84 F6 00 A8             |           |
  |  <<30        |  SHLD     |    2    | A4 A5                   |           |
  |  <<30        |  SHRD     |    2    | AC AD                   |           |
  |  <<31        |  MOV      |    0    |                         |           |
\ |              |           |         |                         |           |
\ ===========================================================================

+: CALLF       ( --- )          \ Short variant call far
               call far ;

+: JMPF        ( --- )          \ Short variant jmp far
               jmp far ;

+: RETF        ( --- )          \ Short variant ret far
               ret far ;


\ ============================================================================
\                              High level words
\ ============================================================================

 forth definitions assembler

: beg-asm      ( --- )       \ Start assembler definition
               also assembler
               0eeh !csp lbsclear
               prefix prefixes 2+ erase
               0reset resup ;

: label        ( name --- )  \ Start procedure definition
                create hide beg-asm ;

: code          ( name --- ) \ Start low level definition
                build> hide beg-asm ;

: asm{          ( --- )      \ Include assembler into the forth word
                csp @ [compile] [
                beg-asm ; immediate

\ : ;code         ( -- )     \ Start assembler instructions
\                ?csp compile (;code) here x,
\                [compile] [ reveal beg-asm ; immediate

 assembler definitions

: next         ( --- )       \ Macro NEXT
               ret ;

: end-asm      ( --- )       \ End of assembler definition
               [ also forth ] asm; ?csp
               0eeh <> AE_INCOMP ?aerror
               [ previous ] frcheck previous ;

: end-code     ( --- )          \ End of low level definition
               end-asm reveal ;

: }            ( --- )          \ End of ASM{ definition
               end-asm csp ! ] ;

: next,        next ;
: next;        next end-code ;

previous definitions

head+

decimal warning on


