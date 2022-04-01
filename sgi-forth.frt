/*****************************************************************************
	SGI-FORTH for Intel Pentium 80586
	Version 1.00

	Copyright (c) 2006 Kuznetsov O.V. All rights reserved.
 	Licensed under the Apache License, Version 2.0
*****************************************************************************/

FLOAD target.frt

TARGET

  HEX
  100 CSEG

        0FFF0  EQU  mem-end         \ Last memory address
         0800  EQU  sp-len          \ Data stack depth
         0400  EQU  rp-len          \ Return stack depth
          088  EQU  tib-len         \ TIB length

  BEG-ASM
        NOP  JMP 119 ( cold )              \ 100  cold start
  END-ASM

L: _init                  0  t,            \ 104  Initial actions
L: _main                  0  t,            \ 106  User program
L: _bye                   0  t,            \ 108  BYE  program

L: r0   mem-end              t,            \ 10A  R0
L: s0   mem-end rp-len -     t,            \ 10C  S0
L: tib  mem-end rp-len -
        sp-len - tib-len -   t,            \ 10E <tib>
L: lastn                  0  t,            \ 110  LASTN
L: dp                     0  t,            \ 112  DP

  BEG-ASM

\ NEXT of address interpreter
L: next       WORD LODS  MOV BX, AX  JMP [BX]     \ 114  next address

\ Entry to cold restart
L: cold       MOV SI, # _init       \ Initial IP  119  cold address
              MOV SP, s0            \ Initial SP
              MOV BP, r0            \ Initial RP
              MOV AX, CS
              MOV DS, AX            \ Initial DS
              MOV ES, AX            \ Initial ES
              MOV SS, AX            \ Initial SS
              CLD                   \ Direction
              jmp next
  END-ASM

  BEG-ASM

L: DO-CREATE
L: DO-VARIABLE        INC BX  INC BX  PUSH BX  NEXT,
L: DO-CONSTANT        INC BX  INC BX  PUSH [BX]  NEXT,
L: DO-:               INC BX  INC BX  DEC BP  DEC BP
                      MOV [BP], SI  MOV SI, BX  NEXT,
L: DO-DEFER           INC BX  INC BX  MOV BX, [BX]  JMP [BX]

  END-ASM

( FORTH library )

    dp  CONSTANT DP
  lastn CONSTANT LASTN

    s0  CONSTANT S0
    r0  CONSTANT R0

    -1  CONSTANT  -1
     0  CONSTANT  0
     1  CONSTANT  1

CODE SP@       ( --- a )
               MOV AX, SP  PUSH AX  NEXT,  END-CODE
CODE RP@       ( --- a )
               PUSH BP  NEXT,  END-CODE
CODE SP!       ( a --- )
               POP SP  NEXT,  END-CODE
CODE RP!       ( a --- )
               POP BP  NEXT,  END-CODE
CODE >R        ( n --- )
               POP BX  DEC BP  DEC BP  MOV [BP], BX  NEXT,  END-CODE
CODE R>        ( --- n )
               PUSH [BP]  INC BP  INC BP  NEXT,  END-CODE
CODE R@        ( --- n )
               PUSH [BP]  NEXT,  END-CODE
CODE RDROP     ( --- )
               INC BP  INC BP  NEXT,  END-CODE

CODE -ROT      ( n n1 n2 --- n2 n n1 )
               POP DX  POP BX  POP AX
               PUSH DX  PUSH AX  PUSH BX  NEXT,  END-CODE
CODE ?DUP      ( n --- n n  0 --- 0 )
               POP CX  PUSH CX JCXZ 1$  PUSH CX  1$: NEXT,  END-CODE
CODE DROP      ( n --- )
               INC SP  INC SP  NEXT,  END-CODE
CODE DUP       ( n --- n n )
               POP AX  PUSH AX  PUSH AX  NEXT,  END-CODE
CODE OVER      ( n n1 --- n n1 n )
               MOV BX, SP PUSH SS: 2 [BX]  NEXT,  END-CODE
CODE NIP       ( n n1 --- n1 )
               POP AX  POP DX  PUSH AX  NEXT,  END-CODE
CODE ROT       ( n n1 n2 --- n1 n2 n )
               POP DX  POP BX  POP AX
               PUSH BX  PUSH DX  PUSH AX  NEXT, END-CODE
CODE SWAP      ( n n1 --- n1 n )
               POP DX  POP AX  PUSH DX  PUSH AX  NEXT,  END-CODE
CODE PICK      ( n --- n1 )
               POP BX  ADD BX, BX  ADD BX, SP
               MOV SS: AX, [BX]  PUSH AX  NEXT,  END-CODE

CODE 2DROP     ( n n1 --- )
               ADD SP, # 4  NEXT,  END-CODE
CODE 2DUP      ( n n1 --- n n1 n n1 )
               MOV BX, SP  PUSH SS: 2 [BX]  PUSH SS: [BX]  NEXT,  END-CODE
CODE 2OVER     ( n n1 n2 n3 --- n n1 n2 n3 n n1 )
               MOV BX, SP  PUSH SS: 6 [BX]  PUSH SS: 4 [BX]  NEXT,  END-CODE
CODE 2SWAP     ( n n1 n2 n3 --- n2 n3 n n1 )
               POP AX  POP BX  POP CX  POP DX
               PUSH BX  PUSH AX  PUSH DX  PUSH CX  NEXT,  END-CODE

CODE +         ( n n1 --- n2 )
               POP AX  POP BX  ADD AX, BX  PUSH AX  NEXT,  END-CODE
CODE -         ( n n1 --- n2 )
               POP BX  POP AX  SUB AX, BX  PUSH AX  NEXT,  END-CODE
CODE 1+        ( n --- n1 )
               POP AX  INC AX  PUSH AX  NEXT,  END-CODE
CODE 1-        ( n --- n1 )
               POP AX  DEC AX  PUSH AX  NEXT,  END-CODE
CODE 2+        ( n --- n1 )
               POP AX  INC AX  INC AX  PUSH AX  NEXT,  END-CODE
CODE 2-        ( n --- n1 )
               POP AX  DEC AX  DEC AX  PUSH AX  NEXT,  END-CODE
CODE 2*        ( n --- n1 )
               POP AX  SAL AX, # 1  PUSH AX  NEXT,  END-CODE
CODE 2/        ( n --- n1 )
               POP AX  SAR AX, # 1  PUSH AX  NEXT,  END-CODE

CODE EXIT      MOV SI, [BP]  INC BP  INC BP  NEXT,  END-CODE

  BEG-ASM
        L: _M/MOD ( DX:AX=DIV BX=MOD AX=-ACTH DX=OCT)
               OR BX, BX  JZ 1$  MOV CX, BX  MOV DI, DX
               XOR DI, BX  OR DX, DX  JNS 2$
               NOT DX  NOT AX  ADD AX, # 1  ADC  DX, # 0
           2$: OR BX, BX  JNS 4$  NEG BX
           4$: CMP DX, BX  JNB 1$  DIV BX
               OR CX, CX  JNS 5$  NEG DX
           5$: OR DI, DI  JNS 3$  NEG AX
               OR DX, DX  JZ 3$  DEC AX
               SUB CX, DX  MOV DX, CX
           3$: RET
           1$: MOV AX, # FFFF  MOV DX, AX  RET
  END-ASM

CODE UM/MOD    ( ud u -- r q )
               POP BX  POP DX POP AX
               CMP DX, BX  JNB 1$  DIV BX  JMP 2$
           1$: MOV AX, # FFFF  MOV DX, AX
           2$: PUSH DX  PUSH AX  NEXT,  END-CODE
: MU/MOD       ( ud u --- u1 ud1 )
               >R 0 R@ UM/MOD R> SWAP >R UM/MOD R> ;
CODE /         ( n n1 --- n2 )
               POP BX  POP AX  CWD
               MOV CX, BX  OR CX, DX  JS 1$
               CMP DX, BX  JNB 2$  IDIV BX  PUSH AX  NEXT,
           1$: CALL _M/MOD  PUSH AX  NEXT,
           2$: MOV AX, # FFFF  PUSH AX NEXT,  END-CODE
CODE UM*       ( u u1 --- ud )
               POP AX  POP BX  MUL BX  PUSH AX  PUSH DX
               NEXT,  END-CODE
CODE M*        ( n n1 --- d )
               POP AX  POP BX  IMUL BX  PUSH AX  PUSH DX
               NEXT,  END-CODE
CODE */MOD     ( n n1 n2 --- rem q )
               POP CX  POP AX  POP BX  IMUL BX  MOV BX, CX
               CALL _M/MOD  PUSH DX  PUSH AX  NEXT,  END-CODE
CODE *         ( n n1 --- n3 )
               POP AX  POP BX  IMUL BX  PUSH AX  NEXT,  END-CODE
: */           ( n n1 n2 --- n3 )
               */MOD NIP ;

CODE DNEGATE   ( d --- d1 )
 L: DO-DNEGATE POP BX  POP CX  NOT BX  NOT CX  ADD CX, # 1
               ADC BX, # 0  PUSH CX  PUSH BX  NEXT,  END-CODE
CODE DABS      ( d --- d1 )
               POP AX  PUSH AX  OR AX, AX  JS DO-DNEGATE
               NEXT,  END-CODE
CODE D+        ( d d1 --- d2 )
               POP AX  POP DX  POP BX  POP CX  ADD DX, CX
               ADC AX, BX  PUSH DX  PUSH AX  NEXT, END-CODE
CODE S>D       ( n --- d )
               POP AX  CWD  PUSH AX  PUSH DX  NEXT,  END-CODE

CODE AND       ( n n1 --- n2 )
               POP AX  POP BX  AND AX, BX  PUSH AX  NEXT,  END-CODE
CODE OR        ( n n1 --- n2 )
               POP AX  POP BX  OR AX, BX  PUSH AX  NEXT,  END-CODE
CODE XOR       ( n n1 --- n2 )
               POP AX  POP BX  XOR AX, BX  PUSH AX  NEXT,  END-CODE
CODE NOT       ( n --- n1 )
               POP AX  NOT AX  PUSH AX  NEXT,  END-CODE
CODE TOGGLE    ( a b --- )
               POP AX  POP BX  XOR [BX], AL  NEXT,  END-CODE

CODE 0<        ( n --- ? )
               POP AX  OR AX, AX  MOV AX, # -1
               JS 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE 0>        ( n --- ? )
               POP AX  OR AX, AX  MOV AX, # 0
               JZ 1$  JS 1$  DEC AX  1$: PUSH AX  NEXT,  END-CODE
CODE 0=        ( n --- ? )
               POP AX  OR AX, AX  MOV AX, # -1
               JZ 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE 0<>       ( n --- ? )
               POP AX  OR AX, AX  MOV AX, # -1
               JNZ 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE <         ( n n1 --- ? )
               POP BX  POP AX  SUB AX, BX  MOV AX, # -1
               JL 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE >         ( n n1 --- ? )
               POP AX  POP BX  SUB AX, BX  MOV AX, # -1
               JL 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE =         ( n n1 --- ? )
               POP AX  POP BX  CMP AX, BX  MOV AX, # -1
               JZ 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE <>        ( n n1 --- ? )
               POP AX  POP BX  CMP AX, BX  MOV AX, # -1
               JNZ 1$  INC AX  1$: PUSH AX  NEXT,  END-CODE
CODE U<        ( u u1 --- ? )
               POP AX  POP DX  SUB DX, AX  MOV AX, # -1
               JB 1$  INC AX  1$: PUSH AX  NEXT, END-CODE
CODE MAX       ( n n1 --- ? )
               POP AX  POP BX  CMP AX, BX
               JL 1$  XCHG BX, AX  1$: PUSH BX  NEXT,  END-CODE
CODE MIN       ( n n1 --- ? )
               POP AX  POP BX  CMP AX, BX
               JL 1$  XCHG BX, AX  1$: PUSH AX  NEXT,  END-CODE

CODE !         ( n a --- )
               POP BX  POP [BX]  NEXT,  END-CODE
CODE @         ( a --- n )
               POP BX  PUSH [BX]  NEXT,  END-CODE
CODE C!        ( n a --- )
               POP BX  POP AX  MOV [BX], AL  NEXT,  END-CODE
CODE C@        ( a --- n )
               POP BX  MOV AL, [BX]  MOV AH, # 0  PUSH AX  NEXT,  END-CODE
CODE +!        ( n a --- )
               POP BX  POP AX  ADD [BX], AX  NEXT,  END-CODE
CODE 1+!       ( a --- )
               POP BX  INC WORD [BX]  NEXT,  END-CODE
CODE 0!        ( a --- )
               POP BX  MOV WORD [BX], # 0  NEXT,  END-CODE
CODE 2!        ( d a --- )
               POP BX  POP [BX]  POP 2 [BX]  NEXT,  END-CODE
CODE 2@        ( a --- d )
               POP BX  PUSH 2 [BX]  PUSH [BX]  NEXT,  END-CODE

CODE @L        ( seg a --- n )
               POP BX  POP ES  MOV ES: AX, [BX]  PUSH AX
               MOV AX, DS  MOV ES, AX  NEXT,  END-CODE
CODE !L        ( n seg a --- )
               POP DI  POP ES  POP AX  STOSW
               MOV AX, DS  MOV ES, AX  NEXT,  END-CODE
CODE C@L       ( seg a --- n )
               POP BX  POP ES  MOV ES: AL, [BX]
               SUB AH, AH  PUSH AX  MOV AX, DS  MOV ES, AX  NEXT, END-CODE
CODE C!L       ( n seg a --- )
               POP DI  POP ES  POP AX  STOSB
               MOV AX, DS  MOV ES, AX  NEXT, END-CODE

CODE CMOVE     ( a a1 # --- )
               MOV BX, SI  POP CX  POP DI  POP SI  JCXZ 1$
               REP MOVSB  1$: MOV SI, BX  NEXT,  END-CODE
CODE CMOVE>    ( a a1 # --- )
               MOV BX, SI  POP CX  POP DI  POP SI  JCXZ 1$
               MOV AX, CX  DEC AX  ADD DI, AX  ADD  SI, AX
               STD  REP MOVSB  CLD  1$: MOV SI, BX  NEXT,  END-CODE

CODE CS@       ( --- a )
               PUSH CS  NEXT,  END-CODE

CODE LIT       LODSW  PUSH AX  NEXT,  END-CODE
CODE DLIT      LODSW  MOV BX, AX LODSW  PUSH AX PUSH BX  NEXT,  END-CODE

CODE BRANCH    L: DO-BRANCH
               ADD SI, [SI]  NEXT,  END-CODE
CODE ?BRANCH   L: DO-?BRANCH
               POP CX  JCXZ DO-BRANCH  INC SI  INC SI  NEXT,  END-CODE

CODE (DO)      L: DO-(DO)
               POP DX  POP BX
 L: _(DO)      XCHG BP, SP  MOV AX, SI  ADD AX, [SI]
               PUSH AX  INC SI  INC SI
               MOV AX, # 8000  SUB AX, BX  PUSH AX
               ADD DX, AX  PUSH DX  XCHG BP, SP
               NEXT,  END-CODE

CODE (?DO)     L: DO-(?DO)
               POP DX  POP BX  CMP DX, BX  JNZ _(DO)
               ADD SI, [SI]  NEXT,  END-CODE

CODE (LOOP)    L: DO-(LOOP)
               INC WORD [BP]  JO 1$  ADD SI, [SI]  NEXT,
 L: _(LOOP)    1$:  ADD BP, # 6  INC SI INC SI  NEXT,  END-CODE

CODE (+LOOP)   L: DO-(+LOOP)
               POP BX  ADD [BP], BX JO _(LOOP)
               ADD SI, [SI]  NEXT,  END-CODE

CODE LEAVE     MOV SI, 4 [BP]  ADD BP, # 6  NEXT, END-CODE
CODE I         MOV AX, [BP]  SUB AX, 2 [BP]  PUSH AX  NEXT,  END-CODE
CODE J         MOV AX, 6 [BP]  SUB AX, 8 [BP]  PUSH AX  NEXT,  END-CODE

CODE EXECUTE   POP BX  JMP [BX]  END-CODE

CODE COUNT     ( a --- a1 # )
               POP BX  MOV AL, [BX]  INC BX  PUSH BX
               XOR AH, AH  PUSH AX  NEXT,  END-CODE
CODE FILL      ( a # c --- )
               POP AX  POP CX  POP DI JCXZ 1$
               MOV BX, DS  MOV ES, BX  REP STOSB
           1$: MOV AX, DS  MOV ES, AX  NEXT,  END-CODE


     20 CONSTANT BL

: BLANK        ( a # --- )   BL FILL ;
: ERASE        ( a # --- )   0 FILL ;

CODE -TRAILING ( a # --- a #1 )
               POP CX  POP DI  PUSH DI  ADD DI, CX  DEC DI
               MOV AL, # 20  STD  REPZ SCASB  CLD  JZ 1$  INC CX
           1$: PUSH CX  NEXT,  END-CODE

CODE DIGIT     ( c b -- n -1 / 0 )
               POP DX  POP AX  XOR BX, BX  SUB AL, # 30  JB 2$
               CMP AL, # 9  JBE 1$                ( "0"-"9" )
               SUB AL, # 7  CMP AL, # 0A  JB 2$   ( >= "A" )
           1$: CMP AL, DL  JAE 2$                 ( < BASE )
               SUB AH, AH  PUSH AX  DEC BX
           2$: PUSH BX  NEXT,  END-CODE

CODE FIND      ( a1 --- a2 ? / a1 0 )
               POP DX  PUSH SI  MOV SI, DX  XOR AX, AX
               LODSB  MOV CX, AX  MOV DX, SI  XOR DI, DI  JCXZ 5$
               MOV  DI, LASTN
           1$: \ DX = a1+1, DI = lfa, CX = count
               OR  DI, DI  JZ 5$  MOV BX, DI  ADD DI, # 2
               MOV AL, [DI]  TEST AL, # 20  JNZ 2$
               AND AL, # 01F  CMP AL, CL  JNE  2$
               INC DI  MOV SI, DX  REPZ CMPSB  JZ 3$
               \ not found
               MOV CL, AL  2$: MOV DI, [BX]  JMP 1$
           3$: \ found
               MOV DI, BX  ADD DI, # 2
           5$:  \ DX = a1+1, DI = nfa/0
               POP SI  XOR AX, AX  OR DI, DI  JZ 7$
               XOR BX, BX  MOV  BL, [DI]  MOV AX, # -1
               TEST  BL, # 040  JZ 6$  MOV AX, # 1
           6$: AND BL, # 01F  ADD DI, BX  INC DI
               PUSH DI PUSH AX NEXT,
           7$: DEC DX  PUSH DX  PUSH AX  NEXT,  END-CODE

CODE HERE      PUSH DP  NEXT, END-CODE
: PAD          HERE 100 + ;

CODE ALLOT     ( n --- )
               POP AX  ADD  DP , AX  NEXT, END-CODE
CODE ,         ( n --- )
               POP AX  MOV BX, DP  MOV [BX], AX
               ADD WORD DP , # 2  NEXT, END-CODE
CODE C,        ( c --- )
               POP AX  MOV BX, DP  MOV [BX], AL
               ADD BYTE DP , # 1  NEXT,  END-CODE
CODE ",        ( a --- )
               MOV DX, SI  POP SI  XOR CX, CX
               MOV CL, [SI]  INC CX  MOV DI, DP
               REP MOVSB  MOV DP , DI  MOV SI, DX  NEXT,  END-CODE

CODE BYE       ( --- )
               INT 20  END-CODE

CODE NULL      NEXT, END-CODE

CODE PCKEY     ( --- scan ascii )
           2$: MOV AX, # 100  INT 16  JZ 2$
               MOV AX, # 0  INT 16  OR AX, AX  JZ 2$ ( ctrl/break)
               XOR BX, BX  MOV BL, AH  XOR AH, AH
               PUSH BX  PUSH AX  NEXT,  END-CODE

: KEY          ( --- c )
               PCKEY SWAP DROP ;

CODE TYPE      ( a # --- )
               POP CX  POP DX  PUSH ES  PUSH SI  PUSH BP
               MOV AH, # 040  MOV BX, # 2  INT 21
               POP BP  POP SI  POP ES  CLD  NEXT,  END-CODE

CODE EMIT      ( c --- )
               POP DX  PUSH ES  PUSH SI  PUSH BP
               MOV AH, # 02  INT 21  POP BP  POP SI
               POP ES  CLD  NEXT,  END-CODE

: CR           0D EMIT  0A EMIT ;
: BELL         07 EMIT ;
: BACKSPACE    08 EMIT ;
: SPACE        BL EMIT ;
: SPACES       0 ?DO SPACE LOOP ;

    tib CONSTANT <TIB>
tib-len CONSTANT TIB-LEN
        VARIABLE #TIB

        VARIABLE SPAN
        VARIABLE >IN

: TIB          <TIB> @ ;

: EXPECT       ( a # --- )
               SPAN 0! 0
               ?DO KEY
                   DUP 8 =                        \ BACKSPACE?
                   IF DROP SPAN @                 \ String not empty?
                        IF   -1 SPAN +! 8 EMIT    \ Clear
                             SPACE 8 EMIT         \ last char
                        ELSE 7 EMIT               \ Bell
                        THEN R> 2- >R             \ Decrement counter
                   ELSE DUP 0D =                  \ CR?
                        IF DROP SPACE LEAVE       \ Space
                        ELSE DUP EMIT             \ Echo
                             OVER SPAN @ + C!     \ Save char
                             SPAN 1+!
                        THEN
                   THEN
               LOOP 0 SWAP SPAN @ + C! ;          \ Null at the end

        DEFER ABORT

        CREATE F-MSG
( 00 ) " -?"                               DROP
( 01 ) " is not unique"                    DROP
( 02 ) " Stack is empty"                   DROP
( 03 ) " stack UNDERFLOW"                  DROP
( 04 ) " conditionals not finished"        DROP
( 05 ) " execution ONLY"                   DROP
( 06 ) " compilation ONLY"                 DROP
( 07 ) " conditionals wrong"               DROP

: MESSAGE      SWAP 0 ?DO COUNT + LOOP COUNT TYPE ;

: SHOW-ERROR   CR HERE COUNT TYPE SPACE SPACE MESSAGE ABORT ;
: ERROR        F-MSG SHOW-ERROR ;
: ?ERROR       SWAP IF ERROR ELSE DROP THEN ;

CODE ENCLOSE   ( a  c  ---  a #1 #2 #3 )
               POP AX  POP BX  PUSH BX SUB AH, AH
               MOV DX, # -1  DEC BX
           1$: INC BX  INC DX  CMP AL, [BX]  JZ 1$  PUSH DX
               CMP AH, [BX]  JNZ 2$
               MOV AX, DX  INC DX  JMP 3$
           2$: INC BX  INC DX  CMP AL, [BX]  JZ 4$
               CMP AH, [BX]  JNZ 2$  MOV AX, DX
           3$: PUSH DX  PUSH AX  NEXT,
           4$: MOV AX, DX  INC AX  PUSH DX  PUSH AX
               NEXT,  END-CODE

: WORD         ( c --- a # )
               TIB
               >IN @ + SWAP ENCLOSE
               >IN +!
               OVER - >R                   \ String length
               R@ HERE C!
               + HERE 1+ R> CMOVE          \ Move string
               HERE                        \ Result
               BL OVER DUP C@ 1+ + C! ;    \ Space at the end

: X            RDROP ;  IMMEDIATE  IS-NULL

      2 CONSTANT CFL
        VARIABLE STATE
        VARIABLE WARNING

: >BODY        ( cfa --- pfa )      CFL + ;
: BODY>        ( pfa --- cfa )      CFL - ;
: L>NAME       ( lfa --- nfa )      2+ ;
: N>LINK       ( nfa --- lfa )      2- ;
: LINK>        ( lfa -- cfa )       L>NAME COUNT 01F AND + ;

: LATEST       LASTN @ ;

: IMMEDIATE    LATEST L>NAME 040 TOGGLE ;
: SMUDGE       LATEST L>NAME DUP C@ 020 OR SWAP C! ;
: UNSMUDGE     LATEST L>NAME DUP C@ 0DF AND SWAP C! ;

: HEAD         ( --- | name )
               HERE LATEST ,
               BL WORD WARNING @
               IF  DUP FIND SWAP DROP
                   IF DUP COUNT 01F AND TYPE SPACE
                      1 F-MSG MESSAGE CR THEN
               THEN
               DUP C@ 01F MIN DUP >R 080 OR SWAP C!
               R> 1+ CFL + ALLOT
               LASTN ! ;

: ?COMP        STATE @ 0= 6 ?ERROR ;
: ?EXEC        STATE @    5 ?ERROR ;

: COMPILE      R> DUP 2+ >R @ , ;
: LITERAL      STATE @ IF COMPILE  LIT ,   THEN ;        IMMEDIATE
: DLITERAL     STATE @ IF COMPILE DLIT , , THEN ;        IMMEDIATE

: '            BL WORD FIND 0= 0 ?ERROR ;

: (")          R> DUP COUNT + >R ;
: "            STATE @ IF COMPILE (") ELSE HERE THEN
               C" " WORD ", ;                            IMMEDIATE

: (.")         R> COUNT 2DUP TYPE + >R ;
: .(           C" ) WORD COUNT TYPE ;                    IMMEDIATE
: [COMPILE]    ?COMP ' , ;                               IMMEDIATE
: C"           BL WORD 1+ C@ [COMPILE] LITERAL ;         IMMEDIATE
: [']          ' [COMPILE] LITERAL ;                     IMMEDIATE
: ."           ?COMP COMPILE (.") C" " WORD ", ;         IMMEDIATE
: (            C" )  WORD DROP ;                         IMMEDIATE
: --           #TIB @ >IN ! ;                            IMMEDIATE

: (ABORT")     IF   CR HERE COUNT 01F AND + TYPE SPACE
                    R> COUNT TYPE ABORT
               ELSE R> COUNT + >R THEN ;

: ABORT"       ?COMP COMPILE (ABORT")
               C" " WORD ", ;                            IMMEDIATE

        VARIABLE BASE
        VARIABLE HLD
        VARIABLE DPL

: DECIMAL      0A BASE ! ;
: HEX          10 BASE ! ;

: <#           PAD HLD ! ;
: #>           2DROP HLD @ PAD OVER - ;
: HOLD         -1 HLD +! HLD @ C! ;
: SIGN         ( c --- )     0< IF C" - HOLD THEN ;
: ALPHA        ( b --- c)
               9 OVER U<
               IF   [ C" A  0A - ] LITERAL
               ELSE C" 0 THEN + ;

: #            BASE @ MU/MOD ROT ALPHA HOLD ;
: #S           BEGIN # 2DUP OR 0= UNTIL ;

: D.R          >R DUP >R DABS <# #S R> SIGN #> R>
               OVER - 0 MAX SPACES TYPE ;
: D.           0 D.R SPACE ;
: .            S>D D. ;
: U.           0 D. ;
: U.R          0 SWAP D.R ;
: .R           >R S>D R> D.R ;

: DEPTH        S0 @ SP@ - 2- 2/ ;
: ?STACK       DEPTH 0< 3 ?ERROR ;
: .S           ?STACK CR DEPTH 0>
               IF   0 DEPTH 2- DO I PICK . -1 +LOOP
               ELSE 2 F-MSG MESSAGE THEN ;

: <MARK        HERE ;
: <RESOLVE     HERE - , ;
: >MARK        HERE 0 , ;
: >RESOLVE     HERE OVER - SWAP ! ;
: ?PAIRS       <> 7 ?ERROR ;

: IF           ?COMP COMPILE ?BRANCH >MARK 2 ;           IMMEDIATE
: THEN         ?COMP 2 ?PAIRS >RESOLVE ;                 IMMEDIATE
: ELSE         ?COMP 2 ?PAIRS COMPILE BRANCH >MARK
               SWAP 2 [COMPILE] THEN 2 ;                 IMMEDIATE

: BEGIN        ?COMP <MARK 1 ;                           IMMEDIATE
: UNTIL        ?COMP 1 ?PAIRS COMPILE ?BRANCH <RESOLVE ; IMMEDIATE
: WHILE        ?COMP [COMPILE] IF 2+ ;                   IMMEDIATE
: AGAIN        ?COMP 1 ?PAIRS COMPILE BRANCH <RESOLVE ;  IMMEDIATE
: REPEAT       ?COMP >R >R [COMPILE] AGAIN R> R> 2-
               [COMPILE] THEN ;                          IMMEDIATE

: DO           ?COMP COMPILE (DO) >MARK 3 ;              IMMEDIATE
: ?DO          ?COMP COMPILE (?DO) >MARK 3 ;             IMMEDIATE
: LOOP         ?COMP 3 ?PAIRS COMPILE (LOOP)
               DUP 2+ <RESOLVE >RESOLVE ;                IMMEDIATE
: +LOOP        ?COMP 3 ?PAIRS COMPILE (+LOOP)
               DUP 2+ <RESOLVE >RESOLVE ;                IMMEDIATE

: CONVERT      ( ud a --- ud1 a1 )
               BEGIN 1+ DUP >R C@ BASE @ DIGIT
               WHILE SWAP BASE @ UM* DROP ROT BASE @ UM* D+
                     DPL @ 1+ IF DPL 1+! THEN R>
               REPEAT R> ;

: NUMBER       ( a --- d )
               0 0 ROT DUP 1+ C@ C" - =    ( on stack 0 or -1 )
               DUP >R - -1
               BEGIN DPL ! CONVERT DUP
                     DUP C@ BL <> SWAP 0<> AND
               WHILE DUP C@ C" . <> 0 ?ERROR 0
               REPEAT DROP R> IF DNEGATE THEN ;

: QUERY        CR  TIB TIB-LEN EXPECT
               SPAN @ #TIB !
               >IN 0! ;

: INTERPRET    BEGIN BL WORD FIND ?DUP
                     IF   0< STATE @ AND
                          IF , ELSE  EXECUTE THEN
                     ELSE NUMBER DPL @ 1+
                          IF   [COMPILE] DLITERAL
                          ELSE DROP [COMPILE] LITERAL
                          THEN
                     THEN ?STACK
               AGAIN ;

: [            STATE 0! ;                                IMMEDIATE
: ]            -1 STATE ! ;

: QUIT         R0 @ RP!
               CR
               [COMPILE] [
               BEGIN QUERY INTERPRET
                     STATE @ 0= IF ."  Ok" THEN
               AGAIN ;

: (ABORT)      S0 @ SP!  QUIT ;

' (ABORT) IS ABORT

: COLD         -1 WARNING ! DECIMAL
               ." Available " TIB HERE - U. ." bytes"
               QUIT ;

BEG-ASM
L: DO-DOES     XCHG BP, SP  PUSH SI  XCHG BP, SP
               POP SI  INC BX  INC BX  PUSH BX
               NEXT,
END-ASM

: (;CODE)      R> LATEST LINK> ! ;

: DOES>        COMPILE (;CODE)
               0E8 C, [ DO-DOES ] LITERAL HERE 2+ - , ;  IMMEDIATE


        VARIABLE CSP

: !CSP         SP@ CSP ! ;
: ?CSP         SP@ CSP @ <> 4 ?ERROR ;

: CREATE       HEAD
               ;CODE  INC BX  INC BX  PUSH BX
                      NEXT, END-CODE

: CONSTANT     HEAD ,
               ;CODE  INC BX  INC BX  PUSH [BX]
                      NEXT, END-CODE

: 2CONSTANT    HEAD , ,
               ;CODE  PUSH 4 [BX]  PUSH 2 [BX]
                      NEXT, END-CODE

: VARIABLE     CREATE 0 , ;
: 2VARIABLE    CREATE 0. , , ;

: ;            ?COMP ?CSP COMPILE EXIT UNSMUDGE
               [COMPILE] [ ;                             IMMEDIATE

: :            ?EXEC !CSP
               HEAD SMUDGE [COMPILE] ]
               ;CODE  DEC BP  DEC BP  MOV [BP], SI
                      INC BX  INC BX  MOV SI, BX
                      NEXT, END-CODE

TSAVE sgi-forth.com

ENDTARGET
