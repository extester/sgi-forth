/*****************************************************************************
	Target environment for SGI-FORTH building
	Version 1.00

	Copyright (c) 2006 Kuznetsov O.V. All rights reserved.
 	Licensed under the Apache License, Version 2.0
*****************************************************************************/

anew xxx

fload asm586.frt

only forth also definitions decimal

variable t-seg        0 t-seg !
variable t-dp         0 t-dp !
variable t-lastn      0 t-lastn !

: t!           ( n addr --- )       t-seg @ swap !l ;
: t@           ( addr --- n )       t-seg @ swap @l ;
: tc!          ( c addr --- )       t-seg @ swap c!l ;
: tc@          ( addr --- c )       t-seg @ swap c@l ;

: t-allot      ( n --- )            t-dp @ + t-dp ! ;
: t-here       ( --- addr )         t-dp @ ;
: t,           ( n --- )            t-here t! 2 t-allot ;
: tc,          ( c --- )            t-here tc! 1 t-allot ;

also assembler
' t-here  is  here-a
' tc,     is  c,-a
' t,      is  ,-a
' tc!     is  c!-a
' t!      is  !-a
' tc@     is  c@-a
previous

: cseg         ( org --- )          t-dp ! ;

: t-",         ( a --- )
               count >r ?cs: swap t-seg @ t-here 1+ r@
               dup tc, ( 2drop 2drop drop ) cmovel r> t-allot ;

: lcomp        ( s1 a1 #1 s2 a2 #2 -- ? )  \ <>0 - equal
               >r rot r@ <> if r>drop 2drop 2drop 0 else

               -1   r> 0
               ?do  ( s1 a1 s2 a2 b -- ) >r
                      2over c@l >r 2dup c@l r> <>
                      if r>drop 0 leave then
                      1+ rot 1+ -rot r> loop
               >r 2drop 2drop r> then ;


2 constant T-CFL

: TLATEST      ( -- taddr )  t-lastn @ ;
: !TLATEST     ( taddr -- )  t-lastn ! ;

: T-N>LINK     ( nfa -- lfa )       2- ;
: T-L>NAME     ( lfa -- nfa )       2+ ;
: T-LINK>      ( lfa -- cfa )       t-l>name dup tc@ 1fh and + 1+ ;
: T-NAME>      ( nfa -- cfa )       dup tc@ 1fh and + 1+ ;
: T->BODY      ( cfa -- pfa )       t-cfl + ;
: T-BODY>      ( pfa -- cfa )       t-cfl - ;

: T-COMP       ( m-a t-a -- ? )  \ 0 - doesn't match
               >r  count ?cs: -rot
               r> dup tc@ 1fh and >r 1+ t-seg @ swap r> lcomp ;

create tbuf 100 allot
: ttype       ( a # --- )
               >r t-seg @ swap ?cs: tbuf r@ cmovel
               tbuf r>   2dup 1 = swap c@ 0= and
               if 2drop " null" then type ;

variable by
: y     1 by ! ;
: yo    0 by ! ;
yo

: T-FIND       ( a1 -- a2 ? )  \ found: ? = -1/1, a2 = cfa
                               \ not found ? = 0, a2 = a1
        by @ if  cr ." T-FIND `" dup count type ." ': " then
               0 >r tlatest
               begin  ( a1 latest -- )
                      dup 0<> r@ 0= and
               while  ( a1 latest -- )
        by @ if  ." - `" dup t-l>name dup 1+ swap tc@ 1fh and ttype ." ' " then

                      over over t-l>name t-comp
                      ( a1 latest ? )
                      over t-l>name tc@ 20h and 0= and
                      if  r>drop dup t-l>name >r then
                      t@
               repeat
               ( a1 latest )
               r@ if 2drop r@ t-name> r> tc@ 40h and if -1 else 1 then
                     by @ if ." <- found " then
                  else r>drop drop 0 then ;

VOCABULARY TFORTH

\ TFORTH DEFINITIONS
\ \: {M           ONLY FORTH ALSO DEFINITIONS ; IMMEDIATE
\ FORTH DEFINITIONS
\ : {T           ONLY TFORTH ALSO DEFINITIONS ; IMMEDIATE

: T-HEAD       ( | name -- )
               \ >in @ >r
               bl word dup t-find nip
               if cr count type -1 abort" - T is not unique" then

               t-here >r tlatest t, t-", r> !tlatest
               tlatest t-l>name dup tc@ 80h or swap tc!

               \ r> >in ! also tforth definitions
               \ tlatest t-link> constant
               \ previous definitions
               ;


\ VARIABLE T-STATE      0 T-STATE !   \ 0 - interpret, -1 - compile
\ VARIABLE T-CSP
\ : T-!CSP       sp@ csp ! ;
\ : T-?CSP       sp@ csp @ <> abort" Stack changed"  ;
\ : T-?EXEC      t-state @ 0<> abort" not interpret" ;
\ : T-?COMP      t-state @ 0= abort" not compilation" ;


VARIABLE META
: SET-META            1 META ! ;
: SET-TARG            0 META ! ;
SET-META

: T-SMUDGE     tlatest t-l>name dup tc@ 20h or swap tc! ;
: T-UNSMUDGE   tlatest t-l>name dup tc@ DFh and swap tc! ;
: T-IMMEDIATE  tlatest t-l>name dup tc@ 40h or swap tc! ;
variable bz
: z  1 bz ! ;
: zo 0 bz ! ; zo
: (COMPILE)   ( a # --- )
               drop 1- dup>r find 0=
               if cr ." (COMPILE): can't find " r@ count type abort then
               r>drop execute bz @ if dup h. t-here h. then t, ;

: (T-COMPILE)  ( a # --- )
               drop 1- dup>r t-find 0=
               if cr ." (T-COMPILE): can't find " r@ count type abort then
               r>drop t, ;


variable bx
: x 1 bx ! ;
: xo 0 bx ! ; xo

: T-DONE?      ( --- ? )
        tib #tib @  swap >in @ + swap >in @ -
        -trailing nip 0= ;

: T-INTERPRET1  ( --- )
        begin
               bl word
     bx @ if cr state @ if ." T-comp: " else ." T-inter: " then
\    ." `" tib #tib @ type ." '  "
     dup count type then

               meta @
               if     \ Meta context ( a -- )
                      ?uppercase find ?dup
                      if     \ Found in meta vocabulary ( a ? -- )
                             0< state @ and
                             if ,   bx @ if ."  - meta comp " then
                             else   bx @ if ."  - meta exec " then
                                    execute
                             then
                      else   \ Not found ( a -- )
                             bx @ if ."  - meta number " then
                             number dpl @ 1+
                             if
                                    state @
                                    if [compile] dliteral then

                             else   drop state @
                                    if [compile] literal then
                             then
                      then
               else   \ This is target context
                      \ Executing assembler instructions
                      \ or compiling target word
                      ( a --- )
                      state @
                      if     \ target compiling, :
                             dup>r t-find ?dup
                             if     \ found in target
                                    -1 = if drop r@ find 1 <>
                                            abort" execute TARG word!"
                                            execute
                                            bx @ if ." - duplicate meta exec " then
                                         else
                                            t,
                                            bx @ if ."  - targ t-find comp " then
                                         then
                                         r>drop
                             else
                                    r>drop ?uppercase
                                    find dup 0< abort" compile META word!"
                                    if     bx @ if ."  - targ find exec " then
                                           execute
                                    else  number dpl @ 1+
                                      bx @ if ."  - targ numb comp " then
                                      if " DLIT" (t-compile) swap t, t,
                                      else " LIT" (t-compile) drop t, then
                                   then
                             then
                      else   \ target executing
                             ?uppercase find ?dup
                             if     \ Found META word
                                 0< state @ and
                                 if  1 abort" compiling META word!"
                                 else bx @ if ."  - targ find exec " then
                                      execute then
                             else
                                 number  dpl @ 1+ 0= if drop then
                                 \ 1 abort" comp[iling META literal"
                             then
                      then
               then   ?stack  state @ done?
        until  bx @ if ." END OF LINE" then ;

                 comment:
                      find ?dup
                      if     \ Found META word ( a ? -- )
                             0< state @ and
                             if  1 abort" compiling META word!"
                             else bx @ if ."  - targ find exec " then
                                  execute then
                      else   \ Not found in meta voc ( a -- )
                             bx @ if ."  - targ number " then
                             t-find
                             if     \ Found in target vov ( cfa --- )
                                    state @
                                    if   t,  bx @ if ."  - targ t-find comp" then
                                    else 1 abort" executing TARGET word!"
                                    then
                             else   \ Probably number
                                    number dpl @ 1+
                                    if   state @
                                         if   " DLIT" (t-compile)
                                              swap t, t,
                                         then
                                    else drop state @
                                         if   " LIT" (t-compile)
                                              t,
                                         then
                                    then
                             then
                 comment;

: T-COMP       -1 STATE ! T-INTERPRET1 ;
: T-INTERP     0 STATE ! T-INTERPRET1 ;

: M:           1 meta ! also [compile] : ;                      immediate

: ;            meta @ if  [compile] ; else
               0 meta !  ?comp ?csp t-unsmudge
               " EXIT" (t-compile)
               [compile] ; then previous definitions ;          immediate

M: T:          1 meta ! also tforth definitions [compile] : ;   immediate

M: :           0 meta ! ?exec !csp t-head " DO-:" (compile)
               t-smudge -1 state ! also tforth ] ;              immediate

M: MVARIABLE   variable ;
M: MCREATE     create ;
M: MCONSTANT   constant ;
M: M2CONSTANT  2constant ;
M: MIS         [compile] is ; immediate
M: M."         [compile] ." ; immediate
M: M"          [compile] " ; immediate

M: EQU         MCONSTANT ;
M: VARIABLE    t-head " DO-VARIABLE" (compile) 0 t, ;
M: CREATE      t-head " DO-CREATE" (compile) ;
M: CONSTANT    t-head " DO-CONSTANT" (compile) t, ;
M: 2CONSTANT   t-head " DO-2CONSTANT" (compile) swap t, t, ;
M: DEFER       t-head " DO-DEFER" (compile) " NULL" (t-compile) ;
M: CODE        t-head t-smudge t-here 2+ t, beg-asm ;

M: ;CODE       " (;CODE)" (t-compile)
               beg-asm [COMPILE] [ ; immediate

also assembler also
M: L:          lab: ;
M: NEXT,       asm; 0E9h tc, 114h t-here 2+ - t, ;
assembler definitions
M: END-CODE    end-asm t-unsmudge ;
previous previous definitions

M: T-<MARK            t-here ;
M: T-<RESOLVE         t-here - t, ;
M: T->MARK            t-here 0 t, ;
M: T->RESOLVE         t-here over - swap t! ;
M: T-?PAIRS           <> abort" T-?PAIRS error!" ;


T: IF          ?comp " ?BRANCH" (t-compile) t->mark 2 ;  immediate

T: THEN        ?comp 2 t-?pairs t->resolve ;             immediate
T: ELSE        ?comp 2 t-?pairs " BRANCH" (t-compile)
               t->mark swap 2 [compile] then 2 ;         immediate

T: BEGIN       ?comp t-<mark 1 ;                         immediate
T: UNTIL       ?comp 1 t-?pairs " ?BRANCH" (t-compile)
               t-<resolve ;                              immediate
T: WHILE       ?comp [compile] IF 2+ ;                   immediate
T: AGAIN       ?comp 1 t-?pairs " BRANCH" (t-compile)
               t-<resolve ;                              immediate
T: REPEAT      ?comp >r >r [compile] again r> r> 2-
               [compile] then ;                          immediate

T: DO          ?comp " (DO)" (t-compile) t->mark 3 ;     immediate
T: ?DO         ?comp " (?DO)" (t-compile) t->mark 3 ;    immediate
T: LOOP        ?comp 3 t-?pairs " (LOOP)" (t-compile)
               dup 2+ t-<resolve t->resolve ;            immediate
T: +LOOP       ?comp 3 t-?pairs " (+LOOP)" (t-compile)
               dup 2+ t-<resolve t->resolve ;            immediate

T: [COMPILE]   ?comp BL WORD COUNT (t-compile) ;         immediate
here 4 c, ascii ( c, ascii . c, ascii " c, ascii ) c, mconstant <.">
T: ."          ?comp <."> count  (t-compile)
                ascii " word t-", ;                      immediate
M: '           bl word t-find 0= abort" can't find by (')"
               state @ if " LIT" (t-compile) t, then ;
M: IS          ?exec bl word t-find 0= abort" can't find by (IS)"
               t->body t! ; immediate

T: LITERAL     ?comp " LIT" (t-compile) t, ;             immediate
T: DLITERAL    ?comp " DLIT" (t-compile) t, t, ;         immediate

M: C"          bl word 1+ c@ state @
               if " LIT" (t-compile) t, then ;           immediate

here 3 c, ascii ( c, ascii " c, ascii ) c, mconstant <">
M: "           ( --- a | ccc" )
               ascii " word state @
               if <"> count (t-compile) t-",
               else t-here swap t-", then ; immediate

M: IS-NULL     ( --- )
               ?exec tlatest t-l>name 1+ 0 swap tc! ; IMMEDIATE

M: IMMEDIATE   T-IMMEDIATE ;

HANDLE t-handle

M: TARGET
        \ Allocate target segment
        t-seg @ 0<> abort" t-seg already allocated"
        1000 alloc abort" can't allocate memory" t-seg ! drop
        cr  ." Allocated target segment at " t-seg @ h. cr

        \ Setup interpreter
        set-meta
        ['] t-interp mis interpret
        ['] t-comp mis ] ;

M: ENDTARGET
        \ Free memory
        t-seg @ 0= abort" t-seg is freed"
        t-seg @ dealloc abort" invalid address"

        \ Restore interpreter
       ['] interp mis interpret
       ['] (]) mis ]  ;

M: TSAVE ( | filename )
        \ Display info

        \ Setup DP pointer
        cr m." T-HERE = " t-here h. space space
        m" DP" drop 1- find 0= abort" DP" execute
        t-here swap t!

        \ Setup LASTN pointer
        m." LATEST = " tlatest h.
        m" LASTN" drop 1- find 0= abort" LASTN" execute
        tlatest swap t!

        \ Setup init function
        m" COLD" drop 1- t-find 0= abort" COLD"
        m" _INIT" drop 1- find 0= abort" _init" execute t!

        \ Save exeutable file
        bl word t-handle $>handle
        t-handle hcreate abort" can't create output file"
        100h t-dp @ over - dup>r t-handle t-seg @ exhwrite
        r@ <> abort" cant write"
        t-handle hclose drop
        cr ." write " r> base @ >r decimal . m." bytes" r> base ! ;


M: .wtitle     ( ? --- )
        \     012345 012345 012345 0123456
           ."   LINK    NFA    CFA   [CFA]    NAME" cr
        if ." ------------------------------------------" cr then ;

M: .word       ( lfa --- )
        base @ >r hex
        dup t@ 6 .r space
        t-l>name dup dup>r 6 .r space
        t-name> dup 6 .r space t@ 7 .r 4 spaces
        r@ 1+ r> tc@ 1fh and ttype cr
        r> base ! ;

M: T-WORDS     ( --- )
        cr -1 .wtitle
        0 tlatest
        begin
               ?dup
        while
               dup .word
               t@
               swap 1+ dup 20 mod 0= if 0 key drop .wtitle then
               swap
        repeat drop ;

only forth also definitions
autoeditoff


comment:

: ;CODE        COMPILE (;CODE) HERE 2+ ,
               [COMPILE] [ BEG-ASM ;                     IMMEDIATE

CODE PAGE      ( --- )
               PUSH ES PUSH SI PUSH BP PUSHF
               MOV AX, # 3 INT 10 POPF POP BP
               POP SI POP ES NEXT, END-CODE

\ ALSO ASSEMBLER DEFINITIONS
\ M: NEXT,       asm; 0E9h tc, 114h t-here 2+ - t, ;
\ M: NEXT,       asm; JMP next ;
\ PREVIOUS DEFINITIONS

: COLD         -1 WARNING !  HEX
               ." HERE: " HERE U. SPACE ." TIB: " TIB U. SPACE
               ." S0: " S0 @ U. SPACE ." R0: " R0 @ U. SPACE
               ." Available " TIB HERE - DECIMAL U. ." bytes" HEX
               QUIT ;

comment;
