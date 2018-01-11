\ brk_forth: B. R. K.'s simple FORTH compiler for x86-64 GNU/Linux.
\
\ Based on the public domain FORTH compiler `jonesforth` by Richard W. M. Jones
\ <rich@annexia.org> http://annexia.org/forth.
\
\ License: Creative Commons 0 (CC0)
\ <http://creativecommons.org/publicdomain/zero/1.0/>.  To the extent possible
\ under law, B. R. K. has waived all copyright and related or neighboring rights
\ to `brk_forth`.

\ This file contains the rest of the implementation of brk_forth, continued from
\ `brk_forth.S`.

\ Control structures
\ ==================
\
\ These control structures currently only work in compiling mode (TODO: make
\ them work in immediate mode).

\ If-expressions
\ --------------
\
\ 	IF true-part THEN rest
\
\ compiles to
\
\ 	0BRANCH OFFSET-REST true-part rest
\
\ And
\
\ 	IF true-part ELSE false-part THEN rest
\
\ compiles to
\
\ 	0BRANCH OFFSET-FALSE-PART true-part BRANCH OFFSET-REST false-part rest

\ ( -- c-offset ) Start compiling the true part of an if-expression.
: IF IMMEDIATE
     \ compile 0BRANCH
     ' 0BRANCH ,
     \ c-offset = address of offset
     HERE @
     \ compile dummy OFFSET
     0 ,
     ;

\ ( c-offset -- ) End compiling an if-expression.
: THEN IMMEDIATE
       DUP
       \ calculate offset
       HERE @ SWAP -
       \ store it
       SWAP !
;

\ ( c-offset-false -- c-offset-rest ) Start compiling the false part of an
\ if-else-expression.
: ELSE IMMEDIATE
       \ compile BRANCH
       ' BRANCH ,
       \ c-offset-rest = address of offset
       HERE @
       \ compile dummy offset-rest
       0 ,

       SWAP DUP
       \ calculate offset-false
       HERE @ SWAP -
       \ store it
       SWAP !
       ;

\ Loops
\ -----
\
\ 	BEGIN loop-part condition UNTIL
\
\ compiles to
\
\ 	loop-part condition 0BRANCH OFFSET-LOOP-PART

\ ( -- c-begin ) Start compiling a loop.
: BEGIN IMMEDIATE
        \ save c-begin
        HERE @
        ;

\ ( c-begin -- ) End compiling a begin-until loop.
: UNTIL IMMEDIATE
        \ compile 0BRANCH
        ' 0BRANCH ,
        \ calculate offset-loop-part
        HERE @ -
        \ compile it
        ,
;

\ Compiling
\ =========

\ ( w -- ) Compile LIT w.
: LITERAL IMMEDIATE
          \ compile LIT
          ' LIT ,
          \ compile w
          ,
;

\ Characters
\ ==========

\ ( -- c ) Left parenthesis.
: '('  [ CHAR ( ] LITERAL ;

\ ( -- c ) Right parenthesis.
: ')'  [ CHAR ) ] LITERAL ;

\ Comments
\ ========

\ ( -- ) Start a multiline comment, which lasts until the corresponding right
\ parenthesis.  Nested parenthesis are allowed.
: ( IMMEDIATE
    \ nesting depth: nr of unmatched left parentheses
    1
    BEGIN
      \ read next character
      KEY
      \ left parenthesis: increase depth
      DUP '(' = IF
        DROP
        1+
      ELSE
        \ right parenthesis: decrease depth
        ')' = IF
          1-
        THEN
      THEN
      \ stop when depth is 0: initial left parenthesis is balanced out
      DUP 0=
    UNTIL
    \ discard depth
    DROP
;

( From now on ( ... ) comments shall be used where appropriate.

Compiling
========= )

( Compile the following word, even if it is an immediate word.  For example:

	: EXAMPLE_WORD ... [COMPILE] IF ... ;

would compile IF into EXAMPLE_WORD instead of executing it. )
: [COMPILE]
IMMEDIATE ( -- ) WORD FIND >CFA , ;

( Compile a recursive call to the currently compiling word. )
: RECURSE IMMEDIATE ( -- )
          LATEST @
          >CFA
          ,
;

( Booleans
  ======== )

( Boolean true. )
: TRUE ( -- f ) 1 ;

( Boolean false. )
: FALSE ( -- f ) 0 ;

( Boolean NOT: fr = ¬f. )
: NOT ( f -- fr )
  0=
;

( Control structures
  ==================

Loops
-----

	BEGIN loop-part AGAIN

compiles to

	loop-part BRANCH OFFSET-LOOP-PART )

( End compiling an infinite loop. )
: AGAIN IMMEDIATE ( c-begin -- )
        \ compile BRANCH
        ' BRANCH ,
        \ calculate offset-loop-part
        HERE @ -
        \ compile it
        ,
       ;

( 	BEGIN condition WHILE loop-part REPEAT rest

compiles to

	condition 0BRANCH OFFSET-REST loop-part BRANCH OFFSET-CONDITION rest )

( Start compiling loop part of a while-loop. )
: WHILE IMMEDIATE ( -- c-offset-rest )
        \ compile 0BRANCH
        ' 0BRANCH ,
        \ save c-offset-rest
        HERE @
        \ compile dummy offset-rest
        0 ,
        ;

( End compiling a while-loop. )
: REPEAT IMMEDIATE ( c-begin c-offset-rest -- )
         \ compile BRANCH
         ' BRANCH ,
         \ calculate offset-condition
         SWAP
         HERE @ -
         \ compile it
         ,
         \ calculate offset-rest
         DUP
         HERE @ SWAP -
         \ store it
         SWAP !
       ;

( If-expressions
  --------------

	UNLESS ...

is equivalent to

	NOT IF ... )

( Start compiling the true part of an unless-expression. )
: UNLESS IMMEDIATE ( -- c-offset )
         \ compile NOT
         ' NOT ,
         \ compile IF
         [COMPILE] IF
           ;

( Arithmetics
  =========== )

( Integer-divide ndividend by ndivisor. )
: / ( ndividend ndivisor -- nquotient )
  /MOD SWAP DROP
;

( Integer-divide udividend by udivisor. )
: U/ ( udividend udivisor -- uquotient )
  U/MOD SWAP DROP
;

( Compute remainder of integer-division of ndividend by ndivisor. )
: MOD ( ndividend ndivisor -- nremainder )
  /MOD DROP
;

( Compute remainder of integer-division of udividend by udivisor. )
: UMOD ( udividend udivisor -- uremainder )
  U/MOD DROP
;

( Negate n: nr = −n. )
: NEGATE ( n -- nr )
  0 SWAP -
;

( Characters
  ========== )

( TODO generate a word for each character 0x21 - 0x7E using a loop and string
construction + CREATE + DOCOL + ' EXIT , )

: '"'  ( -- c ) [ CHAR " ] LITERAL ;
: '-'  ( -- c ) [ CHAR - ] LITERAL ;
: '.'  ( -- c ) [ CHAR . ] LITERAL ;
: '0'  ( -- c ) [ CHAR 0 ] LITERAL ;
: ':'  ( -- c ) [ CHAR : ] LITERAL ;
: ';'  ( -- c ) [ CHAR ; ] LITERAL ;
: 'A'  ( -- c ) [ CHAR A ] LITERAL ;

( Null. )
: '\0' ( -- c ) 00 ;

( Reverse solidus. )
: '\\' ( -- c ) 92 ;

( Bell. )
: '\a' ( -- c ) 07 ;

( Backspace. )
: '\b' ( -- c ) 08 ;

( Form feed (FF). )
: '\f' ( -- c ) 12 ;

( Line feed (LF). )
: '\n' ( -- c ) 10 ;

( Carriage return (CR). )
: '\r' ( -- c ) 13 ;

( Character tabulation. )
: '\t' ( -- c ) 09 ;

( Line tabulation. )
: '\v' ( -- c ) 11 ;

( Space. )
: BL   ( -- c ) 32 ;

( Emit a line feed. )
: CR ( -- )
  '\n' EMIT
;

( Emit a space. )
: SPACE ( -- )
  BL EMIT
;

( Emit u spaces. )
: SPACES ( u -- )
  BEGIN
    DUP 0<>
  WHILE
    SPACE
    1-
  REPEAT
  DROP
;

( Emit a character tabulation. )
: TAB ( -- )
  '\t' EMIT
;

( Stack manipulation
  ================== )

( Drop 2nd element. )
: NIP ( w1 w2 -- w2 )
  SWAP
  DROP
;

( Duplicate top element below 2nd element. )
: TUCK ( w1 w2 -- w2 w1 w2 )
  SWAP
  OVER
;

( Replace u by duplicate of (u + 1)-th element after u, i.e. wu. )
: PICK ( wu ... w1 w0 u -- wu ... w1 w0 wu )
  \ increase u to skip element on top, i.e. u
  1+
  \ multiply by cell size
  8 U*
  \ get address of top + u
  DSP@ +
  \ fetch wu from address
  @
;

( Base manipulation
  ================= )

( Parse the following word as a base-u number.  In compiling mode, compile it as
  LIT n.  Print an error message and push / compile nothing if the parsing
  fails. )
: NUMBER-BASE ( u -- n )
  \ preserve old base
  BASE @
  \ set new base
  SWAP BASE !

  WORD
  \ duplicate for possible error message
  2DUP

  NUMBER
  \ if cannot parse number
  0<> IF
    DROP
    \ TODO instead of exiting, print PARSE ERROR: number not valid in base %u%:
    \ '%word%'
    100 1 SYS_EXIT SYSCALL
    EXIT
  THEN

  \ drop word
  -ROT 2DROP
  \ if in compiling mode
  STATE @ IF
    \ compile LIT n
    ' LIT ,
    ,
  THEN

  \ restore old base
  SWAP BASE !
;

( Switch to base-10. )
: DECIMAL ( -- )
  10 BASE !
;

( Parse the following word as a base-10 number. )
: [DECIMAL] IMMEDIATE ( -- )
  10 NUMBER-BASE
;

( Switch to base-16. )
: HEX ( -- )
  16 BASE !
;

( Parse the following word as a base-16 number. )
: [HEX] IMMEDIATE ( -- )
  16 NUMBER-BASE
;

( Switch to base-8. )
: OCTAL
  8 BASE !
;

( Parse the following word as a base-8 number. )
: [OCTAL] IMMEDIATE ( -- )
  8 NUMBER-BASE
;

( Switch to base-2. )
: BINARY
  2 BASE !
;

( Parse the following word as a base-2 number. )
: [BINARY] IMMEDIATE ( -- )
  2 NUMBER-BASE
;

\ auxiliary word
HIDE NUMBER-BASE

( Printing
  ======== )

( Convert digit u to its character representation.  If the digit is not
  representable as a character, c is '?' and an error message is printed. )
: DIGIT->CHAR ( u -- c )
  DUP 10 < IF
    '0' +
    EXIT
  THEN

  10 -
  DUP 26 < IF
    'A' +
    EXIT
  THEN

  DROP
  \ TODO print BASE ERROR: digit %u% (base-10) is not representable as a
  \ character; BASE %BASE% is too great
  [ CHAR ? ] LITERAL
;

( Calculate width in characters of unumber in the current base. )
: UWIDTH ( unumber -- uwidth )
  \ width
  0 SWAP
  BEGIN
    \ remove last digit of number
    BASE @ U/
    \ increase width for the digit
    SWAP 1+ SWAP
    \ until there are no more digits
    DUP 0= UNTIL
  DROP
;

( Print unumber in the current base. )
: U. ( unumber -- )
  BASE @ U/MOD	( udigit uquotient )
  \ if there are preceding digits
  ?DUP IF
    \ print them
    RECURSE	( udigit )
  THEN
  \ print last digit	( udigit )
  DIGIT->CHAR EMIT
;

( Print unumber in the current base, left-padded to be at least uwidth wide. )
: U.R ( unumber uwidth -- )
  SWAP
  DUP UWIDTH	( uwidth unumber uwidth-number )
  ROT SWAP -	( unumber uwidth-padding )
  \ if padding is positive
  DUP 0> IF
    \ print it
    SPACES
  ELSE
    DROP
  THEN	( unumber )
  \ print number
  U.
;

( Print nnumber in the current base, left-padded such that at least uwidth
  characters are printed. )
: .R ( nnumber uwidth -- )
  \ save negative flag
  SWAP
  DUP 0< IF
    \ decrease width because of '-'
    SWAP 1- SWAP
    NEGATE
    TRUE
  ELSE
    FALSE
  THEN	( uwidth unumber fnegative )
  -ROT
  DUP UWIDTH	( fnegative uwidth unumber uwidth-number )
  ROT SWAP -	( fnegative unumber uwidth-padding )
  \ print padding
  DUP 0> IF
    SPACES
  ELSE
    DROP
  THEN	( fnegative unumber )
  \ print sign
  SWAP IF
    '-' EMIT
  THEN	( unumber )
  \ print number
  U.
;

( Print unumber in the current base followed by a space. )
: U. ( unumber -- )
  U. SPACE
;

( Print nnumber in the current base followed by a space. )
: . ( nnumber -- )
  0 .R SPACE
;

\ auxiliary word
HIDE DIGIT->CHAR

( Print contents of stack, top to bottom, using xt. to print each number. )
: .STACK ( xt. -- )
  DSP@ 8+	( xt. a-cell )
  BEGIN
    \ while a-cell < S0
    DUP S0 @ <
  WHILE
    \ print current cell
    DUP @	( xt. a-cell wcell )
    2 PICK EXECUTE	( xt. a-cell )
    TAB
    \ switch to next a-cell
    8+
  REPEAT
  DROP DROP
;

( Print contents of stack as signed values, top to bottom.  Useful for
  debugging. )
: .S
  ' . .STACK
;

( Print contents of stack as unsigned values, top to bottom.  Useful for
  debugging. )
: .SU ( -- )
  ' U. .STACK
;

\ auxiliary word
HIDE .STACK

( Print words in dictionary, newest to oldest.  Useful for debugging. )
: .DICTIONARY ( -- )
  \ initial link pointer
  LATEST
  @ DUP	( a-link a-link )
  BEGIN
    \ while a-link is not NULL
    DUP 0<>
  WHILE
    8+ DUP 1+	( a-link c-len c-name )
    SWAP C@	( a-link c-name ulen-byte )
    DUP FLAG_LENGTH_MASK AND SWAP	( a-link c-name ulen ulen-byte )
    DUP FLAG_IMMEDIATE AND SWAP	( a-link c-name ulen fimmediate ulen-byte )
    FLAG_HIDDEN AND	( a-link c-name ulen fimmediate fhidden )
    \ print flags
    IF [ CHAR H ] LITERAL ELSE '-' THEN EMIT
    IF [ CHAR I ] LITERAL ELSE '-' THEN EMIT
    SPACE	( a-link c-name ulen)
    \ print name
    TELL SPACE TAB	( a-link )
    \ switch to link pointer of next word
    @ DUP
  REPEAT
;

( Print signed integer at a-addr in the current base. )
: ? ( a-addr )
  @ .
;

( Print unsigned integer at a-addr in the current base. )
: ?U ( a-addr )
  @ U.
;
