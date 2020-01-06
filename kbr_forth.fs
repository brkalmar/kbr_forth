\ kbr_forth: K. B. R's simple FORTH compiler for x86-64 GNU/Linux.
\
\ Based on the public domain FORTH compiler `jonesforth` by Richard W. M. Jones
\ <rich@annexia.org> http://annexia.org/forth.
\
\ License: Creative Commons 0 (CC0)
\ <http://creativecommons.org/publicdomain/zero/1.0/>.  To the extent possible
\ under law, K. B. R. has waived all copyright and related or neighboring rights
\ to `kbr_forth`.

\ This file contains the rest of the implementation of kbr_forth, continued from
\ `kbr_forth.S`.

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
: IF
  \ compile 0BRANCH
  ' 0BRANCH ,
  \ c-offset = address of offset
  HERE @
  \ compile dummy OFFSET
  0 ,
; IMMEDIATE

\ ( c-offset -- ) End compiling an if-expression.
: THEN
  DUP
  \ calculate offset
  HERE @ SWAP -
  \ store it
  SWAP !
; IMMEDIATE

\ ( c-offset-false -- c-offset-rest ) Start compiling the false part of an
\ if-else-expression.
: ELSE
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
; IMMEDIATE

\ Loops
\ -----
\
\ 	BEGIN loop-part condition UNTIL
\
\ compiles to
\
\ 	loop-part condition 0BRANCH OFFSET-LOOP-PART

\ ( -- c-begin ) Start compiling a loop.
: BEGIN
  \ save c-begin
  HERE @
; IMMEDIATE

\ ( c-begin -- ) End compiling a begin-until loop.
: UNTIL
  \ compile 0BRANCH
  ' 0BRANCH ,
  \ calculate offset-loop-part
  HERE @ -
  \ compile it
  ,
; IMMEDIATE

\ Compiling
\ =========

\ ( w -- ) Compile LIT w.
: LITERAL
  \ compile LIT
  ' LIT ,
  \ compile w
  ,
; IMMEDIATE

\ Comments
\ ========
\
\ TODO first ()-comment(s) in word saved docstring-like for stack effects and
\ documentation

\ ( -- ) Start a multiline comment, which lasts until the corresponding right
\ parenthesis.  Nested parenthesis are allowed.
: (
  \ nesting depth: nr of unmatched left parentheses
  1
  BEGIN
    \ read next character
    KEY
    \ left parenthesis: increase depth
    DUP [ CHAR ( ] LITERAL = IF
      DROP
      1+
    ELSE
      \ right parenthesis: decrease depth
      [ CHAR ) ] LITERAL = IF
        1-
      THEN
    THEN
    \ stop when depth is 0: initial left parenthesis is balanced out
    DUP 0=
  UNTIL
  \ discard depth
  DROP
; IMMEDIATE

( From now on ( ... ) comments shall be used where appropriate.

Compiling
========= )

( Compile the following word, even if it is an immediate word.  For example:

	: EXAMPLE_WORD ... [COMPILE] IF ... ;

would compile IF into EXAMPLE_WORD instead of executing it. )
: [COMPILE] ( -- )
  WORD FIND >CFA ,
; IMMEDIATE

( Compile a recursive call to the currently compiling word. )
: RECURSE ( -- )
  LATEST @
  >CFA
  ,
; IMMEDIATE

( Booleans
  ======== )

( Boolean true. )
: TRUE  ( -- f ) 1 ;

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
: AGAIN ( c-begin -- )
  \ compile BRANCH
  ' BRANCH ,
  \ calculate offset-loop-part
  HERE @ -
  \ compile it
  ,
; IMMEDIATE

( 	BEGIN condition WHILE loop-part REPEAT rest

compiles to

	condition 0BRANCH OFFSET-REST loop-part BRANCH OFFSET-CONDITION rest )

( Start compiling loop part of a while-loop. )
: WHILE ( -- c-offset-rest )
  \ compile 0BRANCH
  ' 0BRANCH ,
  \ save c-offset-rest
  HERE @
  \ compile dummy offset-rest
  0 ,
; IMMEDIATE

( End compiling a while-loop. )
: REPEAT ( c-begin c-offset-rest -- )
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
; IMMEDIATE

( If-expressions
  --------------

	UNLESS ...

is equivalent to

	NOT IF ... )

( Start compiling the true part of an unless-expression. )
: UNLESS ( -- c-offset )
  \ compile NOT
  ' NOT ,
  \ compile IF
  [COMPILE] IF
; IMMEDIATE

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
  IF
    DROP
    \ TODO instead of exiting, print PARSE ERROR: number not valid in base %u%:
    \ '%word%'
    100 1 SYS_EXIT SYSCALL
    EXIT
  THEN
  \ drop word
  -ROT 2DROP

  STATE @ IF
    \ compile LIT
    ' LIT ,
    \ compile n
    ,
  ELSE
    SWAP
  THEN

  \ restore old base
  BASE !
;

( Switch to base-10. )
: DECIMAL ( -- )
  10 BASE !
;

( Parse the following word as a base-10 number. )
: [DECIMAL] ( -- )
  10 NUMBER-BASE
; IMMEDIATE

( Switch to base-16. )
: HEX ( -- )
  16 BASE !
;

( Parse the following word as a base-16 number. )
: [HEX] ( -- )
  16 NUMBER-BASE
; IMMEDIATE

( Switch to base-8. )
: OCTAL
  8 BASE !
;

( Parse the following word as a base-8 number. )
: [OCTAL] ( -- )
  8 NUMBER-BASE
; IMMEDIATE

( Switch to base-2. )
: BINARY
  2 BASE !
;

( Parse the following word as a base-2 number. )
: [BINARY] ( -- )
  2 NUMBER-BASE
; IMMEDIATE

\ auxiliary word
HIDE NUMBER-BASE

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

( Find the lesser of n1 and n2. )
: MIN ( n1 n2 -- nmin )
  2DUP > IF
    SWAP
  THEN
  DROP
;

( Find the lesser of u1 and u2. )
: UMIN ( u1 u2 -- umin )
  2DUP U> IF
    SWAP
  THEN
  DROP
;

( Find the greater of n1 and n2. )
: MAX ( n1 n2 -- nmax )
  2DUP < IF
    SWAP
  THEN
  DROP
;

( Find the greater of u1 and u2. )
: UMAX ( u1 u2 -- umax )
  2DUP U< IF
    SWAP
  THEN
  DROP
;

( Characters
  ========== )

( Null. )
: '0 ( -- c ) [HEX] 00 ;

( Reverse solidus. )
: '\ ( -- c ) [HEX] 5C ;

( Bell. )
: 'a ( -- c ) [HEX] 07 ;

( Backspace. )
: 'b ( -- c ) [HEX] 08 ;

( Form feed (FF). )
: 'f ( -- c ) [HEX] 0C ;

( Line feed (LF). )
: 'n ( -- c ) [HEX] 0A ;

( Carriage return (CR). )
: 'r ( -- c ) [HEX] 0D ;

( Character tabulation. )
: 't ( -- c ) [HEX] 09 ;

( Line tabulation. )
: 'v ( -- c ) [HEX] 0B ;

( Space. )
: BL ( -- c ) [HEX] 20 ;

( Emit a line feed. )
: CR ( -- )
  'n EMIT
;

( Emit a space. )
: SPACE ( -- )
  BL EMIT
;

( Emit u spaces. )
: SPACES ( u -- )
  BEGIN
    DUP
  WHILE
    SPACE
    1-
  REPEAT
  DROP
;

( Emit a character tabulation. )
: TAB ( -- )
  't EMIT
;

( Stack manipulation
  ================== )

( Get depth data stack in bytes, before this word ran. )
: DEPTH ( -- u )
  S0 @ DSP@ -
  \ subtract 1 cell for S0
  8-
;

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

( Printing
  ======== )

( Convert digit u to its character representation.  If the digit is not
  representable as a character, c is '?' and an error message is printed. )
: DIGIT->CHAR ( u -- c )
  DUP 10 < IF
    [ CHAR 0 ] LITERAL +
    EXIT
  THEN

  10 -
  DUP 26 < IF
    [ CHAR A ] LITERAL +
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
    [ CHAR - ] LITERAL EMIT
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

( Print signed integer at a-addr in the current base. )
: ? ( a-addr )
  @ .
;

( Print unsigned integer at a-addr in the current base. )
: ?U ( a-addr )
  @ U.
;

( Strings
  ======= )

( Check whether nbeg ≤ n1 < nend or ubeg ≤ u1 < uend. )
: WITHIN ( n1 nbeg nend -- f ) ( u1 ubeg uend -- f )
  OVER -	( n1 nbeg nsize )
  -ROT -	( nsize nindex )
  U>
;

( Round c-addr up to the next cell (8-byte) boundary. )
: ALIGNED ( c-addr -- a-addr )
  [BINARY] 0111 +
  [BINARY] 0111 INVERT AND
;

( Align HERE to the next cell boundary after it. )
: ALIGN ( -- )
  HERE @ ALIGNED HERE !
;

( Store byte c at HERE and increment HERE by 1. )
: C, ( c -- )
  HERE @
  C!
  1 HERE +!
;

( Parse the following input until the next quotation mark, store it at HERE
  without changing HERE, and push the start and the length of the stored string
  on stack.  c-str is stored at HERE and could be overwritten by anything that
  uses HERE.  In compiling mode, compile it as

	LITSTRING ulen c-str padding

at HERE, and increment HERE to point after padding. )
: S" ( -- c-str ulen )
  STATE @ IF
    \ compile LITSTRING
    ' LITSTRING ,
    \ preserve address of string length	( a-len )
    HERE @
    \ compile dummy length
    0 ,
    \ read and compile string itself
    BEGIN
      KEY
      DUP [ CHAR " ] LITERAL <>
    WHILE	( a-len c )
      C,
    REPEAT
    DROP	( a-len )
    \ calculate and save length
    DUP
    HERE @	( a-len a-len c-end )
    \ subtract 8 because a-len points to start of length, not string
    SWAP - 8-	( a-len ulen )
    SWAP !
    \ compile padding
    ALIGN
  ELSE
    HERE @	( c-str-end )
    \ read and store string
    BEGIN
      KEY
      DUP [ CHAR " ] LITERAL <>
    WHILE	( c-str-end c )
      OVER C!
      1+
    REPEAT
    DROP	( c-str-end )
    HERE @ -	( ulen )
    HERE @ SWAP	( c-str ulen )
  THEN
; IMMEDIATE

( Parse the following input until the next quotation mark as a string, and print
  it to stdout.  In compiling mode, compile it as if compiling S" followed by
  TELL. )
: ." ( -- )
  STATE @ IF
    \ call S", which compiles LITSTRING ulen c-str padding
    [COMPILE] S"
    \ compile TELL
    ' TELL ,
  ELSE
    \ read bytes and emit each one until a quotation mark is encountered
    BEGIN
      KEY
      DUP [ CHAR " ] LITERAL <>
    WHILE
      EMIT
    REPEAT
    DROP
  THEN
; IMMEDIATE

( Constants, variables, values
  ============================

A constant is a word which pushes a constant value on stack; similarly, a
variable is a word which pushes an address on stack, at which the contents of
the variable may be accessed.

Constants are compiled directly into their words as literals.  Variables on the
other hand are compiled as an address literal that points to a cell allocated
directly before the word definition in the data segment, where the contents of
the variable are stored.

	   ┌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┐
	   ▼                                                ╎
	┏━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┳━━━━━━━━━━┳━━━━━━━━━━┳━━╎━━━━━━━┳━━━━━━━━━━┓
	┃ contents ┃ header        ┃ DOCOL    ┃ LIT      ┃ address  ┃ EXIT     ┃
	┃      8 B ┃               ┃      8 B ┃      8 B ┃      8 B ┃      8 B ┃
	┗━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┛


This simple example demonstrates how FORTH-implemented global constants and
variables are defined and used.

	299_792_458 CONSTANT V_LIGHT
	." c = " V_LIGHT . ." m/s" CR

	VARIABLE DISTANCE
	10_000_000_000_000 DISTANCE !

	." It takes approximately " DISTANCE @ V_LIGHT / . ." s "
	." for light to travel " DISTANCE ? ." m in vacuum." CR )

( Compile a constant with the following word as its name with value w. )
: CONSTANT ( w -- )
  WORD-CUT	( c-str ulen )
  \ header and codeword
  CREATE
  DOCOL ,
  \ LIT w
  ' LIT ,
  ,
  \ finish compiling
  ' EXIT ,
;

( Address 0 in memory.  Used to semantically differentiate the integer 0 and the
  address 0. )
0 CONSTANT NULL

( Allocate u bytes of memory at HERE and increase HERE to point after it.
  a-addr is the start of the allocated memory.  HERE should be on a cell
  boundary (a multiple of 8) to prevent any undefined behaviour. )
: ALLOT ( u -- a-addr )
  HERE @ SWAP	( a-start u )
  \ increment HERE past the allocated space
  HERE +!	( a-start )
;

( Get the number of bytes in ucells cells, i.e. multiply ucells by 8. )
: CELLS ( ucells -- ubytes )
  8 U*
;

( Compile a variable with the following word as its name. )
: VARIABLE ( -- )
  \ contents
  1 CELLS ALLOT	( a-contents )
  WORD-CUT	( a-contents c-str ulen )
  \ header and codeword
  CREATE	( a-contents )
  DOCOL ,
  \ LIT address
  ' LIT ,
  ,
  \ finish compiling
  ' EXIT ,
;

( A value is conceptually just like a variable, only with simpler, albeit less
  powerful syntax.  Like constants, values push their contents directly on
  stack.  Unlike constants, they may be modified via TO. Values are therefore
  better suited for use cases where reads are frequent and writes are
  infrequent.

A value is compiled into its word as a literal, just like a constant.  TO
modifies the literal itself in the word through an address to it.  The address
is found when compiling and compiled as a literal followed by a !.

	┏╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┳━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━┳━━━━━━━━━━┓
	┃ header        ┃ DOCOL    ┃ LIT      ┃ contents ┃ EXIT     ┃ : word
	┃               ┃      8 B ┃      8 B ┃      8 B ┃      8 B ┃   compiled
	┗╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┛   by VALUE
	                                         ▲
	                     ┌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┘
	╺╸╺╸╺╸╺┳━━━━━━━━━━┳━━╎━━━━━━━┳━━━━━━━━━━┳╸╺╸╺╸╺╸
	  ...  ┃ LIT      ┃ address  ┃ !        ┃  ...    : code compiled by TO
	       ┃      8 B ┃      8 B ┃      8 B ┃
	╺╸╺╸╺╸╺┻━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━┻╸╺╸╺╸╺╸

The following simple example demonstrates how values are defined and used.

	54 VALUE NUMBER

	: SUM_NUMBER
	  ." n = " NUMBER . CR
	  ." 1 + 2 + ... + n = " NUMBER NUMBER 1+ * 2 / . CR
	;

	SUM_NUMBER
	26 TO NUMBER
	SUM_NUMBER )

( Compile a value with the following word as its name with initial value w. )
: VALUE ( w -- )
  WORD-CUT CREATE
  DOCOL ,
  ' LIT ,
  \ the initial value w
  ,
  ' EXIT ,
;

( Store w in the value with the same name as the following word. )
: TO ( w -- )
  WORD FIND
  >DFA 8+
  STATE @ IF	( a-contents )
    \ compile LIT a-contents !
    ' LIT ,
    ,
    ' ! ,
  ELSE	( w a-contents )
    \ immediate mode: simply store w
    !
  THEN
; IMMEDIATE

( Add u to the value with the same name as the following word. )
: +TO ( u -- )
  WORD FIND
  >DFA 8+
  STATE @ IF
    \ compile LIT a-contents +!
    ' LIT ,
    ,
    ' +! ,
  ELSE
    \ immediate mode: simply add u
    +!
      THEN
; IMMEDIATE

( Printing
  ======== )

( Print the name of the word in the dictionary entry at a-word. )
: ID. ( a-word -- )
  8+	( a-len-byte )
  DUP 1+ SWAP	( c-name a-len-byte )
  C@ FLAG_LENGTH_MASK AND	( c-name ulen )
  \ print name
  TELL
;

( Check whether the dictionary entry at a-word is hidden. )
: ?HIDDEN ( a-word -- f )
  8+
  C@ FLAG_HIDDEN AND
;

( Check whether the dictionary entry at a-word is immediate. )
: ?IMMEDIATE ( a-word -- f )
  8+
  C@ FLAG_IMMEDIATE AND
;

( Print all words in dictionary, newest to oldest, skipping hidden words.
  Useful for debugging. )
: WORDS ( -- )
  \ initial link pointer
  LATEST @	( a-word )
  BEGIN
    \ while a-word is not NULL
    ?DUP
  WHILE
    DUP ?HIDDEN UNLESS
      DUP ID.
      TAB
      THEN
    \ continue to next word
    @	( a-word )
  REPEAT
;

( Delete the word with the following word as its name, and all other words
  defined and memory allocated more recently than that word. )
: FORGET ( -- )
  WORD FIND
  \ set LATEST to previous word in dictionary
  DUP @ LATEST !
  \ set HERE to start of word
  HERE !
;

( Number of bytes DUMP prints per line.  DUMP rounds it up to the next multiple
  of 8. )
16 VALUE DUMP-LINE-BYTES

( Dump the ulen bytes at c-start in memory in a hexadecimal format. )
: DUMP ( c-start ulen -- )
  BASE @ -ROT	( ubase c-addr ulen )
  HEX

  BEGIN
    \ while ulen is not 0
    ?DUP
  WHILE
    \ print address
    OVER 8 U.R 2 SPACES

    \ find number of bytes to print
    DUP DUMP-LINE-BYTES ALIGNED UMIN	( ubase c-addr ulen ubytes )
    \ preserve it on return stack
    >R	( ubase c-addr ulen )

    \ print up to next 16 bytes as hexadecimal numbers
    OVER RSP@ @	( ubase c-addr ulen c-addr ubytes )
    BEGIN
      \ while ubytes is not 0
      ?DUP
    WHILE
      \ additional space between quadwords
      \ DUP 8 UMOD 7 = IF
      \   SPACE
      \ THEN
      \ print byte
      SWAP
      DUP C@
      2 U.R SPACE
      \ increase c-addr, decrease ubytes
      1+ SWAP 1-
    REPEAT
    DROP	( ubase c-addr ulen )

    \ pad between hexadecimal and ASCII
    DUMP-LINE-BYTES ALIGNED RSP@ @ -	( ubase c-addr ulen ubytes-left )
    \ 3 spaces for each byte, plus 1 space so the gap is at least 2-wide
    3 * 1+	( ubase c-addr ulen upadding )
    SPACES	( ubase c-addr ulen )

    \ print up to next 16 bytes as ASCII characters
    OVER RSP@ @	( ubase c-addr ulen c-addr ubytes )
    BEGIN
      ?DUP
    WHILE
      SWAP
      DUP C@
      DUP BL [HEX] 80 WITHIN IF
        \ printable: emit
        EMIT
      ELSE
        \ not printable: emit a dot
        DROP
        [ CHAR . ] LITERAL EMIT
      THEN
      1+ SWAP 1-	( ubase c-addr ulen c-addr ubytes )
    REPEAT
    DROP	( ubase c-addr ulen )

    \ finish line
    CR

    \ decrease ulen and increase c-addr by ubytes
    R> TUCK	( ubase c-addr ubytes ulen ubytes )
    -	( ubase c-addr ubytes ulen-new )
    -ROT + SWAP	( ubase c-addr-new ulen-new )
  REPEAT
  DROP	( ubase )

  BASE !
;

( Control flow
  ============

Case-expressions
----------------

A case-expression may be used if there are multiple possible branches of
execution depending on top of stack.  CASE consists of 0 or more tests and
corresponding branches of execution, and an optional default-branch.  CASE
executes each test in turn, then compares top two elements and executes the
first branch where they are equal.  If none matches, the default-branch — if any
— is executed.

Top of stack is popped before execution of the branch, except for the
default-branch, in which it is accessible and popped only after execution.  In
any case, top of stack will have been popped after ENDCASE.

	CASE
		test-1 OF branch-1 ENDOF
		test-2 OF branch-2 ENDOF
		...
		test-n OF branch-n ENDOF
		branch-default
	ENDCASE

compiles to nested if-else expressions, one if-expression for each test

	test-1 OVER = IF
		DROP branch-1
	ELSE
		test-2 OVER = IF
			DROP branch-2
		ELSE
			...
				test-n OVER = IF
					DROP branch-n
				ELSE
					branch-default
					DROP
				THEN
			...
		THEN
	THEN )

( Start compiling a case-expression. )
: CASE ( -- c-null )
  \ NULL to mark last IF to compile a THEN for
  NULL
; IMMEDIATE

( Start compiling an execution branch of a case-expression. )
: OF ( -- c-offset-false )
  ' OVER ,
  ' = ,
  [COMPILE] IF
  ' DROP ,
; IMMEDIATE

( End compiling an execution branch of a case-expression, start compiling the
  next test or the default-branch. )
: ENDOF ( c-offset-false -- c-offset-rest )
  [COMPILE] ELSE
; IMMEDIATE

( End compiling a case-expression. )
: ENDCASE
  ( c-null c-offset-rest-1 c-offset-rest-2 ... c-offset-rest-n -- )
  ' DROP ,
  BEGIN
    \ while current offset is not NULL
    ?DUP
  WHILE
    \ compile matching THEN
    [COMPILE] THEN
  REPEAT
; IMMEDIATE

( Decompiling
  =========== )

( Find the dictionary entry that c-codeword points into.  a-word is NULL if
  c-codeword does not point into any word. )
: CFA> ( c-codeword -- a-word )
  LATEST @	( c-codeword a-current )
  BEGIN
    ?DUP
  WHILE
    2DUP SWAP	( c-codeword a-current a-current c-codeword )
    < IF	( c-codeword a-current )
      \ match: most recent word that starts before c-codeword
      NIP	( a-current )
      EXIT
    THEN
    @
  REPEAT	( c-codeword )
  DROP
  \ not found: NULL
  NULL
;

( Decompile the following word to stdout.  If the following word cannot be found
  in the dictionary or is assembly-implemented, the behaviour is undefined. )
: SEE ( -- TODO )
  \ find dictionary entry
  WORD FIND

  \ find next entry after the entry being decompiled: start of next entry is
  \ approximately the end of the word's entry
  HERE @
  LATEST @	( a-word a-next a-current )
  BEGIN
    \ while current is not the word being decompiled
    2 PICK OVER	( a-word a-last a-current a-word a-current )
    <>
  WHILE
    \ step back in dictionary: next <- current, current <- word before current
    NIP
    DUP @	( a-word a-next a-current )
  REPEAT
  DROP SWAP	( a-end a-start )

  \ print `: WORD-NAME` and optionally `IMMEDIATE`
  [ CHAR : ] LITERAL EMIT SPACE
  DUP ID. SPACE
  DUP ?IMMEDIATE IF ." IMMEDIATE " THEN

  \ start printing codeword pointers
  >DFA	( a-end a-codeword-ptr )
  BEGIN
    \ while not at the end
    2DUP >
  WHILE
    DUP @	( a-end a-codeword-ptr a-codeword )
    \ handle special words
    CASE
      ' LIT OF
        \ just print the literal
        8+ DUP @
        .
      ENDOF
      ' LITSTRING OF
        \ just print the string literal
        \ emit start
        [ CHAR S ] LITERAL EMIT [ CHAR " ] LITERAL EMIT SPACE
        8+ DUP @	( a-end a-len ulen )
        SWAP 8+ SWAP	( a-end c-str ulen )
        \ emit string itself
        2DUP TELL
        \ emit end
        [ CHAR " ] LITERAL EMIT SPACE
        + ALIGNED	( a-end a-codeword-next )
        8-	( a-end a-codeword )
      ENDOF
      ' 0BRANCH OF
        ." 0BRANCH [ "
        \ print offset
        8+ DUP @	( a-end a-offset uoffset )
        .
        ." , ] "
      ENDOF
      ' BRANCH OF
        ." BRANCH [ "
        \ print offset
        8+ DUP @ .
        ." , ] "
      ENDOF
      ' ' OF
        ." ' "
        \ print word whose codeword address ' is getting
        8+ DUP @	( a-end a-codeword-ptr a-codeword-following )
        CFA> ID. SPACE	( a-end a-codeword-ptr )
      ENDOF
      ' EXIT OF
        \ print EXIT if not at the end of the word
        2DUP 8+ <> IF
          ." EXIT "
        THEN
      ENDOF
      \ not a special word: just print it
      DUP CFA>
      ID. SPACE
    ENDCASE
    8+
  REPEAT	( a-end a-codeword-ptr )
  2DROP

  \ finish the word
  [ CHAR ; ] LITERAL EMIT CR
;

( Execution tokens
  ================

An execution token is a concept allowing us to store a reference to a word as a
value which may be passed around like any other, and most importantly may be
executed.  In this implementation, an execution token of a word is the address
of its codeword, i.e. that obtained by >CFA.

EXECUTE executes an execution token.  ' pushes the codeword pointer — that is,
execution token — after it in a compiled word on stack (works the same way as
LIT).  ['] does the same thing, except it pushes the execution token of the
following word from stdin.

Anonymous words are referred to via their execution tokens, as they have no
names.

	:NONAME word-definition ;

works much the same way as

	: word-name word-definition ;

except instead of making a new dictionary entry with name word-name, it pushes
an execution token for the new word on stack. )

( Get execution token of the following word from stdin.  Only works in compiled
  words. )
: ['] ( -- xt )
  \ compile LIT
  ' LIT ,
; IMMEDIATE

( Start compilation of anonymous word; enter compiling mode.  xt is the
  execution token of the anonymous word. )
: :NONAME ( -- xt )
  \ create dictionary header with empty name
  NULL 0 CREATE
  HERE @	( xt )
  \ compile codeword
  DOCOL ,
  \ compile the anoymous word
  ]
;

( Exceptions
  ==========

An exception is just a non-zero signed integer.  Negative integers are reserved
for ANSI-FORTH-defined exceptions.

CATCH runs an execution token, after pushing data stack pointer to directly
below the execution token, and address of EXCEPTION-MARKER, on return stack.

	       ┏━━━━━━━━━━━━━━━━━━━━━━━━━┓
	       ┃ EXCEPTION-MARKER        ┃
	       ┣━━━━━━━━━━━━━━━━━━━━━━━━━┫
	  −  │ ┃ data stack pointer      ┃
	     │ ┣━━━━━━━━━━━━━━━━━━━━━━━━━┫
	addr │ ┃ return address of CATCH ┃
	     │ ┣━━━━━━━━━━━━━━━━━━━━━━━━━┫
	  +  ▼ ╏  .                      ╏
	       ╏  .                      ╏
	       ╻  .                      ╻
	       ╻                         ╻


THROW unwinds the return stack until it finds the first EXCEPTION-MARKER, and
restores the data stack pointer stored under it; after this it pushes the
exception integer on data stack.  If THROW does not find an EXCEPTION-MARKER
during stack unwinding (i.e. it is uncaught), it prints an error message and
QUITs.  If no exception is thrown in the word run by CATCH, top of return stack
— EXCEPTION-MARKER — is returned to as normally, which pushes 0 on data stack.

In any case, the exception marker and data stack pointer are dropped from return
stack, so execution continues with the next word after CATCH.

This effectively makes exceptions straightforward, albeit simple.  After a
CATCH, top of stack is either 0 if no exception occured, or the exception
integer.  In the former case, the rest of data stack may be used as after a
regular execution of the execution token.  In the latter case, the rest of data
stack is restored to have as many cells as before the CATCH, but the parameters
of the execution token may have been modified and in general have undefined
values.

	parameter-1 parameter-2 ... parameter-n xt CATCH
	?DUP IF
		\ exception ne caught
		( wundefined-1 wundefined-2 ... wundefined-n ne )
		caught-part
		( wundefined-1 wundefined-2 ... wundefined-n )
		n NDROP
	ELSE
		\ no exception caught
		( wreturn-1 wreturn-2 ... wreturn-m )
		normal-part
	THEN
	rest

Throwing exceptions is as simple as

	exception-integer THROW )

( Push 0 on stack, and return normally from this exception stack frame to after
  the catch. )
: EXCEPTION-MARKER ( -- n )
  RDROP
  0
;

( Execute xt with the parameters.  If no uncaught exception was thrown inside
  xt, push xt's return values and nzero.  If an uncaught exception was thrown
  inside xt, push n undefined cells and nexception. )
: CATCH ( wparam1 wparam2 ... wparamn xt -- wret1 wret2 ... wretm nzero )
  ( wparam1 wparam2 ... wparamn xt -- wundef1 wundef2 ... wundefn nexception )
  \ 8+ to skip over xt
  DSP@ 8+ >R
  \ 8+ to jump directly into EXCEPTION-MARKER
  ' EXCEPTION-MARKER 8+ >R
  EXECUTE
;

( Throw exception n.  If n is 0, do nothing. )
: THROW ( n -- ? )
  \ exit if n is 0
  ?DUP UNLESS
    EXIT
  THEN

  \ unwind stack
  RSP@	( n a-return )
  BEGIN
    \ while not at bottom of return stack
    DUP [ R0 8- ] LITERAL <
  WHILE
    \ if found EXCEPTION-MARKER
    DUP @ ' EXCEPTION-MARKER 8+ = IF
      8+	( n a-a-dsp-old )
      \ set return stack pointer to return address of CATCH
      \  and get frame's data stack pointer
      RSP! R>	( n a-dsp-old )
      \ preserve n
      SWAP >R
      \ restore frame's data stack pointer
      DSP!
      \ store n
      R>
      EXIT
    THEN
    8+	( n a-return-next )
  REPEAT

  \ uncaught exception
  DROP	( n )
  CASE
     \ exception -1: ABORT
    -1 OF
      ." ABORTED" CR
    ENDOF
    \ other exception
    ." UNCAUGHT THROW: " DUP . CR
  ENDCASE
  QUIT
;

( Throw abort exception: -1. )
: ABORT ( -- )
  -1 THROW
;

( Print stack trace from current position to bottom of return stack. )
: PRINT-STACK-TRACE ( -- )
  RSP@	( a-return )
  BEGIN
    DUP [ R0 8- ] LITERAL <
  WHILE
    DUP @ CASE
      \ exception stack frame
      ' EXCEPTION-MARKER 8+ OF
        ." CATCH ( DSP = "
        \ print and skip over data stack pointer
        8+
        DUP @ U.
        ."  ) "
      ENDOF
      \ return address
      DUP CFA>	( a-return a-codeword a-word )
      ?DUP IF
        2DUP	( a-return a-codeword a-word a-codeword a-word )
        \ print word name
        ID.	( a-return a-codeword a-word a-codeword )
        ."  + "
        \ print offset
        SWAP >DFA 8+ -	( a-return a-codeword uoffset )
        .
      THEN
    ENDCASE	( a-return )
    TAB
    8+
  REPEAT
  DROP
;

( C strings
  =========

A C string is just a pointer to a byte sequence, where the NULL-byte 0x00 marks
the end.

FORTH strings are created via S" ..." and C strings via Z" ...".  Converting
from a FORTH to a C string is done via CSTRING; converting from a C string to a
FORTH string is done using STRLEN, via the expression DUP STRLEN. )

( Parse the following input until the next quotation mark, store it followed by
  a terminating NULL byte at HERE without changing HERE, and push the start of
  the stored string on stack.  c-str-z is stored at HERE and could be
  overwritten by anything that uses HERE.  In compiling mode, compile it as

	LITSTRING ulen c-str-z padding DROP

at HERE, and increment HERE to point after padding. )
: Z" ( -- c-str-z )
  STATE @ IF
    \ LITSTRING
    ' LITSTRING ,
    HERE @	( a-len )
    \ dummy length
    0 ,
    \ the string
    BEGIN
      KEY
      DUP [ CHAR " ] LITERAL <>
    WHILE	( a-len c )
      C,
    REPEAT
    DROP	( a-len )
    \ terminating NULL byte
    '0 C,
    \ calculate and save length
    DUP HERE @	( a-len a-len c-end )
    SWAP - 8-	( a-len ulen )
    SWAP !
    \ padding
    ALIGN
    \ DROP to drop ulen after LITSTRING pushed it
    ' DROP ,
  ELSE
    HERE @	( c-str-z-end )
    \ the string
    BEGIN
      KEY
      DUP [ CHAR " ] LITERAL <>
    WHILE	( c-str-z-end c )
      OVER C!
      1+
    REPEAT
    DROP	( c-str-z-end )
    \ terminating NULL byte
    '0 SWAP C!
    HERE @	( c-str-z )
  THEN
; IMMEDIATE

( Count the length of a C string, i.e. the number of bytes starting at c-str-z
  and not including the terminating NULL byte. )
: STRLEN ( c-str-z -- ulen )
  \ preserve start
  DUP
  BEGIN
    DUP C@ 0<>
  WHILE
    1+	( c-str-z c-current )
  REPEAT	( c-str-z c-end )
  SWAP -	( ulen )
;

( Convert FORTH string to C string.  c-str-z is stored at HERE and could be
  overwritten by anything that uses HERE. )
: CSTRING ( c-str ulen -- c-str-z )
  SWAP OVER	( ulen c-str ulen )
  \ copy the string to HERE
  HERE @ SWAP	( ulen c-str c-str-z ulen )
  CMOVE	( ulen )
  \ append terminating NULL byte
  HERE @ +
  '0 SWAP C!
  \ push c-str-z
  HERE @
;

( The environment
  ===============

S0 is initialized to the top of stack at program startup, which is where the
environment of the program is passed in from the OS.

	       ╹                ╹
	       ╹  .             ╹
	       ╏  .             ╏
	       ╏ stack          ╏
	       ╏  .             ╏
	       ╏  .             ╏
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ argc           ┃◀ ── S0
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ argv[0]        ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ argv[1]        ┃
	  −  │ ┣━━━━━━━━━━━━━━━━┫
	     │ ╏  .             ╏
	addr │ ╏  .             ╏
	     │ ╏  .             ╏
	  +  ▼ ┣━━━━━━━━━━━━━━━━┫
	       ┃ argv[argc – 1] ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ NULL           ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ envp[0]        ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ envp[1]        ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ╏  .             ╏
	       ╏  .             ╏
	       ╏  .             ╏
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ envp[n – 1]    ┃
	       ┣━━━━━━━━━━━━━━━━┫
	       ┃ NULL           ┃
	       ┗━━━━━━━━━━━━━━━━┛

argc is a 64-bit unsigned integer storing the number of elements in argv.
argv[0] through argv[argc – 1] are pointers to C strings containing the program
arguments.  argv[argc] is NULL.  envp[0] through envp[n – 1] are pointers to C
strings containing the environment variables and their values, each of the form

	<name> "=" <value>

envp[n] is NULL. )

( Get number of arguments to the program, including the name of the program. )
: ARGC ( -- uargc )
  S0 @ @
;

( Get u-th argument to the program, the 0-th being the program name. )
: ARGV ( u -- c-str ulen )
  1+ CELLS	( uoffset )
  S0 @	( uoffset a-argv-0 )
  + @	( c-argv-u-z )
  DUP STRLEN	( c-argv-u ulen )
;

( Get start of environment variable pointer array. )
: ENVIRON ( -- a-envp-0 )
  ARGC 2 + CELLS	( uoffset )
  S0 @ +	( a-envp-0 )
;

( System
  ====== )

( Print the error message in c-str ulen, followed by ": ERRNO = ", followed by
  nerrno and a newline. )
: PERROR ( nerrno c-str ulen -- )
  TELL
  ." : ERRNO = "
  . CR
;

( Exit the program normally. )
: BYE ( -- )
  \ exit status 0
  0
  1 SYS_EXIT SYSCALL
;

( Attempt to set new break via syscall brk.  If brk succeeds, a-break is greater
  than or equal to a-break-new; otherwise a-break is the current break. )
: BRK ( a-break-new -- a-break )
  1 SYS_BRK SYSCALL
;

( Get current break via syscall brk. )
: GET-BRK ( -- a-break )
  \ NULL is never accepted by brk, so it returns the current break
  NULL BRK
;

( Calculate number of cells in data segment after HERE. )
: UNUSED ( -- ucells )
  GET-BRK
  HERE @
  - 8 /
;

( Increase size of data segment by ucells cells.  fsuccess indicates whether the
  allocation succeeded. )
: MORECORE ( ucells -- fsuccess )
  CELLS GET-BRK +	( a-break-new )
  DUP BRK	( a-break-new a-break )
  U<=	( fsuccess )
;

( Files
  ===== )

( File access mode for opening read-only. )
: R/O ( -- naccess-mode ) O_RDONLY ;

( File access mode for opening read-write. )
: R/W ( -- naccess-mode ) O_RDWR ;

( If n negative, nerrno is −n, otherwise nerrno is 0. )
: WITH-ERRNO ( n -- n nerrno )
  DUP	( n n )
  DUP 0< IF
    NEGATE	( n nerrno )
  ELSE
    DROP 0	( n 0 )
  THEN
;

( Open file with filename c-str ulen and the file access modes in the bits of
  naccess-modes.  If the file was opened successfully, nerrno is 0 and nfd is
  the file descriptor.  Otherwise, nerrno is non-0 and nfd is indeterminate. )
: OPEN-FILE ( c-str ulen naccess-modes -- nfd nerrno )
  -ROT CSTRING SWAP	( c-str-z naccess-modes )
  2 SYS_OPEN SYSCALL	( nfd )
  WITH-ERRNO
;

( Create and open write-only a new empty file with filename c-str ulen,
  truncating if it already exists.  If the file was created and opened
  successfully, nerrno is 0 and nfd is the file descriptor.  Otherwise, nerrno
  is non-0 and nfd is indeterminate. )
: CREATE-FILE ( c-str ulen -- nfd nerrno )
  CSTRING
  [OCTAL] 644	( c-str-z nmode-bits )
  2 SYS_CREAT SYSCALL	( nfd )
  WITH-ERRNO
;

( Close file with file descriptor nfd.  nerrno is 0 on success, non-0 on
  failure. )
: CLOSE-FILE ( nfd -- nerrno )
  1 SYS_CLOSE SYSCALL
  NEGATE
;

( Read up to ucount bytes from nfd to c-buffer.  If successful, nerrno is 0 and
  uread is the number of bytes actually read (0 on end-of-file).  Otherwise,
  nerrno is non-0 and uread is indeterminate. )
: READ-FILE ( c-buffer ucount nfd -- uread nerrno )
  -ROT
  3 SYS_READ SYSCALL	( nread )
  WITH-ERRNO
;

( Write up to ucount bytes from c-buffer to nfd, unbuffered.  If successful,
  nerrno is 0 and uwritten is the number of bytes actually written (which may be
  0).  Otherwise, nerrno is non-0 and uwritten is indeterminate. )
: WRITE-FILE ( c-buffer ucount nfd -- uwritten nerrno )
  -ROT
  3 SYS_WRITE SYSCALL	( nwritten )
  WITH-ERRNO
;

\ auxiliary word
HIDE WITH-ERRNO

( Assembly in FORTH
  =================

Assembly words are defined similaryly to FORTH words.

	: WORD-NAME [ assembler-code ... ] ;CODE

;CODE works analogously to ;, except it appends NEXT instead of EXIT at the end
of the word's definition, and it replaces DOCOL with a pointer to the assembler
code in the word definition.

Within assembly words, the programmer may use machine instructions directly via
compiling literals, e.g.

	[HEX] F7 C, [BINARY] 11_101_011 C,	( imul EBX )
	[HEX] 89 C, [BINARY] 11_000_010 C,	( mov EDX, EAX )

or indirectly via immediate FORTH words that compile down to machine code, e.g.

	EBX IMUL
	EDX EAX MOV )

( Inserted at the end of assembler-words. )
: NEXT
  \ lodsq
  \  REX : W
  [BINARY] 0100_1_0_0_0 C,
  \  AD
  [HEX] AD C,
  \ jmp [RAX] : absolute indirect
  \  FF
  [HEX] FF C,
  \  ModR/M : mod = unused, opcode-ext = 4, r/m = RAX
  [BINARY] 00_100_000 C,
; IMMEDIATE

( Terminate the definition of an assembler-word.  Corresponds to the previous
  :. )
: ;CODE ( -- )
  \ insert NEXT
  [COMPILE] NEXT
  ALIGN
  LATEST @ DUP	( a-word a-word )
  \ turn off hidden flag
  HIDDEN	( a-word )
  \ store pointer to DFA in codeword
  DUP >DFA SWAP >CFA	( a-dfa a-cfa )
  !
  \ exit compiling mode
  [COMPILE] [
; IMMEDIATE

( Operands to assembly instructions are reprensented by 2 cells.  The top one is
  the type of the operand, one of the ASM-* constants, the bottom one is the
  value, whose interpretation is determined by the type. )

( Immediate byte value. )
000 CONSTANT ASM-IMM8

( Immediate word value. )
001 CONSTANT ASM-IMM16

( Immediate doubleword value. )
002 CONSTANT ASM-IMM32

( Immediate quadword value. )
003 CONSTANT ASM-IMM64

( General-purpose byte register. AL through R15L. )
100 CONSTANT ASM-R8

( General-purpose word register. AX through R15W. )
101 CONSTANT ASM-R16

( General-purpose doubleword register. EAX through R15D. )
102 CONSTANT ASM-R32

( General-purpose quadword register. RAX through R15. )
103 CONSTANT ASM-R64

( Direct absolute address, word offset. )
200 CONSTANT ASM-PTR16:16

( Direct absolute address, doubleword offset. )
201 CONSTANT ASM-PTR16:32

( Direct relative byte address. )
300 CONSTANT ASM-REL8

( Direct relative word address. )
301 CONSTANT ASM-REL16

( Direct relative doubleword address. )
302 CONSTANT ASM-REL32

( Memory byte effective address. )
400 CONSTANT ASM-M8

( Memory word effective address. )
401 CONSTANT ASM-M16

( Memory doubleword effective address. )
402 CONSTANT ASM-M32

( Memory quadword effective address. )
403 CONSTANT ASM-M64

( Indirect absolute address, word offset. )
500 CONSTANT ASM-M16:16

( Indirect absolute address, doubleword offset. )
500 CONSTANT ASM-M16:32

( Indirect absolute address, quadword offset. )
500 CONSTANT ASM-M16:64

HEX

( Choose the smalles AMS-IMM* type which nimm fits into. )
: ASM:IMM ( nimm -- nimm utype )
  DUP -80 >= OVER 7F <= AND IF
    ASM-IMM8
  ELSE
    DUP -8000 >= OVER 7FFF <= AND IF
      ASM-IMM16
    ELSE
      DUP -80000000 >= OVER 7FFFFFFF <= AND IF
        ASM-IMM32
      ELSE
        ASM-IMM64
      THEN
    THEN
  THEN
; IMMEDIATE

( Choose the smallest ASM-IMM* type which uimm fits into. )
: ASM:UIMM ( uimm -- uimm utype )
  DUP FF U<= IF
    ASM-IMM8
  ELSE
    DUP FFFF U<= IF
      ASM-IMM16
    ELSE
      DUP FFFFFFFF U<= IF
        ASM-IMM32
      ELSE
        ASM-IMM64
      THEN
    THEN
  THEN
; IMMEDIATE

OCTAL

: ASM:AL   00 ASM-R8 ; IMMEDIATE
: ASM:CL   01 ASM-R8 ; IMMEDIATE
: ASM:DL   02 ASM-R8 ; IMMEDIATE
: ASM:BL   03 ASM-R8 ; IMMEDIATE
: ASM:SPL  04 ASM-R8 ; IMMEDIATE
: ASM:BPL  05 ASM-R8 ; IMMEDIATE
: ASM:SIL  06 ASM-R8 ; IMMEDIATE
: ASM:DIL  07 ASM-R8 ; IMMEDIATE
: ASM:R8L  10 ASM-R8 ; IMMEDIATE
: ASM:R9L  11 ASM-R8 ; IMMEDIATE
: ASM:R10L 12 ASM-R8 ; IMMEDIATE
: ASM:R11L 13 ASM-R8 ; IMMEDIATE
: ASM:R12L 14 ASM-R8 ; IMMEDIATE
: ASM:R13L 15 ASM-R8 ; IMMEDIATE
: ASM:R14L 16 ASM-R8 ; IMMEDIATE
: ASM:R15L 17 ASM-R8 ; IMMEDIATE

: ASM:AX   00 ASM-R16 ; IMMEDIATE
: ASM:CX   01 ASM-R16 ; IMMEDIATE
: ASM:DX   02 ASM-R16 ; IMMEDIATE
: ASM:BX   03 ASM-R16 ; IMMEDIATE
: ASM:SP   04 ASM-R16 ; IMMEDIATE
: ASM:BP   05 ASM-R16 ; IMMEDIATE
: ASM:SI   06 ASM-R16 ; IMMEDIATE
: ASM:DI   07 ASM-R16 ; IMMEDIATE
: ASM:R8W  10 ASM-R16 ; IMMEDIATE
: ASM:R9W  11 ASM-R16 ; IMMEDIATE
: ASM:R10W 12 ASM-R16 ; IMMEDIATE
: ASM:R11W 13 ASM-R16 ; IMMEDIATE
: ASM:R12W 14 ASM-R16 ; IMMEDIATE
: ASM:R13W 15 ASM-R16 ; IMMEDIATE
: ASM:R14W 16 ASM-R16 ; IMMEDIATE
: ASM:R15W 17 ASM-R16 ; IMMEDIATE

: ASM:EAX  00 ASM-R32 ; IMMEDIATE
: ASM:ECX  01 ASM-R32 ; IMMEDIATE
: ASM:EDX  02 ASM-R32 ; IMMEDIATE
: ASM:EBX  03 ASM-R32 ; IMMEDIATE
: ASM:ESP  04 ASM-R32 ; IMMEDIATE
: ASM:EBP  05 ASM-R32 ; IMMEDIATE
: ASM:ESI  06 ASM-R32 ; IMMEDIATE
: ASM:EDI  07 ASM-R32 ; IMMEDIATE
: ASM:R8D  10 ASM-R32 ; IMMEDIATE
: ASM:R9D  11 ASM-R32 ; IMMEDIATE
: ASM:R10D 12 ASM-R32 ; IMMEDIATE
: ASM:R11D 13 ASM-R32 ; IMMEDIATE
: ASM:R12D 14 ASM-R32 ; IMMEDIATE
: ASM:R13D 15 ASM-R32 ; IMMEDIATE
: ASM:R14D 16 ASM-R32 ; IMMEDIATE
: ASM:R15D 17 ASM-R32 ; IMMEDIATE

: ASM:RAX  00 ASM-R64 ; IMMEDIATE
: ASM:RCX  01 ASM-R64 ; IMMEDIATE
: ASM:RDX  02 ASM-R64 ; IMMEDIATE
: ASM:RBX  03 ASM-R64 ; IMMEDIATE
: ASM:RSP  04 ASM-R64 ; IMMEDIATE
: ASM:RBP  05 ASM-R64 ; IMMEDIATE
: ASM:RSI  06 ASM-R64 ; IMMEDIATE
: ASM:RDI  07 ASM-R64 ; IMMEDIATE
: ASM:R8   10 ASM-R64 ; IMMEDIATE
: ASM:R9   11 ASM-R64 ; IMMEDIATE
: ASM:R10  12 ASM-R64 ; IMMEDIATE
: ASM:R11  13 ASM-R64 ; IMMEDIATE
: ASM:R12  14 ASM-R64 ; IMMEDIATE
: ASM:R13  15 ASM-R64 ; IMMEDIATE
: ASM:R14  16 ASM-R64 ; IMMEDIATE
: ASM:R15  17 ASM-R64 ; IMMEDIATE

DECIMAL

( Store word (lower 16 bits) in w at HERE in little-endian form, and increment
  HERE to point one past the stored word. )
: W, ( w -- )
  DUP C,
  8 >> C,
;

( Store doubleword (lower 32 bits) in w at HERE in little-endian form, and
  increment HERE to point one paste the stored doubleword. )
: D, ( w -- )
  DUP C,
  8 >> DUP C,
  8 >> DUP C,
  8 >> C,
;

HEX

( Instruction prefix operand-size override. )
66 CONSTANT ASM-PREFIX-OPERAND-SO

( Instruction prefix address-size override. )
67 CONSTANT ASM:PREFIX-ADDRESS-SO

( Instruction prefix REX with no flags set. )
[BINARY] 0100_0000 CONSTANT ASM-PREFIX-REX

( Instruction prefix REX with flag B set. )
ASM-PREFIX-REX [BINARY] 0001 OR CONSTANT ASM-PREFIX-REX.B

( Instruction prefix REX with flag X set. )
ASM-PREFIX-REX [BINARY] 0010 OR CONSTANT ASM-PREFIX-REX.X

( Instruction prefix REX with flag R set. )
ASM-PREFIX-REX [BINARY] 0100 OR CONSTANT ASM-PREFIX-REX.R

( Instruction prefix REX with flag W set. )
ASM-PREFIX-REX [BINARY] 1000 OR CONSTANT ASM-PREFIX-REX.W

( Compile instruction PUSH operand1. )
: ASM:PUSH ( woperand1 utype1 -- )
  CASE
    ASM-IMM8 OF
      6A C,
      C,
    ENDOF
    ASM-IMM16 OF
      ASM-PREFIX-OPERAND-SO C,
      68 C,
      W,
    ENDOF
    ASM-IMM32 OF
      68 C,
      D,
    ENDOF
    ASM-R16 OF
      ASM-PREFIX-OPERAND-SO C,
      DUP [BINARY] 1000 AND IF
        [BINARY] 0111 AND
        ASM-PREFIX-REX.B C,
      THEN
      50 OR C,
    ENDOF
    ASM-R64 OF
      DUP [BINARY] 1000 AND IF
        [BINARY] 0111 AND
        ASM-PREFIX-REX.B C,
      THEN
      50 OR C,
    ENDOF
    \ TODO: not implemented: r/m16, r/m64, FS, GS
    ." ERROR: invalid operand type for PUSH: " DUP U. CR
    NIP
  ENDCASE
; IMMEDIATE

DECIMAL
