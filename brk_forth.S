	/* brk_forth: B. R. K.'s simple FORTH compiler for x86-64 GNU/Linux.

	Based on the public domain FORTH compiler `jonesforth` by Richard
	W. M. Jones <rich@annexia.org> http://annexia.org/forth.

	License: Creative Commons 0 (CC0)
	<http://creativecommons.org/publicdomain/zero/1.0/>.  To the extent
	possible under law, B. R. K. has waived all copyright and related or
	neighboring rights to `brk_forth`.
	*/

	/* This file contains the basic assembly implementation of brk_forth.
	It uses GNU as-specific syntax and should be assembled by GCC (see
	`Makefile`).
	*/

	.intel_syntax	noprefix

	// Numeric codes for Linux syscalls.
#include	<asm/unistd.h>

	// Version.
	.set	BRK_FORTH_VERSION_MAJOR,	0
	.set	BRK_FORTH_VERSION_MINOR,	1
	.set	BRK_FORTH_VERSION_PATCH,	0

	/* fatal_error prints string error_msg preceded by "FATAL ERROR: " and
	followed by a newline to stderr, and exits the program with exit status
	status.
	*/
	.macro	fatal_error	error_msg:req,	status=100
	//// the error message
	.section	.rodata
10001:
	.ascii	"FATAL ERROR: \error_msg\n"
10002:
	.text
	// long /* ssize_t */ write(int fildes, void *buf,
	//                          unsigned long /* size_t */ nbyte)
	/// EDI = fildes = 2 /* STDERR_FILENO */
	mov	edi,	2
	/// RSI = buf = message start
	lea	rsi,	[10001b]
	/// RDX = nbyte = message end - message start
	lea	rdx,	[10002b - 10001b]
	mov	rax,	__NR_write
	syscall
	//  NOTE: no need to check return value
	// void exit(int status)
	/// EDI = status = status
	mov	edi,	\status
	mov	rax,	__NR_exit
	syscall
	//  NOTE: no need to check return value
	.endm

	/* Design
	   ======

	This FORTH has 64-bit (8-byte) cells.

	The dictionary is a linked list of words, in the following format:

	  NULL
	   ▲
	   ╎
	┏━━╎━━━━━━━━━━━━━┳━━━━━━━┯━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍┓
	┃ link pointer   ┃ flags │ length ┃ name      ┃ padding   ┃ definition ┃
	┃            8 B ┃   3 b │    5 b ┃  length B ┃  padlen B ┃            ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━┷━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍┛
	   ▲
	   ╎
	   ╎
	  . . . .
	   ▲
	   ╎
	┏━━╎━━━━━━━━━━━━━┳━━━━━━━┯━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍┓
	┃ link pointer   ┃ flags │ length ┃ name      ┃ padding   ┃ definition ┃
	┃            8 B ┃   3 b │    5 b ┃  length B ┃  padlen B ┃            ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━┷━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍┛
	   ▲
	   ╎
	┏━━╎━━━━━━━━━━━━━┳━━━━━━━┯━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍┓
	┃ link pointer   ┃ flags │ length ┃ name      ┃ padding   ┃ definition ┃
	┃            8 B ┃   3 b │    5 b ┃  length B ┃  padlen B ┃            ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━┷━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍┛
	   ▲
	   ╎
	   ╎
	  . . . .
	   ▲
	   ╎
	┏━━╎━━━━━━━━━━━━━┳━━━━━━━┯━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍┓
	┃ link pointer   ┃ flags │ length ┃ name      ┃ padding   ┃ definition ┃
	┃            8 B ┃   3 b │    5 b ┃  length B ┃  padlen B ┃            ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━┷━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍┛
	   ▲
	   ╎
	   ╎
	  LATEST

	The FORTH variable LATEST points to the most recently defined word, each
	word contains a pointer to the word defined directly before it, and the
	oldest word contains a pointer to NULL.

	The padding ensures that the definition always starts on a 64-bit
	boundary.  It is therefore the smallest number of zero-bytes such that

		(1 + length + padlen) % 8 = 0.

	A definition normally has the following format for words implemented in
	assembly (where codeword points to the start of assembly):

	┏━━━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┳━━━━━━━━━━━━━━━━━━━┓
	┃ codeword    ┃ assembly                ┃ assembly NEXT     ┃
	┃         8 B ┃               [0 – ∞) B ┃         unknown B ┃
	┗━━╎━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍╍┻━━━━━━━━━━━━━━━━━━━┛
	   ╎             ▲
	   └╌╌╌╌╌╌╌╌╌╌╌╌╌┘

	and normally the following format for words implemented in FORTH (where
	codeword is DOCOL):

	┏━━━━━━━━━━━━━┳╍╍╍╍╍╍╍╍╍╍╍╍╍┳╍╍╍╍╍╍╍╍╍╍╍╍╍┳╸╺╸╺╸╺━╸╺╸╺╸╺┳╍╍╍╍╍╍╍╍╍╍╍╍╍┓
	┃ codeword    ┃ cw ptr 1    ┃ cw ptr 2    ┃ . . .       ┃ cw ptr EXIT ┃
	┃         8 B ┃         8 B ┃         8 B ┃             ┃         8 B ┃
	┗━━━━━━━━━━━━━┻╍╍╍╍╍╍╍╍╍╍╍╍╍┻╍╍╍╍╍╍╍╍╍╍╍╍╍┻╸╺╸╺╸╺━╸╺╸╺╸╺┻╍╍╍╍╍╍╍╍╍╍╍╍╍┛

	RSI always contains the address of the next codeword to be entered after
	the current word finishes.
	*/

	/* NEXT is used at the end of each word.  It increments RSI to point to
	the address of the *new* next codeword, and jumps to what the *old* next
	codeword points to.
	*/
	.macro	NEXT
	lodsq
	jmp	[rax]
	.endm

	/* RSP points to the top of the data stack.  The data stack grows
	towards zero in memory.  It starts directly before `data_stack_top` and
	ends at a system-dependent location.  RSP must point before (non-empty
	stack) or at (empty stack) `data_stack_top`.

	       ╎  . . .      ╎
	       ╎             ╎
	       │ BELOW STACK │
	       ┟━━━━━━━━━━━━━┧
	       ╏  .          ╏
	       ╏  .          ╏
	       ╏             ╏
	       ╏ FREE SPACE  ╏
	       ╏             ╏
	       ╏  .          ╏
	       ╏  .          ╏
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell n      ┃◀ ── RSP
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell n - 1  ┃
	  −  │ ┣━━━━━━━━━━━━━┫
	     │ ╏  .          ╏
	addr │ ╏  .          ╏
	     │ ╏  .          ╏
	  +  ▼ ┣━━━━━━━━━━━━━┫
	       ┃ cell 1      ┃
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell 0      ┃
	       ┞━━━━━━━━━━━━━┦
	       │ ABOVE STACK │◁ ── data_stack_top
	       ├─ ─ ─ ─ ─ ─ ─┤
	       ╎  . . .      ╎
	*/

	/* DS_RSP_VALID calls fatal_error if RSP does not point to a valid
	address within the data stack.
	*/
	.macro	DS_RSP_VALID
#ifndef	NDEBUG
	cmp	rsp,	data_stack_top
	jbe	11001f
	// if RSP > top: not within stack
	fatal_error	"top of data stack is not a valid address within data stack"
11001:
#else
	// noop for non-DEBUG
#endif
	.endm

	/* DS_PUSH pushes x on the data stack.
	*/
	.macro	DS_PUSH	x:req
	// NOTE: stack overflow is left to be detected by the system
	push	\x
	.endm

	/* DS_POP pops the top of the data stack into x.
	*/
	.macro	DS_POP	x:req
#ifndef	NDEBUG
	cmp	rsp,	data_stack_top
	jb	12001f
	// if RSP == top: stack empty, cannot pop
	fatal_error	"data stack underflow"
12001:
#endif
	pop	\x
	.endm

	/* DS_DROP drops top n elements of data stack.
	*/
	.macro	DS_DROP	n=1
	lea	rsp,	[rsp + (8 * \n)]
	DS_RSP_VALID
	.endm

	/* DS_MIN_SIZE calls fatal_error if there are not at least n elements on
	data stack.
	*/
	.macro	DS_MIN_SIZE	n:req
#ifndef	NDEBUG
	// ! preserve RAX !
	push	rax
	//// compare
	mov	rax,	data_stack_top
	sub	rax,	rsp
	// n + 1 because of the previously pushed RAX
	cmp	rax,	8 * (\n + 1)
	jae	13001f
	// if stack size < n
	fatal_error	"need at least \n elements on data stack"
13001:
	// ! restore RAX !
	pop	rax
#else
	// noop for non-DEBUG
#endif
	.endm

	/* RBP points to the top of the return stack (unlike its conventional
	use as base of the stack frame).  The return stack grows towards zero in
	memory.  It starts directly before `return_stack_top` and ends at
	`return_stack`.  RBP must point between `return_stack` (full stack) and
	`return_stack_top` (empty stack), inclusive.

	       ╎  . . .      ╎
	       ╎             ╎
	       │ BELOW STACK │
	       ┟━━━━━━━━━━━━━┧
	       ╏ LAST SPACE  ╏◀ ── return_stack
	       ┣╍╍╍╍╍╍╍╍╍╍╍╍╍┫
	       ╏  .          ╏
	       ╏  .          ╏
	       ╏             ╏
	       ╏ FREE SPACE  ╏
	       ╏             ╏
	       ╏  .          ╏
	       ╏  .          ╏
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell n      ┃◀ ── RBP
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell n - 1  ┃
	  −  │ ┣━━━━━━━━━━━━━┫
	     │ ╏  .          ╏
	addr │ ╏  .          ╏
	     │ ╏  .          ╏
	  +  ▼ ┣━━━━━━━━━━━━━┫
	       ┃ cell 1      ┃
	       ┣━━━━━━━━━━━━━┫
	       ┃ cell 0      ┃
	       ┞━━━━━━━━━━━━━┦
	       │ ABOVE STACK │◀ ── return_stack_top
	       ├─ ─ ─ ─ ─ ─ ─┤
	       ╎  . . .      ╎
	*/

	/* RS_RBP_VALID calls fatal_error if RBP does not point to a valid
	address within the return stack.
	*/
	.macro	RS_RBP_VALID
#ifndef	NDEBUG
	cmp	rbp,	offset return_stack
	jb	14001f
	cmp	rbp,	offset return_stack_top
	jbe	14002f
14001:
	// if RBP < return_stack or RBP > return_stack_top: not within stack
	fatal_error	"top of return stack is not a valid address within return stack"
14002:
#else
	// noop for non-DEBUG
#endif
	.endm

	/* RS_PUSH pushes x on the return stack.
	*/
	.macro	RS_PUSH	x:req
#ifndef	NDEBUG
	cmp	rbp,	offset return_stack
	ja	15001f
	// if RBP == return_stack: stack full, cannot push
	fatal_error	"return stack overflow"
15001:
#endif
	lea	rbp,	[rbp - 8]
	mov	[rbp],	\x
	.endm

	/* RS_POP pops the top of the return stack into x.
	*/
	.macro	RS_POP	x:req
#ifndef	NDEBUG
	cmp	rbp,	offset return_stack_top
	jb	16001f
	// if RBP == return_stack_top: stack empty, cannot pop
	fatal_error	"return stack underflow"
16001:
#endif
	mov	\x,	[rbp]
	lea	rbp,	[rbp + 8]
	.endm

	/* RS_DROP drops top n elements of return stack.
	*/
	.macro	RS_DROP	n=1
	lea	rbp,	[rbp + (8 * \n)]
	RS_RBP_VALID
	.endm

	/* DOCOL is used as the codeword in FORTH-implemented words.  It pushes
	the old RSI on the return stack, makes RSI the address of the first
	codeword pointer in the word, and calls NEXT.
	*/
	.text
	.align 8
DOCOL:
	RS_PUSH	rsi
	// RAX contains address of codeword from NEXT
	lea	rsi,	[rax + 8]
	NEXT

	/* Entry point
	   ===========

	We set any necessary flags and variables, the return stack, data stack,
	and the data segment. Finally we jump to the FORTH word QUIT.
	*/
	.text
	.global	_start
_start:
	// clear flag DF: make sure LODS and friends increment their register
	cld

	// initialize data stack
	mov	data_stack_top,	rsp
	mov	var_S0,	rsp
	// initialize return stack
	lea	rbp,	[return_stack_top]

	// set up data segment
	call	data_segment_set_up

	// initialize & run interpreter
	lea	rsi,	[cold_start]
	NEXT

	.section	.rodata
	.align	8
cold_start:
	.8byte	QUIT

	/* FORTH primitives
	   ================

	Some of these are defined in assembly not because they cannot by defined
	in FORTH, but for speed.
	*/

	/* Bitmask for length in length-flags byte.
	*/
	.set	FLAG_LENGTH_MASK,	0b00011111

	/* Bitmask for immediate flag in length-flags byte.  When set, the word
	is run immediately by INTERPRET even in compile mode.
	*/
	.set	FLAG_IMMEDIATE,	0b01000000

	/* Bitmask for hidden flag in length-flags byte.  When set, the word
	cannot be found by FIND.
	*/
	.set	FLAG_HIDDEN,		0b00100000

	// link to previously defined word, used in define_header
	// initial value 0: oldest word points to NULL
	.set	link_ptr,	0

	/* Auxiliary macro used by define_word and define_code.
	*/
	.macro	define_header	label:req,	name:req,	flags:req
	.section	.rodata
	.align	8
	.global	name_\label
name_\label :
	// link pointer
	.8byte	link_ptr
	// set link pointer to this word
	.set	link_ptr,	name_\label
	// length-flags byte
	.set	name_length,	17002f - 17001f
	.byte	\flags | name_length
	// name
17001:
	.ascii	"\name"
17002:
	// padding
	.align	8
	.global	\label
\label :
	// codeword to follow
	.endm

	/* define_word defines a FORTH word as if it were FORTH-implemented.  It
	must be followed by the codeword pointers making up the word.  label is
	the name of the word as a label; name is the name string of the word;
	flags is a bitmask of the word flags.
	*/
	.macro	define_word	label:req,	name:req,	flags=0
	define_header	\label,	"\name",	\flags
	// codeword: DOCOL
	.8byte	DOCOL
	// codeword pointers to follow
	.endm

	/* define_code defines a FORTH word with assembly implementation.  It
	must be followed by the assembly code making up the word.  label is the
	name of the word as a label; name is the name string of the word; flags
	is a bitmask of the word flags.
	*/
	.macro	define_code	label:req,	name:req,	flags=0
	define_header	\label,	"\name",	\flags
	// codeword: pointer to the assembly code
	.8byte	code_\label
	.text
	.global	code_\label
code_\label :
	// assembly code to follow
	.endm

	/* Stack manipulation
	   ------------------
	*/

	/* ( w -- ) Drop top element.
	*/
	define_code	DROP,	"DROP"
	DS_DROP
	NEXT

	/* ( wu ... w2 w1 u -- ) Drop u and top u elements below it.
	*/
	define_code	NDROP,	"NDROP"
	DS_POP	rax
	DS_DROP	rax
	NEXT

	/* ( w1 w2 -- w2 w1 ) Swap top 2 elements.
	*/
	define_code	SWAP,	"SWAP"
	DS_POP	rax
	DS_POP	rbx
	DS_PUSH	rax
	DS_PUSH	rbx
	NEXT

	/* ( w1 -- w1 w1 ) Duplicate top element.
	*/
	define_code	DUP,	"DUP"
	DS_MIN_SIZE	1
	mov	rax,	[rsp]
	DS_PUSH	rax
	NEXT

	/* ( w1 w2 -- w1 w2 w1 ) Duplicate w1 on top of stack.
	*/
	define_code	OVER,	"OVER"
	DS_MIN_SIZE	2
	mov	rax,	[rsp + 8]
	DS_PUSH	rax
	NEXT

	/* ( w1 w2 w3 -- w2 w3 w1 ) Rotate top 3 elements downwards.
	*/
	define_code	ROT,	"ROT"
	DS_POP	rax
	DS_POP	rbx
	DS_POP	rcx
	DS_PUSH	rbx
	DS_PUSH	rax
	DS_PUSH	rcx
	NEXT

	/* ( w1 w2 w3 -- w3 w1 w2 ) Rotate top 3 elements upwards.
	*/
	define_code	$HYPHEN_MINUS$ROT,	"-ROT"
	DS_POP	rax
	DS_POP	rbx
	DS_POP	rcx
	DS_PUSH	rax
	DS_PUSH	rcx
	DS_PUSH	rbx
	NEXT

	/* ( w1 w2 -- ) Drop top 2 elements.
	*/
	define_code	$DIGIT_TWO$DROP,	"2DROP"
	DS_DROP	2
	NEXT

	/* ( w1 w2 -- w1 w2 w1 w2 ) Duplicate top 2 elements.
	*/
	define_code	$DIGIT_TWO$DUP,	"2DUP"
	DS_MIN_SIZE	2
	mov	rax,	[rsp]
	mov	rbx,	[rsp + 8]
	DS_PUSH	rbx
	DS_PUSH	rax
	NEXT

	/* ( w1 w2 w3 w4 -- w3 w4 w1 w2 ) Swap top 2 pairs of elements.
	*/
	define_code	$DIGIT_TWO$SWAP,	"2SWAP"
	DS_POP	rax
	DS_POP	rbx
	DS_POP	rcx
	DS_POP	rdx
	DS_PUSH	rbx
	DS_PUSH	rax
	DS_PUSH	rdx
	DS_PUSH	rcx
	NEXT

	/* ( w -- w ) OR ( w -- ) Duplicate top element if it is non-zero.
	*/
	define_code	$QUESTION_MARK$DUP,	"?DUP"
	DS_MIN_SIZE	1
	mov	rax,	[rsp]
	// NOTE: not using RCX with JRCXZ here as that would be slower
	test	rax,	rax
	// jump if zero
	jz	1f
	// if non-zero: duplicate
	DS_PUSH	rax
1:
	NEXT

	/* Arithmetics
	   -----------
	*/

	/* ( n1 -- nr ) Increment top element by 1.
	*/
	define_code	$DIGIT_ONE$$PLUS_SIGN$,	"1+"
	DS_MIN_SIZE	1
	inc	qword ptr [rsp]
	NEXT

	/* ( n1 -- nr ) Decrement top element by 1.
	*/
	define_code	$DIGIT_ONE$$HYPEN_MINUS$,	"1-"
	DS_MIN_SIZE	1
	dec	qword ptr [rsp]
	NEXT

	/* ( n1 -- nr ) Increment top element by 8, a cell's size.
	*/
	define_code	$DIGIT_EIGHT$$PLUS_SIGN$,	"8+"
	DS_MIN_SIZE	1
	add	qword ptr [rsp],	8
	NEXT

	/* ( n1 -- nr ) Decrement top element by 8, a cell's size.
	*/
	define_code	$DIGIT_EIGHT$$HYPEN_MINUS$,	"8-"
	DS_MIN_SIZE	1
	sub	qword ptr [rsp],	8
	NEXT

	/* ( n1 n2 -- nsum ) Add top 2 elements.
	*/
	define_code	$PLUS_SIGN$,	"+"
	DS_MIN_SIZE	2
	DS_POP	rax
	add	[rsp],	rax
	NEXT

	/* ( nminuend nsubtrahend -- ndifference ) Subtract top 2 elements.
	*/
	define_code	$HYPHEN_MINUS$,	"-"
	DS_MIN_SIZE	2
	DS_POP	rax
	sub	[rsp],	rax
	NEXT

	/* ( n1 n2 -- nproduct ) Multiply top 2 elements.
	*/
	define_code	$ASTERISK$,	"*"
	DS_POP	rax
	DS_POP	rbx
	imul	rax,	rbx
	// ignore overflow: flag OF
	DS_PUSH	rax
	NEXT

	/* ( u1 u2 -- uproduct ) Multiply top 2 elements.
	*/
	define_code	U$ASTERISK$,	"U*"
	DS_POP	rax
	DS_POP	rbx
	mul	rbx
	// ignore overflow: flag OF
	DS_PUSH	rax
	NEXT

	/* ( ndividend ndivisor -- nremainder nquotient ) Integer-divide top 2
	elements, resulting in a remainder and a quotient.
	*/
	define_code	$SOLIDUS$MOD,	"/MOD"
	// RBX = divisor
	DS_POP	rbx
	// RDX:RAX = dividend
	xor	rdx,	rdx
	DS_POP	rax
	// RAX = quotient; RDX = remainder
	idiv	rbx
	// ignore divide by zero and overflow: exception #DE
	DS_PUSH	rdx
	DS_PUSH	rax
	NEXT

	/* ( udividend udivisor -- uremainder uquotient ) Integer-divide top 2
	elements, resulting in a remainder and a quotient.
	*/
	define_code	U$SOLIDUS$MOD,	"U/MOD"
	// RBX = divisor
	DS_POP	rbx
	// RDX:RAX = dividend
	xor	rdx,	rdx
	DS_POP	rax
	// RAX = quotient; RDX = remainder
	div	rbx
	// ignore divide by zero and overflow: exception #DE
	DS_PUSH	rdx
	DS_PUSH	rax
	NEXT

	/* Auxiliary macro used by compare operators.
	*/
	.macro	operator_cmp_2	cc:req
	DS_POP	rax
	DS_POP	rbx
	cmp	rbx,	rax
	set\cc	al
	movzx	rax,	al
	DS_PUSH	rax
	.endm

	/* ( w1 w2 -- f ) Check whether w1 = w2.
	*/
	define_code	$EQUALS_SIGN$,	"="
	operator_cmp_2	e
	NEXT

	/* ( w1 w2 -- f ) Check whether w1 ≠ w2.
	*/
	define_code	$LESS_THAN_SIGN$$GREATER_THAN_SIGN$,	"<>"
	operator_cmp_2	ne
	NEXT

	/* ( n1 n2 -- f ) Check whether n1 < n2.
	*/
	define_code	$LESS_THAN_SIGN$,	"<"
	operator_cmp_2	l
	NEXT

	/* ( u1 u2 -- f ) Check whether u1 < u2.
	*/
	define_code	U$LESS_THAN_SIGN$,	"U<"
	operator_cmp_2	b
	NEXT

	/* ( n1 n2 -- f ) Check whether n1 > n2.
	*/
	define_code	$GREATER_THAN_SIGN$,	">"
	operator_cmp_2	g
	NEXT

	/* ( u1 u2 -- f ) Check whether u1 > u2.
	*/
	define_code	U$GREATER_THAN_SIGN$,	"U>"
	operator_cmp_2	a
	NEXT

	/* ( n1 n2 -- f ) Check whether n1 ≤ n2.
	*/
	define_code	$LESS_THAN_SIGN$$EQUALS_SIGN$,	"<="
	operator_cmp_2	le
	NEXT

	/* ( u1 u2 -- f ) Check whether u1 ≤ u2.
	*/
	define_code	U$LESS_THAN_SIGN$$EQUALS_SIGN$,	"U<="
	operator_cmp_2	be
	NEXT

	/* ( n1 n2 -- f ) Check whether n1 ≥ n2.
	*/
	define_code	$GREATER_THAN_SIGN$$EQUALS_SIGN$,	">="
	operator_cmp_2	ge
	NEXT

	/* ( u1 u2 -- f ) Check whether u1 ≥ u2.
	*/
	define_code	U$GREATER_THAN_SIGN$$EQUALS_SIGN$,	"U>="
	operator_cmp_2	ae
	NEXT

	/* Auxiliary macro used by compare-to-0 predicates.
	*/
	.macro	operator_test_1	cc:req
	DS_POP	rax
	test	rax,	rax
	set\cc	al
	movzx	rax,	al
	DS_PUSH	rax
	.endm

	/* ( w -- f ) Check whether w = 0.
	*/
	define_code	$DIGIT_ZERO$$EQUALS_SIGN$,	"0="
	operator_test_1	z
	NEXT

	/* ( w -- f ) Check whether  w ≠ 0.
	*/
	define_code	$DIGIT_ZERO$$LESS_THAN_SIGN$$GREATER_THAN_SIGN$,	"0<>"
	operator_test_1	nz
	NEXT

	/* ( n -- f ) Check whether n < 0.
	*/
	define_code	$DIGIT_ZERO$$LESS_THAN_SIGN$,	"0<"
	operator_test_1	l
	NEXT

	/* ( n -- f ) Check whether n > 0.
	*/
	define_code	$DIGIT_ZERO$$GREATER_THAN_SIGN$,	"0>"
	operator_test_1	g
	NEXT

	/* ( n -- f ) Check whether n ≤ 0.
	*/
	define_code	$DIGIT_ZERO$$LESS_THAN_SIGN$$EQUALS_SIGN$,	"0<="
	operator_test_1	le
	NEXT

	/* ( n -- f ) Check whether n ≥ 0.
	*/
	define_code	$DIGIT_ZERO$$GREATER_THAN_SIGN$$EQUALS_SIGN$,	"0>="
	operator_test_1	ge
	NEXT

	/* ( w1 w2 -- wr ) Bitwise AND top 2 elements.
	*/
	define_code	AND,	"AND"
	DS_MIN_SIZE	2
	DS_POP	rax
	and	[rsp],	rax
	NEXT

	/* ( w1 w2 -- wr ) Bitwise OR top 2 elements.
	*/
	define_code	OR,	"OR"
	DS_MIN_SIZE	2
	DS_POP	rax
	or	[rsp],	rax
	NEXT

	/* ( w1 w2 -- wr ) Bitwise XOR top 2 elements.
	*/
	define_code	XOR,	"XOR"
	DS_MIN_SIZE	2
	DS_POP	rax
	xor	[rsp],	rax
	NEXT

	/* ( w1 -- wr ) Bitwise negate top element.
	*/
	define_code	INVERT,	"INVERT"
	DS_MIN_SIZE	1
	not	qword ptr [rsp]
	NEXT

	/* Auxiliary macro used by bitwise shifts. */
	.macro	pop_shift_count	reg:req
	DS_POP	\reg
#ifndef	NDEBUG
	cmp	\reg,	63
	jbe	1f
	fatal_error	"bitwise shift count greater than 63"
1:
#endif
	.endm

	/* ( w1 ucount -- wr ) Shift w1 ucount bits to the left, filling with
	0-bits on the right.  ucount must not be greater than 63.
	*/
	define_code	$LESS_THAN_SIGN$$LESS_THAN_SIGN$,	"<<"
	DS_MIN_SIZE	2
	pop_shift_count	rcx
	shl	qword ptr [rsp],	cl
	NEXT

	/* ( w1 ucount -- wr ) Shift w1 ucount bits to the right, filling with
	0-bits on the left.  ucount must not be greater than 63.
	*/
	define_code	$GREATER_THAN_SIGN$$GREATER_THAN_SIGN$,	">>"
	DS_MIN_SIZE	2
	pop_shift_count	rcx
	shr	qword ptr [rsp],	cl
	NEXT

	/* ( w1 ucount -- wr ) Shift w1 ucount bits to the right, filling with
	w1's sign bit on the left (arithmetic shift).  ucount must not be
	greater than 63.
	*/
	define_code	A$GREATER_THAN_SIGN$$GREATER_THAN_SIGN$,	"A>>"
	DS_MIN_SIZE	2
	pop_shift_count	rcx
	sar	qword ptr [rsp],	cl
	NEXT

	/* Special words
	   -------------
	*/

	/* ( -- ) Exit the FORTH-implemented word previously entered via DOCOL.
	Restore RSI to one down on the return stack, and enter the next word via
	NEXT.
	*/
	define_code	EXIT,	"EXIT"
	RS_POP	rsi
	NEXT

	/* Cell literals are implemented via LIT.  LIT pushes the cell contained
	in the next codeword pointer after it on stack.

	╺╸╺╸╺╸╺┳━━━━━━━━━━━┳━━━━━━━━━━━┳╸╺╸╺╸╺╸
	  ...  ┃ LIT       ┃ w         ┃ ...
	       ┃       8 B ┃       8 B ┃
	╺╸╺╸╺╸╺┻━━━━━━━━━━━┻━━━━━━━━━━━┻╸╺╸╺╸╺╸
	*/

	/* ( -- w ) Push the cell literal in the next codeword to be entered
	(that is, pointed to by RSI) on stack, and increase RSI to point to the
	next codeword after the literal.
	*/
	define_code	LIT,	"LIT"
	lodsq
	DS_PUSH	rax
	NEXT

	/* Memory access
	   -------------
	*/

	/* ( a-addr -- w ) Fetch cell at a-addr.
	*/
	define_code	$COMMERCIAL_AT$,	"@"
	DS_POP	rax
	DS_PUSH	[rax]
	NEXT

	/* ( w a-addr -- ) Store w at a-addr.
	*/
	define_code	$EXCLAMATION_MARK$,	"!"
	DS_POP	rax
	DS_POP	rbx
	mov	[rax],	rbx
	NEXT

	/* ( n a-addr -- ) Add n to number at a-addr.
	*/
	define_code	$PLUS_SIGN$$EXCLAMATION_MARK$,	"+!"
	DS_POP	rax
	DS_POP	rbx
	add	[rax],	rbx
	NEXT

	/* ( n a-addr -- ) Subtract n from number at a-addr.
	*/
	define_code	$HYPHEN_MINUS$$EXCLAMATION_MARK$,	"-!"
	DS_POP	rax
	DS_POP	rbx
	sub	[rax],	rbx
	NEXT

	/* ( c-addr -- c ) Fetch byte at c-addr as a cell.
	*/
	define_code	C$COMMERCIAL_AT$,	"C@"
	DS_POP	rax
	movzx	rbx,	byte ptr [rax]
	DS_PUSH	rbx
	NEXT

	/* ( c c-addr -- ) Store byte c at c-addr.
	*/
	define_code	C$EXCLAMATION_MARK$,	"C!"
	DS_POP	rax
	DS_POP	rbx
	mov	[rax],	bl
	NEXT

	/* ( c-1 c-2 -- c-1-new c-2-new ) Copy byte at c-1 into byte at c-2, and
	increment both by 1.
	*/
	define_code	C$COMMERCIAL_AT$C$EXCLAMATION_MARK$,	"C@C!"
	DS_MIN_SIZE	2
	// RDI = c-2
	DS_POP	rdi
	// RBX = c-1
	mov	rbx,	[rsp]
	// AL = [c-1]
	mov	al,	[rbx]
	// [RDI++] = AL
	stosb
	// c-1++
	inc	qword ptr [rsp]
	// push incremented c-2
	DS_PUSH	rdi
	NEXT

	/* ( c-from c-to u -- ) Copy u bytes starting at c-from to c-to.
	*/
	define_code CMOVE,	"CMOVE"
	// ! preserve RSI !
	mov	rdx,	rsi
	DS_POP	rcx
	DS_POP	rdi
	DS_POP	rsi
	// move RCX bytes from from [RSI] to [RDI]
	rep	movsb
	// ! restore RSI !
	mov	rsi,	rdx
	NEXT

	/* FORTH built-in variables
	   ========================

	A variable is a word which pushes an address in memory on the stack.
	The contents of the variable are at that address, and may be accessed
	and modified via @ and !.
	*/

	/* define_variable defines a word for a built-in FORTH variable.  label
	is the name of the variable as a label; name is the name string of the
	variable; initial is the initial value of the variable; flags is a
	bitmask of the word flags.
	*/
	.macro	define_variable	label:req,	name:req,	initial=0,	flags=0
	define_code	\label,	"\name",	\flags
	// assembly for the variable word
	DS_PUSH	"offset var_\label"
	NEXT
	// location of the variable
	.data
	.align	8
var_\label :
	.8byte	\initial
	.endm

	/* ( -- a-addr ) Address of the most recent word entry in the
	dictionary.
	*/
	define_variable	LATEST,	"LATEST",	name_SYSCALL
	// NOTE: SYSCALL must be the last defined built-in word for this
	// initial value to be correct

	/* ( -- c-addr ) Address of next free byte in data segment.  Compiled
	words are placed here when compiling.
	*/
	define_variable	HERE,	"HERE"

	/* ( -- u ) State of the interpreter.  0 is immediate mode, non-0 is
	compiling mode.
	*/
	define_variable	STATE,	"STATE"

	/* ( -- a-addr ) Address of the top of data stack.
	*/
	define_variable	S0,	"S0"

	/* ( -- u ) Base for reading and printing numbers.  Must be between 2
	and 36, inclusive.  Initial value: 10.
	*/
	define_variable	BASE,	"BASE",	10

	/* FORTH built-in constants
	   ========================

	A constant is a word which pushes a constant value on the stack.
	*/

	/* define_constant defines a word for a built-in FORTH constant.  label
	is the name of the constant as a label; name is the name string of the
	constant; value is the value of the constant; flags is a bitmask of the
	word flags.
	*/
	.macro	define_constant	label:req,	name:req,	value:req,	flags=0
	define_code	\label,	"\name",	\flags
	// push the constant on stack
	DS_PUSH	"\value"
	NEXT
	.endm

	/* ( -- u ) The major version of this FORTH.  major in
	major.minor.patch.
	*/
	define_constant	VERSION_MAJOR,	"VERSION_MAJOR",	"offset BRK_FORTH_VERSION_MAJOR"

	/* ( -- u ) The minor version of this FORTH.  minor in
	major.minor.patch.
	*/
	define_constant	VERSION_MINOR,	"VERSION_MINOR",	"offset BRK_FORTH_VERSION_MINOR"

	/* ( -- u ) The patch version of this FORTH.  patch in
	major.minor.patch.
	*/
	define_constant	VERSION_PATCH,	"VERSION_PATCH",	"offset BRK_FORTH_VERSION_PATCH"

	/* ( -- umajor uminor upatch ) The full version major.minor.patch of
	this FORTH.
	*/
	define_word	VERSION,	"VERSION"
	.8byte	VERSION_MAJOR
	.8byte	VERSION_MINOR
	.8byte	VERSION_PATCH
	.8byte	EXIT

	/* ( -- a-addr ) Address of the initial top of return stack.  Useful for
	clearing return stack.
	*/
	define_constant	R0,	"R0",	"offset return_stack_top"

	/* ( -- a-addr ) Address of codeword DOCOL, used in FORTH-implemented
	words.
	*/
	define_constant	__DOCOL,	"DOCOL",	"offset DOCOL"

	/* ( -- c ) Bitmask of name length in flag-length byte in word header.
	*/
	define_constant	__FLAG_LENGTH_MASK,	"FLAG_LENGTH_MASK",	"offset FLAG_LENGTH_MASK"

	/* ( -- c ) Flag IMMEDIATE in flag-length byte in word header.
	*/
	define_constant	__FLAG_IMMEDIATE,	"FLAG_IMMEDIATE",	"offset FLAG_IMMEDIATE"

	/* ( -- c ) Flag HIDDEN in flag-length byte in word header.
	*/
	define_constant	__FLAG_HIDDEN,	"FLAG_HIDDEN",	"offset FLAG_HIDDEN"

	/* ( -- n ) Numeric code of Linux syscall exit.
	*/
	define_constant	SYS_EXIT,	"SYS_EXIT",	__NR_exit

	/* ( -- n ) Numeric code of Linux syscall open.
	*/
	define_constant	SYS_OPEN,	"SYS_OPEN",	__NR_open

	/* ( -- n ) Numeric code of Linux syscall close.
	*/
	define_constant	SYS_CLOSE,	"SYS_CLOSE",	__NR_close

	/* ( -- n ) Numeric code of Linux syscall read.
	*/
	define_constant	SYS_READ,	"SYS_READ",	__NR_read

	/* ( -- n ) Numeric code of Linux syscall write.
	*/
	define_constant	SYS_WRITE,	"SYS_WRITE",	__NR_write

	/* ( -- n ) Numeric code of Linux syscall creat.
	*/
	define_constant	SYS_CREAT,	"SYS_CREAT",	__NR_creat

	/* ( -- n ) Numeric code of Linux syscall brk.
	*/
	define_constant	SYS_BRK,	"SYS_BRK",	__NR_brk

	/* ( -- u ) File creation / status flag O_APPEND for Linux I/O syscalls.
	*/
	define_constant	O_APPEND,	"O_APPEND",	0b0000010000000000

	/* ( -- u ) File creation / status flag O_CREAT for Linux I/O syscalls.
	*/
	define_constant	O_CREAT,	"O_CREAT",	0b0000000001000000

	/* ( -- u ) File creation / status flag O_EXCL for Linux I/O syscalls.
	*/
	define_constant	O_EXCL,	"O_EXCL",	0b0000000010000000

	/* ( -- u ) File creation / status flag O_NONBLOCK for Linux I/O
	syscalls.
	*/
	define_constant	O_NONBLOCK,	"O_NONBLOCK",	0b0000100000000000

	/* ( -- u ) File creation / status flag O_RDONLY for Linux I/O syscalls.
	*/
	define_constant	O_RDONLY,	"O_RDONLY",	0b0000000000000000

	/* ( -- u ) File creation / status flag O_RDWR for Linux I/O syscalls.
	*/
	define_constant	O_RDWR,	"O_RDWR",	0b0000000000000010

	/* ( -- u ) File creation / status flag O_TRUNC for Linux I/O syscalls.
	*/
	define_constant	O_TRUNC,	"O_TRUNC",	0b0000001000000000

	/* ( -- u ) File creation / status flag O_WRONLY for Linux I/O syscalls.
	*/
	define_constant	O_WRONLY,	"O_WRONLY",	0b0000000000000001

	/* ( -- n ) File descriptor number of standard input.
	*/
	define_constant	FILENO_STDIN,	"FILENO_STDIN",	0

	/* ( -- n ) File descriptor number of standard output.
	*/
	define_constant	FILENO_STDOUT,	"FILENO_STDOUT",	1

	/* ( -- n ) File descriptor number of standard error.
	*/
	define_constant	FILENO_STDERR,	"FILENO_STDERR",	2

	/* More FORTH primitives
	   =====================

	   Return stack manipulation
	   -------------------------
	*/

	/* ( -- w1 ) { w1 -- } Pop top of return stack and push it on data
	stack.
	*/
	define_code	R$GREATER_THAN_SIGN$,	"R>"
	RS_POP	rax
	DS_PUSH	rax
	NEXT

	/* ( w1 -- ) { -- w1 } Pop top of data stack and push it on return
	stack.
	*/
	define_code	$GREATER_THAN_SIGN$R,	">R"
	DS_POP	rax
	RS_PUSH	rax
	NEXT

	/* ( -- a-addr ) { -- } Push address of top of return stack on data
	stack.
	*/
	define_code	RSP$COMMERCIAL_AT$,	"RSP@"
	DS_PUSH	rbp
	NEXT

	/* ( a-addr -- ) { UNDEFINED } Pop top of data stack and make top of
	return stack point to it.
	*/
	define_code	RSP$EXCLAMATION_MARK$,	"RSP!"
	DS_POP	rbp
	RS_RBP_VALID
	NEXT

	/* ( -- ) { w -- } Drop top of return stack.
	*/
	define_code	RDROP,	"RDROP"
	RS_DROP
	NEXT

	/* Data stack manipulation
	   -----------------------
	*/

	/* ( -- a-addr ) Push address of top of stack, before this word ran, on
	stack.
	*/
	define_code	DSP$COMMERCIAL_AT$,	"DSP@"
	mov	rax,	rsp
	DS_PUSH	rax
	NEXT

	/* ( a-addr -- ) Make top of data stack point to a-addr.
	*/
	define_code	DSP$EXCLAMATION_MARK$,	"DSP!"
	DS_POP	rsp
	DS_RSP_VALID
	NEXT

	/* I/O
	   ===

	Byte input is done via KEY, which reads bytes from a buffer, and fills
	the buffer from stdin when its contents have been used up.  The buffer
	starts at `key_buffer` and stretches `KEY_BUFFER_SIZE` bytes, away from
	zero in memory.  Input is read into the beginning of the buffer,
	`key_buffer_top` pointing to one past the end of the input.  The current
	position that KEY reads from is at `key_buffer_current`.

	This is illustrated here, where `b` is the next character KEY will push
	on stack:

	key_buffer       key_buffer_top         (key_buffer + KEY_BUFFER_SIZE)
	 ╎                ╎                      ╎
	 ▼                ▽                      ▼
	┏━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┓
	┃ input …… a b c ┃ unused part          ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━┛
	             △
	             ╎
	            key_buffer_current
	*/

	/* ( -- c ) Attempt to read next byte from stdin, buffered.  If there
	are no more bytes (EOF or error), exit the program.
	*/
	define_code	KEY,	"KEY"
	call	_KEY
	DS_PUSH	rax
	NEXT

	// CALLable implementation of KEY
	//  input:  none
	//  output: RAX = c
_KEY:
	// R8 = current
	mov	r8,	key_buffer_current
	// jump if current >= top: buffer's contents have been used up
	cmp	r8,	key_buffer_top
	jae	2f
1:
	//// read next byte from buffer
	movzx	rax,	byte ptr [r8]
	inc	qword ptr key_buffer_current
	ret
2:
	//// read more input to refill buffer
	// long /* ssize_t */ read(int fildes, void *buf,
	//                         unsigned long /* size_t */ nbyte)
	/// EDI = fildes = 0 /* STDIN_FILENO */
	xor	edi,	edi
	// ! preserve RSI !
	push	rsi
	/// RSI = buf = key_buffer
	lea	rsi,	[key_buffer]
	/// RDX = nbyte = KEY_BUFFER_SIZE
	lea	rdx,	[KEY_BUFFER_SIZE]
	mov	rax,	__NR_read
	syscall
	// ! restore RSI !
	pop	rsi
	// jump if return value <= 0: read fail or EOF
	test	rax,	rax
	js	3f
	jz	4f
	//// adjust top and reset current
	lea	r8,	[key_buffer + rax]
	mov	key_buffer_top,	r8
	lea	r8,	[key_buffer]
	// R8 = current
	mov	key_buffer_current,	r8
	// read from refilled buffer
	jmp	1b
3:
	//// read error
	fatal_error	"failed read from stdin",	110
4:
	//// exit program
	// void exit(int status)
	/// EDI = status = 0
	xor	edi,	edi
	mov	rax,	__NR_exit
	syscall
	//  NOTE: no need to check return value
	ret

	.data
	.align	8
key_buffer_current:
	// initially the first byte in buffer: buffer already "used up"
	.8byte	key_buffer
key_buffer_top:
	// initially the first byte in buffer: empty buffer
	.8byte	key_buffer

	/* Byte output is done via EMIT, which writes bytes to a buffer, and
	flushes the buffer to stdout when it is full, or when writing a newline
	character.  The buffer starts at `emit_buffer` and stretches
	`EMIT_BUFFER_SIZE` bytes, away from zero in memory.  `emit_buffer_end`
	points one past the end of the buffer.  Output is written into the
	beginning of the buffer, `emit_buffer_current` pointing to one past the
	last written byte.

	                                        (emit_buffer + EMIT_BUFFER_SIZE)
	emit_buffer      emit_buffer_current    emit_buffer_end
	 ╎                ╎                      ╎
	 ▼                ▽                      ▼
	┏━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┓
	┃ output ……      ┃ unused part          ┃
	┗━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━┛
	*/

	/* ( c -- ) Attempt to write byte c to stdout, buffered.  If the write
	fails, exit the program with a non-zero exit status.
	*/
	define_code	EMIT,	"EMIT"
	DS_POP	rax
	call	_EMIT
	NEXT

	// CALLable implementation of EMIT
	//  input:  RAX = c
	//  output: none
_EMIT:
	// RDX = current
	mov	rdx,	emit_buffer_current
	// jump if current < end: there's place in buffer
	cmp	rdx,	offset emit_buffer_end
	jb	1f
	// flush full buffer
	call	_EMIT_FLUSH
	// RDX = current
	mov	rdx,	emit_buffer_current
1:
	//// write byte into buffer
	mov	[rdx],	al
	inc	qword ptr [emit_buffer_current]
	// flush on newline
	cmp	al,	'\n
	je	_EMIT_FLUSH
	ret

	/* ( -- ) Attempt to flush the output buffer of EMIT.  If the write
	fails, exit the program with a non-zero exit status.
	*/
	define_code	EMIT_FLUSH,	"EMIT_FLUSH"
	call	_EMIT_FLUSH
	NEXT

	// CALLable implementation of EMIT_FLUSH
	//  input:  none
	//  output: none
_EMIT_FLUSH:
	// ! preserve R12 !
	push	r12
	// R12 = current - buffer = number of bytes in buffer
	mov	r12,	emit_buffer_current
	sub	r12,	offset emit_buffer
	// long /* ssize_t */ write(int fildes, void *buf,
	//                          unsigned long /* size_t */ nbyte)
	// jump if nbyte = 0: behavior is unspecified if fildes is not a regular
	// file
	jz	1f
	/// EDI = fildes = 1 /* STDOUT_FILENO */
	mov	edi,	1
	// ! preserve RSI !
	push	rsi
	/// RSI = buf = emit_buffer
	lea	rsi,	[emit_buffer]
	/// RDX = nbyte = R12
	mov	rdx,	r12
	mov	rax,	__NR_write
	syscall
	// ! restore RSI !
	pop	rsi
	// jump if return value != nbyte: write failed
	cmp	rax,	r12
	jne	2f
	//// reset current
	lea	r8,	[emit_buffer]
	mov	emit_buffer_current,	r8
1:
	//// return
	// ! restore R12 !
	pop	r12
	ret
2:
	//// write error
	fatal_error	"failed write to stdout",	111
	ret

	.data
	.align	8
emit_buffer_current:
	// initially the first byte in buffer: empty buffer
	.8byte	emit_buffer

	/* Reading words is done via WORD.  It skips any initial whitespace, and
	reads all following non-whitespace characters into the static word
	buffer.  If the word turns out to be longer than the word buffer's size
	when reading, only the first size bytes are read into the buffer, and
	the rest of the word is read in but silently discarded.
	*/

	/* ( -- c-word ulen ) Read next word from stdin into static word buffer;
	exit upon EOF or read error.  c-word is the buffer's address, ulen is
	the word's length.  If the word is too long for the buffer, it is
	silently truncated.  The word at c-word will be overwritten by
	subsequent calls to WORD.
	*/
	define_code	__WORD,	"WORD"
	// NOTE: do not use BYTE, WORD, DWORD or QWORD as symbol names; they
	// appear to have special meanings in Intel syntax; prefix them instead
	// with `__`
	call	_WORD
	DS_PUSH	rdi
	DS_PUSH	rcx
	NEXT

	/* jmp_whitespace jumps to label if reg contains a whitespace character.

	Modifies: R11.
	*/
	.macro	jmp_whitespace	reg:req,	label:req
	mov	r11b,	[byte_char_properties + 2 * \reg + 0]
	test	r11b,	r11b
	jnz	\label
	.endm

	/* jmp_not_whitespace jumps to label if reg does not contain a
	whitespace character.

	Modifies: R11.
	*/
	.macro	jmp_not_whitespace	reg:req,	label:req
	mov	r11b,	[byte_char_properties + 2 * \reg + 0]
	test	r11b,	r11b
	jz	\label
	.endm

	// CALLable implementation of WORD
	//  input:  none
	//  output: RDI = c-word, RCX = ulen
_WORD:
	// ! preserve R12 !
	push	r12
1:
	//// skip whitespace
	call	_KEY
	jmp_whitespace	rax,	1b
	// jump if start of '\'-comment
	cmp	al,	'\\'
	je	3f
	//// start reading the word
	// RDI = position in word buffer
	lea	rdi,	[word_buffer]
2:
	// jump if position >= end: buffer is full
	cmp	rdi,	offset word_buffer_end
	jae	4f
	//// store byte and continue reading
	stosb
	mov	r12,	rdi
	call	_KEY
	mov	rdi,	r12
	jmp_not_whitespace	rax,	2b
	//// end of word
	// RCX = ulen
	mov	rcx,	rdi
	sub	rcx,	offset word_buffer
	// RDI = c-word
	lea	rdi,	[word_buffer]
	// ! restore R12 !
	pop	r12
	ret
3:
	//// skip rest of line and restart
	call	_KEY
	// NOTE: no need to preserve RDI here
	cmp	al,	'\n
	jne	3b
	jmp	1b
4:
	//// read and discard rest of word and return
	call	_KEY
	// NOTE: no need to preserve RDI here
	jmp_not_whitespace	rax,	4b
	// c-word = buffer
	lea	rdi,	[word_buffer]
	// ulen = end - buffer
	lea	rcx,	[word_buffer_end - word_buffer]
	// ! restore R12 !
	pop	r12
	ret

	.data
	.align	8
word_buffer:
	.space	1 << 10
word_buffer_end:

	/* ( c-str ulen -- n uleft ) Parse string at c-str of length ulen as an
	integer.  n is the parsed number, uleft is the number of unconverted
	bytes.  If uleft is 0, either no parsing error occured, or the string is
	empty in which case n is 0.  If uleft is not 0, the string is partially
	or entirely an illegal number.

	A valid number matches the regexp "[-+]?[0-9A-Za-z][0-9A-Za-z_]*".  It
	starts with an optional sign, followed by digits — '0' – '9'
	representing 0 – 9, 'A' – 'Z' and 'a' – 'z' representing 10 – 35 — and
	may include low lines which are ignored, for improved readability.
	Illegal digits in the current input base are not considered valid.

	For example: "-123_456_789_ABC" in base-16 parses to -20,015,998,343,868
	in base-10; "+1010_1101" in base-2 parses to 173 in base-10.
	*/
	define_code	NUMBER,	"NUMBER"
	DS_POP	rcx
	DS_POP	rdi
	call	_NUMBER
	DS_PUSH	rax
	DS_PUSH	rcx
	NEXT

	// CALLable implementation of NUMBER
	//  input:  RDI = c-str, RCX = ulen
	//  output: RAX = n, RCX = uleft
_NUMBER:
1:
	// n = 0
	xor	rax,	rax
	/// NONE -> error
	test	rcx,	rcx
	jz	1002f
	// read
	movzx	r8,	byte ptr [rdi]
	inc	rdi
	dec	rcx
	// R10B = is negative = false
	xor	r10b,	r10b
	/// '-' -> 1.1
	cmp	r8b,	'-
	je	11f
	/// '+' -> 1.2
	cmp	r8b,	'+
	je	12f
	/// OTHER -> 2
2:
	// R9 = base
	mov	r9,	var_BASE
	/// OTHER -> error
	mov	r8b,	[byte_char_properties + 2 * r8 + 1]
	cmp	r8b,	offset BYTE_CHAR_DIGIT_NONE
	je	1000f
	/// digit -> 3
	// digit too large for base -> error
	cmp	r8,	r9
	jae	1000f
	// n = digit
	mov	al,	r8b
3:
	/// NONE -> end
	test	rcx,	rcx
	jz	1001f
	// read
	movzx	r8,	byte ptr [rdi]
	inc	rdi
	dec	rcx
	/// '_' -> 3
	cmp	r8b,	'_
	je	3b
	/// OTHER -> end
	mov	r8b,	[byte_char_properties + 2 * r8 + 1]
	cmp	r8b,	offset BYTE_CHAR_DIGIT_NONE
	je	1000f
	/// digit -> 3
	// digit too large for base -> end
	cmp	r8,	r9
	jae	1000f
	// n = n * base + digit
	mul	r9
	add	rax,	r8
	jmp	3b
11:
	// is negative = true
	mov	r10b,	r8b
12:
	/// NONE -> error
	test	rcx,	rcx
	jz	1000f
	// read
	movzx	r8,	byte ptr [rdi]
	inc	rdi
	dec	rcx
	/// OTHER -> 2
	jmp	2b
1000:
	//// did not parse current char
	inc	rcx
1001:
	//// did not parse *after* current char
	// negate n if negative
	test	r10b,	r10b
	jz	1002f
	neg	rax
1002:
	ret

	// used as digit value when a character does not represent a digit
	.set	BYTE_CHAR_DIGIT_NONE,	0xFF

	.section	.rodata

	// [byte_char_properites + 2 * b + n] contains the character property n
	// of byte b.
	//
	// Property 0 is 1 if the character is considered whitespace, 0 if not.
	// Property 1 is BYTE_CHAR_DIGIT_NONE if the character does not
	// represent a digit, otherwise the digit value of the character.

byte_char_properties:
	//	wspace	digit value		byte	name
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 00	<control>	(NULL)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 01	<control>	(START OF HEADING)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 02	<control>	(START OF TEXT)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 03	<control>	(END OF TEXT)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 04	<control>	(END OF TRANSMISSION)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 05	<control>	(ENQUIRY)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 06	<control>	(ACKNOWLEDGE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 07	<control>	(BELL)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 08	<control>	(BACKSPACE)
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 09	<control>	(CHARACTER TABULATION)
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 0A	<control>	(LINE FEED (LF))
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 0B	<control>	(LINE TABULATION)
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 0C	<control>	(FORM FEED (FF))
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 0D	<control>	(CARRIAGE RETURN (CR))
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 0E	<control>	(SHIFT OUT)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 0F	<control>	(SHIFT IN)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 10	<control>	(DATA LINK ESCAPE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 11	<control>	(DEVICE CONTROL ONE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 12	<control>	(DEVICE CONTROL TWO)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 13	<control>	(DEVICE CONTROL THREE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 14	<control>	(DEVICE CONTROL FOUR)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 15	<control>	(NEGATIVE ACKNOWLEDGE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 16	<control>	(SYNCHRONOUS IDLE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 17	<control>	(END OF TRANSMISSION BLOCK)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 18	<control>	(CANCEL)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 19	<control>	(END OF MEDIUM)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1A	<control>	(SUBSTITUTE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1B	<control>	(ESCAPE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1C	<control>	(INFORMATION SEPARATOR FOUR)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1D	<control>	(INFORMATION SEPARATOR THREE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1E	<control>	(INFORMATION SEPARATOR TWO)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 1F	<control>	(INFORMATION SEPARATOR ONE)
	.byte	1,	BYTE_CHAR_DIGIT_NONE	// 20	SPACE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 21	EXCLAMATION MARK
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 22	QUOTATION MARK
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 23	NUMBER SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 24	DOLLAR SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 25	PERCENT SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 26	AMPERSAND
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 27	APOSTROPHE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 28	LEFT PARENTHESIS
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 29	RIGHT PARENTHESIS
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2A	ASTERISK
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2B	PLUS SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2C	COMMA
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2D	HYPHEN-MINUS
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2E	FULL STOP
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 2F	SOLIDUS
	.byte	0,	0	// 30	DIGIT ZERO
	.byte	0,	1	// 31	DIGIT ONE
	.byte	0,	2	// 32	DIGIT TWO
	.byte	0,	3	// 33	DIGIT THREE
	.byte	0,	4	// 34	DIGIT FOUR
	.byte	0,	5	// 35	DIGIT FIVE
	.byte	0,	6	// 36	DIGIT SIX
	.byte	0,	7	// 37	DIGIT SEVEN
	.byte	0,	8	// 38	DIGIT EIGHT
	.byte	0,	9	// 39	DIGIT NINE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3A	COLON
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3B	SEMICOLON
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3C	LESS-THAN SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3D	EQUALS SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3E	GREATER-THAN SIGN
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 3F	QUESTION MARK
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 40	COMMERCIAL AT
	.byte	0,	10	// 41	LATIN CAPITAL LETTER A
	.byte	0,	11	// 42	LATIN CAPITAL LETTER B
	.byte	0,	12	// 43	LATIN CAPITAL LETTER C
	.byte	0,	13	// 44	LATIN CAPITAL LETTER D
	.byte	0,	14	// 45	LATIN CAPITAL LETTER E
	.byte	0,	15	// 46	LATIN CAPITAL LETTER F
	.byte	0,	16	// 47	LATIN CAPITAL LETTER G
	.byte	0,	17	// 48	LATIN CAPITAL LETTER H
	.byte	0,	18	// 49	LATIN CAPITAL LETTER I
	.byte	0,	19	// 4A	LATIN CAPITAL LETTER J
	.byte	0,	20	// 4B	LATIN CAPITAL LETTER K
	.byte	0,	21	// 4C	LATIN CAPITAL LETTER L
	.byte	0,	22	// 4D	LATIN CAPITAL LETTER M
	.byte	0,	23	// 4E	LATIN CAPITAL LETTER N
	.byte	0,	24	// 4F	LATIN CAPITAL LETTER O
	.byte	0,	25	// 50	LATIN CAPITAL LETTER P
	.byte	0,	26	// 51	LATIN CAPITAL LETTER Q
	.byte	0,	27	// 52	LATIN CAPITAL LETTER R
	.byte	0,	28	// 53	LATIN CAPITAL LETTER S
	.byte	0,	29	// 54	LATIN CAPITAL LETTER T
	.byte	0,	30	// 55	LATIN CAPITAL LETTER U
	.byte	0,	31	// 56	LATIN CAPITAL LETTER V
	.byte	0,	32	// 57	LATIN CAPITAL LETTER W
	.byte	0,	33	// 58	LATIN CAPITAL LETTER X
	.byte	0,	34	// 59	LATIN CAPITAL LETTER Y
	.byte	0,	35	// 5A	LATIN CAPITAL LETTER Z
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 5B	LEFT SQUARE BRACKET
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 5C	REVERSE SOLIDUS
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 5D	RIGHT SQUARE BRACKET
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 5E	CIRCUMFLEX ACCENT
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 5F	LOW LINE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 60	GRAVE ACCENT
	.byte	0,	10	// 61	LATIN SMALL LETTER A
	.byte	0,	11	// 62	LATIN SMALL LETTER B
	.byte	0,	12	// 63	LATIN SMALL LETTER C
	.byte	0,	13	// 64	LATIN SMALL LETTER D
	.byte	0,	14	// 65	LATIN SMALL LETTER E
	.byte	0,	15	// 66	LATIN SMALL LETTER F
	.byte	0,	16	// 67	LATIN SMALL LETTER G
	.byte	0,	17	// 68	LATIN SMALL LETTER H
	.byte	0,	18	// 69	LATIN SMALL LETTER I
	.byte	0,	19	// 6A	LATIN SMALL LETTER J
	.byte	0,	20	// 6B	LATIN SMALL LETTER K
	.byte	0,	21	// 6C	LATIN SMALL LETTER L
	.byte	0,	22	// 6D	LATIN SMALL LETTER M
	.byte	0,	23	// 6E	LATIN SMALL LETTER N
	.byte	0,	24	// 6F	LATIN SMALL LETTER O
	.byte	0,	25	// 70	LATIN SMALL LETTER P
	.byte	0,	26	// 71	LATIN SMALL LETTER Q
	.byte	0,	27	// 72	LATIN SMALL LETTER R
	.byte	0,	28	// 73	LATIN SMALL LETTER S
	.byte	0,	29	// 74	LATIN SMALL LETTER T
	.byte	0,	30	// 75	LATIN SMALL LETTER U
	.byte	0,	31	// 76	LATIN SMALL LETTER V
	.byte	0,	32	// 77	LATIN SMALL LETTER W
	.byte	0,	33	// 78	LATIN SMALL LETTER X
	.byte	0,	34	// 79	LATIN SMALL LETTER Y
	.byte	0,	35	// 7A	LATIN SMALL LETTER Z
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 7B	LEFT CURLY BRACKET
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 7C	VERTICAL LINE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 7D	RIGHT CURLY BRACKET
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 7E	TILDE
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 7F	<control>	(DELETE)
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 80	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 81	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 82	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 83	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 84	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 85	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 86	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 87	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 88	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 89	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8A	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8B	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8C	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8D	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8E	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 8F	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 90	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 91	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 92	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 93	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 94	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 95	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 96	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 97	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 98	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 99	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9A	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9B	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9C	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9D	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9E	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// 9F	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// A9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AD	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// AF	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// B9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BD	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// BF	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// C9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CD	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// CF	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// D9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DD	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// DF	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// E9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// EA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// EB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// EC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// ED	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// EE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// EF	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F0	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F1	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F2	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F3	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F4	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F5	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F6	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F7	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F8	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// F9	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FA	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FB	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FC	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FD	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FE	*invalid ASCII character*
	.byte	0,	BYTE_CHAR_DIGIT_NONE	// FF	*invalid ASCII character*

	/* Compiling
	   =========

	New word entries are compiled into space starting at HERE.
	*/

	/* ( c-name ulen -- a-word ) Find the dictionary entry of the most
	recent word with name at c-name of length ulen.  If the word is not in
	the dictionary or is flagged hidden, a-word is 0.
	*/
	define_code	FIND,	"FIND"
	DS_POP	rcx
	DS_POP	rdi
	call	_FIND
	DS_PUSH	rax
	NEXT

	// CALLable implementation of FIND
	//  input:  RDI = c-name, RCX = ulen
	//  output: RAX = a-word
_FIND:
	// ! preserve RSI !
	push	rsi
	// R8 = ulen
	mov	r8,	rcx
	// R9 = c-name
	mov	r9,	rdi
	// RDX = LATEST = initial current word
	mov	rdx,	var_LATEST
1:
	//// check current word
	// jump if current word is NULL: end of dictionary
	test	rdx,	rdx
	jz	3f
	// RAX = current length | hidden flag
	movzx	rax,	byte ptr [rdx + 8]
	and	al,	(offset FLAG_LENGTH_MASK | offset FLAG_HIDDEN)
	// jump if lengths are not the same or hidden flag is set
	cmp	rax,	r8
	jne	11f
	//// compare strings
	/// RSI = current name address
	lea	rsi,	[rdx + 9]
	/// RDI = c-name
	mov	rdi,	r9
	/// RCX = ulen
	mov	rcx,	r8
	// compare up to RCX bytes at RSI and RDI
	repe	cmpsb
	// jump if names are equal
	je	2f
11:
	//// continue with next word in dictionary
	mov	rdx,	[rdx]
	jmp	1b
2:
	//// word found
	// a-addr = current word
	mov	rax,	rdx
	// ! restore RSI !
	pop	rsi
	ret
3:
	//// word not found
	// a-addr = NULL
	xor	rax,	rax
	// ! restore RSI !
	pop	rsi
	ret

	/* ( -- c-word ulen ) Read next word via WORD and cut off the end to fit
	in a dictionary entry header if too long.
	*/
	define_code	WORD$HYPHEN_MINUS$CUT,	"WORD-CUT"
	call	_WORD_CUT
	DS_PUSH	rdi
	DS_PUSH	rcx
	NEXT

	// CALLable implementation of WORD-CUT
	//  input:  none
	//  output: RDI = c-word, RCX = ulen
_WORD_CUT:
	call	_WORD
	cmp	rcx,	31
	jbe	1f
	mov	rcx,	31
1:
	ret

	/* ( a-word -- a-codeword ) Compute address of codeword in word entry.
	*/
	define_code	$GREATER_THAN_SIGN$CFA,	">CFA"
	DS_POP	rdi
	call	_$GREATER_THAN_SIGN$CFA
	DS_PUSH	rdi
	NEXT

	// CALLable implementation of >CFA
	//  input:  RDI = a-word
	//  output: RDI = a-codeword
_$GREATER_THAN_SIGN$CFA:
	// RDI = address of length-flags byte
	lea	rdi,	[rdi + 8]
	// RAX = length of name
	movzx	rax,	byte ptr [rdi]
	and	al,	offset FLAG_LENGTH_MASK
	// RDI = address of start of padding
	lea	rdi,	[rdi + 1 + rax]
	// RDI = next 8-byte boundary
	add	rdi,	0b0111
	and	rdi,	~0b0111
	ret

	/* ( a-word -- a-codeword-ptr ) Compute address of first codeword
	pointer in word entry.
	*/
	define_word	$GREATER_THAN_SIGN$DFA,	">DFA"
	.8byte	$GREATER_THAN_SIGN$CFA
	// simply increment by 8 after >CFA
	.8byte	$DIGIT_EIGHT$$PLUS_SIGN$
	.8byte	EXIT

	/* ( c-name ulen -- ) Create standard word entry header of word with
	name at c-name of length ulen and with link pointer = LATEST, into space
	starting at HERE.  Increment HERE to point one past the header, and
	update LATEST to point to the start of the header.
	*/
	define_code	CREATE,	"CREATE"
	// RDI = HERE
	mov	rdi,	var_HERE
	// link pointer: [RDI] = LATEST, RDI += 8
	mov	rax,	var_LATEST
	stosq
	// RCX = ulen
	DS_POP	rcx
	// length-flags byte: [RDI++] = ulen
	mov	al,	cl
	stosb
	// ! preserve RSI !
	mov	r12,	rsi
	// RSI = c-name
	DS_POP	rsi
	// name: copy RCX bytes from RSI to RDI
	rep	movsb
	// ! restore RSI !
	mov	rsi,	r12
	// padding: RDI = next 8-byte boundary
	add	rdi,	0b0111
	and	rdi,	~0b0111
	// update LATEST
	mov	rax,	var_HERE
	mov	var_LATEST,	rax
	// increment HERE
	mov	var_HERE,	rdi
	// writing outside data segment is left to be detected by the system
	NEXT

	/* ( w -- ) Store w in space starting at HERE in little-endian form, and
	increment HERE to point one past the stored cell.
	*/
	define_code	$COMMA$,	","
	DS_POP	rax
	call	_$COMMA$
	NEXT

	// CALLable implementation of ,
	//  input:  RAX = w
	//  output: none
_$COMMA$:
	//// [HERE] = w, HERE += 8
	mov	rdi,	var_HERE
	stosq
	mov	var_HERE,	rdi
	// writing outside data segment is left to be detected by the system
	ret

	/* ( -- ) Switch to immediate mode; set STATE to 0.
	*/
	define_code	$LEFT_SQUARE_BRACKET$,	"[",	FLAG_IMMEDIATE
	mov	qword ptr var_STATE,	0
	NEXT

	/* ( -- ) Switch to compiling mode; set STATE to non-0.
	*/
	define_code	$RIGHT_SQUARE_BRACKET$,	"]"
	mov	qword ptr var_STATE,	1
	NEXT

	/* ( -- ) Start compilation of word with the following word as the name;
	enter compiling mode.  If the name is too long, silently truncate it.
	*/
	define_word	$COLON$,	":"
	// read next word for name
	.8byte	WORD$HYPHEN_MINUS$CUT
	// create header
	.8byte	CREATE
	// add codeword: DOCOL
	.8byte	__DOCOL
	.8byte	$COMMA$
	// temporarily turn on hidden flag for compiled word
	.8byte	LATEST
	.8byte	$COMMERCIAL_AT$
	.8byte	HIDDEN
	// enter compiling mode
	.8byte	$RIGHT_SQUARE_BRACKET$
	.8byte	EXIT

	/* ( -- ) End current compilation; enter immediate mode.
	*/
	define_word	$SEMICOLON$,	";",	FLAG_IMMEDIATE
	// add EXIT to end
	.8byte	LIT,	EXIT
	.8byte	$COMMA$
	// turn off hidden flag
	.8byte	LATEST
	.8byte	$COMMERCIAL_AT$
	.8byte	HIDDEN
	// exit compiling mode
	.8byte	$LEFT_SQUARE_BRACKET$
	.8byte	EXIT

	/* ( -- ) Toggle the IMMEDIATE flag of the most recent word, or the
	currently compiling word if compiling.  Usually used like

		: EXAMPLE_WORD IMMEDIATE ... ;

	*/
	define_code	IMMEDIATE,	"IMMEDIATE",	FLAG_IMMEDIATE
	// RDI = address of LATEST entry's length-flags byte
	mov	rdi,	var_LATEST
	lea	rdi,	[rdi + 8]
	// [RDI] ^= FLAG_IMMEDIATE
	xor	byte ptr [rdi],	offset FLAG_IMMEDIATE
	NEXT

	/* ( a-word -- ) Toggle the HIDDEN flag of the word entry at a-word.
	*/
	define_code	HIDDEN,	"HIDDEN"
	// RDI = address of a-word's length-flags byte
	DS_POP	rdi
	lea	rdi,	[rdi + 8]
	// [RDI] ^= FLAG_HIDDEN
	xor	byte ptr [rdi],	offset FLAG_HIDDEN
	NEXT

	/* ( -- ) Read following word from stdin and hide the word with that
	name.  If no word with that name is found, the behaviour is undefined.
	*/
	define_word	HIDE,	"HIDE"
	.8byte	__WORD
	.8byte	FIND
	.8byte	HIDDEN
	.8byte	EXIT

	/* ( -- xt ) Get codeword pointer (execution token) of next word to be
	entered and skip next word.  Only works in compiled words.
	*/
	define_code	$APOSTROPHE$,	"'"
	// RAX = [RSI++]
	lodsq
	DS_PUSH	rax
	NEXT

	/* Branching
	   =========

	Branching is done via BRANCH (unconditional) and 0BRANCH (conditional).
	The codeword pointer following the branch codeword pointer is the offset
	which shall be added to RSI.

	╺╸╺╸╺╸╺┳━━━━━━━━┳━━━━━━━━┳╸╺╸╺╸╺╸╺╸╺╸╺╸╺╸╺┳━━━━━━━━┳╸╺╸╺╸╺╸
	  ...  ┃ branch ┃ offset ┃ . . .          ┃ cw ptr ┃ ...
	       ┃    8 B ┃    8 B ┃ (offset - 8) B ┃    8 B ┃
	╺╸╺╸╺╸╺┻━━━━━━━━┻━━┊━━━━━┻╸╺╸╺╸╺╸╺╸╺╸╺╸╺╸╺┻━━━━━━━━┻╸╺╸╺╸╺╸
	                   ┊                         ▲
	                   └┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┘
	*/

	/* ( -- ) Jump unconditionally; increase codeword pointer by offset at
	codeword pointer.
	*/
	define_code	BRANCH,	"BRANCH"
	// RSI points to the next codeword ptr, i.e. the offset
	add	rsi,	[rsi]
	NEXT

	/* ( w -- ) Jump conditionally; if w is 0, increase codeword pointer by
	offset at codeword pointer; if w is not 0, set codeword pointer to next
	codeword pointer.
	*/
	define_code	$DIGIT_ZERO$BRANCH,	"0BRANCH"
	DS_POP	rax
	// jump if RAX = 0
	test	rax,	rax
	jz	code_BRANCH
	//// jump over offset cell and continue with codeword pointer directly
	//// after it
	lodsq
	NEXT

	/* String literals
	   ===============

	A string literal, analogously to a cell literal, is stored in place of
	codeword pointers.  It starts with a cell containing the length, length
	bytes containing the string, and padding up to the next cell boundary
	(next 8-byte boundary).  LITSTRING, analogously to LIT, pushes the
	following string literal on stack.

	╺╸╺╸╺╸╺┳━━━━━━━━━━━┳━━━━━━━━━━━┳━━━━━━━━━━━━━━━━┳━━━━━━━━━┳╸╺╸╺╸╺╸
	  ...  ┃ LITSTRING ┃ ulen      ┃ c-str          ┃ padding ┃ ...
	       ┃       8 B ┃       8 B ┃       length B ┃     p B ┃
	╺╸╺╸╺╸╺┻━━━━━━━━━━━┻━━━━━━━━━━━┻━━━━━━━━━━━━━━━━┻━━━━━━━━━┻╸╺╸╺╸╺╸
	*/

	/* ( -- c-str ulen ) Push the string literal in the next codewords to be
	entered (that is, pointed to by RSI) on stack, and increase RSI to point
	to the next codeword after the literal.  c-str points to the start of
	the string, and ulen is the length of the string.
	*/
	define_code	LITSTRING,	"LITSTRING"
	// RAX = ulen
	lodsq
	// c-str = RSI
	DS_PUSH	rsi
	// ulen = RAX
	DS_PUSH	rax
	// skip over string and padding
	lea	rsi,	[rsi + rax]
	add	rsi,	0b0111
	and	rsi,	~0b0111
	NEXT

	/* ( c-str ulen -- ) Print string starting at c-str of length ulen to
	stdout, buffered using the same buffer as EMIT.

	Note: the buffer is *not* flushed on newline like EMIT.
	*/
	define_code	TELL,	"TELL"
	/// R12 = ulen
	DS_POP	r12
	// RBX = unused size
	lea	rbx,	[emit_buffer_end]
	sub	rbx,	emit_buffer_current
	// jump if ulen <= unused size: enough space in buffer
	cmp	r12,	rbx
	jbe	1f
	//// flush emit_buffer, and write out string
	call	_EMIT_FLUSH
	// long /* ssize_t */ write(int fildes, void *buf,
	//                          unsigned long /* size_t */ nbyte)
	/// EDI = fildes = 1 /* STDOUT_FILENO */
	mov	edi,	1
	// ! preserve RSI !
	mov	r13,	rsi
	/// RSI = buf = c-str
	DS_POP	rsi
	/// RDX = nbyte = ulen
	mov	rdx,	r12
	mov	rax,	__NR_write
	syscall
	// ! restore RSI !
	mov	rsi,	r13
	// jump if return value == nbyte
	cmp	rax,	r12
	je	2f
	//// write error
	fatal_error	"failed write to stdout",	112
1:
	//// append string to emit_buffer
	// ! preserve RSI !
	mov	r13,	rsi
	// copy RCX = ulen bytes
	mov	rcx,	r12
	//  from RSI = c-str
	DS_POP	rsi
	//  to RDI = current
	mov	rdi,	emit_buffer_current
	rep	movsb
	// ! restore RSI !
	mov	rsi,	r13
	// update current
	mov	emit_buffer_current,	rdi
2:
	NEXT


	/* The interpreter
	   ===============

	QUIT starts or restarts the interpreting loop.  INTERPRET reads the next
	word and attempts to interpret it.
	*/

	/* ( UNDEFINED ) { RESET } Clear return stack and start interpreter
	loop.
	*/
	define_word	QUIT,	"QUIT"
	// clear return stack
	.8byte	R0
	.8byte	RSP$EXCLAMATION_MARK$
	// interpret next word
	.8byte	INTERPRET
	// unconditionally loop back to INTERPRET
	.8byte	BRANCH,	-16
	// NOTE: QUIT must not call EXIT

	/* ( -- u ) The maximum number of bytes of context INTERPRET prints upon
	error.  Initial value: 40.
	*/
	define_variable	INTERPRET_ERROR_CONTEXT,	"INTERPRET_ERROR_CONTEXT",	40

	/* ( UNDEFINED ) { UNDEFINED } Read and interpret the next word.  If it
	is in the dictionary, then execute it if it is immediate; if not
	immediate, execute it if in immediate mode or append it to the currently
	compiling word if in compiling mode.

	If not in the dictionary, attempt to parse it as a number.  If it is a
	valid number, push it on stack if in immediate mode or append it as a
	literal to the currently compiling word if in compiling mode.

	If it is neither in the dictionary nor a valid number, print an error
	message to stderr.
	*/
	define_code	INTERPRET,	"INTERPRET"
	// R15B = is literal = false
	xor	r15b,	r15b
	//// read next word
	// RDI = c-word; RCX = ulen
	call	_WORD
	// R13 = c-word; R14 = ulen
	mov	r13,	rdi
	mov	r14,	rcx
	// RAX = a-word
	call	_FIND
	// jump if not in dictionary
	test	rax,	rax
	jz	1f
	//// in dictionary
	// R12B = length-flags byte
	mov	r12b,	[rax + 8]
	// RDI = a-word
	mov	rdi,	rax
	// RAX = codeword
	call	_$GREATER_THAN_SIGN$CFA
	mov	rax,	rdi
	// jump if immediate
	test	r12b,	offset FLAG_IMMEDIATE
	jnz	4f
	//// not immediate
	jmp	2f
1:
	//// not in dictionary
	// is literal = true
	inc	r15b
	//// parse it as a number
	// RDI = c-word; RCX = ulen
	mov	rdi,	r13
	mov	rcx,	r14
	// RAX = n; RCX = uleft
	call	_NUMBER
	// jump if uleft != 0: cannot parse
	test	rcx,	rcx
	jnz	6f
	//// successfully parsed
	// RBX = n
	mov	rbx,	rax
	// RAX = LIT: literal
	lea	rax,	[LIT]
2:
	//// not immediate word
	// jump if in immediate mode
	mov	rdx,	var_STATE
	test	rdx,	rdx
	jz	4f
	//// compiling
	// append codeword at HERE
	call	_$COMMA$
	// jump if not literal
	test	r15b,	r15b
	jz	3f
	//// literal
	// append n at HERE
	mov	rax,	rbx
	call	_$COMMA$
3:
	NEXT
4:
	//// executing
	// jump if literal
	test	r15b,	r15b
	jnz	5f
	//// execute codeword
	jmp	[rax]
	// NOTE: codeword must eventually call NEXT
5:
	//// execute literal: push it on stack
	DS_PUSH	rbx
	NEXT
6:
	//// parse error: print error message and context
	// long /* ssize_t */ write(int fildes, void *buf,
	//                          unsigned long /* size_t */ nbyte)
	/// EDI = fildes = 2 /* STDERR_FILENO */
	mov	edi,	2
	// ! preserve RSI !
	mov	r13,	rsi
	/// RSI = buf = error_msg_before
	lea	rsi,	[interpret_error_msg_before]
	/// RDX = nbyte = error_msg_before_end - error_msg_before
	lea	rdx,	[interpret_error_msg_before_end - interpret_error_msg_before]
	mov	rax,	__NR_write
	syscall
	//  NOTE: no need to check return value
	//// print context preceding current position in input buffer
	/// EDI = fildes = 2 /* STDERR_FILENO */
	mov	edi,	2
	/// RSI = buf = key_buffer_current
	mov	rsi,	key_buffer_current
	/// RDX = nbyte = min(key_buffer_current - key_buffer, ERROR_CONTEXT)
	mov	rdx,	rsi
	sub	rdx,	offset key_buffer
	// jump if there is nothing to print
	test	rdx,	rdx
	jz	8f
	// jump if (current - buffer) <= ERROR_CONTEXT
	cmp	rdx,	var_INTERPRET_ERROR_CONTEXT
	jbe	7f
	/// RDX = nbyte = ERROR_CONTEXT
	mov	rdx,	var_INTERPRET_ERROR_CONTEXT
7:
	/// RSI = buf = current - nbyte: print nbyte bytes preceding current
	sub	rsi,	rdx
	// nbyte--: do not print terminating whitespace
	dec	rdx
	mov	rax,	__NR_write
	syscall
	//  NOTE: no need to check return value
8:
	//// print error message after
	/// EDI = fildes = 2 /* STDERR_FILENO */
	mov	edi,	2
	/// RSI = buf = error_msg_after
	lea	rsi,	[interpret_error_msg_after]
	/// RDX = nbyte = error_msg_after_end - error_msg_after
	lea	rdx,	[interpret_error_msg_after_end - interpret_error_msg_after]
	mov	rax,	__NR_write
	syscall
	//  NOTE: no need to check return value
	// ! restore RSI !
	mov	rsi,	r13
	NEXT

	.section	.rodata
interpret_error_msg_before:
	.ascii	"PARSE ERROR: not a known word or a number in the current base:\n "
interpret_error_msg_before_end:

interpret_error_msg_after:
	.ascii	" <<<\n"
interpret_error_msg_after_end:

	/* Miscellaneous
	   =============
	*/

	/* ( -- c ) Read next word and get its first character c.
	*/
	define_code	CHAR,	"CHAR"
	// read next word: RDI = c-word, RCX = ulen
	call	_WORD
	// get first character
	movzx	rax,	byte ptr [rdi]
	DS_PUSH	rax
	NEXT

	/* ( xt -- ) Execute xt.
	*/
	define_code	EXECUTE,	"EXECUTE"
	// RAX = xt
	DS_POP	rax
	// jump to xt
	jmp	[rax]
	// NOTE: EXECUTE must not call NEXT: leave it to xt's NEXT

	/* ( warg1 warg2 ... wargu u ncode -- wr ) Call syscall with numeric
	code ncode passing u arguments (warg1, warg2, ..., wargu).  wr is the
	return value from the syscall.

	If there are fewer arguments than the syscall takes, the behaviour is
	undefined.  If there are more arguments than the syscall takes, the
	unused arguments are popped but not used by the syscall.  In any case,
	only up to 6 arguments are supported, and if u > 6 then warg7, warg8,
	..., wargu are silently dropped.
	*/
	define_code	SYSCALL,	"SYSCALL"
	// RAX = ncode
	DS_POP	rax
	// RCX = u
	DS_POP	rcx
	// jump if u <= 6
	cmp	rcx,	6
	jbe	11f
	//// drop warg7, warg8, ..., wargu and decrease RCX accordingly
	lea	rbx,	[rcx - 6]
	lea	rsp,	[rsp + 8 * rbx]
	sub	rcx,	rbx
11:
	//// u <= 6:
	// ! preserve RSI !
	mov	r12,	rsi
	// choose correct label for u
	mov	rbx,	[syscall_label_u + 8 * rcx]
	jmp	rbx
6:	/// u = 6
	// R9 = warg6
	DS_POP	r9
5:	/// u ≥ 5
	// R8 = warg5
	DS_POP	r8
4:	/// u ≥ 4
	// R10 = warg4
	DS_POP	r10
3:	/// u ≥ 3
	// RDX = warg3
	DS_POP	rdx
2:	/// u ≥ 2
	// RSI = warg2
	DS_POP	rsi
1:	/// u ≥ 1
	// RDI = warg1
	DS_POP	rdi
0:	/// u ≥ 0
	syscall
	// wr = RAX
	DS_PUSH	rax
	// ! restore RSI !
	mov	rsi,	r12
	NEXT
	// NOTE: SYSCALL must be the last defined built-in word for LATEST's
	// initial value to be correct

	.section	.rodata
	.align	8
	// used in SYSCALL
syscall_label_u:
	.8byte	0b
	.8byte	1b
	.8byte	2b
	.8byte	3b
	.8byte	4b
	.8byte	5b
	.8byte	6b

	/* Memory regions
	   ==============

	   Data segment
	   ------------

	The data segment is allocated from the kernel at startup.
	*/

	.set	DATA_SEGMENT_SIZE_INITIAL,	1 << 17

	.text

	// CALLable set up of data segment
data_segment_set_up:
	//// find beginning of data segment
	// void *brk(void *addr)
	/// RDI = addr = 0 /* NULL */
	xor	rdi,	rdi
	// RAX = current break (brk() fails because NULL is never accepted)
	mov	rax,	__NR_brk
	syscall
	// HERE = beginning of data segment (current break)
	mov	var_HERE,	rax
	//// allocate data segment
	/// RDI = addr = new break (end of data segment)
	lea	rdi,	[rax + DATA_SEGMENT_SIZE_INITIAL]
	// ! preserve new break !
	push	rdi
	mov	rax,	__NR_brk
	syscall
	// ! restore new break !
	pop	rdi
	// jump if return value >= new break: no failure
	cmp	rax,	rdi
	jae	1f
	//// brk() failure
	fatal_error	"failed allocation of data segment",	101
1:
	ret

	/* Data stack and return stack
	   ---------------------------
	*/

	.set	RETURN_STACK_SIZE,	1 << 14

	.data
	.align	8
data_stack_top:
	.8byte	0	// uninitialized

	.bss

	.align	1 << 12
return_stack:
	.space	RETURN_STACK_SIZE
return_stack_top:

	/* Buffers
	   -------
	*/

	.set	KEY_BUFFER_SIZE,	1 << 12
	.set	EMIT_BUFFER_SIZE,	1 << 12

	.bss

	.align	1 << 12
key_buffer:
	.space	KEY_BUFFER_SIZE

	.align	1 << 12
emit_buffer:
	.space	EMIT_BUFFER_SIZE
emit_buffer_end:

	/* FORTH code
	   ==========

	The rest of the implementation of brk_forth is in `brk_forth.f`.
	*/
