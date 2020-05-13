
	.186
	.model	tiny,stdcall

;=======================================================================
;				Mersenne Twister
;-----------------------------------------------------------------------
;	Reference:
;	http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
;-----------------------------------------------------------------------
;   A C-program for MT19937, with initialization improved 2002/1/26.
;   Coded by Takuji Nishimura and Makoto Matsumoto.
;
;   Before using, initialize the state by using init_genrand(seed)  
;   or init_by_array(init_key, key_length).
;
;   Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
;   All rights reserved.                          
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;     1. Redistributions of source code must retain the above copyright
;        notice, this list of conditions and the following disclaimer.
;
;     2. Redistributions in binary form must reproduce the above copyright
;        notice, this list of conditions and the following disclaimer in the
;        documentation and/or other materials provided with the distribution.
;
;     3. The names of its contributors may not be used to endorse or promote 
;        products derived from this software without specific prior written 
;        permission.
;
;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;   A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;
;
;   Any feedback is very welcome.
;   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
;   email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
;
;=======================================================================

;****************************************************************
;*		外部宣言					*
;****************************************************************
include	lib_dos.inc


;****************************************************************
;*		define						*
;****************************************************************
N		equ	624
M		equ	397
MATRIX_A	equ	9908b0dfh	;constant vector a
UPPER_MASK	equ	80000000h	;most significant w-r bits
LOWER_MASK	equ	7fffffffh	;least significant r bits

;****************************************************************
;*		glocal value					*
;****************************************************************
.data
mti		word	N*4+4		;mti==N+1 means mt[N] is not initialized

.data?
mt		dword	N	dup(?)	;the array for the state vector

;****************************************************************
;*		コード						*
;****************************************************************
;---------------------------------------------------------------|
;#define MIXBITS(u,v) ( ((u) & UMASK) | ((v) & LMASK) )
;#define TWIST(u,v) ((MIXBITS(u,v) >> 1) ^ ((v)&1UL ? MATRIX_A : 0UL))
;---------------------------------------------------------------|
TWIST	macro	s2, s1, p1, p2

	local	__TWIST_1
	local	__TWIST_2

	mov	s1, p1+0
	mov	s2, p2+0
	and	s1, (UPPER_MASK AND 0FFFFh)
	and	s2, (LOWER_MASK AND 0FFFFh)
	or	s1, s2

	push	s1
	mov	s2, p1+2
	mov	s1, p2+2
	and	s2, (UPPER_MASK SHR 16)
	and	s1, (LOWER_MASK SHR 16)
	or	s2, s1				;s2::s1 = (p1 | p2)
	pop	s1

	shr	s2, 1
	rcr	s1, 1				;s2::s1 >>= 1
	jc	__TWIST_1			;if (最下位bit = L){
	xor	s2, 0				;	s2::s1 ^= 0
	xor	s1, 0				;
	jmp	__TWIST_2			;}
__TWIST_1:					; else {
	xor	s2, (MATRIX_A SHR 16)		;	s2::s1 ^= MATRIX_A
	xor	s1, (MATRIX_A AND 0FFFFh)	;
__TWIST_2:					; }

	endm

;---------------------------------------------------------------|
;		initialize					|
;---------------------------------------------------------------|
;	処理							|
;		initializes mt[N] with a seed			|
;	引数							|
;		unsigned __int32	S	Seed		|
;	返り値							|
;		無し						|
;---------------------------------------------------------------|
.code
	even
init_genrand	proc	near,
		s:DWORD			;Seed

	pusha

	mov	ax, word ptr s[0]
	mov	dx, word ptr s[2]	;dx::ax = s
	xor	di, di			;di = 0		加算用
	mov	word ptr mt[0],ax
	xor	si, si			;si = 0		mt[]の添え字
	mov	word ptr mt[2],dx	;mt[0] = dx::ax

	.repeat
		; See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
		; In the previous versions, MSBs of the seed affect
		; only MSBs of the array mt[].
		; 2002/01/09 modified by Makoto Matsumoto


		mov	bx, dx
		inc	di					; di++
		shr	bx, (30-16)
		xor	dx, 0					; 
		add	si, 4					; si += 4
		xor	ax, bx					; dx::ax ^= dx::ax >> 30

		Mul32	dx::ax, 1812433253		; dx::ax *= 1812433253

		add	ax,di
		adc	dx,0					; dx::ax += di

		mov	word ptr mt[si+0],ax
		mov	word ptr mt[si+2],dx			; mt[si] = dx::ax
	.until	(si >= N*4)

	mov	mti, si

	popa
	ret
init_genrand	endp
;---------------------------------------------------------------|
;		initialize by array				|
;---------------------------------------------------------------|
;	処理							|
;		initialize by an array with array-length	|
;		init_key is the array for initializing keys	|
;		key_length is its length			|
;	引数							|
;		DWORD PTR	init_key[]			|
;		SWORD		key_length			|
;---------------------------------------------------------------|
init_by_array	proc	near,
		init_key:FAR PTR DWORD,
		key_length:SWORD

	local	key_length4:SWORD

	pusha

	invoke	init_genrand,	19650218

	les	di, init_key		;es:[di]	unsighed __int32 init_key[]

	mov	ax, key_length
	mov	cx, ax
	.if	(cx < N)
		mov	cx, N		;cx = (N > key_length)? N : key_length;
	.endif
	shl	ax, 2
	xor	bx, bx
	mov	key_length4, ax
	xor	si, si			;si = 0


	mov	ax, word ptr mt[0]
	mov	dx, word ptr mt[2]	;dx::ax = mt[0]
	.repeat
		push	cx
		add	si, 4					; si += 4

		mov	cx, dx
		shr	cx, (30-16)
		xor	dx, 0					; dx::ax ^= dx::ax >> 30
		xor	ax, cx		
		Mul32	dx::ax, 1664525		; dx::ax *= 1664525
		xor	ax, word ptr mt[si+0]
		xor	dx, word ptr mt[si+2]			; dx::ax ^= mt[si]

		add	ax, word ptr es:[di+bx+0]
		adc	dx, word ptr es:[di+bx+2]		; dx::ax += key_length[bx]
		mov	cx, bx
		shr	cx, 2
		add	ax, cx		;non linear
		adc	dx, 0					; dx::ax += bx>>2
		mov	word ptr mt[si+0], ax
		mov	word ptr mt[si+2], dx			; mt[si] = dx::ax

		.if	(si >= N*4 - 4)			;if (si >= 4*N - 4){
			mov	word ptr mt[0], ax	;	mt[0] = dx::ax
			mov	word ptr mt[2], dx	;	si = 0
			xor	si, si			;}
		.endif

		add	bx, 4
		.if	(bx >= key_length4)		;if (bx >= key_length){
			xor	bx, bx			;	bx = 0;
		.endif					;}

		pop	cx
	.untilcxz

	mov	cx, N-1
	.repeat
		push	cx
		add	si, 4					; si += 4

		mov	cx, dx
		shr	cx, (30-16)
		xor	dx, 0					; dx::ax ^= dx::ax >> 30
		xor	ax, cx
		Mul32	dx::ax, 1566083941	; dx::ax *= 1566083941
		xor	ax, word ptr mt[si+0]
		xor	dx, word ptr mt[si+2]			; dx::ax ^= mt[si]
		mov	cx, si
		shr	cx, 2
		sub	ax, cx		; non linear
		sbb	dx, 0					; dx::ax -= si>>2
		mov	word ptr mt[si+0], ax
		mov	word ptr mt[si+2], dx			; mt[si] = dx::ax

		.if	(si >= N*4 - 4)			;if (si >= 4*N - 4){
			mov	word ptr mt[0], ax	;	mt[0] = dx::ax
			mov	word ptr mt[2], dx	;	si = 0
			xor	si, si			;}
		.endif

		pop	cx
	.untilcxz

	mov	word ptr mt[0], 0
	mov	word ptr mt[2], 8000h	;  MSB is 1; assuring non-zero initial array

	popa
	ret
init_by_array	endp
;---------------------------------------------------------------|
;		get the 32bit random number			|
;---------------------------------------------------------------|
;	処理							|
;		generates a random number on [0,0xffffffff]-interval
;	返り値							|
;		dx::ax	32bit rundom				|
;---------------------------------------------------------------|
.code
genrand_int32	proc	near	uses	bx cx di si

	mov	si, mti

	.if	(si >= N*4)				;generate N words at one time

		.if	(si == N*4 +4)			; if init_genrand() has not been called,
			invoke	init_genrand,	5489	; a default initial seed is used
		.endif

		xor	si, si
		.repeat
			TWIST	dx, ax, word ptr mt[si+0], word ptr mt[si+4]

			xor	ax, word ptr mt[si + M*4 + 0]
			xor	dx, word ptr mt[si + M*4 + 2]	;dx::ax ^= mt[si + M*4]
			mov	word ptr mt[si+0], ax
			mov	word ptr mt[si+2], dx		;mt[si] = dx::ax
			add	si, 4
		.until	(si >= N*4 - M*4)

		.repeat
			TWIST	dx, ax, word ptr mt[si+0], word ptr mt[si+4]

			xor	ax, word ptr mt[si + (M-N)*4 + 0]
			xor	dx, word ptr mt[si + (M-N)*4 + 2]	;dx::ax ^= mt[si + (M-N)*4]
			mov	word ptr mt[si+0], ax
			mov	word ptr mt[si+2], dx		;mt[si] = dx::ax
			add	si, 4
		.until	(si >= N*4 - 4)

		TWIST	dx, ax, word ptr mt[N*4 -4], word ptr mt[0]

		xor	ax, word ptr mt[M*4 - 4]
		xor	dx, word ptr mt[M*4 - 2]	;dx::ax ^= mt[M-1]
		mov	word ptr mt[N*4 -4], ax
		mov	word ptr mt[N*4 -2], dx		;mt[N-1] = dx::ax

		xor	si, si
	.endif

	mov	ax, word ptr mt[si + 0]
	mov	dx, word ptr mt[si + 2]
	add	si, 4
	mov	mti, si

	SHR32	dx, ax, 11
	xor	ax, di
	xor	dx, si		;dx::ax ^= si::di

	SHL32	dx, ax, 7
	and	di, 05680h	;di &= 0x5680
	and	si, 09d2ch	;si &= 0x9D2C
	xor	ax, di
	xor	dx, si		;dx::ax ^= si::di

	SHL32	dx, ax, 15
	and	si, 0efc6h	;si &= 0xEFC6
	xor	ax, 0
	xor	dx, si		;dx::ax ^= si::di

	mov	di, dx
	shr	di, 18-16
	xor	dx, 0
	xor	ax, di		;dx::ax ^= dx>>2

	ret
genrand_int32	endp
end
