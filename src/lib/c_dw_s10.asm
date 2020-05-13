
	.186
	.model	tiny,stdcall


;****************************************************************
;*		外部宣言					*
;****************************************************************
include	lib_dos.inc

extrn	H2A_Div:WORD

;****************************************************************
;*		コード						*
;****************************************************************
;---------------------------------------------------------------|
;		１６進数コード～ASCII CODE(4,294,967,295)	|
;---------------------------------------------------------------|
;	処理							|
;		１６進コードを１０進のアスキーコードに変換	|
;	引数							|
;		iValue		変換したい数値			|
;		*strBuff	文字列を格納するアドレス	|
;	返り値							|
;		strBuff		変換後の文字列			|
;---------------------------------------------------------------|

D_5E8	equ	01DCD6500h
D_6E8	equ	023C34600h
D_7E8	equ	029B92700h
D_8E8	equ	02FAF0800h
D_9E8	equ	035A4E900h

D_1E9	equ	03B9ACA00h
D_2E9	equ	077359400h
D_3E9	equ	0B2D05E00h
D_4D9	equ	0EE6B2800h

.code
DWord_2_Str10	proc	near	uses es,
	iValue	:DWORD,
	strBuff	:FAR PTR BYTE

	local	flag:byte
	local	ValueHigh:WORD	;上 10000
	local	ValueLow:WORD	;下 10000

	pusha

	xor	bx, bx
	mov	flag, bl		;flag reset

	les	di, strBuff
	mov	dx, WORD PTR [iValue + 2]
	mov	ax, WORD PTR [iValue + 0]

	.if	(dx > HIGHWORD D_4D9) || ( (dx == HIGHWORD D_4D9) && (ax >= LOWWORD D_4D9) )
		sub	ax, LOWWORD  D_4D9
		sbb	dx, HIGHWORD D_4D9
		mov	bl,'4'
	.elseif	(dx > HIGHWORD D_3E9) || ( (dx == HIGHWORD D_3E9) && (ax >= LOWWORD D_3E9) )
		sub	ax, LOWWORD  D_3E9
		sbb	dx, HIGHWORD D_3E9
		mov	bl,'3'
	.elseif	(dx > HIGHWORD D_2E9) || ( (dx == HIGHWORD D_2E9) && (ax >= LOWWORD D_2E9) )
		sub	ax, LOWWORD  D_2E9
		sbb	dx, HIGHWORD D_2E9
		mov	bl,'2'
	.elseif	(dx > HIGHWORD D_1E9) || ( (dx == HIGHWORD D_1E9) && (ax >= LOWWORD D_1E9) )
		sub	ax, LOWWORD  D_1E9
		sbb	dx, HIGHWORD D_1E9
		mov	bl,'1'
	.elseif
		mov	bl,0
	.endif
	.if	(bl != 0)
		push	ax
		mov	al, bl
		stosb
		mov	flag, 1
		pop	ax
	.endif


	.if	(dx > HIGHWORD D_9E8) || ( (dx == HIGHWORD D_9E8) && (ax >= LOWWORD D_9E8) )
		sub	ax, LOWWORD  D_9E8
		sbb	dx, HIGHWORD D_9E8
		mov	bl,'9'
	.elseif	(dx > HIGHWORD D_8E8) || ( (dx == HIGHWORD D_8E8) && (ax >= LOWWORD D_8E8) )
		sub	ax, LOWWORD  D_8E8
		sbb	dx, HIGHWORD D_8E8
		mov	bl,'8'
	.elseif	(dx > HIGHWORD D_7E8) || ( (dx == HIGHWORD D_7E8) && (ax >= LOWWORD D_7E8) )
		sub	ax, LOWWORD  D_7E8
		sbb	dx, HIGHWORD D_7E8
		mov	bl,'7'
	.elseif	(dx > HIGHWORD D_6E8) || ( (dx == HIGHWORD D_6E8) && (ax >= LOWWORD D_6E8) )
		sub	ax, LOWWORD  D_6E8
		sbb	dx, HIGHWORD D_6E8
		mov	bl,'6'
	.elseif	(dx > HIGHWORD D_5E8) || ( (dx == HIGHWORD D_5E8) && (ax >= LOWWORD D_5E8) )
		sub	ax, LOWWORD  D_5E8
		sbb	dx, HIGHWORD D_5E8
		mov	bl,'5'
	.elseif
		mov	bl,0
	.endif
	.if	(bl != 0)
		push	ax
		mov	al, bl
		stosb
		mov	flag, 1
		pop	ax

		mov	bx, 2
		mov	cx, 4
	.else
		xor	bx,bx
		mov	cx, 5
	.endif

	div	word ptr [H2A_Div]
	mov	ValueHigh, ax
	mov	ValueLow,  dx

	mov	dx, ax		;ValueHigh
	.repeat
	   .if	( (dx >= word ptr [H2A_Div + bx]) || (flag != 0))
		mov	flag, 1
		mov	ax, dx
		xor	dx, dx
		div	word ptr [H2A_Div + bx]
		add	al,30h
		stosb
	   .endif
	   add	bx,2
	   dec	cx
	.until	(zero?)

	mov	bx, 2
	mov	cx, 4
	mov	dx, ValueLow
	.repeat
	   .if	( (dx >= word ptr [H2A_Div + bx]) || (flag != 0)  || (cx==1))
		mov	flag, 1
		mov	ax, dx
		xor	dx, dx
		div	word ptr [H2A_Div + bx]
		add	al,30h
		stosb
	   .endif
	   add	bx,2
	   dec	cx
	.until	(zero?)

	mov	word ptr es:[di], '$'

	popa
	ret

DWord_2_Str10	endp
;****************************************************************
	end
