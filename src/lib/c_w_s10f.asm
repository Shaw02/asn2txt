
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
;		１６進数コード～ASCII CODE(255)			|
;---------------------------------------------------------------|
;	処理							|
;		１６進コードを１０進のアスキーコードに変換	|
;		スペースで、右揃え				|
;	引数							|
;		iValue		変換したい数値			|
;		*strBuff	文字列を格納するアドレス	|
;		iForm		整形文字数 (5～1)		|
;	返り値							|
;		strBuff		変換後の文字列			|
;---------------------------------------------------------------|
.code
Word_2_Str10_F	proc	near	uses es,
	iValue	:WORD,
	strBuff	:FAR PTR BYTE,
	iForm	:WORD

	local	flag:byte

	pusha

	les	di, strBuff
	mov	flag, bl		;flag reset
	mov	cx, iForm
	mov	dx, iValue
	mov	bx, 5
	sub	bx, cx
	shl	bx, 1
	.repeat
	   .if	( (ax >= word ptr cs:[H2A_Div + bx]) || (flag != 0) || (cx==1))
		mov	flag, 1
		mov	ax, dx
		xor	dx, dx
		div	word ptr cs:[H2A_Div + bx]
		add	al,30h
	   .elseif
		mov	al,' '
	   .endif
	   stosb
	   add	bx,2
	   dec	cx
	.until	(zero?)
	mov	word ptr es:[di], '$'

	popa
	ret
Word_2_Str10_F	endp
;****************************************************************
	end
