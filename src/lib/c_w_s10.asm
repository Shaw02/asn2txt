
	.186
	.model	tiny,stdcall


;****************************************************************
;*		外部宣言					*
;****************************************************************
include	lib_dos.inc

public	H2A_Div

;****************************************************************
;*		コード						*
;****************************************************************
;---------------------------------------------------------------|
;		１６進数コード～ASCII CODE(65535)		|
;---------------------------------------------------------------|
;	処理							|
;		１６進コードを１０進のアスキーコードに変換	|
;	引数							|
;		iValue		変換したい数値			|
;		*strBuff	文字列を格納するアドレス	|
;	返り値							|
;		strBuff		変換後の文字列			|
;---------------------------------------------------------------|
.data
H2A_Div	dw	10000,1000,100,10,1
.code
Word_2_Str10	proc	near	uses es,
	iValue	:WORD,
	strBuff	:FAR PTR BYTE

	local	flag:byte

	pusha

	les	di, strBuff
	xor	bx, bx
	mov	flag, bl		;flag reset

	mov	ax, iValue
	mov	cx, 5
	mov	dx, ax
	.repeat
	   .if	( (ax >= word ptr cs:[H2A_Div + bx]) || (flag != 0)  || (cx==1))
		mov	flag, 1
		mov	ax, dx
		xor	dx, dx
		div	word ptr cs:[H2A_Div + bx]
		add	al,30h
		stosb
	   .endif
	   add	bx,2
	   dec	cx
	.until	(zero?)
	mov	word ptr es:[di], '$'

	popa
	ret

Word_2_Str10	endp
;****************************************************************
	end
