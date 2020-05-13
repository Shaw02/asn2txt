
	.186
	.model	tiny,stdcall


;****************************************************************
;*		外部宣言					*
;****************************************************************
include	lib_dos.inc


;****************************************************************
;*		コード						*
;****************************************************************
;---------------------------------------------------------------|
;		ASCII CODE→１６進コード(-32768～65535)		|
;---------------------------------------------------------------|
;	引数							|
;		strValue	変換したい文字列先頭アドレス	|
;	返り値							|
;		AX		変換後				|
;		DS:SI		次のアドレス			|
;		carry flag	0:成功 / 1:失敗			|
;---------------------------------------------------------------|
.data
S16W_Mul	dw	1,16,256,4096
.code
Str16_2_Word	proc	near	uses bx cx di es,
		strValue	:FAR PTR BYTE

	local	dhex8		:WORD
	local	hex_8[4]	:BYTE

	push	ss			;◆初期化
	pop	es
	lea	di, hex_8		;es:di 保存先
	lds	si, strValue		;ds:si 読込元
	xor	cx, cx
	mov	dhex8,	cx		

	.repeat				;◆読み込み＆一次保存
	   lodsb
	   .if		(al >= '0') && (al <= '9')
		sub	al, 30h
	   .elseif	(al >= 'A') && (al <= 'F')
		sub	al, 37h
	   .elseif	(al >= 'a') && (al <= 'f')
		sub	al, 57h
	   .else
		.break
	   .endif
	   stosb
	   inc	cx
	.until		(cx >= 4)	;4桁まで

	.if		(cx > 0)	;◆変換
	   dec	si	;ポインターを戻す
	   xor	bx, bx
	   .repeat
		dec	di
		xor	ax, ax
		mov	al, es:[di]
		mul	cs:[S16W_Mul + bx]
		add	DHEX8, ax
		add	bx, 2
		dec	cx
	   .until	(zero?)
	   mov	ax, DHEX8
	   clc		;Not Error
	.else
Error:	   stc		;Error
	.endif

	RET				;
Str16_2_Word	endp			;
;;****************************************************************
	end
