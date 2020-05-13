
	.186
	.model	tiny,stdcall


;****************************************************************
;*		�O���錾					*
;****************************************************************
include	lib_dos.inc


;****************************************************************
;*		�R�[�h						*
;****************************************************************
;---------------------------------------------------------------|
;		�������m��					|
;---------------------------------------------------------------|
;	������							|
;		iKeta		���͂��錅���i1�`5�j		|
;	���Ԃ�l						|
;		ax		���͂��ꂽ���l			|
;	������							|
;		���l����͂��܂��B				|
;---------------------------------------------------------------|
.const
MES_BS	db	08h,20h,08h,24h
.code
Input_int10	proc	near	uses cx dx di si ds,
	iKeta:word

	local	cINPUT[5]:BYTE

	mov	cx, iKeta
	xor	ax, ax
	xor	di, di
	mov	word ptr cINPUT[0], ax
	mov	word ptr cINPUT[2], ax
	mov	byte ptr cINPUT[4], al

	.repeat
		mov	ah, 08h		;Input char (no echo)
		int	21h
		mov	ah, 02h		;Output char
		.if	(((al == 0ah) || (al == 0dH)) && (di > 0))
			.break
		.elseif	((al >= '0') && (al <= '9') && (di < cx))
			mov	cINPUT[di],al
			inc	di
			mov	dl, al
			int	21h
		.elseif	((al == 08h) && (di > 0))
			mov	cINPUT[di],0
			dec	di
			print	MES_BS
		.endif
	.until	0

	invoke	Str10_2_Word, addr cINPUT

	ret				;
Input_int10	endp			;
;****************************************************************
	end
