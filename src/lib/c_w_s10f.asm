
	.186
	.model	tiny,stdcall


;****************************************************************
;*		�O���錾					*
;****************************************************************
include	lib_dos.inc

extrn	H2A_Div:WORD

;****************************************************************
;*		�R�[�h						*
;****************************************************************
;---------------------------------------------------------------|
;		�P�U�i���R�[�h�`ASCII CODE(255)			|
;---------------------------------------------------------------|
;	����							|
;		�P�U�i�R�[�h���P�O�i�̃A�X�L�[�R�[�h�ɕϊ�	|
;		�X�y�[�X�ŁA�E����				|
;	����							|
;		iValue		�ϊ����������l			|
;		*strBuff	��������i�[����A�h���X	|
;		iForm		���`������ (5�`1)		|
;	�Ԃ�l							|
;		strBuff		�ϊ���̕�����			|
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
