
	.186
	.model	tiny,stdcall


;****************************************************************
;*		�O���錾					*
;****************************************************************
include	lib_dos.inc

public	H2A_Div

;****************************************************************
;*		�R�[�h						*
;****************************************************************
;---------------------------------------------------------------|
;		�P�U�i���R�[�h�`ASCII CODE(65535)		|
;---------------------------------------------------------------|
;	����							|
;		�P�U�i�R�[�h���P�O�i�̃A�X�L�[�R�[�h�ɕϊ�	|
;	����							|
;		iValue		�ϊ����������l			|
;		*strBuff	��������i�[����A�h���X	|
;	�Ԃ�l							|
;		strBuff		�ϊ���̕�����			|
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
