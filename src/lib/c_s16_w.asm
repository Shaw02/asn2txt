
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
;		ASCII CODE���P�U�i�R�[�h(-32768�`65535)		|
;---------------------------------------------------------------|
;	����							|
;		strValue	�ϊ�������������擪�A�h���X	|
;	�Ԃ�l							|
;		AX		�ϊ���				|
;		DS:SI		���̃A�h���X			|
;		carry flag	0:���� / 1:���s			|
;---------------------------------------------------------------|
.data
S16W_Mul	dw	1,16,256,4096
.code
Str16_2_Word	proc	near	uses bx cx di es,
		strValue	:FAR PTR BYTE

	local	dhex8		:WORD
	local	hex_8[4]	:BYTE

	push	ss			;��������
	pop	es
	lea	di, hex_8		;es:di �ۑ���
	lds	si, strValue		;ds:si �Ǎ���
	xor	cx, cx
	mov	dhex8,	cx		

	.repeat				;���ǂݍ��݁��ꎟ�ۑ�
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
	.until		(cx >= 4)	;4���܂�

	.if		(cx > 0)	;���ϊ�
	   dec	si	;�|�C���^�[��߂�
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
