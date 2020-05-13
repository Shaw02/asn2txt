;****************************************************************
;*		�v���g�^�C�v�錾				*
;****************************************************************
;=======================================
;�����l�E�����ϊ�
;��10�i��
;�E���������l�iDS:SI�ɁA�o�b�t�@�̎��̃A�h���X������B�j
Str10_2_Byte		proto	near,	strValue:FAR PTR BYTE	;				c_s10_b.asm
Str10_2_Word		proto	near,	strValue:FAR PTR BYTE	;				c_s10_w.asm
;�E���l��������
Byte_2_Str10		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s10.asm
Word_2_Str10		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE	;		c_w_s10.asm
DWord_2_Str10		proto	near,	iValue	:DWORD,	strBuff:FAR PTR BYTE	;		c_dw_s10.asm
SByte_2_Str10		proto	near,	iValue	:SBYTE,	strBuff:FAR PTR BYTE	;		c_sb_s10.asm
SWord_2_Str10		proto	near,	iValue	:SWORD,	strBuff:FAR PTR BYTE	;		c_sw_s10.asm
;�E���l��10�i��������i�E�������`�j
Byte_2_Str10_F		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s10f.asm
Word_2_Str10_F		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE,	iForm	:WORD;	c_w_s10f.asm

;��16�i��
;�E���������l�iDS:SI�ɁA�o�b�t�@�̎��̃A�h���X������B�j
Str16_2_Byte		proto	near,	strValue:FAR PTR BYTE	;				c_s16_b.asm	��
Str16_2_Word		proto	near,	strValue:FAR PTR BYTE	;				c_s16_b.asm	��
;�E���l��������
Byte_2_Str16		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s16.asm
Word_2_Str16		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE	;		c_w_s16.asm

;=======================================
;��10�i�����l����
Input_int10		proto	near,	iKeta:word


;---------------------------------------------------------------|
;	������\��						|
;---------------------------------------------------------------|
;	����							|
;		DS	�Z�O�����g�A�h���X			|
;		str	�\��������i������A�ϐ��A���W�X�^�j	|
;	�Ԃ�l							|
;		����						|
;	����							|
;		DS DX AX �j��					|
;---------------------------------------------------------------|
print_c	macro	str

		push	ax
		push	dx

		mov	dl, str
		mov	ah, 02h
		int	21h

		pop	dx
		pop	ax

		endm
;---------------------------------------------------------------|
;	������\��						|
;---------------------------------------------------------------|
;	����							|
;		DS	�Z�O�����g�A�h���X			|
;		str	�\��������i������A�ϐ��A���W�X�^�j	|
;	�Ԃ�l							|
;		����						|
;	����							|
;		DS DX AX �j��					|
;---------------------------------------------------------------|
print_b	macro	str:VARARG

	local	msg

		;���������n���ꂽ�ꍇ
		if	(@InStr(1, <str>, <!">) EQ 1)
			.data
			msg	db	str, 24h

			.code
			lea	dx, msg

		;��far pointer�ϐ���n���ꂽ�B
		elseif	(TYPE(str) EQ 4)
			lds	dx, str
		;��near pointer�ϐ�, ���W�X�^��n���ꂽ�B
		elseif	(TYPE(str) EQ 2)
			mov	dx, str
		;���A�h���X���n���ꂽ�B
		else
			lea	dx, str
		endif

		mov	ah, 09h
		int	21h

		endm
;---------------------------------------------------------------|
;	������\��						|
;---------------------------------------------------------------|
;	����							|
;		DS	�Z�O�����g�A�h���X			|
;		str	�\��������i������A�ϐ��A���W�X�^�j	|
;	�Ԃ�l							|
;		����						|
;---------------------------------------------------------------|
print	macro	str:VARARG

		;�����W�X�^�ۑ�
		push	ax
		push	dx
		if	(@InStr(1, <str>, <!">) EQ 1)
		elseif	(TYPE(str) EQ 4)	;far pointer�ϐ���n���ꂽ�B
		push	ds
		endif

		print_b	str

		;�����W�X�^���A
		if	(@InStr(1, <str>, <!">) EQ 1)
		elseif	(TYPE(str) EQ 4)	;far pointer�ϐ���n���ꂽ�B
		pop	ds
		endif
		pop	dx
		pop	ax

	endm
;---------------------------------------------------------------|
;	������\��						|
;---------------------------------------------------------------|
;	����							|
;		DS		������̃Z�O�����g�E�A�h���X	|
;		str		�\��������(near pointer)	|
;	�Ԃ�l							|
;		ax		09h				|
;		dx		str				|
;---------------------------------------------------------------|
print_dx	macro	str

		lea	dx, str
		int	21h

		endm