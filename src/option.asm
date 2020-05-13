
.186
.model	small,stdcall

;=======================================================================|
;				define					|
;=======================================================================|
include	asn2txt.inc

;===============================================================|
;								|
;		�I�v�V��������					|
;								|
;===============================================================|
.data

public	iLoop
public	iBR

iLoop	dw	2
iBR	dw	12

;****************************************************************
;*		�G���[						*
;****************************************************************
.const
OPTION_ERROR_MSG	db	'�I�v�V�������s���ł��B',0dh,0ah,24h

.code
OPTION_ERROR	proc	near
	invoke	PRINT_ERR, addr OPTION_ERROR_MSG
OPTION_ERROR	endp
;****************************************************************
;*		�w���v						*
;****************************************************************
.const
OP_HELP	DB	0DH,0AH
	DB	"PKCS#7 to Text",0DH,0AH
	DB	"	Programmed by A.Watanabe",0DH,0AH
	DB	0DH,0AH
	DB	"P7C2TXT [/option] [source(.P7C)] [>out.txt]",0DH,0AH
	DB	0DH,0AH
;	DB	"   filename1	Source SPC filename",0DH,0AH
;	DB	"  >filename2	Output MML filename",0DH,0AH
	DB	"  /?		Display help",0DH,0AH
	DB	024H

.code
OPTION_HELP	proc	near

	push	DGROUP
	pop	ds
	lea	dx,[OP_HELP]
	mov	ah,09h
	int	21h

	.exit	0
OPTION_HELP	endp			;
;****************************************************************
;*		�I�v�V��������					*
;****************************************************************
;������								*
;	ptFilename	�t�@�C����������|�C���^		*
;���Ԃ�l							*
;	ptFilename[]	�I�v�V����������Ŏw�肳�ꂽ�t�@�C����	*
;���g�p���W�X�^							*
;	ds:si		�I�v�V����������A�h���X		*
;	es:di		ptFilename				*
;	ax		�ėp					*
;	cx		�I�v�V����������ŏI�A�h���X�{�P	*
;****************************************************************
.code
Op_	proc	near	uses ds es,
		ptPSP:DWORD,
		ptFilename:DWORD	;�t�@�C����������|�C���^

	local	dotFlag:BYTE		;�t�@�C�����̊g���q�̗L��
	local	cHelp:BYTE		;�w���v�\���̗L��

	pusha

	lds	si,ptPSP		;
	les	di,ptFilename		;

	;-----------------------
	;�t���O����
	xor	ax,ax			;ax �� 0
	stosb				;�擪�ɂ����A0�����Ă����B
	mov	cHelp,al		;
	mov	dotFlag,al		;

	;-----------------------
	;�I�v�V����
	lodsb				;AL���I�v�V����������
	ADD	ax,si			;
	MOV	cx,ax			;CX���I�[�v�V����������ŏI�Ԓn

    .while	(cx>=si)

	lodsb

	.if	(al<21h)
		.continue

	.elseif	((al=='/')||(al=='-'))
		.if	(cx<si)
			JMP	OPTION_ERROR	;��������G���[
		.endif

		lodsb
		;-----------------------
		.if	((al=='h')||(al=='H')||(al=='?'))
			mov	cHelp,0FFh
		;-----------------------
		.elseif	((al=='l')||(al=='L'))
			.repeat
				.if	(cx<si)			;����������G���[
				   JMP	OPTION_ERROR
				.endif
				invoke	Str10_2_Word,	ds::si	;AH��������i���l�j
				mov	si,bx
			.until	(!(Carry?))
			mov	es:[iLoop],ax
		;-----------------------
		.elseif	((al=='b')||(al=='B'))
			.repeat
				.if	(cx<si)			;����������G���[
				   JMP	OPTION_ERROR
				.endif
				invoke	Str10_2_Word,	ds::si	;AH��������i���l�j
				mov	si,bx
			.until	(!(Carry?))
			mov	es:[iBR],ax
	;	;-----------------------
		.else
			JMP	OPTION_ERROR	;����������G���[
		;-----------------------
		.endif

	.else
		les	di,ptFilename		;�t�@�C����������|�C���^
		.if	(byte ptr es:[di] != 0)
			JMP	OPTION_ERROR	;�t�@�C�������Q�����Ă���悧�`
		.endif

		dec	si			;�|�C���^��߂�
		mov	dotFlag,0
		.while	(cx>=si)
			lodsb
			.if	(al == '.')
				mov	dotFlag,1
			.elseif	(al < 21h)
				dec	si
				.break		;21h������������I���B
			.endif
			stosb			;�t�@�C�����Z�b�g
		.endw

		.if	(dotFlag==0)		;�g���q����`����Ȃ��������`
			mov	al,'.'
			stosb
			mov	al,'P'		;".SND"�ɂ���B�ꉞ�A�啶���ŁB
			stosb
			mov	al,'7'
			stosb
			mov	al,'C'
			stosb
		.endif

		mov	al,0			;
		stosb
		mov	al,24h			;
		stosb
	.endif

    .endw

	;-----------------------
	;�t���O�ɉ���������
	;���w���v
	.if	(cHelp != 0)
		jmp	OPTION_HELP		;
	.endif

	;���t�@�C������
	les	di,ptFilename			;�t�@�C����������|�C���^
	.if	(byte ptr es:[di] == 0)
		jmp	OPTION_HELP	;�t�@�C�����̎w�肪������΁A
	.endif

	popa
	RET				;RETURN
Op_	endp
;=======================================================================|
	end
