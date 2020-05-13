
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
;		�P�U�i���R�[�h�`ASCII CODE(FF)			|
;---------------------------------------------------------------|
;	����							|
;		�P�U�i�R�[�h���P�U�i�̃A�X�L�[�R�[�h�ɕϊ�	|
;	����							|
;		iValue		�ϊ����������l			|
;		*strBuff	��������i�[����A�h���X	|
;	�Ԃ�l							|
;		strBuff		�ϊ���̕�����			|
;---------------------------------------------------------------|
.code
Word_2_Str16	proc	near	uses ax dx di es,
		iValue	:WORD,
		strBuff	:FAR PTR BYTE

		les	di, strBuff

		mov	ax, iValue
		mov	dx, ax		;
		and	ax, 0f0fh	; ax <- ---- BA98 ---- 3210
		shr	dx, 4		; dx <- ---- FEDC BA98 7654
		and	dx, 0f0fh	; dx <- ---- FEDC ---- 7654

		.if	(dl>9)
			add	dl,7
		.endif

		.if	(dh>9)
			add	dh,7
		.endif

		.if	(al>9)
			add	al,7
		.endif

		.if	(ah>9)
			add	ah,7
		.endif

		add	dx,3030h	; ax <- ---- BA98 ---- FEDC
		add	ax,3030h	; dx <- ---- 3210 ---- 7654
		xchg	al,dh

		stosw
		mov	ax, dx
		stosw
		mov	al,'$'
		mov	es:[di],al

		ret			;
Word_2_Str16	endp			;
;****************************************************************
	end
