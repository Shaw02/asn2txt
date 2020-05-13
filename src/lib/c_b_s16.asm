
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
;		１６進数コード〜ASCII CODE(FF)			|
;---------------------------------------------------------------|
;	処理							|
;		１６進コードを１６進のアスキーコードに変換	|
;	引数							|
;		iValue		変換したい数値			|
;		*strBuff	文字列を格納するアドレス	|
;	返り値							|
;		strBuff		変換後の文字列			|
;---------------------------------------------------------------|
.code
Byte_2_Str16	proc	near	uses ax di es,
		iValue	:BYTE,
		strBuff	:FAR PTR BYTE

		les	di, strBuff
		mov	ah,al		;
		shr	al,4		;ah←下位4bit
		and	ax,0f0fh	;al←上位4bit

		.if	(al>9)
			add	al,7
		.endif

		.if	(ah>9)
			add	ah,7
		.endif

		add	ax,3030h	;
		stosw
		mov	al,'$'
		mov	es:[di],al

		ret			;
Byte_2_Str16	endp			;
;****************************************************************
	end
