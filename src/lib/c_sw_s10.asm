
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
;		１６進数コード〜ASCII CODE(65535)（符号付き）	|
;---------------------------------------------------------------|
;	処理							|
;		１６進コードを１０進のアスキーコードに変換	|
;	引数							|
;		iValue		変換したい数値			|
;		*strBuff	文字列を格納するアドレス	|
;	返り値							|
;		strBuff		変換後の文字列			|
;---------------------------------------------------------------|
.code
SWord_2_Str10	proc	near	uses ax es di,
	iValue	:SWORD,
	strBuff	:FAR PTR BYTE

	les	di, strBuff
	mov	al, '+'

	or	iValue, 0
	.if	(sign?)
		neg	iValue
		mov	al, '-'
	.endif

	stosb
	invoke	Word_2_Str10,	iValue,	es::di

	ret
SWord_2_Str10	endp
;****************************************************************
	end
