
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
;		１６進数コード〜ASCII CODE(255)			|
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
Byte_2_Str10	proc	near	uses ax,
	iValue	:BYTE,
	strBuff	:FAR PTR BYTE

	xor	ax,ax
	mov	al,iValue
	invoke	Word_2_Str10,	ax,strBuff
	ret

Byte_2_Str10	endp
;****************************************************************
	end
