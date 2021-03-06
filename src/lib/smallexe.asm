
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
;		ＥＸＥファイルのメモリー最小化			|
;---------------------------------------------------------------|
;	●処理							|
;		ＥＸＥプログラム実行時にメモリーを		|
;		最小限にする					|
;	●引数							|
;		szDGROUP	DGROUPのサイズ（スタック含む）	|
;		ES		Segment address of PSP		|
;		DS		Segment address of DGROUP	|
;	●返り値						|
;		無し						|
;---------------------------------------------------------------|
.code
Memory_Small_Exe	proc	near	uses ax bx es,	;メモリーの最小化
		szDGROUP:WORD

;	EXEメモリの最小化
;	size = (DS - ES) + (stack/16)

	mov	bx, ds
	mov	ax, es
	sub	bx, ax
	mov	ax, szDGROUP
	shr	ax,4
	add	bx,ax
	MOV	AH,04AH		;
	INT	21H		;最小化
	.if	(carry?)
	invoke	File_Err
	.endif

	push	es:[002CH]	;環境セグメントの開放
	pop	es		;
	MOV	AH,49H		;
	INT	21H		;
	.if	(carry?)
	invoke	File_Err
	.endif

	ret			;RETURN
Memory_Small_Exe	endp		;
;****************************************************************
	end
