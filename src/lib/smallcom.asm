
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
;		ＣＯＭファイルのメモリー最小化			|
;---------------------------------------------------------------|
;	●処理							|
;		ＣＯＭプログラム実行時にメモリーを		|
;		最小限にする					|
;	●引数							|
;		sizeCOM		プログラムサイズ（スタック含む）|
;	●返り値							|
;		無し						|
;---------------------------------------------------------------|
.code
Memory_Small_Com	proc	near	uses ax bx es,	;メモリーの最小化
		sizeCOM:WORD

	MOV	ES,CS:[002CH]	;環境セグメントの開放
	MOV	AH,49H		;
	INT	21H		;
	.if	(carry?)
	invoke	File_Err
	.endif

	push	cs		;
	pop	es		;ES←CS
	MOV	bx, sizeCOM
	shr	bx, 4
	MOV	AH,04AH		;
	INT	21H		;最小化
	.if	(carry?)
	invoke	File_Err
	.endif

	ret			;RETURN
Memory_Small_Com	endp		;
;****************************************************************
	end
