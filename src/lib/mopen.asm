
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
;		メモリ確保					|
;---------------------------------------------------------------|
;	●引数							|
;		ParaSize	割り当てたいパラグラム		|
;	●返り値						|
;		ax		割り当てたメモリのセグメント	|
;	●処理							|
;		メモリを確保し、当該メモリを０クリアします。	|
;---------------------------------------------------------------|
.code
Memory_Open	proc	near	uses bx cx di es,
		ParaSize:word

		MOV	bx,ParaSize		;データ領域の確保
		MOV	AH,48H			;
		INT	21H			;

		.if(carry?)			;割り当て失敗時に飛ぶ。
			invoke	File_Err	;
		.endif

		mov	es,ax			;
		xor	di,di			;
		xor	ax,ax			;
		mov	cx,ParaSize		;
		shl	cx,3			;
		cld
	rep	stosw				;メモリ空間を０クリア
		mov	ax,es			;

		RET				;
Memory_Open	endp			;
;****************************************************************
	end
