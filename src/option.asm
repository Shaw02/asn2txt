
.186
.model	small,stdcall

;=======================================================================|
;				define					|
;=======================================================================|
include	p7c2txt.inc

;===============================================================|
;								|
;		オプション処理					|
;								|
;===============================================================|
.data

public	iLoop
public	iBR

iLoop	dw	2
iBR	dw	12

;****************************************************************
;*		エラー						*
;****************************************************************
.const
OPTION_ERROR_MSG	db	'オプションが不正です。',0dh,0ah,24h

.code
OPTION_ERROR	proc	near
	invoke	PRINT_ERR, addr OPTION_ERROR_MSG
OPTION_ERROR	endp
;****************************************************************
;*		ヘルプ						*
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
;*		オプション処理					*
;****************************************************************
;●引数								*
;	ptFilename	ファイル名を入れるポインタ		*
;●返り値							*
;	ptFilename[]	オプション文字列で指定されたファイル名	*
;●使用レジスタ							*
;	ds:si		オプション文字列アドレス		*
;	es:di		ptFilename				*
;	ax		汎用					*
;	cx		オプション文字列最終アドレス＋１	*
;****************************************************************
.code
Op_	proc	near	uses ds es,
		ptPSP:DWORD,
		ptFilename:DWORD	;ファイル名を入れるポインタ

	local	dotFlag:BYTE		;ファイル名の拡張子の有無
	local	cHelp:BYTE		;ヘルプ表示の有無

	pusha

	lds	si,ptPSP		;
	les	di,ptFilename		;

	;-----------------------
	;フラグ立て
	xor	ax,ax			;ax ← 0
	stosb				;先頭にだけ、0を入れておく。
	mov	cHelp,al		;
	mov	dotFlag,al		;

	;-----------------------
	;オプション
	lodsb				;AL←オプション文字数
	ADD	ax,si			;
	MOV	cx,ax			;CX←オープション文字列最終番地

    .while	(cx>=si)

	lodsb

	.if	(al<21h)
		.continue

	.elseif	((al=='/')||(al=='-'))
		.if	(cx<si)
			JMP	OPTION_ERROR	;だったらエラー
		.endif

		lodsb
		;-----------------------
		.if	((al=='h')||(al=='H')||(al=='?'))
			mov	cHelp,0FFh
		;-----------------------
		.elseif	((al=='l')||(al=='L'))
			.repeat
				.if	(cx<si)			;無かったらエラー
				   JMP	OPTION_ERROR
				.endif
				invoke	Str10_2_Word,	ds::si	;AH←文字列（数値）
				mov	si,bx
			.until	(!(Carry?))
			mov	es:[iLoop],ax
		;-----------------------
		.elseif	((al=='b')||(al=='B'))
			.repeat
				.if	(cx<si)			;無かったらエラー
				   JMP	OPTION_ERROR
				.endif
				invoke	Str10_2_Word,	ds::si	;AH←文字列（数値）
				mov	si,bx
			.until	(!(Carry?))
			mov	es:[iBR],ax
	;	;-----------------------
		.else
			JMP	OPTION_ERROR	;無かったらエラー
		;-----------------------
		.endif

	.else
		les	di,ptFilename		;ファイル名を入れるポインタ
		.if	(byte ptr es:[di] != 0)
			JMP	OPTION_ERROR	;ファイル名が２つ書いてあるよぉ～
		.endif

		dec	si			;ポインタ一つ戻す
		mov	dotFlag,0
		.while	(cx>=si)
			lodsb
			.if	(al == '.')
				mov	dotFlag,1
			.elseif	(al < 21h)
				dec	si
				.break		;21h未満だったら終わり。
			.endif
			stosb			;ファイル名セット
		.endw

		.if	(dotFlag==0)		;拡張子が定義されなかったら定義
			mov	al,'.'
			stosb
			mov	al,'P'		;".SND"にする。一応、大文字で。
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
	;フラグに応じた処理
	;●ヘルプ
	.if	(cHelp != 0)
		jmp	OPTION_HELP		;
	.endif

	;●ファイル処理
	les	di,ptFilename			;ファイル名があるポインタ
	.if	(byte ptr es:[di] == 0)
		jmp	OPTION_HELP	;ファイル名の指定が無ければ、
	.endif

	popa
	RET				;RETURN
Op_	endp
;=======================================================================|
	end
