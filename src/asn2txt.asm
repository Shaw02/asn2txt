;|==============================================================|
;|								|
;|		PKCS#7 to Text					|
;|								|
;===============================================================|

.186
.model	small,stdcall

.dosseg

.stack	01000h

;=======================================================================|
;				define					|
;=======================================================================|
include	asn2txt.inc


.data
Level		dw	0

.data?
strBuff		db	16	dup(?)

opCode		db	?	;op-code
opCode_pt	dd	?	;op-code's pointer
opTAG		db	?	;op-code's TAG
opStruct	db	?	;op-code's Struct flag
opClass		db	?	;op-code's Class

fSize		db	256	dup(?)	;Data Size flag (size = unknown?)
szData		dd	256	dup(?)	;Data Size
ptDataEnd	dd	256	dup(?)	;End of Data's Pointer

.const
CR		db	0dh, 0ah, 24h

;=======================================================================|
;		General TAG						|
;-----------------------------------------------------------------------|
;	Output
;		al :	data
;=======================================================================|
.data
code_n	db	0
.code
Convert_Load		proc	near

	.if	(code_n > 7)
		mov	code_n, 0
		.if
			print	CR
			print	"      "
		.endif
	.endif

	lodsb	es:[si]
	invoke	Byte_2_Str16,	al,	addr strBuff
	print	strBuff
	print_C	' '

	inc	code_n

	ret
Convert_Load		endp
;=======================================================================|
;		General TAG						|
;=======================================================================|
.code
Convert_Struct_Indent	proc	near	uses ax cx dx,

	mov	al, code_n
	mov	ah, 3
	mov	cx, 25
	mul	ah
	sub	cx, ax
	mov	ah, 02h
	mov	dl, ' '
	.while	(cx > 0)
		int	21h
		dec	cx
	.endw

	print_c	':'
	mov	code_n, 0

	mov	ah, 02h
	mov	dl, ' '
	mov	cx, Level
	.while	(cx > 0)
		int	21h
		dec	cx
	.endw

	ret
Convert_Struct_Indent	endp
;=======================================================================|
;		General TAG						|
;=======================================================================|
.code
Convert_OutData	proc	near	uses bx cx

	push	si		;ポインタ一次保存
	mov	bx, Level
	.if	(fSize[bx] == 0)
		;サイズ分
		shl	bx, 2
		mov	cx, word ptr szData[bx + 0]
		.while	(cx > 0)
			invoke	Convert_Load
			dec	cx
		.endw
	.else
		;0x00 00	まで

	.endif
	pop	si
	invoke	Convert_Struct_Indent

	ret
Convert_OutData	endp
;=======================================================================|
;	変換								|
;=======================================================================|
.code
Convert	proc	near

	local	flagEND:BYTE

	xor	si,si

	.repeat

		invoke	Word_2_Str16,	si,	addr strBuff
		print	strBuff
		print	"h:"

		;-------------------------------
		;Opcode
		mov	WORD PTR [opCode_pt + 0], si
		mov	WORD PTR [opCode_pt + 2], 0

		invoke	Convert_Load
		mov	opCode, al

		mov	ah, al
		shr	ah, 6
		mov	opClass, ah

		mov	ah, al
		shr	al, 5
		and	ax, 01F01h
		mov	opStruct, al
		mov	opTAG, ah

		;-------------------------------
		;Size
		xor	ax, ax
		mov	bx, Level
		mov	fSize[bx], al
		shl	bx, 2
		mov	word ptr szData[bx + 0], ax
		mov	word ptr szData[bx + 2], ax

		invoke	Convert_Load
		.if	(al < 80h)
			mov	byte ptr szData[bx + 0], al
		.elseif	(al == 80h)
			mov	bx, Level
			mov	fSize[bx], 1
		.elseif	(al > 80h)
			and	ax, 0007Fh
			.if	(al > 4)
				.const
				ERR_Size_Large	db	"サイズが大きい。",0ah,0dh,24h
				.code
				invoke	PRINT_ERR,	addr ERR_Size_Large
			.endif
			mov	cx, ax
			dec	ax
			add	bx, ax
			.while(cx > 0)
				invoke	Convert_Load
				mov	byte ptr szData[bx], al
				dec	bx
				dec	cx
			.endw
		.endif

		mov	bx, Level
		shl	bx, 2

		mov	ax, si
		add	ax, word ptr szData[bx + 0]
		mov	word ptr ptDataEnd[bx + 0], ax
		mov	ax, 0
		adc	ax, word ptr szData[bx + 2]
		mov	word ptr ptDataEnd[bx + 2], ax

		;-------------------------------
		;Data
		.if	(opClass == 0)
			;汎用
			call	Convert_TAG_call

		.elseif	(opClass == 1)
			invoke	Convert_Struct_Indent
			;応用

		.elseif	(opClass == 2)
			;Context
			invoke	Convert_Struct_Indent
			print	"Context["
			invoke	Byte_2_Str10,	opTAG,	addr strBuff
			print	strBuff
			print_c	']'

		.elseif	(opClass == 3)
			;Private
			invoke	Convert_Struct_Indent
			print	"Private"

		.endif

		;-------------------------------
		;レベルを上げるか？
		.if	(opStruct == 1)
			print_c	'{'
			inc	Level
		.endif
		print	CR
	
		;-------------------------------
		;レベルを下げるか？
		mov	flagEND, 0
		.repeat
		  mov	bx, Level
		  dec	bx
		  .if	(fSize[bx] != 0)
			.if(word ptr es:[si] == 0)
				invoke	Word_2_Str16,	si,	addr strBuff
				print	strBuff
				print	"h:00 00 "
				dec	Level
				add	si, 2
				invoke	Convert_Struct_Indent
				print	"}",0dh,0ah
			.else
				mov	flagEND, 1
			.endif
		  .else
			shl	bx, 2
			.if	(word ptr ptDataEnd[bx + 0] <= si)
				invoke	Word_2_Str16,	si,	addr strBuff
				print	strBuff
				print	"h:"
				dec	Level
				invoke	Convert_Struct_Indent
				print	"}",0dh,0ah
			.else
				mov	flagEND, 1
			.endif
		  .endif
		.until	((flagEND == 1) || (Level == 0))

	.until	(Level == 0)

	ret
Convert	endp
;=======================================================================|
;		Out String						|
;=======================================================================|
.code
Convert_OutStrings	proc	near

	print_c	'"'

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		dec	cx
		lodsb	es:[si]
		mov	dl, al
		mov	ah, 02h
		int	21h
	.endw

	print_c	'"'

	ret
Convert_OutStrings	endp
;=======================================================================|
;		General TAG						|
;=======================================================================|
.const
Convert_TAG_lst	dw	offset	Convert_0			;00
		dw	offset	Convert_BOOLEAN			;01
		dw	offset	Convert_INTEGER			;02
		dw	offset	Convert_BIT_STRING		;03
		dw	offset	Convert_OCTET_STRING		;04
		dw	offset	Convert_NULL			;05
		dw	offset	Convert_OBJECT_IDENTIFIER	;06
		dw	offset	Convert_ObjectDescriptor	;07
		dw	offset	Convert_EXTERNAL		;08
		dw	offset	Convert_REAL			;09
		dw	offset	Convert_ENUMERATED		;0A
		dw	offset	Convert_undefined		;0B
		dw	offset	Convert_UTF8String		;0C
		dw	offset	Convert_undefined		;0D
		dw	offset	Convert_undefined		;0E
		dw	offset	Convert_undefined		;0F
		dw	offset	Convert_SEQUENCE		;10
		dw	offset	Convert_SET			;11
		dw	offset	Convert_NumericString		;12
		dw	offset	Convert_PrintableString		;13
		dw	offset	Convert_TeletexString		;14
		dw	offset	Convert_VideotexString		;15
		dw	offset	Convert_IA5String		;16
		dw	offset	Convert_UTCTime			;17
		dw	offset	Convert_GeneralizedTime		;18
		dw	offset	Convert_GraphicString		;19
		dw	offset	Convert_VisibleString		;1A
		dw	offset	Convert_GeneralString		;1B
		dw	offset	Convert_CharacterString		;1C
		dw	offset	Convert_undefined		;1D
		dw	offset	Convert_undefined		;1E
		dw	offset	Convert_undefined		;1F
.code
Convert_TAG_call	proc	near

	xor	bx, bx
	mov	bl, opTAG
	shl	bx, 1
	mov	dx, Convert_TAG_lst[bx]
	mov	bx, Level
	call	dx

	ret
Convert_TAG_call	endp
;=======================================================================|
;		0B, 0D, 0E, 0F, 1D, 1E, 1F				|
;=======================================================================|
Convert_undefined	proc	near
	invoke	Convert_OutData
	print	"ERROR:unKnown"
	.exit	-1

	ret
Convert_undefined	endp
;=======================================================================|
;		00							|
;=======================================================================|
Convert_0		proc	near
	invoke	Convert_OutData
	print	"Tag = 0x00"

	shl	bx, 2
	add	si, word ptr szData[bx]

	ret
Convert_0		endp
;=======================================================================|
;		01							|
;=======================================================================|
Convert_BOOLEAN		proc	near
	invoke	Convert_OutData
	print	"Boolean ::= "

	lodsb	es:[si]
	.if	(al == 0)
		print	"FALSE"
	.else
		print	"TRUE"
	.endif

	ret
Convert_BOOLEAN		endp
;=======================================================================|
;		02							|
;=======================================================================|
Convert_INTEGER		proc	near
	invoke	Convert_OutData
	print	"Integer ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		lodsb	es:[si]
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
		dec	cx
	.endw

	ret
Convert_INTEGER		endp
;=======================================================================|
;		03							|
;=======================================================================|
Convert_BIT_STRING	proc	near

	local	unUseBit:BYTE

	invoke	Convert_OutData
	print	"Bit String ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	push	cx

	lodsb	es:[si]
	mov	cl, al
	mov	ah, -1
	shl	ah, cl
	mov	unUseBit, ah

	pop	cx
	dec	cx

	.while	(cx > 0)
		dec	cx
		lodsb	es:[si]
		.if	(cx == 0)
			and	al, unUseBit
		.endif
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
	.endw

	ret
Convert_BIT_STRING	endp
;=======================================================================|
;		04							|
;=======================================================================|
Convert_OCTET_STRING	proc	near

	invoke	Convert_OutData
	print	"Octet String ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		dec	cx
		lodsb	es:[si]
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
	.endw

	ret
Convert_OCTET_STRING	endp
;=======================================================================|
;		05							|
;=======================================================================|
Convert_NULL		proc	near
	invoke	Convert_OutData
	print	"Null"

	ret
Convert_NULL		endp
;=======================================================================|
;		06							|
;=======================================================================|
Convert_OBJECT_IDENTIFIER	proc	near

	local	Value:DWORD

	invoke	Convert_OutData
	print	"Object Identifier ::= "

	xor	ax, ax
	mov	WORD PTR [Value + 0],ax
	mov	WORD PTR [Value + 2],ax

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	add	cx, si

	xor	ax, ax
	lodsb	es:[si]
	mov	dl, 40
	div	dl

	push	ax
	invoke	Byte_2_Str10, al, addr strBuff
	print	strBuff
	print_c	'.'
	pop	ax
	invoke	Byte_2_Str10, ah, addr strBuff
	print	strBuff

	.while	(si < cx)

		lodsb	es:[si]
		.if	(al & 80h)
			and	al, 07Fh
			or	BYTE PTR [Value],al
			mov	ax, WORD PTR [Value + 0]
			shr	ax, (16-7)
			shl	WORD PTR [Value + 2],7
			or	WORD PTR [Value + 2], ax
			shl	WORD PTR [Value + 0],7
		.else
			or	BYTE PTR [Value],al
			print_c	'.'
			invoke	DWORD_2_Str10, Value, addr strBuff
			print	strBuff
			xor	ax, ax
			mov	WORD PTR [Value + 0],ax
			mov	WORD PTR [Value + 2],ax
		.endif

	.endw

	ret
Convert_OBJECT_IDENTIFIER	endp
;=======================================================================|
;		07							|
;=======================================================================|
Convert_ObjectDescriptor	proc	near

	invoke	Convert_OutData
	print	"Object Descriptor ::= "
	invoke	Convert_OutStrings

	ret
Convert_ObjectDescriptor	endp
;=======================================================================|
;		08							|
;=======================================================================|
Convert_EXTERNAL	proc	near

	invoke	Convert_OutData
	print	"External ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		lodsb	es:[si]
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
		dec	cx
	.endw

	ret
Convert_EXTERNAL	endp
;=======================================================================|
;		09							|
;=======================================================================|
Convert_REAL		proc	near

	invoke	Convert_OutData
	print	"Real ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		lodsb	es:[si]
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
		dec	cx
	.endw

	ret
Convert_REAL		endp
;=======================================================================|
;		0A							|
;=======================================================================|
Convert_ENUMERATED	proc	near

	invoke	Convert_OutData
	print	"Enumerated ::= "

	shl	bx, 2
	mov	cx, word ptr szData[bx]
	.while	(cx > 0)
		lodsb	es:[si]
		invoke	Byte_2_Str16,	al,	addr strBuff
		print	strBuff
		print_c	' '
		dec	cx
	.endw

	ret
Convert_ENUMERATED	endp
;=======================================================================|
;		0C							|
;=======================================================================|
Convert_UTF8String	proc	near

	invoke	Convert_OutData
	print	"UTF8 String ::= "
	invoke	Convert_OutStrings

	ret
Convert_UTF8String	endp
;=======================================================================|
;		10							|
;=======================================================================|
Convert_SEQUENCE	proc	near

	invoke	Convert_Struct_Indent
	print	"Sequence"

	ret
Convert_SEQUENCE	endp
;=======================================================================|
;		11							|
;=======================================================================|
Convert_SET		proc	near

	invoke	Convert_Struct_Indent
	print	"Set"

	ret
Convert_SET		endp
;=======================================================================|
;		12							|
;=======================================================================|
Convert_NumericString	proc	near

	invoke	Convert_OutData
	print	"Numeric String ::= "
	invoke	Convert_OutStrings

	ret
Convert_NumericString	endp
;=======================================================================|
;		13							|
;=======================================================================|
Convert_PrintableString	proc	near

	invoke	Convert_OutData
	print	"Printable String ::= "
	invoke	Convert_OutStrings

	ret
Convert_PrintableString	endp
;=======================================================================|
;		14							|
;=======================================================================|
Convert_TeletexString	proc	near

	invoke	Convert_OutData
	print	"Teletex String ::= "
	invoke	Convert_OutStrings

	ret
Convert_TeletexString	endp
;=======================================================================|
;		15							|
;=======================================================================|
Convert_VideotexString	proc	near

	invoke	Convert_OutData
	print	"Videotex String ::= "
	invoke	Convert_OutStrings

	ret
Convert_VideotexString	endp
;=======================================================================|
;		16							|
;=======================================================================|
Convert_IA5String	proc	near

	invoke	Convert_OutData
	print	"IA5String ::= "
	invoke	Convert_OutStrings

	ret
Convert_IA5String	endp
;=======================================================================|
;		17							|
;=======================================================================|
Convert_UTCTime		proc	near

	invoke	Convert_OutData
	print	"UTC Time ::= "
	invoke	Convert_OutStrings

	ret
Convert_UTCTime		endp
;=======================================================================|
;		18							|
;=======================================================================|
Convert_GeneralizedTime	proc	near

	invoke	Convert_OutData
	print	"Generalized Time ::= "
	invoke	Convert_OutStrings

	ret
Convert_GeneralizedTime	endp
;=======================================================================|
;		19							|
;=======================================================================|
Convert_GraphicString	proc	near

	invoke	Convert_OutData
	print	"Graphic String ::= "
	invoke	Convert_OutStrings

	ret
Convert_GraphicString	endp
;=======================================================================|
;		1A							|
;=======================================================================|
Convert_VisibleString	proc	near

	invoke	Convert_OutData
	print	"Visible String ::= "
	invoke	Convert_OutStrings

	ret
Convert_VisibleString	endp
;=======================================================================|
;		1B							|
;=======================================================================|
Convert_GeneralString	proc	near

	invoke	Convert_OutData
	print	"General String ::= "
	invoke	Convert_OutStrings

	ret
Convert_GeneralString	endp
;=======================================================================|
;		1C							|
;=======================================================================|
Convert_CharacterString	proc	near

	invoke	Convert_OutData
	print	"Character String ::= "
	invoke	Convert_OutStrings

	ret
Convert_CharacterString	endp

;=======================================================================|
;	メインルーチン							|
;=======================================================================|
.code
_main	proc	near

	local	Ext[3]:BYTE		;拡張子　変更用
	local	cFilename[134]:BYTE	;ファイル名

	local	mesBuff[16]:BYTE

	;-------------------------------
	;■オプション処理
	lea	di, cFilename
	invoke	Op_,	addr es:[0080h],	ss::di	;オプションに記述されたファイル名を取得する。

	;-------------------------------
	;■ファイルを開く
	invoke	Change_Current_Dir,		ss::di
	invoke	File_Open,			ss::di, 0
	mov	cx, ax

	;-------------------------------
	;■メモリ確保
	invoke	Memory_Open,	01000h
	mov	es, ax

	;-------------------------------
	;■ファイル読み込み
	invoke	File_Load,	cx, addr es:[0]

	;-------------------------------
	;■逆コンパイル
	invoke	Convert


	;-------------------------------
	;■ファイル・クローズ
	invoke	File_Close,	cx	;hSND

	;-------------------------------
	;■メモリ開放
	invoke	Memory_Close,	es

	ret
_main	endp
;---------------------------------------------------------------|
;		エラー処理					|
;---------------------------------------------------------------|
;引数								|
;	ptMES	表示文字列（エラー内容）			|
;---------------------------------------------------------------|
.code
PRINT_ERR	proc	near,
	ptMES:ptr WORD
	push	DGROUP
	pop	ds
	print	ptMES

	.exit	255			;
PRINT_ERR	endp
;=======================================================================|
;				Start Up				|
;=======================================================================|
.code
	.startup

	invoke	Memory_Small_Exe, offset DGROUP:stack

	invoke	_main

	.exit	0

;=======================================================================|
	end
