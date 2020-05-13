;****************************************************************
;*		プロトタイプ宣言				*
;****************************************************************
;=======================================
;■数値・文字変換
;●10進数
;・文字→数値（DS:SIに、バッファの次のアドレスが入る。）
Str10_2_Byte		proto	near,	strValue:FAR PTR BYTE	;				c_s10_b.asm
Str10_2_Word		proto	near,	strValue:FAR PTR BYTE	;				c_s10_w.asm
;・数値→文字列
Byte_2_Str10		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s10.asm
Word_2_Str10		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE	;		c_w_s10.asm
DWord_2_Str10		proto	near,	iValue	:DWORD,	strBuff:FAR PTR BYTE	;		c_dw_s10.asm
SByte_2_Str10		proto	near,	iValue	:SBYTE,	strBuff:FAR PTR BYTE	;		c_sb_s10.asm
SWord_2_Str10		proto	near,	iValue	:SWORD,	strBuff:FAR PTR BYTE	;		c_sw_s10.asm
;・数値→10進数文字列（右揃え整形）
Byte_2_Str10_F		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s10f.asm
Word_2_Str10_F		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE,	iForm	:WORD;	c_w_s10f.asm

;●16進数
;・文字→数値（DS:SIに、バッファの次のアドレスが入る。）
Str16_2_Byte		proto	near,	strValue:FAR PTR BYTE	;				c_s16_b.asm	□
Str16_2_Word		proto	near,	strValue:FAR PTR BYTE	;				c_s16_b.asm	□
;・数値→文字列
Byte_2_Str16		proto	near,	iValue	:BYTE,	strBuff:FAR PTR BYTE	;		c_b_s16.asm
Word_2_Str16		proto	near,	iValue	:WORD,	strBuff:FAR PTR BYTE	;		c_w_s16.asm

;=======================================
;■10進数数値入力
Input_int10		proto	near,	iKeta:word


;---------------------------------------------------------------|
;	文字列表示						|
;---------------------------------------------------------------|
;	引数							|
;		DS	セグメントアドレス			|
;		str	表示文字列（文字列、変数、レジスタ可）	|
;	返り値							|
;		無し						|
;	注意							|
;		DS DX AX 破壊					|
;---------------------------------------------------------------|
print_c	macro	str

		push	ax
		push	dx

		mov	dl, str
		mov	ah, 02h
		int	21h

		pop	dx
		pop	ax

		endm
;---------------------------------------------------------------|
;	文字列表示						|
;---------------------------------------------------------------|
;	引数							|
;		DS	セグメントアドレス			|
;		str	表示文字列（文字列、変数、レジスタ可）	|
;	返り値							|
;		無し						|
;	注意							|
;		DS DX AX 破壊					|
;---------------------------------------------------------------|
print_b	macro	str:VARARG

	local	msg

		;◆文字列を渡された場合
		if	(@InStr(1, <str>, <!">) EQ 1)
			.data
			msg	db	str, 24h

			.code
			lea	dx, msg

		;◆far pointer変数を渡された。
		elseif	(TYPE(str) EQ 4)
			lds	dx, str
		;◆near pointer変数, レジスタを渡された。
		elseif	(TYPE(str) EQ 2)
			mov	dx, str
		;◆アドレスが渡された。
		else
			lea	dx, str
		endif

		mov	ah, 09h
		int	21h

		endm
;---------------------------------------------------------------|
;	文字列表示						|
;---------------------------------------------------------------|
;	引数							|
;		DS	セグメントアドレス			|
;		str	表示文字列（文字列、変数、レジスタ可）	|
;	返り値							|
;		無し						|
;---------------------------------------------------------------|
print	macro	str:VARARG

		;●レジスタ保存
		push	ax
		push	dx
		if	(@InStr(1, <str>, <!">) EQ 1)
		elseif	(TYPE(str) EQ 4)	;far pointer変数を渡された。
		push	ds
		endif

		print_b	str

		;●レジスタ復帰
		if	(@InStr(1, <str>, <!">) EQ 1)
		elseif	(TYPE(str) EQ 4)	;far pointer変数を渡された。
		pop	ds
		endif
		pop	dx
		pop	ax

	endm
;---------------------------------------------------------------|
;	文字列表示						|
;---------------------------------------------------------------|
;	引数							|
;		DS		文字列のセグメント・アドレス	|
;		str		表示文字列(near pointer)	|
;	返り値							|
;		ax		09h				|
;		dx		str				|
;---------------------------------------------------------------|
print_dx	macro	str

		lea	dx, str
		int	21h

		endm
