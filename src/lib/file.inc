;****************************************************************
;*		プロトタイプ宣言				*
;****************************************************************
File_Create		proto	near,	strName:far ptr byte,	iAttr:word
File_Open		proto	near,	strName:far ptr byte,	cMode:byte
File_Close		proto	near,	hFile:word
File_Load		proto	near,	hFile:word,	cBuff:far ptr byte
File_Load_S		proto	near,	hFile:word,	iSize:word,	cBuff:far ptr byte
File_Write		proto	near,	hFile:word,	iSize:word,	cBuff:far ptr byte
File_Seek		proto	near,	hFile:word,	iControl:BYTE,	dOffset	:dword
File_Err		proto	near

Change_Filename_Ext	proto	near,	strName:far ptr byte,	strExt	:far ptr byte
Change_Current_Dir	proto	near,	strName:far ptr byte



;****************************************************************
;*		マクロ						*
;****************************************************************
;---------------------------------------------------------------|
;	ファイルを開く						|
;---------------------------------------------------------------|
;	引数							|
;		DS	セグメントアドレス			|
;		fname	ファイル名（文字列、変数、レジスタ可）	|
;		mode		0 ･･･ Read			|
;				1 ･･･ Write			|
;				2 ･･･ Randam			|
;	返り値							|
;		ax	ファイルハンドル			|
;---------------------------------------------------------------|
fopen	macro	fname, mode

	local	name

		;◆文字列を渡された場合
		if	(@InStr(1, fname, <!">) EQ 1)
			.data
			name	db	fname,0,24h
			.code
			invoke	File_Open,	addr name, mode

		;◆far pointer変数, レジスタを渡された。
		elseif	(TYPE(fname) EQ 4)
			invoke	File_Open,	fname, mode

		;◆near pointer変数, レジスタを渡された。
		elseif	(TYPE(fname) EQ 2)
			mov	ax, fname
			invoke	File_Open,	ds::ax, mode

		;◆アドレスが渡された。
		else
			invoke	File_Open,	addr fname, mode

		endif

	endm
;---------------------------------------------------------------|
;	ファイルを開く						|
;---------------------------------------------------------------|
;	引数							|
;		DS	セグメントアドレス			|
;		fname	ファイル名（文字列、変数、レジスタ可）	|
;		attr	アトリビュート				|
;		ax	ファイルハンドル			|
;---------------------------------------------------------------|
fcreate	macro	fname, attr

	local	name

		;◆文字列を渡された場合
		if	(@InStr(1, fname, <!">) EQ 1)
			.data
			name	db	fname,0,24h
			.code
			invoke	File_Create,	addr name, attr

		;◆far pointer変数, レジスタを渡された。
		elseif	(TYPE(fname) EQ 4)
			invoke	File_Create,	fname, attr

		;◆near pointer変数, レジスタを渡された。
		elseif	(TYPE(fname) EQ 2)
			mov	ax, fname
			invoke	File_Create,	ds::ax, attr

		;◆アドレスが渡された。
		else
			invoke	File_Create,	addr fname, attr

		endif

	endm
;---------------------------------------------------------------|
;	ファイルを閉じる					|
;---------------------------------------------------------------|
;	引数							|
;		hFile	ファイル・ハンドル			|
;	返り値							|
;		無し						|
;---------------------------------------------------------------|
fclose	macro	hFile

	invoke	File_Close,	hFile

	endm
;---------------------------------------------------------------|
;	ファイルを読む						|
;---------------------------------------------------------------|
;	●引数							|
;		ptrBuff	読み込み先のfar address			|
;		size_t	読み込む型				|
;		size	読み込む数量				|
;		hFile	ファイル・ハンドル			|
;	●返り値						|
;		無し						|
;	●注意							|
;		ptrBuffを、DWORD型の(配列)変数にすると動作しない|
;---------------------------------------------------------------|
fread	macro	ptrBuff, size_t, size, hFile

		;◆far pointer変数, レジスタを渡された。
		if	(TYPE(ptrBuff) EQ 4)
			invoke	File_Load_S,	hFile, (size_t * size), ptrBuff

		;◆アドレスが渡された。
		else
			push	ax
			if (@InStr(1, hFile, <ax>) EQ 1)
			push	dx
			mov	dx, ax
			invoke	File_Load_S,	dx, (size_t * size), addr ptrBuff
			pop	dx
			else
			invoke	File_Load_S,	hFile, (size_t * size), addr ptrBuff
			endif
			pop	ax
		endif

	endm
;---------------------------------------------------------------|
;	ファイルを書く						|
;---------------------------------------------------------------|
;	●引数							|
;		ptrBuff	読み込み先のfar address			|
;		size_t	読み込む型				|
;		size	読み込む数量				|
;		hFile	ファイル・ハンドル			|
;	●返り値						|
;		無し						|
;	●注意							|
;		ptrBuffを、DWORD型の(配列)変数にすると動作しない|
;---------------------------------------------------------------|
fwrite	macro	ptrBuff, size_t, size, hFile

		;◆far pointer変数, レジスタを渡された。
		if	(TYPE(ptrBuff) EQ 4)
			invoke	File_Write,	hFile, (size_t * size), ptrBuff

		;◆アドレスが渡された。
		else
			push	ax
			if (@InStr(1, hFile, <ax>) EQ 1)
			push	dx
			mov	dx, ax
			invoke	File_Write,	dx, (size_t * size), addr ptrBuff
			pop	dx
			else
			invoke	File_Write,	hFile, (size_t * size), addr ptrBuff
			endif
			pop	ax
		endif

	endm
