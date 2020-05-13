;****************************************************************
;*		�v���g�^�C�v�錾				*
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
;*		�}�N��						*
;****************************************************************
;---------------------------------------------------------------|
;	�t�@�C�����J��						|
;---------------------------------------------------------------|
;	����							|
;		DS	�Z�O�����g�A�h���X			|
;		fname	�t�@�C�����i������A�ϐ��A���W�X�^�j	|
;		mode		0 ��� Read			|
;				1 ��� Write			|
;				2 ��� Randam			|
;	�Ԃ�l							|
;		ax	�t�@�C���n���h��			|
;---------------------------------------------------------------|
fopen	macro	fname, mode

	local	name

		;���������n���ꂽ�ꍇ
		if	(@InStr(1, fname, <!">) EQ 1)
			.data
			name	db	fname,0,24h
			.code
			invoke	File_Open,	addr name, mode

		;��far pointer�ϐ�, ���W�X�^��n���ꂽ�B
		elseif	(TYPE(fname) EQ 4)
			invoke	File_Open,	fname, mode

		;��near pointer�ϐ�, ���W�X�^��n���ꂽ�B
		elseif	(TYPE(fname) EQ 2)
			mov	ax, fname
			invoke	File_Open,	ds::ax, mode

		;���A�h���X���n���ꂽ�B
		else
			invoke	File_Open,	addr fname, mode

		endif

	endm
;---------------------------------------------------------------|
;	�t�@�C�����J��						|
;---------------------------------------------------------------|
;	����							|
;		DS	�Z�O�����g�A�h���X			|
;		fname	�t�@�C�����i������A�ϐ��A���W�X�^�j	|
;		attr	�A�g���r���[�g				|
;		ax	�t�@�C���n���h��			|
;---------------------------------------------------------------|
fcreate	macro	fname, attr

	local	name

		;���������n���ꂽ�ꍇ
		if	(@InStr(1, fname, <!">) EQ 1)
			.data
			name	db	fname,0,24h
			.code
			invoke	File_Create,	addr name, attr

		;��far pointer�ϐ�, ���W�X�^��n���ꂽ�B
		elseif	(TYPE(fname) EQ 4)
			invoke	File_Create,	fname, attr

		;��near pointer�ϐ�, ���W�X�^��n���ꂽ�B
		elseif	(TYPE(fname) EQ 2)
			mov	ax, fname
			invoke	File_Create,	ds::ax, attr

		;���A�h���X���n���ꂽ�B
		else
			invoke	File_Create,	addr fname, attr

		endif

	endm
;---------------------------------------------------------------|
;	�t�@�C�������					|
;---------------------------------------------------------------|
;	����							|
;		hFile	�t�@�C���E�n���h��			|
;	�Ԃ�l							|
;		����						|
;---------------------------------------------------------------|
fclose	macro	hFile

	invoke	File_Close,	hFile

	endm
;---------------------------------------------------------------|
;	�t�@�C����ǂ�						|
;---------------------------------------------------------------|
;	������							|
;		ptrBuff	�ǂݍ��ݐ��far address			|
;		size_t	�ǂݍ��ތ^				|
;		size	�ǂݍ��ސ���				|
;		hFile	�t�@�C���E�n���h��			|
;	���Ԃ�l						|
;		����						|
;	������							|
;		ptrBuff���ADWORD�^��(�z��)�ϐ��ɂ���Ɠ��삵�Ȃ�|
;---------------------------------------------------------------|
fread	macro	ptrBuff, size_t, size, hFile

		;��far pointer�ϐ�, ���W�X�^��n���ꂽ�B
		if	(TYPE(ptrBuff) EQ 4)
			invoke	File_Load_S,	hFile, (size_t * size), ptrBuff

		;���A�h���X���n���ꂽ�B
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
;	�t�@�C��������						|
;---------------------------------------------------------------|
;	������							|
;		ptrBuff	�ǂݍ��ݐ��far address			|
;		size_t	�ǂݍ��ތ^				|
;		size	�ǂݍ��ސ���				|
;		hFile	�t�@�C���E�n���h��			|
;	���Ԃ�l						|
;		����						|
;	������							|
;		ptrBuff���ADWORD�^��(�z��)�ϐ��ɂ���Ɠ��삵�Ȃ�|
;---------------------------------------------------------------|
fwrite	macro	ptrBuff, size_t, size, hFile

		;��far pointer�ϐ�, ���W�X�^��n���ꂽ�B
		if	(TYPE(ptrBuff) EQ 4)
			invoke	File_Write,	hFile, (size_t * size), ptrBuff

		;���A�h���X���n���ꂽ�B
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