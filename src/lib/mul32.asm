
	.186
	.model	tiny,stdcall

;****************************************************************
;*		�O���錾					*
;****************************************************************
include	lib_dos.inc


;---------------------------------------------------------------|
;	32bit mul						|
;---------------------------------------------------------------|
;	����							|
;		unsigned __int32	m1			|
;		unsigned __int32	m2			|
;	�Ԃ�l							|
;		dx::ax			m1 �~ m2		|
;---------------------------------------------------------------|
.code
	even
Mul_32	proc	near	uses bx cx di si,
	m1:DWORD,	;�|���Z�̌�
	m2:DWORD	;�|���Z�̌�

	mov	di, word ptr m1[0]	
	mov	si, word ptr m2[0]	

	mov	ax, si			
	mul	di			
	mov	bx, ax			
	mov	cx, dx			

	mov	ax, word ptr m1[2]	
	mul	si			
	add	cx, ax			

	mov	ax, word ptr m2[2]	
	mul	di			
	add	cx, ax			

	mov	ax, bx			
	mov	dx, cx			

	ret				
Mul_32	endp
;---------------------------------------------------------------|
	end
