		org	0x0100

stack:	equ	0123H
cpm:	equ	0005H
bdos	equ	0005H
		jmp	start

		ascii	'Hi there'	; comment for test purposes.
start:	ani	0x20
		inr	c
		add	b
		adc	b
		sub	b
		sbb	b
;		list	off
		xra	a
		ora	a
		ana	a
		cmp	a
		mov	b,c
		mvi	a,0x20
		lxi	hl, 0x0200
		inx	sp
		push	bc
		push	hl
		pop	psw
		push	af
		stax	bc
		ldax	de
		rst	7
		rst	3
; Comment
;		list	on
buff:	ds	20H		; buffer
		db		'A'
		db		0x20
		dw		stack
		end

