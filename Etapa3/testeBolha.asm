		.data
valor:		.word 0x7

		.text
		.globl main

main:
		la	$t0, valor
		li	$t1, 0x3
		nop
		nop
		lw	$t1,0($t0)	
		addiu	$t2,$t1,1
fim:	
		j	fim