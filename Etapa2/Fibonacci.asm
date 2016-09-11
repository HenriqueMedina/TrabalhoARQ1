			.data
sequencia:	.word 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
size:		.word 20

			.text
			.globl main

main:
		lui			$at, 4097
		nop
		nop
		ori			$s0,$at,0
		lui			$at, 4097
		nop
		nop
		ori			$s1,$at,80
		nop
		nop
		lw			$s1,0($s1)			# recupera o vaor de size que eh a quantidade de numeros a ser gerados pelo fibonaccp
		addiu		$t1,$zero,0			# inicia o $t0 e $t1 com os primeiros valores da sequencia
		addiu		$t2,$zero,1			# 
		nop
		sw			$t1,0($s0)			# salva o  primeiro valor da sequencia que eh 0
loop:
		addiu 		$s1,$s1,-1			# decrementa o size 
		nop
		nop
		beq 		$s1,$zero,fim		# verifica quando acaba a sequeancia 
		nop
		nop
		nop
		addu		$t2,$t1,$t2			# faz a soma do ultimo valor com o valor atual
		lw 			$t1,0($s0)			# recupera o ultimo valor da sequencia
		addiu 		$s0,$s0,4			# passa para a proxima endereco do vetor size
		nop
		nop
		sw			$t2,0($s0)			# grava o valor no vetor da memoria
		j			loop				# pula para o loop
		nop
		nop
		nop
fim:	
		j			fim					# loop de fim do programa 
		nop
		nop
		nop