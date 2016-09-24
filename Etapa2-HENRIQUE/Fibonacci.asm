			.data
sequencia:	.word 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
size:		.word 19

			.text
			.globl main

main:
		lui			$at, 4097
		addiu		$t1,$zero,0			# inicia o $t0 e $t1 com os primeiros valores da sequencia
		addiu		$t2,$zero,1			# 
		ori			$s1,$at,80
		ori			$s0,$at,0
		nop
		lw			$s1,0($s1)			# recupera o vaor de size que eh a quantidade de numeros a ser gerados pelo fibonaccp
		sw			$t1,0($s0)			# salva o  primeiro valor da sequencia que eh 
		nop
loop:
		beq 		$s1,$zero,fim		# verifica quando acaba a sequeancia 
		nop
		nop
		
		addu		$t2,$t1,$t2			# faz a soma do ultimo valor com o valor atual
		addiu 		$s0,$s0,4			# passa para a proxima endereco do vetor size
		lw 			$t1,0($s0)			# recupera o ultimo valor da sequencia		
		j			loop
		sw			$t2,0($s0)			# grava o valor no vetor da memoria
		addiu 		$s1,$s1,-1
fim:	
		j			fim					# loop de fim do programa 
		nop
		nop
	
