			.data
sequencia:	.word 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0 0x0
size:		.word 20

			.text
			.globl main

main:
		la			$s0, sequencia		# carrega o endereco de sequencia
		nop								# nop entre lui e ori da para optimizar 
		nop
		la			$s1, size			# carrga o endereco de size
		nop
		nop
		lw			$s1,0($s1)			# recupera o vaor de size que eh a quantidade de numeros a ser gerados pelo fibonaccp
		nop
		nop
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
fim:	
		j			fim					# loop de fim do programa 