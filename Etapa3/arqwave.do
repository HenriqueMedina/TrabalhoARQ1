onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /cpu_tb/cpu/ct/ck
add wave -noupdate /cpu_tb/cpu/ct/rst
add wave -noupdate /cpu_tb/cpu/ct/i
add wave -noupdate -radix hexadecimal -childformat {{/cpu_tb/cpu/dp/pc(31) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(30) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(29) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(28) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(27) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(26) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(25) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(24) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(23) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(22) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(21) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(20) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(19) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(18) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(17) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(16) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(15) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(14) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(13) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(12) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(11) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(10) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(9) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(8) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(7) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(6) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(5) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(4) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(3) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(2) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(1) -radix hexadecimal} {/cpu_tb/cpu/dp/pc(0) -radix hexadecimal}} -radixshowbase 0 -subitemconfig {/cpu_tb/cpu/dp/pc(31) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(30) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(29) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(28) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(27) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(26) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(25) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(24) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(23) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(22) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(21) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(20) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(19) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(18) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(17) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(16) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(15) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(14) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(13) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(12) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(11) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(10) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(9) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(8) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(7) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(6) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(5) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(4) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(3) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(2) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(1) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/pc(0) {-height 15 -radix hexadecimal -radixshowbase 0}} /cpu_tb/cpu/dp/pc
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/ck
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/rst
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/rst_jump
add wave -noupdate -group BI_DI -group BI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/D_incpc
add wave -noupdate -group BI_DI -group BI -radix hexadecimal -childformat {{/cpu_tb/cpu/dp/Bi_Di/D_instruction(31) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(30) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(29) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(28) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(27) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(26) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(25) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(24) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(23) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(22) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(21) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(20) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(19) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(18) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(17) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(16) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(15) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(14) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(13) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(12) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(11) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(10) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(9) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(8) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(7) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(6) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(5) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(4) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(3) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(2) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(1) -radix hexadecimal} {/cpu_tb/cpu/dp/Bi_Di/D_instruction(0) -radix hexadecimal}} -radixshowbase 0 -subitemconfig {/cpu_tb/cpu/dp/Bi_Di/D_instruction(31) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(30) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(29) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(28) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(27) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(26) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(25) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(24) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(23) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(22) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(21) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(20) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(19) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(18) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(17) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(16) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(15) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(14) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(13) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(12) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(11) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(10) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(9) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(8) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(7) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(6) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(5) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(4) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(3) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(2) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(1) {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Bi_Di/D_instruction(0) {-height 15 -radix hexadecimal -radixshowbase 0}} /cpu_tb/cpu/dp/Bi_Di/D_instruction
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/npc
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/ir
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rs
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rt
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rd
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/ext
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/en
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/ck
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/rst
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/rst_jump
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -childformat {{/cpu_tb/cpu/dp/Di_Ex/in_uins.wreg -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.ce -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.rw -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.bw -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.inst_grupo1 -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.inst_branch -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.inst_grupoI -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.RegDst -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.i -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.ini_mult -radix hexadecimal} {/cpu_tb/cpu/dp/Di_Ex/in_uins.ini_div -radix hexadecimal}} -radixshowbase 0 -subitemconfig {/cpu_tb/cpu/dp/Di_Ex/in_uins.wreg {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.ce {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.rw {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.bw {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.inst_grupo1 {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.inst_branch {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.inst_grupoI {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.RegDst {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.i {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.ini_mult {-height 15 -radix hexadecimal -radixshowbase 0} /cpu_tb/cpu/dp/Di_Ex/in_uins.ini_div {-height 15 -radix hexadecimal -radixshowbase 0}} /cpu_tb/cpu/dp/Di_Ex/in_uins
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_incpc
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_R1
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_R2
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_ext16
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_shift2
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_aD_jump
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_ext_zero
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_rd
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/D_rt
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/uins_EX
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/npc
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/RA
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/RB
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/Q_ext16
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/Q_shift2
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/Q_aD_jump
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/Q_ext_zero
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/rd
add wave -noupdate -group DI/EX -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/rt
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/en
add wave -noupdate -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/IMED
add wave -noupdate -group ula -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/inst_alu/op1
add wave -noupdate -group ula -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/inst_alu/op2
add wave -noupdate -group ula -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/inst_alu/outalu
add wave -noupdate -group ula -radix hexadecimal /cpu_tb/cpu/dp/inst_alu/op_alu
add wave -noupdate -group ula -radix hexadecimal /cpu_tb/cpu/dp/inst_alu/menorU
add wave -noupdate -group ula -radix hexadecimal /cpu_tb/cpu/dp/inst_alu/menorS
add wave -noupdate -group EX/MEM -radix hexadecimal /cpu_tb/cpu/dp/Ex_Mem/ck
add wave -noupdate -group EX/MEM -radix hexadecimal /cpu_tb/cpu/dp/Ex_Mem/rst
add wave -noupdate -group EX/MEM -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/in_uins
add wave -noupdate -group EX/MEM -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/D_outAlu
add wave -noupdate -group EX/MEM -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/D_EscMem
add wave -noupdate -group EX/MEM -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/D_npc
add wave -noupdate -group EX/MEM -group EX -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/D_rd
add wave -noupdate -group EX/MEM -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/uins_Mem
add wave -noupdate -group EX/MEM -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/RALU
add wave -noupdate -group EX/MEM -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/EscMem
add wave -noupdate -group EX/MEM -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/npc
add wave -noupdate -group EX/MEM -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/rd
add wave -noupdate -group EX/MEM -radix hexadecimal /cpu_tb/cpu/dp/Ex_Mem/en
add wave -noupdate -group mem -radix hexadecimal /cpu_tb/cpu/ce
add wave -noupdate -group mem -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/data
add wave -noupdate -group mem -radix hexadecimal /cpu_tb/cpu/rw
add wave -noupdate -group mem -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/d_address
add wave -noupdate -group mem -radix hexadecimal /cpu_tb/cpu/bw
add wave -noupdate -group MEM/ER -radix hexadecimal /cpu_tb/cpu/dp/Mem_ER/ck
add wave -noupdate -group MEM/ER -radix hexadecimal /cpu_tb/cpu/dp/Mem_ER/rst
add wave -noupdate -group MEM/ER -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/in_uins
add wave -noupdate -group MEM/ER -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/D_RALU
add wave -noupdate -group MEM/ER -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/D_MDR
add wave -noupdate -group MEM/ER -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/D_npc
add wave -noupdate -group MEM/ER -group MEM -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/D_rd
add wave -noupdate -group MEM/ER -group ER -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/uins_ER
add wave -noupdate -group MEM/ER -group ER -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/RALU_ER
add wave -noupdate -group MEM/ER -group ER -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/npc
add wave -noupdate -group MEM/ER -group ER -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/MDR
add wave -noupdate -group MEM/ER -group ER -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/rd
add wave -noupdate -group MEM/ER -radix hexadecimal /cpu_tb/cpu/dp/Mem_ER/en
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/ck
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/rst
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/wreg
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/AdRs
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/AdRt
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/adRd
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/RD
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/R1
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/R2
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/reg
add wave -noupdate -group bank_reg /cpu_tb/cpu/dp/REGS/wen
add wave -noupdate -group PipelineView -label BI_inst -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/D_instruction
add wave -noupdate -group PipelineView -label DI_inst -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/in_uins.i
add wave -noupdate -group PipelineView -label EX_inst -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/uins_EX.i
add wave -noupdate -group PipelineView -label MEM_inst -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Ex_Mem/uins_Mem.i
add wave -noupdate -group PipelineView -label ER_inst -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Mem_ER/uins_ER.i
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {40 ns} 0}
quietly wave cursor active 1
configure wave -namecolwidth 151
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ns
update
WaveRestoreZoom {0 ns} {162 ns}
