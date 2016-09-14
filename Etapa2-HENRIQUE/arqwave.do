onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /cpu_tb/cpu/ct/ck
add wave -noupdate /cpu_tb/cpu/ct/rst
add wave -noupdate /cpu_tb/cpu/ct/i
add wave -noupdate -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/pc
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/ck
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/rst
add wave -noupdate -group BI_DI -group BI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/D_incpc
add wave -noupdate -group BI_DI -group BI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/D_instruction
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/npc
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/ir
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rs
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rt
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/rd
add wave -noupdate -group BI_DI -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/ext
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 1 /cpu_tb/cpu/dp/Bi_Di/en
add wave -noupdate -group BI_DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Bi_Di/instruction
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/ck
add wave -noupdate -group DI/EX -radix hexadecimal /cpu_tb/cpu/dp/Di_Ex/rst
add wave -noupdate -group DI/EX -group DI -radix hexadecimal -radixshowbase 0 /cpu_tb/cpu/dp/Di_Ex/in_uins
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
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {613 ns} 0}
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
WaveRestoreZoom {600 ns} {641 ns}
