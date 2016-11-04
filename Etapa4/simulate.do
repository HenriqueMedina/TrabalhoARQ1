vlib work
vmap work work
vcom -reportprogress 300 -work work {mips_pipeline.vhd}
vcom -reportprogress 30 -work work {mrStd_tb.vhd}
vsim work.cpu_tb