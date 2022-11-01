# Ubuntu 20 (or similar)
# apt-get install verilator gtkwave

# Yosys is too old on Ubuntu 20
# build yourself

# Wavedrom: Use pre-built package

# sudo apt-get install libreadline-dev tcl-dev

# sudo apt-get install nodejs npm
# sudo npm install -g netlistsvg

.ONESHELL:

.PHONY: spinal clean simulate repl sim_repl

# continuous build (using sbt "~" REPL feature) on save in editor
repl:
	set -e
	sbt "~ \
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	runMain corundum.CorundumFrameFilterAxi4Verilog; \
	runMain corundum.CorundumFrameWriterAxi4Verilog; \
	runMain corundum.CorundumFrameReaderAxi4Verilog; \
	runMain corundum.AxisExtractHeaderVerilog \
	"

sim_repl:
	set -e
# run in background
	#gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst   -a ./CorundumFrameStash.gtkw   &
	#gtkwave -F -f ./simWorkspace/CorundumFrameMuxPrio/test.fst -a ./CorundumFrameMuxPrio.gtkw &
	#gtkwave -F -f ./simWorkspace/CorundumFrameFilter/test.fst -a ./CorundumFrameFilter.gtkw &
	gtkwave -F -f ./simWorkspace/AxisExtractHeader/test.fst -a ./CorundumFrameEthAxisRx.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.CorundumFrameMuxPrioSim; \
	test:runMain corundum.CorundumFrameStashSim; \
	test:runMain corundum.CorundumFrameFilterSim; \
	test:runMain corundum.AxisExtractHeaderSim; \
	"
	# @TODO can we kill gtkwave here?

sim_repl_eth:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/AxisExtractHeader/test.fst -a ./AxisExtractHeader.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.AxisExtractHeaderSim; \
	"

sim_repl_filter:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/CorundumFrameFilterAxi4/test.fst -a ./CorundumFrameFilterAxi4.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.CorundumFrameFilterAxi4Sim; \
	"

sim_writer:
	set -e
# run sim once, so that we have a wavefile to start with
	sbt "test:runMain corundum.CorundumFrameWriterSim"
# run in background
	#mkdir -p ./simWorkspace/CorundumFrameWriterDut
	#touch ./simWorkspace/CorundumFrameWriterDut/test.fst
	#./gtkwave_trigger_reload.sh ./simWorkspace/CorundumFrameWriterDut/test.fst &
	gtkwave -F -f ./simWorkspace/CorundumFrameWriterDut/test.fst   -a ./CorundumFrameWriterDut.gtkw   &
# continuous build/simulate on saved source code changes
# press Shift-Ctrl-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ test:runMain corundum.CorundumFrameWriterSim"
	#killall gtkwave_trigger_reload.sh

sim_reader:
	set -e
# run sim once, so that we have a wavefile to start with
	sbt "test:runMain corundum.CorundumFrameReaderSim"
	gtkwave -F -f ./simWorkspace/CorundumFrameReaderAxi4/test.fst   -a ./CorundumFrameWriterAxi4.gtkw   &
	sbt "~ test:runMain corundum.CorundumFrameReaderSim"

stash:
	set -e
	gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst -a ./CorundumFrameStash.gtkw &
	sbt "\
	~test:runMain corundum.CorundumFrameStashSim;"

spinal: src/main/scala/corundum/CorundumFrameMuxPrio.scala
	set -e
	sbt "runMain corundum.CorundumFrameMuxPrioVerilog"

# generate Verilog
rtl: src/main/scala/corundum/CorundumFrameMuxPrio.scala
rtl: src/main/scala/corundum/CorundumFrameStash.scala
rtl: src/main/scala/corundum/CorundumFrameFilter.scala
rtl: src/main/scala/corundum/CorundumFrameReader.scala
rtl: src/main/scala/corundum/CorundumFrameWriter.scala
rtl: src/main/scala/corundum/AxisExtractHeader.scala
	set -e
	sbt " \
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	runMain corundum.CorundumFrameWriterAxi4Verilog; \
	runMain corundum.CorundumFrameReaderAxi4Verilog; \
	runMain corundum.AxisExtractHeaderVerilog; \
	"

formal:
	set -e
	sbt "runMain corundum.CorundumFrameStashSystemVerilogWithFormal"
	sby -f CorundumFrameStash.sby task_proof -d formalWorkdir/CorundumFrameStash 
	sby -f CorundumFrameStash.sby task_cover -d formalWorkdir/CorundumFrameStash/cover 

CorundumFrameMuxPrio.json: CorundumFrameMuxPrio.v CorundumFrameMuxPrio.ys
	set -e
	yosys CorundumFrameMuxPrio.ys

CorundumFrameMuxPrio.svg: CorundumFrameMuxPrio.json
	set -e
	netlistsvg CorundumFrameMuxPrio.json -o CorundumFrameMuxPrio.svg &

simulate: src/main/scala/corundum/CorundumFrameMuxPrio.scala src/main/scala/corundum/CorundumFrameMuxPrioSim.scala
	set -e
	sbt "runMain corundum.CorundumFrameMuxPrioSim"
	gtkwave -f ./simWorkspace/CorundumFrameMuxPrio/test.vcd -a ./CorundumFrameMuxPrio.gtkw &

clean:
	rm -rf simWorkspace *.svg formalWorkdir

%.json: %.ys %.v
	set -e
	yosys $< -o $@

%.svg: %.json
	set -e
	netlistsvg $< -o $@

fix_gtkw:
	sed -i -e "s@$PWD@.@" -e "s@/home/vexriscv/project@.@" *.gtkw
