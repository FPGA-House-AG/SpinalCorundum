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

# continuous build (using sbt ~)
repl:
	set -e
	sbt "~ \
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	runMain corundum.CorundumFrameWriterVerilog; \
	"

sim_repl:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst   -a ./CorundumFrameStash.gtkw   &
	gtkwave -F -f ./simWorkspace/CorundumFrameMuxPrio/test.fst -a ./CorundumFrameMuxPrio.gtkw &
	gtkwave -F -f ./simWorkspace/CorundumFrameFilter/test.fst -a ./CorundumFrameFilter.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ test:runMain corundum.CorundumFrameMuxPrioSim; \
	test:runMain corundum.CorundumFrameStashSim; \
	test:runMain corundum.CorundumFrameFilterSim;"
	# @TODO can we kill gtkwave here?

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
rtl: src/main/scala/corundum/CorundumFrameWriterSim.scala
	set -e
	sbt " \
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	runMain corundum.CorundumFrameWriterAxi4Verilog; \
	runMain corundum.CorundumFrameWriterDutVerilog; \
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
	rm -rf simWorkspace *.svg

%.json: %.ys %.v
	set -e
	yosys $< -o $@

%.svg: %.json
	set -e
	netlistsvg $< -o $@
