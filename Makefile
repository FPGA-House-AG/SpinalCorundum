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
	sbt "~ runMain corundum.CorundumFrameMuxPrioVerilog; runMain corundum.CorundumFrameStashVerilog"

sim_repl:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst   -a ./CorundumFrameStash.gtkw   &
	gtkwave -F -f ./simWorkspace/CorundumFrameMuxPrio/test.fst -a ./CorundumFrameMuxPrio.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ test:runMain corundum.CorundumFrameMuxPrioSim; \
	test:runMain corundum.CorundumFrameStashSim; \
	test:runMain corundum.CorundumFrameFilterSim;"
	# @TODO can we kill gtkwave here?

spinal: src/main/scala/corundum/CorundumFrameMuxPrio.scala
	set -e
	sbt "runMain corundum.CorundumFrameMuxPrioVerilog"

rtl: src/main/scala/corundum/CorundumFrameMuxPrio.scala
rtl: src/main/scala/corundum/CorundumFrameStash.scala
rtl: src/main/scala/corundum/CorundumFrameFilter.scala
	set -e
	sbt "
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	"


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
