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
	sbt "~runMain corundum.MuxHighPrioFragmentStreamVerilog"

sim_repl:
	set -e
# run in background
	gtkwave -f ./simWorkspace/FragmentStash/test.vcd -a ./FragmentStash.gtkw &
	gtkwave -f ./simWorkspace/MuxHighPrioFragmentStream/test.vcd -a ./MuxHighPrioFragmentStream.gtkw &
# continuous build/simulate, press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~test:runMain corundum.MuxHighPrioFragmentStreamSim; test:runMain corundum.FragmentStashSim;"
	# @TODO can we kill gtkwave here?

spinal: src/main/scala/corundum/MuxHighPrioFragmentStream.scala
	set -e
	sbt "runMain corundum.MuxHighPrioFragmentStreamVerilog"

MuxHighPrioFragmentStream.json: MuxHighPrioFragmentStream.v MuxHighPrioFragmentStream.ys
	set -e
	yosys MuxHighPrioFragmentStream.ys

MuxHighPrioFragmentStream.svg: MuxHighPrioFragmentStream.json
	set -e
	netlistsvg MuxHighPrioFragmentStream.json -o MuxHighPrioFragmentStream.svg &

simulate: src/main/scala/corundum/MuxHighPrioFragmentStream.scala src/main/scala/corundum/MuxHighPrioFragmentStreamSim.scala
	set -e
	sbt "runMain corundum.MuxHighPrioFragmentStreamSim"
	gtkwave -f ./simWorkspace/MuxHighPrioFragmentStream/test.vcd -a ./MuxHighPrioFragmentStream.gtkw &

clean:
	rm -rf simWorkspace *.svg
