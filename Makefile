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
	sbt "~runMain corundum.MyTopLevelVerilog"

# continuous build/simulate, press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
sim_repl:
	set -e
# run in background
	gtkwave -f ./simWorkspace/MyTopLevel/test.vcd -a ./MyTopLevel.gtkw &
	sbt "~test:runMain corundum.MyTopLevelSim"

spinal: src/main/scala/corundum/MyTopLevel.scala
	set -e
	sbt "runMain corundum.MyTopLevelVerilog"

MyTopLevel.json: MyTopLevel.v MyTopLevel.ys
	set -e
	yosys MyTopLevel.ys

MyTopLevel.svg: MyTopLevel.json
	set -e
	netlistsvg MyTopLevel.json -o MyTopLevel.svg &

simulate: src/main/scala/corundum/MyTopLevel.scala src/main/scala/corundum/MyTopLevelSim.scala
	set -e
	sbt "runMain corundum.MyTopLevelSim"
	gtkwave -f ./simWorkspace/MyTopLevel/test.vcd -a ./MyTopLevel.gtkw &

clean:
	rm -rf simWorkspace MyTopLevel.svg
