# Ubuntu 20 (or similar)
# apt-get install verilator gtkwave

# Yosys is too old on Ubuntu 20
# build yourself

# Wavedrom: Use pre-built package

# sudo apt-get install libreadline-dev tcl-dev

# sudo apt-get install nodejs npm
# sudo npm install -g netlistsvg

.ONESHELL:

.PHONY: spinal clean simulate

spinal: src/main/scala/mylib/MyTopLevel.scala
	set -e
	sbt "runMain mylib.MyTopLevelVerilog"
	yosys MyTopLevel.ys
	netlistsvg MyTopLevel.json -o MyTopLevel.svg &

simulate: src/main/scala/mylib/MyTopLevel.scala src/main/scala/mylib/MyTopLevelSim.scala
	sbt "runMain mylib.MyTopLevelSim"
	gtkwave simWorkspace/MyTopLevel/test.vcd &

clean:
	rm -rf simWorkspace MyTopLevel.svg