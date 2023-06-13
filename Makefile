# Ubuntu 20 (or similar)
# apt-get install verilator gtkwave

# Yosys is too old on Ubuntu 20
# build yourself

# Wavedrom: Use pre-built package

# sudo apt-get install libreadline-dev tcl-dev

# sudo apt-get install nodejs npm
# sudo npm install -g netlistsvg

# Run all commands of one make target in the same shell, by default
.ONESHELL:

.PHONY: spinal clean simulate repl sim_repl build rtl

build: rtl

# CI/CD runs "make test"
test: build formal sim_extract sim_counter code_analysis

code_analysis:
	grep -rne '.m2sPipe().s2mPipe()' src/main/scala && \
	echo "Check reverse use of .m2sPipe().s2mPipe()." || true

# continuous build (using sbt "~" REPL feature) on save in editor
repl:
	set -e
	sbt "~ \
	runMain corundum.CorundumFrameMuxPrioVerilog; \
	runMain corundum.CorundumFrameStashVerilog; \
	runMain corundum.CorundumFrameStashVhdl; \
	runMain corundum.CorundumFrameDropVerilog; \
	runMain corundum.CorundumFrameFilterVerilog; \
	runMain corundum.CorundumFrameMatchHeaderVerilog; \
	runMain corundum.CorundumFrameFilterAxi4Verilog; \
	runMain corundum.CorundumFrameWriterAxi4Verilog; \
	runMain corundum.CorundumFrameReaderAxi4Verilog; \
	runMain corundum.AxisExtractHeaderVerilog; \
	runMain corundum.AxisWidthAdapterVerilog; \
	"

sim_repl:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst   -a ./CorundumFrameStash.gtkw   &
	#gtkwave -F -f ./simWorkspace/CorundumFrameMuxPrio/test.fst -a ./CorundumFrameMuxPrio.gtkw &
	#gtkwave -F -f ./simWorkspace/CorundumFrameFilter/test.fst -a ./CorundumFrameFilter.gtkw &
	#gtkwave -F -f ./simWorkspace/AxisExtractHeader/test.fst -a ./CorundumFrameEthAxisRx.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.CorundumFrameMuxPrioSim; \
	test:runMain corundum.CorundumFrameStashSim; \
	test:runMain corundum.CorundumFrameFilterSim; \
	test:runMain corundum.AxisExtractHeaderSim; \
	"
	# @TODO can we kill gtkwave here?

sim_repl_header:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/AxisExtractHeader/test.fst -a ./AxisExtractHeader.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.AxisExtractHeaderSim; \
	"

sim_repl_width:
	set -e
# run in background
	gtkwave -F -f ./simWorkspace/AxisWidthAdapter/test.fst -a ./AxisWidthAdapter.gtkw &
# continuous build/simulate on saved source code changes
# press Shift-Alt-R in GTKWave to reload waveform after code change/save/compilation
	sbt "~ \
	test:runMain corundum.AxisWidthAdapterSim; \
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

# AxisExtractHeader and PreventReplay simulation verification test
sim_extract:
	set -e
	sbt "runMain corundum.AxisExtractHeaderSim"
	sbt "runMain corundum.PreventReplayRFC6479_MN"

# LookupCounter{,Axi4} simulation verification test
sim_counter:
	set -e
	sbt "runMain corundum.LookupCounterSim"
	sbt "runMain corundum.LookupCounterAxi4Sim"

stash:
	set -e
	gtkwave -F -f ./simWorkspace/CorundumFrameStash/test.fst -a ./CorundumFrameStash.gtkw &
	sbt "\
	~test:runMain corundum.CorundumFrameStashSim;"

spinal: src/main/scala/corundum/CorundumFrameMuxPrio.scala
	set -e
	sbt "runMain corundum.CorundumFrameMuxPrioVerilog"

# generate Verilog, VHDL and SystemVerilog RTL
rtl: src/main/scala/corundum/CorundumFrameMuxPrio.scala
rtl: src/main/scala/corundum/CorundumFrameDrop.scala
rtl: src/main/scala/corundum/CorundumFrameMatchHeader.scala
rtl: src/main/scala/corundum/CorundumFrameStash.scala
rtl: src/main/scala/corundum/CorundumFrameFilter.scala
rtl: src/main/scala/corundum/CorundumFrameReader.scala
rtl: src/main/scala/corundum/CorundumFrameWriter.scala
rtl: src/main/scala/corundum/AxisExtractHeader.scala
rtl: src/main/scala/corundum/AxisInsertHeader.scala
rtl: src/main/scala/corundum/AxisDownSizer.scala
rtl: src/main/scala/corundum/AxisUpSizer.scala
rtl: src/main/scala/corundum/AxisToCorundumFrame.scala
rtl: src/main/scala/corundum/AxisWireguardKeyLookup.scala
rtl: src/main/scala/corundum/LookupTable.scala
rtl: src/main/scala/corundum/PreventReplayMN.scala
	set -e
	sbt " \
	runMain corundum.CorundumFrameMuxPrio; \
	runMain corundum.CorundumFrameDrop; \
	runMain corundum.CorundumFrameMatchHeader; \
	runMain corundum.CorundumFrameMatchWireguard; \
	runMain corundum.CorundumFrameStash; \
	runMain corundum.CorundumFrameFilter; \
	runMain corundum.CorundumFrameWriterAxi4; \
	runMain corundum.CorundumFrameReaderAxi4; \
	runMain corundum.AxisExtractHeader; \
	runMain corundum.AxisInsertHeader; \
	runMain corundum.AxisDownSizer; \
	runMain corundum.AxisUpSizer; \
	runMain corundum.AxisToCorundumFrame; \
	runMain corundum.AxisWireguardKeyLookup; \
	runMain corundum.LookupTable; \
	runMain corundum.PreventReplayMN; \
	"

# formal verification.

# Old approach: first generate SystemVerilog RTL, then use the .sby
# file with SymbiYosys to test.
# New approach: Use SpinalFormal maybe, this uses SymbiYosys as a back-end.
formal: formal_stash formal_extract formal_upsizer

# Drop is known-broken, but unused anyway.
#formal: formal_drop 

formal_stash:
	set -e
	sby -h || . /home/vivado/oss-cad-suite/environment
	sby -h || false
	sbt "runMain corundum.CorundumFrameStash"
	sby -f CorundumFrameStash.sby task_proof -d formalWorkdir/CorundumFrameStash 
	sby -f CorundumFrameStash.sby task_cover -d formalWorkdir/CorundumFrameStash/cover 

# AxisExtractHeader formal verification tests
formal_extract:
	set -e
	sby -h || . /home/vivado/oss-cad-suite/environment
	sby -h || false
	sbt "runMain corundum.AxisExtractHeaderFormal"

formal_upsizer:
	set -e
	sby -h || . /home/vivado/oss-cad-suite/environment
	sby -h || false
	sbt "runMain corundum.AxisUpSizerFormal"

formal_drop:
# fail on first error
	set -e
# add SymbiYosys (sby) to PATH if not yet in PATH	
	sby -h || . /home/vivado/oss-cad-suite/environment
	sby -h
	sbt "runMain corundum.CorundumFrameDropFormal"

LookupTable_netlist:
	yosys -h || . /home/vivado/oss-cad-suite/environment
	yosys LookupTable.ys
	netlistsvg LookupTable.json -o LookupTable.svg
	xviewer LookupTable.svg

# The following two targets build an SVG diagram of the Priority Mux
# @TODO support these tools in our Docker image
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

# The paths in .gtkw files are absolute, not very handy
# make them relative 
fix_gtkw:
	sed -i -e "s@$(PWD)@.@" *.gtkw
	sed -i -e "s@./SpinalCorundum@.@" *.gtkw
