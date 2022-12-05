# Copyright 2020, The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#    1. Redistributions of source code must retain the above copyright notice,
#       this list of conditions and the following disclaimer.
#
#    2. Redistributions in binary form must reproduce the above copyright notice,
#       this list of conditions and the following disclaimer in the documentation
#       and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERSITY OF CALIFORNIA ''AS
# IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
# OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
# OF SUCH DAMAGE.
#
# The views and conclusions contained in the software and documentation are those
# of the authors and should not be interpreted as representing official policies,
# either expressed or implied, of The Regents of the University of California.

TOPLEVEL_LANG ?= vhdl
SIM ?= ghdl
WAVES ?= 0

ifeq ($(TOPLEVEL_LANG),vhdl)
    # ifneq ($(filter $(SIM),ghdl questa modelsim riviera activehdl),)
    #     # ghdl, questa, and aldec all use SIM_ARGS with '-g' for setting generics
    #     SIM_ARGS += -gDATA_WIDTH=$(DATA_WIDTH) -gA_ROWS=$(A_ROWS) -gB_COLUMNS=$(B_COLUMNS) -gA_COLUMNS_B_ROWS=$(A_COLUMNS_B_ROWS)
    # else ifneq ($(filter $(SIM),ius xcelium),)
    #     SIM_ARGS += -generic "matrix_multiplier:DATA_WIDTH=>$(DATA_WIDTH)" -generic "matrix_multiplier:A_ROWS=>$(A_ROWS)" -generic "matrix_multiplier:B_COLUMNS=>$(B_COLUMNS)" -generic "matrix_multiplier:A_COLUMNS_B_ROWS=>$(A_COLUMNS_B_ROWS)"
    # endif

    ifeq ($(SIM),ghdl)
        EXTRA_ARGS += --std=08
        SIM_ARGS += --wave=wave.ghw  --backtrace-severity=warning --assert-level=none --ieee-asserts=disable
    else ifneq ($(filter $(SIM),questa modelsim riviera activehdl),)
        COMPILE_ARGS += -2008
    endif
else
    $(error A valid value (verilog or vhdl) was not provided for TOPLEVEL_LANG=$(TOPLEVEL_LANG))
endif

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ps

DUT      = BlackwireReceive
TOPLEVEL = blackwirereceive #$(DUT)
MODULE   = test_$(DUT)
#VERILOG_SOURCES += ../../$(DUT).v
VHDL_SOURCES += ../../build/rtl/$(DUT).vhd

VHDL_SOURCES += ../../MaximVHDL/imports/project_1/ChaCha20.vhd
VHDL_SOURCES += ../../MaximVHDL/new/AEAD_ChaCha_Poly.vhd
VHDL_SOURCES += ../../MaximVHDL/imports/project_1/q_round.vhd
VHDL_SOURCES += ../../MaximVHDL/imports/project_1/diag_round.vhd
VHDL_SOURCES += ../../MaximVHDL/imports/project_1/col_round.vhd
VHDL_SOURCES += ../../MaximVHDL/imports/project_1/half_round.vhd
VHDL_SOURCES += ../../MaximVHDL/new/test_top_ChaCha.vhd
VHDL_SOURCES += ../../MaximVHDL/new/Poly1305.vhd
VHDL_SOURCES += ../../MaximVHDL/new/ChaCha20_128.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mul136_mod_red.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mul_red_pipeline.vhd
VHDL_SOURCES += ../../MaximVHDL/new/test_top_mod_red.vhd
VHDL_SOURCES += ../../MaximVHDL/new/ChaCha_int.vhd
VHDL_SOURCES += ../../MaximVHDL/new/r_power_n.vhd
VHDL_SOURCES += ../../MaximVHDL/mul_gen_0.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mul_36.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mul_72.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mul_144.vhd
VHDL_SOURCES += ../../MaximVHDL/new/mod_red_1305.vhd
VHDL_SOURCES += ../../MaximVHDL/new/Poly_1305_pipe_top.vhd
VHDL_SOURCES += ../../MaximVHDL/new/Poly_1305_pipe.vhd
VHDL_SOURCES += ../../MaximVHDL/new/AEAD_decryption_top.vhd
VHDL_SOURCES += ../../MaximVHDL/new/AEAD_top.vhd
VHDL_SOURCES += ../../MaximVHDL/new/Poly_pipe_top_test.vhd 
VHDL_SOURCES += ../../MaximVHDL/new/AEAD_decryption.vhd
VHDL_SOURCES += ../../MaximVHDL/AEAD_decryption_wrapper.vhd


IGNORE:=$(shell tunctl -d tap0)

include $(shell cocotb-config --makefiles)/Makefile.sim

iverilog_dump.v:
	echo 'module iverilog_dump();' > $@
	echo 'initial begin' >> $@
	echo '    $$dumpfile("$(TOPLEVEL).fst");' >> $@
	echo '    $$dumpvars(0, $(TOPLEVEL));' >> $@
	echo 'end' >> $@
	echo 'endmodule' >> $@

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst
	@rm -rf __pycache__
	@rm -rf results.xml
