#!/usr/bin/env python
"""

Copyright 2020, The Regents of the University of California.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERSITY OF CALIFORNIA ''AS
IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF CALIFORNIA OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies,
either expressed or implied, of The Regents of the University of California.

"""

import itertools
import logging
import os
import binascii
import cocotb_test.simulator

# create_tap
import os
import subprocess
import getpass
import fcntl
import struct
import time

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge
from cocotb.regression import TestFactory
from cocotb.decorators import external

from cocotbext.axi import AxiStreamBus, AxiStreamFrame, AxiStreamSource, AxiStreamSink

# based on an old example in CocoTB (which was removed upstream as it was unmaintained)
# and an slightly updated fix mentioned in a CocoTB issue 
def close_tap(name="tap0"):
	cmd1='sudo tunctl -d %s' % name
	print(cmd1)
	subprocess.run(cmd1, shell=True)

# Linux host interface TAP IP is hardcoded as 192.168.255.2 here 
def create_tap(name="tap0", ip="192.168.255.1"):
	cocotb.log.info("Attempting to create interface %s (%s)" % (name, ip))
	TUNSETIFF = 0x400454ca
	TUNSETOWNER = TUNSETIFF + 2
	#IFF_TUN = 0x0001
	IFF_TAP= 0x0002
	IFF_NO_PI = 0x1000
	tun = open('/dev/net/tun', 'r+b', buffering=0)
	tun_num = int(name.split('tap')[-1])
	cmd1='sudo tunctl -u %s -t %s'% (getpass.getuser(), name)
	print(cmd1)
	subprocess.check_call(cmd1, shell=True)
	subprocess.check_call('sudo ip link set %s up' % (name), shell=True)
	subprocess.check_call('sudo ip addr add 192.168.255.2 peer %s dev %s' % (ip, name), shell=True)

	while True:
		try:
			name = 'tap{}'.format(tun_num)
			ifr = struct.pack('16sH', name.encode('utf-8'), IFF_TAP | IFF_NO_PI)
			print('ifr', ifr.hex())
			cocotb.log.info(name)
			fcntl.ioctl(tun, TUNSETIFF, ifr)
			break
		except IOError as e:
			# Errno 16 if tun device already exists, otherwise this
			# failed for different reason.
			if e.errno != 16:
				raise e

		tun_num += 1
	subprocess.check_call('sudo ip link set address aa:bb:cc:22:22:22 dev %s' % name, shell=True)
    # Prevent ICMP as first packets, forcibly set ARP cache
	subprocess.check_call('sudo arp -s %s aa:bb:cc:11:11:11' % ip, shell=True)

	fcntl.ioctl(tun, TUNSETOWNER, os.getuid())
	name = 'tap{}'.format(tun_num)
	cocotb.log.info("Created interface %s (%s)" % (name, ip))
	return tun,name

class TB(object):
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.fork(Clock(dut.clk, 4, units="ns").start())

        # connect TB source to DUT sink, and vice versa
        # byte_lanes = 16 is a workaround for bug https://github.com/alexforencich/cocotbext-axi/issues/46
        # in case no TKEEP[] signals are used
        self.source = AxiStreamSource(AxiStreamBus.from_prefix(dut, "sink"),     dut.clk, dut.reset) #, byte_lanes = 16)
        self.sink =   AxiStreamSink  (AxiStreamBus.from_prefix(dut, "source"  ), dut.clk, dut.reset) #, byte_lanes = 16)

        tap,tapname = create_tap()
        self.tap = tap
        self.tapname = tapname
        self.tapfd = tap.fileno()

    def set_idle_generator(self, generator=None):
        if generator:
            self.source.set_pause_generator(generator())

    async def reset(self):
        self.dut.reset.setimmediatevalue(0)
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.reset.value = 1
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.reset.value = 0
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)

    # https://docs.python.org/3/library/asyncio-task.html
#    async def tap():
#        task1 = asyncio.create_task(
#            tap2tb())
#
#        task2 = asyncio.create_task(
#            tb2tap())
#
#        print(f"started at {time.strftime('%X')}")
#
#        await task1
#        await task2
#        print(f"stopped at {time.strftime('%X')}")

    async def tapit(self):
        while True:
            # assume Ethernet frame on TB sink
            frame_tb2tap = True
            try:
                # poll on TB sink
                rx_frame = self.sink.recv_nowait()
            except:
                # No Ethernet frame on TB sink
                frame_tb2tap = False
                await RisingEdge(self.dut.clk)
            if (frame_tb2tap == True):
                # forward Ethernet frame from TB to TAP
                rx_pkt = bytes(rx_frame)
                self.log.info("tapit() passing Ethernet frame from TB to TAP: %s" % bytes(rx_pkt).hex())
                os.set_blocking(self.tapfd, True)
                packet = os.write(self.tapfd, rx_pkt)

            # assume Ethernet frame on TAP interface
            frame_tap2tb = True
            try:
                # poll on TAP interface 
                os.set_blocking(self.tapfd, False)
                packet = os.read(self.tapfd, 16 * 1024)
            except:
                frame_tap2tb = False
                await RisingEdge(self.dut.clk)
            if (frame_tap2tb == True):
                # forward Ethernet frame from TAP to TB
                tx_pkt = bytearray(packet)
                self.log.info("--- Passing packet received on TAP to TB: %s" % bytes(tx_pkt).hex())
                tx_frame = AxiStreamFrame(tx_pkt)
                self.dut.sink_length.value = len(tx_pkt)
                await self.source.send(tx_frame)

    @cocotb.function
    async def tap2tb(self):
        print("TAP2TB One")
        await time.sleep(5)
        print("TAP2TB Two")
        while False:
            self.log.info("Waiting for packets on TAP")
            packet = os.read(self.tapfd, 2048)
            test_pkt = bytearray(packet)
            self.log.info("Passing packet received on TAP to DUT: %s" % bytes(test_pkt))
            test_frame = AxiStreamFrame(test_pkt)
            self.dut.sink_length.value = len(test_pkt) #pkt_len
            await self.source.send(test_frame)

    @cocotb.function
    async def tb2tap(self):
        print("TB2TAP One")
        await time.sleep(1)
        print("TB2TAP Two")
        while False:
            err = False
            self.log.info("Waiting to receive packet on TB sink.")
            rx_frame = await self.sink.recv()
            print(rx_frame)
            rx_pkt = bytes(rx_frame)
            self.log.info("Passing packet received on TB to TAP: %s" % rx_pkt)
            packet = os.write(self.tapfd, rx_pkt, len(rx_pkt))

    @cocotb.external
    def call_tap2tb(self):
        return self.tap2tb()

    @cocotb.external
    def call_tb2tap(self):
        return self.tb2tap()

async def run_test(dut, payload_lengths=None, payload_data=None, header_lengths=None, idle_inserter=None):

    tb = TB(dut)

    await tb.reset()

    tb.set_idle_generator(idle_inserter)

    test_pkts = []
    test_frames = []

    #await tb.tapit()

    #t1 = cocotb.start_soon(external(tb.tap2tb()))
    #tb.log.info("started t1")
    #t2 = cocotb.start_soon(external(tb.tb2tap()))
    #tb.log.info("started t2")
    #await t1
    #await t2

    t1 = cocotb.start_soon(tb.tapit())
    tb.log.info("started t1")
    await t1

    while False:
        tb.log.info("Waiting for packets on TAP")
        packet = os.read(tb.tapfd, 2048)
        test_pkt = bytearray(packet)

        #tb.log.info(type('payload'))
        tb.log.info("Passing packet received on TAP to DUT: %s" % bytes(test_pkt))

        test_pkts.append(test_pkt)
        test_frame = AxiStreamFrame(test_pkt)
        test_frames.append(test_frame)

        tb.dut.sink_length.value = len(test_pkt) #pkt_len

        await tb.source.send(test_frame)

    #for test_pkt, test_frame in zip(test_pkts, test_frames):
        tb.log.info("Waiting to receive packet on our DUT sink.")
        rx_frame = await tb.sink.recv()
        rx_pkt = bytes(rx_frame)
        tb.log.info("RX packet: %s", repr(rx_pkt))
        # padded to 60 if the packet size is less than 60
        padded_pkt = test_pkt.ljust(60, b'\x00')
        #assert rx_frame.tdata == padded_pkt
        os.write(tb.tapfd, padded_pkt)


        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)
        await RisingEdge(dut.clk)


    assert tb.sink.empty()

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)

def cycle_pause():
    return itertools.cycle([1, 1, 1, 0])


def size_list():
    return list(range(1, 129))

def payload_size_list():
    #return list(range(14, 10))
    return list(range(20, 21))

def header_size_list():
    return list(range(1, 4))


def incrementing_payload(length):
    #return bytes(itertools.islice(itertools.cycle(range(1, 256)), length))
    return bytearray(itertools.islice(itertools.cycle(range(1, 256)), length))


if cocotb.SIM_NAME:

    factory = TestFactory(run_test)
    factory.add_option("payload_lengths", [payload_size_list])
    factory.add_option("payload_data", [incrementing_payload])
    factory.add_option("idle_inserter", [None]) #, cycle_pause
    factory.generate_tests()

    #factory = TestFactory(run_test_pad)
    #factory.add_option("payload_data", [incrementing_payload])
    #factory.add_option("idle_inserter", [None, cycle_pause])
    #factory.generate_tests()


# cocotb-test

tests_dir = os.path.dirname(__file__)
#rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', '..', 'rtl'))
rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', '..'))
#lib_dir = os.path.abspath(os.path.join(rtl_dir, '..', 'lib'))
#axi_rtl_dir = os.path.abspath(os.path.join(lib_dir, 'axi', 'rtl'))
#axis_rtl_dir = os.path.abspath(os.path.join(lib_dir, 'axis', 'rtl'))
#eth_rtl_dir = os.path.abspath(os.path.join(lib_dir, 'eth', 'rtl'))
#pcie_rtl_dir = os.path.abspath(os.path.join(lib_dir, 'pcie', 'rtl'))


def test_CorundumFrameMatchHeader(request):
    dut = "CorundumFrameMatchHeader"
    module = os.path.splitext(os.path.basename(__file__))[0]
    toplevel = dut

    verilog_sources = [
        os.path.join(rtl_dir, f"{dut}.v"),
    ]

    parameters = {}

    parameters['DATA_WIDTH'] = 16 * 8 #512
    # divide by 8?
    parameters['KEEP_WIDTH'] = parameters['DATA_WIDTH'] / 8

    extra_env = {f'PARAM_{k}': str(v) for k, v in parameters.items()}

    sim_build = os.path.join(tests_dir, "sim_build",
        request.node.name.replace('[', '-').replace(']', ''))

    cocotb_test.simulator.run(
        python_search=[tests_dir],
        verilog_sources=verilog_sources,
        toplevel=toplevel,
        module=module,
        parameters=parameters,
        sim_build=sim_build,
        extra_env=extra_env,
    )
