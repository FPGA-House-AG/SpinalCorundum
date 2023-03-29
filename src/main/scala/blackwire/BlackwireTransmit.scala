package blackwire

import spinal.core._
import spinal.lib._

import spinal.lib.bus.misc._
import spinal.lib.bus.amba4.axi._

import scala.math._

import corundum._

// companion object
object BlackwireTransmit {
  val busconfig = Axi4Config(15, 32, 2, useLock = false, useQos = false, useRegion = false)
  def main(args: Array[String]) : Unit = {
    val vhdlReport = Config.spinal.generateVhdl(new BlackwireTransmit(busconfig))
    val verilogReport = Config.spinal.generateVerilog(new BlackwireTransmit(busconfig))
    //vhdlReport.mergeRTLSource("merge")
  }
}

// composition of the TX data flow towards ChaCha20-Poly1305
// stash -> downsizer -> key lookup ->
case class BlackwireTransmit(busCfg : Axi4Config, include_chacha : Boolean = true, has_busctrl : Boolean = false) extends Component {
  final val corundumDataWidth = 512
  final val cryptoDataWidth = 128
  final val maxPacketLength = 1534
  /* maximum number of peers */
  final val peer_num = 256
  final val keys_num = 4/*sessions per peers*/ * peer_num

  // 1534 rounded up 2048/(512/8) == 32

  final val txkey_addr_width = LookupTableAxi4.slave_width(256, keys_num, busCfg)
  final val p2s_addr_width = LookupTableAxi4.slave_width(32, peer_num, busCfg)
  println("txkey_addr_width = " + txkey_addr_width)

  val txkeySlaveCfg = busCfg.copy(addressWidth = txkey_addr_width)
  val p2sSlaveCfg = busCfg.copy(addressWidth = p2s_addr_width)

  val io = new Bundle {
    // from PCIe
    val sink = slave Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
    // to CMAC
    val source = master Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
    // from RISC-V
    val sink_handshake = slave Stream Fragment(CorundumFrame(corundumDataWidth))
    // from RISC-V
    val ctrl_txkey = (has_busctrl) generate slave(Axi4(txkeySlaveCfg))
    // from RISC-V
    val ctrl_p2s = (has_busctrl) generate slave(Axi4(p2sSlaveCfg))
    // to PCIe
    val cpl_source = master(Stream(Bits(16 bits)))
    // from CMAC
    val cpl_sink = slave(Stream(Bits(16 bits)))
  }

  // stash store-and-forward, so that packets are consecutive in the lookup flow part
  val s = Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
  val in_stash = CorundumFrameStash(corundumDataWidth, userWidth = 17, fifoSize = 32)
  val mid_stash_too_full = Bool()
  in_stash.io.sink << io.sink
  s << in_stash.io.source

  val halt_input_to_lookup = RegInit(False).setWhen(s.lastFire && mid_stash_too_full).clearWhen(!mid_stash_too_full)

  val ping_pong_drop = Reg(Bool()).init(False)
  // set tuser(0) drop flag if this is not IPv4 20-byte header
  val i = Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
  val matcher = CorundumFrameMatchWireguard(corundumDataWidth, userWidth = 17)
  matcher.io.sink << s.haltWhen(halt_input_to_lookup)
  i <-< matcher.io.source
  val drop = RegNextWhen(!matcher.io.is_ipv4l5, i.ready)
  // @TODO Make True in production, False to easily test dropping every other frame
  when (True) {
    i.payload.fragment.tuser(0) := drop
  } otherwise {
    i.payload.fragment.tuser(0) := ping_pong_drop
  }
  when (i.lastFire) {
    ping_pong_drop := !ping_pong_drop
  }

  // e is i, but Ethernet header is replaced by Wireguard Type 4 like header,
  // which is 2 bytes longer, thus can add back-pressure which is no problem here.
  val e = Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
  val e2w_hdr = CorundumFrameInsertHeader(corundumDataWidth, userWidth = 17, 16 - 14)
  e2w_hdr.io.header := 0
  e2w_hdr.io.sink << i
  e << e2w_hdr.io.source

  // lookup peer and session index and if not found; set tuser(0) to drop.
  val a = Stream Fragment(CorundumFrame(corundumDataWidth, userWidth = 17))
  a << e

  // generate a stream of tags for dropped input packets
  // tuser(0) must be set during complete packet, taken on last beat!
  val cpl_drop = Stream(Bits(16 bits))
  // valid on drop flag
  cpl_drop.valid := a.lastFire & a.fragment.tuser(0)
  cpl_drop.payload := a.fragment.tuser(16 downto 1)
  cpl_drop.addAttribute("mark_debug")

  // merge tags for dropped and transmitted packets
  val cpl_arbiter = StreamArbiterFactory().roundRobin.build(Bits(16 bits), 2)
  cpl_arbiter.io.inputs(0) << io.cpl_sink
  cpl_arbiter.io.inputs(1) << cpl_drop
  // output merged completions tags towards PCIe
  io.cpl_source << cpl_arbiter.io.output
  io.cpl_source.addAttribute("mark_debug")

  // x is TDATA+TKEEP Ethernet frame from Corundum
  val x = Stream Fragment(CorundumFrame(corundumDataWidth))
  // x << a, but reduce tuser[16:0] to tuser[0]
  x.valid := a.valid
  x.last := a.last
  x.fragment.tdata := a.fragment.tdata
  x.fragment.tkeep := a.fragment.tkeep
  x.fragment.tuser(0) := a.fragment.tuser(0)
  a.ready := x.ready
  // put x TX tags only for undropped packets in separate stream
  val x_tags = Stream(Bits(16 bits))
  x_tags.payload := a.fragment.tuser(16 downto 1)
  x_tags.valid := x.lastFire & !x.tuser(0)
  // queue the forward completions towards our source
  val out_tags = x_tags.queue(1024)

  val w = Stream Fragment(CorundumFrame(corundumDataWidth))
  w << x.stage().stage()

  // peer index to session index (adds prev, current, next) for x to w
  val session = Bits(2 bits)
  (!has_busctrl) generate new Area {
    val p2s_lut = LookupTable(2/*bits*/, peer_num)
    p2s_lut.mem.initBigInt(Seq.tabulate(peer_num)(n => BigInt(n % 3)))
    p2s_lut.io.portA.en := True
    p2s_lut.io.portA.wr := False
    p2s_lut.io.portA.wrData := 0
    p2s_lut.io.portA.addr := U(x.payload.tdata(7 downto 0))
    p2s_lut.io.portB.en := True
    p2s_lut.io.portB.wr := False
    p2s_lut.io.portB.wrData := 0
    p2s_lut.io.portB.addr := 0
    session := p2s_lut.io.portA.rdData
  }
  // Peer to Session lookup and update via bus controller
  (has_busctrl) generate new Area {
    val p2s_lut = LookupTableAxi4(2/*bits*/, peer_num, busCfg)
    p2s_lut.mem.mem.initBigInt(Seq.tabulate(peer_num)(n => BigInt(n % 3)))
    p2s_lut.io.en := True
    p2s_lut.io.wr := False
    p2s_lut.io.wrData := 0
    val p2s_lut_address = U(x.payload.tdata(7 downto 0))
    p2s_lut.io.addr := p2s_lut_address
    io.ctrl_p2s >> p2s_lut.io.ctrlbus
    session := p2s_lut.io.rdData
  }

  val v = Stream Fragment(CorundumFrame(corundumDataWidth))
  v << w.stage().stage()

  // lookup nonce for w, ready on v
  val nonce_lookup = LookupCounter(64, peer_num * 4, 0, initRAM = true)
  nonce_lookup.io.increment := w.firstFire
  nonce_lookup.io.clear := False
  nonce_lookup.io.address := U(session).resized
  val nonce = nonce_lookup.io.counter

  // @TODO put peer index, session index into right position/order
  when (v.isFirst) {
    // 0S PP 00 00 - 40 41 42 43 44 45 46 47 - 00 00 00 04
    v.payload.fragment.tdata(127 downto 0) := nonce ## 
      B("6'x00") ## Delay(session, 2).resize(2) ## B("24'xAABBCC"/*peer_index*/) ##
      B("24'x000000") ## B("8'x04")
  }

  // lookup TX key for non-dropped packets only
  val txkey = Bits(256 bits)
  val txkey_lookup = v.isFirst & !v.payload.fragment.tuser(0)
  val txkey_lut_address = U(v.payload.fragment.tdata(63 downto 32).subdivideIn(4 slices).reverse.asBits.resize(log2Up(keys_num)))

  val key_fifo = StreamFifo(Bits(256 bits), 8)

  (!has_busctrl) generate new Area {
    val txkey_lut = LookupTable(256/*bits*/, keys_num)
    txkey_lut.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))
    txkey_lut.io.portA.en := txkey_lookup
    txkey_lut.io.portA.wr := False
    txkey_lut.io.portA.wrData := 0
    txkey_lut.io.portA.addr := txkey_lut_address
    txkey_lut.io.portB.en := False
    txkey_lut.io.portB.wr := False
    txkey_lut.io.portB.wrData := 0
    txkey_lut.io.portB.addr := 0
    txkey := txkey_lut.io.portA.rdData
  }
  // TX key lookup and update via bus controller
  (has_busctrl) generate new Area {
    val txkey_lut = LookupTableAxi4(256/*bits*/, keys_num, busCfg)
    txkey_lut.mem.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))
    txkey_lut.io.en := txkey_lookup
    txkey_lut.io.wr := False
    txkey_lut.io.wrData := 0
    txkey_lut.io.addr := txkey_lut_address
    txkey := txkey_lut.io.rdData
    io.ctrl_txkey >> txkey_lut.io.ctrlbus
  }
  // push looked-up TX keys into FIFO
  key_fifo.io.push.valid := Delay(txkey_lookup, cycleCount = 2, init = False)
  key_fifo.io.push.payload := txkey

  // should be room for 1534 + latency of ChaCha20 to FlowStash
  // Flow goes ready after packet last, and room for 26*64=1664 bytes
  val mid_stash = CorundumFrameFlowStash(corundumDataWidth, fifoSize = 32, 24)
  mid_stash.io.sink << v
  mid_stash_too_full := !mid_stash.io.sink.ready

  // y is stash output but in TDATA+length format
  val y = Stream(Fragment(Bits(corundumDataWidth bits)))
  val fff = Fragment(Bits(corundumDataWidth bits))
  fff.last := mid_stash.io.source.payload.last
  fff.fragment := mid_stash.io.source.payload.fragment.tdata
  y << mid_stash.io.source.translateWith(fff)
  // multiple of 16-bytes padded length (in bytes) but remove first header beat
  val y_length = ((mid_stash.io.length + 15) >> 4) << 4
  val y_hdr_length = (((mid_stash.io.length + 15) >> 4) - 1) << 4
  // insert plaintext length into header
  when (y.isFirst) {
    y.payload.fragment(31 downto 8) := y_hdr_length.resize(24).asBits.subdivideIn(3 slices).reverse.asBits
  }
  // x w v y



  // d is the Type 4 plaintext packet in 128 bits
  val d = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val downsizer = AxisDownSizer(corundumDataWidth, cryptoDataWidth)
  downsizer.io.sink <-< y
  downsizer.io.sink_length := RegNextWhen(y_length, y.ready)
  d <-< downsizer.io.source
  val d_length = RegNextWhen(downsizer.io.source_length, d.ready)

  // ehl is the encrypted Type 4 payload but with the length determined from the IP header
  val ehl = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val ehl_length = UInt(12 bits)//Reg(UInt(12 bits))

  // feedback signal
  val flow0_stash_too_full = Bool()
  // halt only after packet boundaries, start anywhere
  val halt_input_to_chacha0 = RegInit(False).setWhen(d.lastFire && flow0_stash_too_full).clearWhen(!flow0_stash_too_full)

  val with_chacha = (include_chacha) generate new Area {
    //halt_input_to_chacha.addAttribute("mark_debug")
    //d.last.addAttribute("mark_debug")
    //output_stash_too_full.addAttribute("mark_debug")

    // en is the encrypted Type 4 payload
    val en = Stream(Fragment(Bits(cryptoDataWidth bits)))
    val encrypt = ChaCha20Poly1305EncryptSpinal()
    encrypt.io.sink << d.haltWhen(halt_input_to_chacha0)
    encrypt.io.key := key_fifo.io.pop.payload
    key_fifo.io.pop.ready := en.lastFire
    en << encrypt.io.source

    // eh is the encrypted Type 4 payload with the WireGuard Type 4 header
    val eh = Stream(Fragment(Bits(cryptoDataWidth bits)))
    val eh_length = UInt(12 bits)//Reg(UInt(12 bits))
    eh <-< en
    // write WireGuard Type 4 header in front of encrypted plaintext
    when (en.isFirst & en.fire) {
      eh.payload.fragment := encrypt.io.header_out(127 downto 32) ## B("32'x00000004")
      eh.valid := True
    }
    // add length for WireGuard Type 4 header itself and the Poly1305 tag
    val wgt4_tagged_len = (1 + encrypt.io.header_out(8, 24 bits).subdivideIn(3 slices).reverse.asBits.resize(8).asUInt + 1) << 4
    eh_length := wgt4_tagged_len

    ehl <-< eh
    ehl_length := RegNextWhen(eh_length, ehl.ready)
  }
  val without_chacha = (!include_chacha) generate new Area { 
    ehl << d.haltWhen(halt_input_to_chacha0)
    ehl.payload.fragment(31 downto 0) := B("32'x00000004")
    ehl_length := d_length
  }

  // ehl << eh << en << d

  // us is the decrypted Type 4 payload but in 512 bits
  val us = Stream(Fragment(Bits(corundumDataWidth bits)))
  val upsizer = AxisUpSizer(cryptoDataWidth, corundumDataWidth)
  // @NOTE consider pipeline stage
  upsizer.io.sink << ehl
  upsizer.io.sink_length := ehl_length
  upsizer.io.sink_drop := False
  us << upsizer.io.source
  val us_length = upsizer.io.source_length
  val us_drop = upsizer.io.source_drop

  // c is y but in 512 bits in Corundum format
  val c = Stream Fragment(CorundumFrame(corundumDataWidth))
  val corundum = AxisToCorundumFrame(corundumDataWidth)
  // @NOTE consider pipeline stage
  corundum.io.sink << us
  corundum.io.sink_length := us_length
  corundum.io.sink_drop := False//y_drop
  c << corundum.io.source

  val c2 = Stream(Fragment(CorundumFrame(corundumDataWidth)))

  val flow0 = CorundumFrameFlowStash(corundumDataWidth, fifoSize = 32, 26)
  flow0.io.sink << c
  c2 << flow0.io.source
  val c2_length = flow0.io.length
  flow0_stash_too_full := !flow0.io.sink.ready

  // full Ethernet packet (c2 with Ethernet, IP and UDP)
  val f = Stream(Fragment(CorundumFrame(corundumDataWidth)))
  
  // add Ethernet, IPv4 and UDP header
  val outhdr = CorundumFrameInsertHeader(corundumDataWidth, userWidth = 1, headerWidthBytes = 14 + 20 + 8)
  outhdr.io.sink << c2
  // (14 + 20 + 8) * 8 = 336 bits
  //outhdr.io.header := B("336'x0")
  // aa:bb:cc:22:22:22 to 00:0a:35:06:a3:be protocol 0x0800
  // 0x45
  val eth_ip_udp_hdr = Bits((14 + 20 + 8) * 8 bits)
  val ip_hdr = Bits(20 * 8 bits)
  // 0x45 IPv4 20-byte IP header, 0x11 UDP protocol, c0a80132 to c0a8011e (192.168.1 .50 to .30)
  ip_hdr := B("16'x4500") ## B(20/*IP hdr*/ + 8/*UDP hdr*/ + c2_length, 16 bits) ## B("32'x0") ## B("32'x08110000") ## B("32'xc0a80132") ## B("32'xc0a8011e")
  
  val ip_chk = UInt(20 bits)
  ip_chk := U(ip_hdr(  7 downto   0) ## ip_hdr( 15 downto   8)).resize(20) +
            U(ip_hdr( 23 downto  16) ## ip_hdr( 31 downto  24)).resize(20) +
            U(ip_hdr( 39 downto  32) ## ip_hdr( 47 downto  40)).resize(20) +
            U(ip_hdr( 55 downto  48) ## ip_hdr( 63 downto  56)).resize(20) +
            U(ip_hdr( 87 downto  80) ## ip_hdr( 95 downto  88)).resize(20) +
            U(ip_hdr(103 downto  96) ## ip_hdr(111 downto 104)).resize(20) +
            U(ip_hdr(119 downto 112) ## ip_hdr(127 downto 120)).resize(20) +
            U(ip_hdr(135 downto 128) ## ip_hdr(143 downto 136)).resize(20) +
            U(ip_hdr(151 downto 144) ## ip_hdr(159 downto 152)).resize(20)
  val ip_chk2 = UInt(16 bits)
  /* add carries */
  ip_chk2 := (ip_chk >> 16).resize(16) + (ip_chk & 0x0ffff).resize(16)
  
  val ip_hdr2 = ip_hdr(159 downto 80) ## ~ip_chk2.asBits.resize(16) ## ip_hdr(63 downto 0)
  val udp_hdr = Bits(8 * 8 bits)
  // 0x15b3 == 5555 to port 0x159a == 5530
  udp_hdr := B("16'x15b3") ## B("16'x159a") ## B(8/*UDP hdr*/ + c2_length, 16 bits) ## B("16'x0"/*checksum==unused*/)

  eth_ip_udp_hdr := B("112'xaabbcc222222000a3506a3be0800") ## ip_hdr2 ## udp_hdr


  outhdr.io.header := eth_ip_udp_hdr.subdivideIn((14 + 20 + 8) slices).reverse.asBits()
  f << outhdr.io.source

  // t is f, but with TX tags re-addedin tuser[16:1], those were split-off early in TX path
  val t = Stream(Fragment(CorundumFrame(corundumDataWidth, userWidth = 17)))
  t.valid := f.valid
  t.last := f.last
  t.fragment.tdata := f.fragment.tdata
  t.fragment.tkeep := f.fragment.tkeep
  t.fragment.tuser(0) := False
  // put tag back
  t.fragment.tuser(16 downto 1) := out_tags.payload
  f.ready := t.ready

  // pop tag from queue
  out_tags.ready := t.lastFire

  val flow = CorundumFrameFlowStash(corundumDataWidth, fifoSize = 32, 26)
  flow.io.sink << io.sink_handshake

  // untagged handshake packets from flow
  val u = Stream(Fragment(CorundumFrame(corundumDataWidth, userWidth = 17)))
  u.valid := flow.io.source.valid
  u.last := flow.io.source.last
  u.fragment.tdata := flow.io.source.fragment.tdata
  u.fragment.tkeep := flow.io.source.fragment.tkeep
  u.fragment.tuser := 0
  flow.io.source.ready := u.ready

  val mux = CorundumFrameMuxPrio(corundumDataWidth, userWidth = 17)
  mux.io.sink0 << u
  mux.io.sink1 << t
  io.source << mux.io.source

  t.addAttribute("mark_debug").addAttribute("keep")
  u.addAttribute("mark_debug").addAttribute("keep")
  mux.io.source.addAttribute("mark_debug").addAttribute("keep")


/*

  // go through stash
  val stash = CorundumFrameStash(corundumDataWidth, 32)
  stash.io.sink << x

  // y is stash output but in TDATA+length format
  val y = Stream(Fragment(Bits(corundumDataWidth bits)))
  val fff = Fragment(Bits(corundumDataWidth bits))
  fff.last := stash.io.source.payload.last
  fff.fragment := stash.io.source.payload.fragment.tdata
  y << stash.io.source.translateWith(fff)
  val y_length = stash.io.length


  val w = Stream(Fragment(Bits(corundumDataWidth bits)))

  // w is with Ethernet header removed, thus the Type 4 payload
  val headers = AxisExtractHeader(corundumDataWidth, 14)
  headers.io.sink << y
  headers.io.sink_length := y_length
  w << headers.io.source
  val w_length = headers.io.source_length

  // allowed ip lookup
  // drop packet if not found

  // lookup session
  val p2s_lut = LookupTableAxi4(4/*bits*/, peer_num, busCfg)
  p2s_lut.mem.mem.initBigInt(Seq.fill(peer_num)(BigInt("0", 16)))
  p2s_lut.io.en := True
  p2s_lut.io.wr := False
  p2s_lut.io.wrData := 0
  p2s_lut.io.addr := 0 /*peer index hardcoded for now*/
  val session_index = B("8'x00") ## lut.io.rdData

  // z is the Type 4 packet in 128 bits
  val z = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val downsizer = AxisDownSizer(corundumDataWidth, cryptoDataWidth)
  downsizer.io.sink << w
  downsizer.io.sink_length := w_length
  z << downsizer.io.source
  val z_length = downsizer.io.source_length

  // upstream back pressure feedback across ChaCha20-Poly1305
  val output_stash_too_full = Bool()

  // k is the Type 4 packet in 128 bits together with key during first beat (Type 4 header)
  val txkey = AxisWireguardKeyLookup(cryptoDataWidth, has_internal_test_lut = false)
  txkey.io.sink << z
  txkey.io.sink_length := z_length

  val k = Stream(Fragment(Bits(cryptoDataWidth bits)))
  k << txkey.io.source
  val key = txkey.io.key_out
  val k_length = txkey.io.source_length
 
  // s is the decrypted Type 4 payload but with the length determined from the IP header
  val s = Stream(Fragment(Bits(cryptoDataWidth bits)))
  val s_length = Reg(UInt(12 bits))
  val s_drop = Reg(Bool()) init(False)

  val with_chacha = (include_chacha) generate new Area { 
    // p is the decrypted Type 4 payload
    val p = Stream(Fragment(Bits(cryptoDataWidth bits)))
    val decrypt = ChaCha20Poly1305DecryptSpinal()
    decrypt.io.sink << k.haltWhen(output_stash_too_full)
    decrypt.io.key := key
    p << decrypt.io.source
    //decrypt.io.addAttribute("mark_debug")

    // from the first word, extract the IPv4 Total Length field to determine packet length
    when (p.isFirst) {
      s_length.assignFromBits(p.payload.fragment(16, 16 bits).resize(12))
    }
    s <-< p
    // @NOTE tag_valid is unknown before TLAST beats, so AND it with TLAST
    // so that we do forward an unknown drop signal on non-last beats to the output
    s_drop := (p.last & !decrypt.io.tag_valid)
  }
  val without_chacha = (!include_chacha) generate new Area { 
    s << k.haltWhen(output_stash_too_full)
    s_length := k_length
    s_drop := False
  }

  // u is the decrypted Type 4 payload but in 512 bits
  val u = Stream(Fragment(Bits(corundumDataWidth bits)))
  val upsizer = AxisUpSizer(cryptoDataWidth, corundumDataWidth)
  // @NOTE consider pipeline stage
  upsizer.io.sink << s
  upsizer.io.sink_length := s_length
  upsizer.io.sink_drop := s_drop
  u << upsizer.io.source
  val u_length = upsizer.io.source_length
  val u_drop = upsizer.io.source_drop

  printf("Upsizer Latency = %d clock cycles.\n", LatencyAnalysis(s.valid, u.valid))

  // c is the decrypted Type 4 payload but in 512 bits in Corundum format
  // c does not experience back pressure during a packet out
  val c = Stream Fragment(CorundumFrame(corundumDataWidth))
  val corundum = AxisToCorundumFrame(corundumDataWidth)
  // @NOTE consider pipeline stage
  corundum.io.sink << u
  corundum.io.sink_length := u_length
  corundum.io.sink_drop := u_drop
  c << corundum.io.source
  
  // r is the decrypted Type 4 payload but in 512 bits in Corundum format
  // r can receive back pressure from Corundum
  val r = Stream Fragment(CorundumFrame(corundumDataWidth))

  // should be room for 1534 + latency of ChaCha20 to FlowStash
  // Flow goes ready after packet last, and room for 26*64=1664 bytes
  val outstash = CorundumFrameFlowStash(corundumDataWidth, fifoSize = 32, 26)
  outstash.io.sink << c
  r << outstash.io.source

  output_stash_too_full := !outstash.io.sink.ready

  val ethhdr = CorundumFrameInsertHeader(corundumDataWidth, 14)
  ethhdr.io.sink << r
  ethhdr.io.header := B("112'x000a3506a3beaabbcc2222220800").subdivideIn(14 slices).reverse.asBits()
  val h = Stream Fragment(CorundumFrame(corundumDataWidth))
  h << ethhdr.io.source

  val fcs = CorundumFrameAppendTailer(corundumDataWidth, 4)
  fcs.io.sink << h
  val f = Stream Fragment(CorundumFrame(corundumDataWidth))
  f << fcs.io.source

  io.source << f

  //printf("x to r = %d clock cycles.\n", LatencyAnalysis(x.valid, r.valid))

  val has_busctrl = true
  (!has_busctrl) generate new Area {
  val lut = LookupTable(256/*bits*/, keys_num/*, ClockDomain.current*/)

  lut.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))

  lut.io.portA.en := True
  lut.io.portA.wr := False
  lut.io.portA.wrData := 0
  lut.io.portA.addr := txkey.io.receiver.resize(log2Up(keys_num))
  txkey.io.key_in := lut.io.portA.rdData

  lut.io.portB.en := True
  lut.io.portB.wr := False
  lut.io.portB.wrData := 0
  lut.io.portB.addr := 0
  }
  // RX key lookup and update via bus controller
  (has_busctrl) generate new Area {
  val lut = LookupTableAxi4(256/*bits*/, keys_num, busCfg)
  lut.mem.mem.initBigInt(Seq.fill(keys_num)(BigInt("80 81 82 83 84 85 86 87 88 89 8a 8b 8c 8d 8e 8f 90 91 92 93 94 95 96 97 98 99 9a 9b 9c 9d 9e 9f".split(" ").reverse.mkString(""), 16)))
  lut.io.en := True
  lut.io.wr := False
  lut.io.wrData := 0
  txkey.io.receiver.addAttribute("mark_debug")
  val lut_address = U(txkey.io.receiver.asBits.subdivideIn(4 slices).reverse.asBits.resize(log2Up(keys_num)))
  lut_address.addAttribute("mark_debug")
  lut.io.addr := lut_address //txkey.io.receiver.resize(log2Up(keys_num))
  
  txkey.io.key_in := lut.io.rdData
  io.ctrl_txkey >> lut.io.ctrlbus
  }
*/

  // Execute the function renameAxiIO after the creation of the component
  addPrePopTask(() => CorundumFrame.renameAxiIO(io))
}

import spinal.sim._
import spinal.core.sim._
import scala.util.Random
import spinal.lib.sim.{ScoreboardInOrder, SimData}

object BlackwireTransmitSim {
  def main(args: Array[String]) : Unit = {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    val include_chacha = true

    SimConfig
    // GHDL can simulate VHDL, required for ChaCha20Poly1305
    .withGhdl.withWave
    //.addRunFlag support is now in SpinalHDL dev branch
    .addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    .addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    .addRunFlag("--backtrace-severity=warning")
    
    //.withXSim.withXilinxDevice("xcu50-fsvh2104-2-e")
    //.addSimulatorFlag("--ieee=standard")
    //.addSimulatorFlag("-v")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unisim/v93")
    //.addSimulatorFlag("-P/project-on-host/SpinalCorundum/xilinx-vivado/unimacro/v93") 
    // these define bus_pkg and bus_pkg1

    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/bus_pkg1.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/AEAD_encryption_wrapper_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/AEAD_encryptor_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/ChaCha20_128.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/ChaCha_int.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/col_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/diag_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/half_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mod_red_1305.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul_136_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul136_mod_red.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul_36.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul_68_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul_gen_0.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/mul_red_pipeline.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/Poly_1305_pipe_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/Poly_1305_pipe_top_kar.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/q_round.vhd")
    .addRtl(s"../ChaCha20Poly1305/src_dsp_opt/r_pow_n_kar.vhd")

    .compile {
      val dut = new BlackwireTransmit(BlackwireTransmit.busconfig, include_chacha = include_chacha)
      //dut.with_chacha.decrypt.io.source.ready.simPublic()
      //dut.with_chacha.decrypt.io.source.valid.simPublic()
      //dut.with_chacha.decrypt.io.source.last.simPublic()
      //dut.with_chacha.decrypt.io.tag_valid.simPublic()
      dut
    }
    //.addSimulatorFlag("-Wno-TIMESCALEMOD")
    // include_chacha = true requires GHDL or XSim
    .doSim { dut =>

      dut.io.sink.valid #= false

      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      var data0 = 0

      var last0 = false
      var valid0 = false
      var tkeep0 = BigInt(0)
      var pause = false

      dut.io.sink.valid #= valid0
      dut.io.sink.payload.tdata #= 0
      dut.io.sink.last #= last0
      dut.io.sink.payload.tkeep #= tkeep0
      dut.io.sink.payload.tuser #= 0

      dut.clockDomain.waitSampling()

// "0102030405060102030405060102" Ethernet
// "xxxx11887766554433221145" IPv4, IHL=5, protocol=0x11 (UDP)
// "0000FF0000000000000000FF"
// "CCCCLLLLb315SSSS", DDDD=port 5555 (0x15b3)
// "00000000FFFF0000"

      var packet_number = 0
      val inter_packet_gap = 1

      val packet_contents = Vector(
        // RFC7539 2.8.2. Example and Test Vector for AEAD_CHACHA20_POLY1305
        // but with zero-length AAD, and Wireguard 64-bit nonce
        
        Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <-Wireguard Type 4, I-> <-- Wireguard NONCE --> <L  a  d  i  e  s
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89".split(" ").reverse.mkString(""), 16),
          BigInt("46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4 93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97".split(" ").reverse.mkString(""), 16),
          BigInt("1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0 46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee".split(" ").reverse.mkString(""), 16),
          BigInt("98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        ),
        Vector( // @TODO UDP length does not match - is ignored 
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <-Wireguard Type 4, I-> <-- Wireguard NONCE --> <- Poly 1305 Tag
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 5a 70 0f 88 e7 87".split(" ").reverse.mkString(""), 16),
          BigInt("fe 1c 1e f6 64 e6 01 ba 93 5f 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        ),
        Vector(
          //      <-------- Ethernet header --------------> <-IPv4 header IHL=5 protocol=0x11->                         <--5555,5555,len0x172-> <-Wireguard Type 4, I-> <-- Wireguard NONCE --> <L  a  d  i  e  s
          BigInt("01 02 03 04 05 06 01 02 03 04 05 06 08 00 45 11 22 33 44 55 66 77 88 11 00 00 00 00 00 00 00 00 00 00 15 b3 15 b3 01 72 00 00 04 00 00 00 00 00 00 01 40 41 42 43 44 45 46 47 a4 79 cb 54 62 89".split(" ").reverse.mkString(""), 16),
          BigInt("46 d6 f4 04 2a 8e 38 4e f4 bd 2f bc 73 30 b8 be 55 eb 2d 8d c1 8a aa 51 d6 6a 8e c1 f8 d3 61 9a 25 8d b0 ac 56 95 60 15 b7 b4 93 7e 9b 8e 6a a9 57 b3 dc 02 14 d8 03 d7 76 60 aa bc 91 30 92 97".split(" ").reverse.mkString(""), 16),
          BigInt("1d a8 f2 07 17 1c e7 84 36 08 16 2e 2e 75 9d 8e fc 25 d8 d0 93 69 90 af 63 c8 20 ba 87 e8 a9 55 b5 c8 27 4e f7 d1 0f 6f af d0 46 47 1b 14 57 76 ac a2 f7 cf 6a 61 d2 16 64 25 2f b1 f5 ba d2 ee".split(" ").reverse.mkString(""), 16),
          BigInt("98 e9 64 8b b1 7f 43 2d cc e4 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00".split(" ").reverse.mkString(""), 16)
        )
      )

      var packet_content_idx = 0

      while (packet_number < 20) {

        packet_content_idx = packet_number % packet_contents.length
        // MUST MATCH "plaintext"
        var packet_content_lengths = Vector(3 * 64 + 10, 64 + 10, 3 * 64 + 10)

        var remaining = packet_content_lengths(packet_content_idx)

        var word_index = 0
        // iterate over frame content
        while (remaining > 0) {
          printf("remaining = %d\n", remaining)
          val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
          printf("tkeep_len = %d\n", tkeep_len)
          valid0 = (Random.nextInt(8) > 2)
          valid0 &= !pause
          if (pause) pause ^= (Random.nextInt(16) >= 15)
          if (!pause) pause ^= (Random.nextInt(128) >= 127)

          assert(tkeep_len <= keepWidth)
          tkeep0 = 0
          data0 = 0
          if (valid0) {
            last0 = (remaining <= keepWidth)
            for (i <- 0 until tkeep_len) {
              tkeep0 = (tkeep0 << 1) | 1
            }
          }

          dut.io.sink.valid #= valid0
          dut.io.sink.payload.tdata #= packet_contents(packet_content_idx)(word_index)
          dut.io.sink.last #= last0
          dut.io.sink.last #= last0
          dut.io.sink.payload.tkeep #= tkeep0
          dut.io.sink.payload.tuser #= (packet_number << 1)

          dut.io.source.ready #= (Random.nextInt(8) > 1)

          // Wait a rising edge on the clock
          dut.clockDomain.waitRisingEdge()

          if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
            remaining -= tkeep_len
            word_index += 1
          }
        }
        // assert full packet is sent
        assert(remaining == 0)
        dut.io.sink.valid #= false

        //printf("remaining = %d after while (remaining > 0))\n", remaining)
        assert(remaining == 0)

        dut.clockDomain.waitRisingEdge(inter_packet_gap)

        packet_number += 1
      } // while remaining_packets

      dut.io.source.ready #= true

      //if (include_chacha) {
      //var limit = 500
      //var good_packets = 0
      //while ((limit > 0)/* && (good_packets == 0)*/) {
      //    if (dut.with_chacha.decrypt.io.source.ready.toBoolean &
      //        dut.with_chacha.decrypt.io.source.valid.toBoolean &
      //        dut.with_chacha.decrypt.io.source.last.toBoolean
      //        ) {
      //          printf("dut.with_chacha.decrypt.io.tag_valid = %b\n", dut.with_chacha.decrypt.io.tag_valid.toBoolean)
      //          if (dut.with_chacha.decrypt.io.tag_valid.toBoolean == true) {
      //            good_packets += 1
      //          }
      //    }
      //    dut.clockDomain.waitRisingEdge()
      //    limit -= 1//          limit -= 1
      //}
      //assert(good_packets == 1)
      //}

      dut.clockDomain.waitRisingEdge(500)

    }
  }
}

object BlackwireTransmitMuxSim {
  def main(args: Array[String]) : Unit = {
    val dataWidth = 512
    val maxDataValue = scala.math.pow(2, dataWidth).intValue - 1
    val keepWidth = dataWidth/8
    val include_chacha = true

    SimConfig
    // GHDL can simulate VHDL, required for ChaCha20Poly1305
    .withGhdl.withWave
    //.addRunFlag support is now in SpinalHDL dev branch
    .addRunFlag("--unbuffered") //.addRunFlag("--disp-tree=inst")
    .addRunFlag("--ieee-asserts=disable").addRunFlag("--assert-level=none")
    .addRunFlag("--backtrace-severity=warning")
    
    .compile {
      val dut = new BlackwireTransmit(BlackwireTransmit.busconfig, include_chacha = include_chacha)
      dut
    }
    .doSim { dut =>

      val completionScoreboard = ScoreboardInOrder[BigInt]()

      dut.io.sink.valid #= false
      dut.clockDomain.forkStimulus(period = 10)

      val dataSenderThread = fork {
        var data0 = 0
        var last0 = false
        var valid0 = false
        var tkeep0 = BigInt(0)
        var pause0 = false
        var packet_number = 0
        while (packet_number < 100) {
          var remaining = Random.nextInt(1534)
          val inter_packet_gap = Random.nextInt(16)

          val tx_tag = (1 << 15) | packet_number

          var word_index = 0
          // iterate over frame content
          while (remaining > 0) {
            printf("data remaining = %d\n", remaining)
            val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
            printf("data tkeep_len = %d\n", tkeep_len)
            valid0 = (Random.nextInt(8) > 2)
            valid0 &= !pause0
            if (pause0) pause0 ^= (Random.nextInt(16) >= 15)
            valid0 = true

            assert(tkeep_len <= keepWidth)
            tkeep0 = 0
            data0 = 0
            if (valid0) {
              last0 = (remaining <= keepWidth)
              for (i <- 0 until tkeep_len) {
                tkeep0 = (tkeep0 << 1) | 1
              }
            }

            dut.io.sink.valid #= valid0
            dut.io.sink.payload.tdata #= BigInt(word_index) << (512 - 16)
            dut.io.sink.last #= last0
            dut.io.sink.payload.tkeep #= tkeep0
            dut.io.sink.payload.tuser #= (tx_tag << 1)

            // Wait a rising edge on the clock
            dut.clockDomain.waitRisingEdge()

            if (dut.io.sink.ready.toBoolean & dut.io.sink.valid.toBoolean) {
              remaining -= tkeep_len
              word_index += 1
            }
          }
          // assert full packet is sent
          assert(remaining == 0)

          printf("completion tag for data packet = %04x\n", tx_tag)
          //completionScoreboard.pushRef(tx_tag)

          dut.io.sink.valid #= false
          dut.clockDomain.waitRisingEdge(inter_packet_gap)
          packet_number += 1
          printf("data packet #%d\n", packet_number)
        } // while remaining_packets
      }

      val handshakeSenderThread = fork {
        var data0 = 0
        var last0 = false
        var valid0 = false
        var tkeep0 = BigInt(0)
        var pause0 = false
        var packet_number = 0
        while (packet_number < 100) {
          var remaining = Random.nextInt(300)
          val inter_packet_gap = 0 //Random.nextInt(16)

          var word_index = 0
          // iterate over frame content
          while (remaining > 0) {
            printf("data remaining = %d\n", remaining)
            val tkeep_len = if (remaining >= keepWidth) keepWidth else remaining;
            printf("data tkeep_len = %d\n", tkeep_len)
            valid0 = (Random.nextInt(8) > 2)
            valid0 &= !pause0
            // unpause
            if (pause0) pause0 ^= (Random.nextInt(16) >= 15)

            assert(tkeep_len <= keepWidth)
            tkeep0 = 0
            data0 = 0
            if (valid0) {
              last0 = (remaining <= keepWidth)
              for (i <- 0 until tkeep_len) {
                tkeep0 = (tkeep0 << 1) | 1
              }
            }

            dut.io.sink_handshake.valid #= valid0
            dut.io.sink_handshake.payload.tdata #= BigInt("AAAAAAAAAAAAAAAAA", 16)
            dut.io.sink_handshake.last #= last0
            dut.io.sink_handshake.payload.tkeep #= tkeep0
            dut.io.sink_handshake.payload.tuser #= 0

            // Wait a rising edge on the clock
            dut.clockDomain.waitRisingEdge()

            if (dut.io.sink_handshake.ready.toBoolean & dut.io.sink_handshake.valid.toBoolean) {
              remaining -= tkeep_len
              word_index += 1
            }
          }
          // assert full packet is sent
          assert(remaining == 0)
          dut.io.sink_handshake.valid #= false
          dut.clockDomain.waitRisingEdge(inter_packet_gap)
          packet_number += 1
          printf("data packet #%d\n", packet_number)
        } // while remaining_packets
      }

      // CMAC side reflection of TUSER to completion tag return path
      val completionReflectorThread = fork {
        dut.io.cpl_source.ready #= true
        dut.io.cpl_sink.valid #= false
        dut.io.cpl_sink.payload #= BigInt(0)
        while (true) {
          // on active last beat, reflect tuser
          val valid_tag = dut.io.source.ready.toBoolean & dut.io.source.valid.toBoolean &
            // valid tag? (bit #16 of tuser set)
            dut.io.source.last.toBoolean & ((dut.io.source.tuser.toBigInt >> 16) != 0)
            printf("valid_tag = %b\n", valid_tag)
            dut.io.cpl_sink.valid #= valid_tag
            if (valid_tag) {
              dut.io.cpl_sink.payload #= (dut.io.source.payload.tuser.toBigInt >> 1)
            } else {
              dut.io.cpl_sink.payload #= BigInt(0)
            }
          if (dut.io.source.ready.toBoolean & dut.io.source.valid.toBoolean & dut.io.source.last.toBoolean) {
          }
          dut.clockDomain.waitRisingEdge()
        }
      }

      // Monitor PCIe side of completion tag return path, to verify tags for dropped packets
      val completionMonitorThread = fork {
        while (true) {
          if (dut.io.cpl_source.ready.toBoolean & dut.io.cpl_source.valid.toBoolean) {
            printf("completion tag returned = %04x\n", dut.io.cpl_source.payload.toBigInt)
            // verify that only active tags (bit #15 set) are returned 
            assert((dut.io.cpl_source.payload.toBigInt >> 15) > 0)
            //completionScoreboard.pushDut(dut.io.cpl_source.payload.toBigInt)
          }
          dut.clockDomain.waitRisingEdge()
        }
      }

      dut.io.source.ready #= true
      dut.clockDomain.waitRisingEdge(1000)
      printf("Scoreboard is empty = %b\n", completionScoreboard.checkEmptyness());
    }
  }
}