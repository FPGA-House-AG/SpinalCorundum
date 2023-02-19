package corundum

import spinal.core._
import spinal.lib._

//import scala.math._

import spinal.core.formal._

object AxisUpSizerFormal extends App {

  final val dataInWidth = 32
  final val dataOutWidth = 64

  // in Blackwire we typically use:
  //final val dataInWidth = 512

  final val dataWidthInBytes = dataInWidth / 8
  final val dataWidthOutBytes = dataOutWidth / 8

  final val maxPacketLength = (dataWidthInBytes * 12)

  FormalConfig
  .withDebug
  .withCover(15)
  .withBMC(15)
  /*.withProve(15)*/
  .doVerify(new Component {

    val dut = FormalDut(AxisUpSizer(dataInWidth, dataOutWidth))
    assumeInitial(ClockDomain.current.isResetActive)

    // randomize DUT inputs
    // apply back-pressure to DUT
    anyseq(dut.io.source.ready)

    // apply pauses in input to DUT
    anyseq(dut.io.sink.valid)
    anyseq(dut.io.sink.payload.fragment)

    anyseq(dut.io.sink_drop)
    assumeInitial(dut.io.sink_drop === False)
    assume(dut.io.sink_drop === False)

    // choose an input packet length at least header_length (both are specified in bytes)
    anyseq(dut.io.sink_length)
    assume(dut.io.sink_length >= 1)
    // not larger than 3 beats (for now)
    assume(dut.io.sink_length <= (dataWidthInBytes * 4))

    val sink_beats = (dut.io.sink_length + dataWidthInBytes - 1)/dataWidthInBytes
    assumeInitial(sink_beats === ((dut.io.sink_length + dataWidthInBytes - 1)/dataWidthInBytes))

    val source_beats = (dut.io.sink_length + dataWidthOutBytes - 1)/dataWidthOutBytes
    assumeInitial(source_beats === ((dut.io.sink_length + dataWidthOutBytes - 1)/dataWidthOutBytes))


    // drive last based on chosen input packet length
    val is_single_beat_packet = dut.io.sink.firstFire & (dut.io.sink_length <= (dataWidthInBytes))
    val sink_beats_remaining = Reg(sink_beats).init(0)
    // calculate number of input beats based on packet length
    when (is_single_beat_packet) {
      // single-beat packet, this handled seperately below
      sink_beats_remaining := 0
    }
    .elsewhen (dut.io.sink.firstFire & !dut.io.sink.lastFire) {
      sink_beats_remaining := (dut.io.sink_length + dataWidthInBytes - 1)/dataWidthInBytes - 1 
    }
    .elsewhen (dut.io.sink.fire) {
      sink_beats_remaining := sink_beats_remaining - 1
    }
    // drive sink.last based on chosen sink_length
    dut.io.sink.last := (sink_beats_remaining === 1) | is_single_beat_packet

    // test for an implication with formal verification, can be written multiple ways:
    //   A -> B, or A implies B, or if (A) then (B)
    //   or if (A) then assert(B) or assert(!A or B)
    //   A is called the precondition
    // in SpinalHDL we thus can write:
    //   assert(!A | B)
    //   or when (A) assert(B)
    // and to prevent vacuous proofs also cover the precondition using cover()
    //   cover(A)

    // Assume AXI input data remains stable when the stream is stalled
    // (VALID & !READY) -> STABLE(DATA)
    // (VALID & !READY) -> STABLE(VALID)
    // (.isStall)
    when (pastValidAfterReset() & past(dut.io.sink.isStall)) {
        assume(stable(dut.io.sink.valid))
        assume(stable(dut.io.sink.last))
        assume(stable(dut.io.sink.payload.fragment))
        assume(stable(dut.io.sink_length))
    }
    cover(pastValidAfterReset() & past(dut.io.sink.isStall))

    // true during all but last beat (thus it is not true for single beat packet)
    val sink_in_packet_but_non_last = (dut.io.sink.isFirst | dut.io.sink.tail) & !dut.io.sink.isLast

    // assume input sink_length remains stable during a packet on input
    when (pastValidAfterReset() && past(sink_in_packet_but_non_last)) {
        assume(stable(dut.io.sink_length))
    }
    cover(pastValidAfterReset() && past(sink_in_packet_but_non_last))

    // Assert AXI signals remain stable when the stream was stalled
    when (pastValidAfterReset() && past(dut.io.source.isStall)) {
        assert(stable(dut.io.source.valid))
        assert(stable(dut.io.source.last))
        assert(stable(dut.io.source.payload.fragment))
        assert(stable(dut.io.source_length))
    }
    cover(pastValidAfterReset() && past(dut.io.source.isStall))

    val source_leon_isFirst = dut.io.source.firstFire
    val source_leon_tail = dut.io.source.tail
    val source_leon_isLast = dut.io.source.lastFire
    // true during all but last beat (thus it is not true for single beat packet)
    //val source_in_packet_but_non_last = (dut.io.source.isFirst | dut.io.source.tail) & !dut.io.source.isLast
    val source_in_packet_but_non_last = (source_leon_isFirst | source_leon_tail) & !source_leon_isLast

    // assert output source_length remains stable during a packet on output
    when (pastValidAfterReset() && past(source_in_packet_but_non_last)) {
      assert(stable(dut.io.source_length))
      assert(stable(dut.io.source_drop))
    }
    cover(pastValidAfterReset() && past(source_in_packet_but_non_last))

    cover(pastValidAfterReset() && (sink_beats === 1))
    cover(pastValidAfterReset() && (sink_beats === 2))
    cover(pastValidAfterReset() && (sink_beats === 3))

    // calculate number of bytes in last input beat of packet
    val bytes_in_last_input_beat = dut.io.sink_length % dataWidthInBytes
    when ((dut.io.sink_length % dataWidthInBytes) === 0) {
      bytes_in_last_input_beat := dataWidthInBytes
    }

    // Count number of ingoing beats, substract number of outgoing beats
    val beats_in_flight = Reg(UInt(widthOf(dut.io.sink_length) + 1 bits)) init(0)
    val increment = False
    val decrement = False
    // last input beat gets dropped on output due to header removal?
    when (dut.io.sink.fire) {
      increment := True
    }
    when (dut.io.source.fire) {
      decrement := True
    }
    when (increment & !decrement) {
      beats_in_flight := beats_in_flight + 1
    } elsewhen (!increment & decrement) {
      beats_in_flight := beats_in_flight - 1
    }

    def max_beats_in_flight = 5
    // check no beats are lost or generated unexpectedly
    //assert(beats_in_flight < max_beats_in_flight)
    cover(beats_in_flight =/= 0)

    //when (pastValidAfterReset() && beats_in_flight > 2 && dut.io.source.ready) {
    //  assert(dut.io.source.valid)
    //}
    cover (pastValidAfterReset() && beats_in_flight > 2 && dut.io.source.ready)
  })
}
