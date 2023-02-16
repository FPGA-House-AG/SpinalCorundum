package corundum

import spinal.core._
import spinal.lib._

//import scala.math._

import spinal.core.formal._

object AxisExtractHeaderFormal extends App {

  final val dataWidth = 32
  final val header_length = 3

  // in Blackwire we typically use:
  //final val dataWidth = 512
  //final val header_length = 14 + 20 + 8

  final val dataWidthBytes = dataWidth / 8
  assert(header_length <= dataWidthBytes)

  final val maxPacketLength = (dataWidthBytes * 12)

  FormalConfig
  .withDebug
  .withCover(15)
  .withBMC(15)
  /*.withProve(15)*/
  .doVerify(new Component {

    val dut = FormalDut(AxisExtractHeader(dataWidth, header_length))
    assumeInitial(ClockDomain.current.isResetActive)

    // randomize DUT inputs
    // apply back-pressure to DUT
    anyseq(dut.io.source.ready)

    // apply pauses in input to DUT
    anyseq(dut.io.sink.valid)
    anyseq(dut.io.sink.payload.fragment)

    // choose an input packet length at least header_length (both are specified in bytes)
    anyseq(dut.io.sink_length)
    assume(dut.io.sink_length >= header_length)
    // not larger than 3 beats (for now)
    assume(dut.io.sink_length <= (dataWidthBytes * 4))

    val sink_beats = (dut.io.sink_length + dataWidthBytes - 1)/dataWidthBytes
    assumeInitial(sink_beats === ((dut.io.sink_length + dataWidthBytes - 1)/dataWidthBytes))
    
    val beats_left = Reg(sink_beats).init(0)

    // calculate number of input beats based on packet length
    when (dut.io.sink.firstFire & !dut.io.sink.lastFire) {
      beats_left := (dut.io.sink_length + dataWidthBytes - 1)/dataWidthBytes
    }
    .elsewhen (dut.io.sink.fire) {
      beats_left := beats_left - 1
    }
    // drive sink.last based on chosen sink_length
    dut.io.sink.last := (beats_left === 1) | (dut.io.sink.firstFire & (dut.io.sink_length <= (dataWidthBytes)))

    // test for an implication with formal verification, can be written multiple ways:
    //   A -> B, or A "implies" B, or if (A) then (B)
    //   or if "A" then assert("B") or assert(!A or B)
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
    cover(dut.io.sink.isStall)

    // true during all but last beat (thus it is not true for single beat packet)
    val sink_in_packet_but_non_last = (dut.io.sink.isFirst | dut.io.sink.tail) & !dut.io.sink.isLast

    // assume input sink_length remains stable during a packet on input
    when (pastValidAfterReset() && past(sink_in_packet_but_non_last)) {
        assume(stable(dut.io.sink_length))
    }
    cover(sink_in_packet_but_non_last)

    // Assert AXI signals remain stable when the stream was stalled
    when (pastValidAfterReset() && past(dut.io.source.isStall)) {
        assert(stable(dut.io.source.valid))
        assert(stable(dut.io.source.last))
        assert(stable(dut.io.source.payload.fragment))
        assert(stable(dut.io.source_length))
    }
    // Assert AXI output packet length remains stable when the stream is stalled
    //assert(!dut.io.source.isStall | stable(dut.io.source_length))
    cover(dut.io.source.isStall)

    val source_leon_isFirst = dut.io.source.firstFire
    val source_leon_tail = dut.io.source.tail
    val source_leon_isLast = dut.io.source.lastFire
    // true during all but last beat (thus it is not true for single beat packet)
    //val source_in_packet_but_non_last = (dut.io.source.isFirst | dut.io.source.tail) & !dut.io.source.isLast
    val source_in_packet_but_non_last = (source_leon_isFirst | source_leon_tail) & !source_leon_isLast

    // assert output source_length remains stable during a packet on output
    when (pastValidAfterReset() && past(source_in_packet_but_non_last)) {
      assert(stable(dut.io.source_length))
    }

    cover(source_in_packet_but_non_last)

    cover(sink_beats === 1)
    cover(sink_beats === 2)
    cover(sink_beats === 3)

    // Count number of ingoing beats, substract number of outgoing beats
    val leftover_beats_count = Reg(UInt(widthOf(dut.io.sink_length) + 1 bits)) init(0)
    when (dut.io.sink.fire && !dut.io.source.fire) {
      leftover_beats_count := leftover_beats_count + 1
    }
    .elsewhen (!dut.io.sink.fire && dut.io.source.fire) {
      leftover_beats_count := leftover_beats_count - 1
    }

    def lock_up_threshold = 5

    // Component is locked-up when count is incremented beyond threshold
    assert(leftover_beats_count < lock_up_threshold)
    cover(leftover_beats_count =/= 0)

  })
}
