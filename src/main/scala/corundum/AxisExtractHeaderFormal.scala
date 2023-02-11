package corundum

import spinal.core._
import spinal.lib._

//import scala.math._

import spinal.core.formal._

object AxisExtractHeaderFormal extends App {

  FormalConfig.withCover(15).withBMC(15).withProve(15).doVerify(new Component {
    final val dataWidth = 32
    final val header_length = 1

    // in Blackwire we typically use:
    //final val dataWidth = 512
    //final val header_length = 14 + 20 + 8

    final val dataWidthBytes = dataWidth / 8
    assert(header_length <= dataWidthBytes)

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
    assume(dut.io.sink_length <= (dataWidthBytes * 3))

    // calculate number of input beats based on packet length
    val sink_beats = Reg((dut.io.sink_length + dataWidthBytes - 1)/dataWidthBytes)
    when (dut.io.sink.fire) {
      sink_beats := sink_beats - 1
    }
    // drive sink.last based on chosen sink_length
    dut.io.sink.last := (sink_beats === 1)

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
    assume(!dut.io.sink.isStall | stable(dut.io.sink.valid))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.last))
    assume(!dut.io.sink.isStall | stable(dut.io.sink.payload.fragment))
    assume(!dut.io.sink.isStall | stable(dut.io.sink_length))
    cover(dut.io.sink.isStall)

    // assume input sink_length remains stable during a packet on input
    assert(past(dut.io.sink.last) | stable(dut.io.sink_length))
    cover(!past(dut.io.sink.last))

    // Assert AXI output data remains stable when the stream is stalled
    assert(!dut.io.source.isStall | stable(dut.io.source.valid))
    assert(!dut.io.source.isStall | stable(dut.io.source.last))
    assert(!dut.io.source.isStall | stable(dut.io.source.payload.fragment))
    // Assert AXI output packet length remains stable when the stream is stalled
    assert(!dut.io.source.isStall | stable(dut.io.source_length))
    cover(dut.io.source.isStall)

    // assert output source_length remains stable during a packet on output
    //  !past(last) -> stable(source_length)
    //  A -> B
    //  !A or B
    assert(past(dut.io.source.last) | stable(dut.io.source_length))
    cover(!past(dut.io.source.last))
  })
}
