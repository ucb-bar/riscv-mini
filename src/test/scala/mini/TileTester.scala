// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.testers._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import junctions._
import org.scalatest.flatspec.AnyFlatSpec

object TileTesterState extends ChiselEnum {
  val sIdle, sWrite, sWrAck, sRead = Value
}

class TileTester(tile: => Tile, benchmark: String, latency: Int = 8, trace: Boolean = false) extends BasicTester {
  val originalHexFile = os.rel / "tests" / f"$benchmark.hex"
  val resizedHexFile = os.rel / "tests" / "64" / f"$benchmark.hex"
  TestUtils.resizeHexFile(os.pwd / originalHexFile, os.pwd / resizedHexFile, 64) // we have 64 bits per memory entry

  val dut = Module(tile)
  // extract parameters from design under test
  val nasti = dut.nastiParams

  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _mem = Mem(1 << 20, UInt(nasti.dataBits.W))
  loadMemoryFromFileInline(_mem, resizedHexFile.toString())
  import TileTesterState._
  val state = RegInit(sIdle)
  val cycle = RegInit(0.U(32.W))

  val id = Reg(UInt(nasti.idBits.W))
  val addr = Reg(UInt(nasti.addrBits.W))
  val len = Reg(UInt(NastiConstants.LenBits.W))
  val off = Reg(UInt(NastiConstants.LenBits.W))
  val write = (0 until (nasti.dataBits / 8)).foldLeft(0.U(nasti.dataBits.W)) { (write, i) =>
    write |
      (Mux(dut.io.nasti.w.bits.strb(i), dut.io.nasti.w.bits.data, _mem(addr))(
        8 * (i + 1) - 1,
        8 * i
      ) << (8 * i).U).asUInt
  }
  val bpipe = WireInit(dut.io.nasti.b)
  val rpipe = WireInit(dut.io.nasti.r)

  dut.reset := reset.asBool
  dut.io.nasti.aw.ready := state === sIdle
  dut.io.nasti.ar.ready := state === sIdle
  dut.io.nasti.w.ready := state === sWrite
  dut.io.nasti.b <> LatencyPipe(bpipe, latency)
  dut.io.nasti.r <> LatencyPipe(rpipe, latency)
  bpipe.bits := NastiWriteResponseBundle(nasti)(id)
  bpipe.valid := state === sWrAck
  rpipe.bits := NastiReadDataBundle(nasti)(id, _mem(addr + off), off === len)
  rpipe.valid := state === sRead

  val isDone = WireInit(false.B)
  val setDone = WireInit(false.B)

  cycle := cycle + 1.U
  when(dut.io.host.tohost =/= 0.U) {
    isDone := true.B
  }

  setDone := isDone
  when(setDone) {
    printf("cycles: %d\n", cycle)
    assert((dut.io.host.tohost >> 1.U) === 0.U, "* tohost: %d *\n", dut.io.host.tohost)
    stop()
  }

  /**
    *   Master(Tile)    ---------   Slaver(Memory)
    *
    *   Address Write > == AW == >
    *   Data Write    > == W  == >
    *   Response      < == B  == <
    *
    *   Address Read  > == AR == >
    *   Data Read     < == R  == <
    */

  switch(state) {
    is(sIdle) {
      when(dut.io.nasti.aw.valid) {
        assert((1.U << dut.io.nasti.aw.bits.size).asUInt === (nasti.dataBits / 8).U)
        addr := dut.io.nasti.aw.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.aw.bits.len
        off := 0.U
        state := sWrite
      }.elsewhen(dut.io.nasti.ar.valid) {
        assert((1.U << dut.io.nasti.ar.bits.size).asUInt === (nasti.dataBits / 8).U)
        addr := dut.io.nasti.ar.bits.addr / (nasti.dataBits / 8).U
        id := dut.io.nasti.ar.bits.id
        len := dut.io.nasti.ar.bits.len
        off := 0.U
        state := sRead
      }
    }
    is(sWrite) {
      when(dut.io.nasti.w.valid) {
        _mem(addr + off) := write
        if (trace) printf("MEM[%x] <= %x\n", (addr + off) * (nasti.dataBits / 8).U, write)
        when(off === len) {
          assert(dut.io.nasti.w.bits.last)
          state := sWrAck
        }.otherwise {
          off := off + 1.U
        }
      }
    }
    is(sWrAck) {
      when(bpipe.ready) {
        state := sIdle
      }
    }
    is(sRead) {
      when(rpipe.ready) {
        when(off === len) {
          state := sIdle
        }.otherwise {
          off := off + 1.U
        }
      }
    }
  }
}

class LatencyPipeIO[T <: Data](val gen: T) extends Bundle {
  val in = Flipped(Decoupled(gen))
  val out = Decoupled(gen)
}

class LatencyPipe[T <: Data](gen: T, latency: Int) extends Module {
  val io = IO(new LatencyPipeIO(chiselTypeOf(gen)))
  io := DontCare
  io.out <> (0 until latency).foldLeft(io.in)((in, i) => Queue(in, 1, pipe = true))
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int) = {
    val pipe = Module(new LatencyPipe(in.bits, latency))
    pipe.io.in <> in
    pipe.io.out
  }
}

class TileSimpleTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Tile")
  val p = MiniConfig()
  it should "execute a simple test" in {
    test(new TileTester(Tile(p), "rv32ui-p-simple")).runUntilStop(15000)
  }
}

abstract class TileTests(cfg: TestConfig, useVerilator: Boolean = false)
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior.of("Tile")
  val opts = if (useVerilator) Seq(VerilatorBackendAnnotation) else Seq()
  val p = MiniConfig()
  cfg.tests.foreach { name =>
    it should s"execute $name" taggedAs IntegrationTest in {
      test(new TileTester(Tile(p), name)).withAnnotations(opts).runUntilStop(cfg.maxcycles)
    }
  }
}

class TileISATests extends TileTests(ISATests, true)
class TileBmarkTests extends TileTests(BmarkTests, true)
class TileLargeBmarkTests extends TileTests(LargeBmarkTests, true)
