// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import junctions._
import org.scalatest.flatspec.AnyFlatSpec


class TileTester(
    tile: => TileBase,
    benchmark: String,
    latency: Int = 8)
   (implicit val p: freechips.rocketchip.config.Parameters)
    extends BasicTester with CacheParams {
  val filename = "tests/64/" + benchmark + ".hex" // we have 64 bits per memory entry

  val dut = Module(tile)

  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _mem = Mem(1 << 20, UInt(nastiXDataBits.W))
  loadMemoryFromFileInline(_mem, filename)
  val sIdle :: sWrite :: sWrAck :: sRead :: Nil = Enum(4)
  val state = RegInit(sIdle)
  val cycle = RegInit(0.U(32.W))

  val id = Reg(UInt(nastiXIdBits.W))
  val addr = Reg(UInt(nastiXAddrBits.W))
  val len = Reg(UInt(nastiXLenBits.W))
  val off = Reg(UInt(nastiXLenBits.W))
  val write = ((0 until (nastiXDataBits / 8)) foldLeft 0.U(nastiXDataBits.W)){ (write, i) => write |
    ((Mux(dut.io.nasti.w.bits.strb(i), dut.io.nasti.w.bits.data, _mem(addr))(8*(i+1)-1, 8*i)) << (8*i).U).asUInt
  }
  val bpipe = WireInit(dut.io.nasti.b)
  val rpipe = WireInit(dut.io.nasti.r)

  dut.reset := reset.asBool
  dut.io.nasti.aw.ready := state === sIdle
  dut.io.nasti.ar.ready := state === sIdle
  dut.io.nasti.w.ready  := state === sWrite
  dut.io.nasti.b <> LatencyPipe(bpipe, latency)
  dut.io.nasti.r <> LatencyPipe(rpipe, latency)
  bpipe.bits  := NastiWriteResponseChannel(id)
  bpipe.valid := state === sWrAck
  rpipe.bits  := NastiReadDataChannel(id, _mem(addr + off), off === len)
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
    assert((dut.io.host.tohost >> 1.U) === 0.U,
      "* tohost: %d *\n", dut.io.host.tohost)
    stop()
  }


  switch(state) {
    is(sIdle) {
      when(dut.io.nasti.aw.valid) {
        assert((1.U << dut.io.nasti.aw.bits.size).asUInt === (nastiXDataBits / 8).U)
        addr := dut.io.nasti.aw.bits.addr / (nastiXDataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.aw.bits.len
        off := 0.U
        state := sWrite
      }.elsewhen(dut.io.nasti.ar.valid) {
        assert((1.U << dut.io.nasti.ar.bits.size).asUInt === (nastiXDataBits / 8).U)
        addr := dut.io.nasti.ar.bits.addr / (nastiXDataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.ar.bits.len
        off := 0.U
        state := sRead
      }
    }
    is(sWrite) {
      when(dut.io.nasti.w.valid) {
        _mem(addr + off) := write
        if (p(Trace)) printf("MEM[%x] <= %x\n", (addr + off) * (nastiXDataBits / 8).U, write) 
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
  io.out <> ((0 until latency) foldLeft io.in)((in, i) => Queue(in, 1, pipe=true))
}

object LatencyPipe {
  def apply[T <: Data](in: DecoupledIO[T], latency: Int) = {
    val pipe = Module(new LatencyPipe(in.bits, latency))
    pipe.io.in <> in
    pipe.io.out
  }
}

class TileSimpleTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Tile"
  implicit val p = (new MiniConfig).toInstance
  it should "execute a simple test" in {
    test(new TileTester(new Tile(p), "rv32ui-p-simple")).runUntilStop(15000)
  }
}

abstract class TileTests(cfg: TestConfig, useVerilator: Boolean = false) extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Tile"
  val opts = if(useVerilator) Seq(VerilatorBackendAnnotation) else Seq()
  implicit val p = (new MiniConfig).toInstance
  cfg.tests.foreach { name =>
    it should s"execute $name" in {
      test(new TileTester(new Tile(p), name)).withAnnotations(opts).runUntilStop(cfg.maxcycles)
    }
  }
}

class TileISATests extends TileTests(ISATests)
class TileBmarkTests extends TileTests(BmarkTests, true)
class TileLargeBmarkTests extends TileTests(LargeBmarkTests, true)

