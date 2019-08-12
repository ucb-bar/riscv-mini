package mini

import chisel3._
import chisel3.aop.Aspect
import chisel3.experimental.ExtModule
import chisel3.testers._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import junctions._
import mini._

trait HexUtils {
  def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'
  // Group 256 chunks together
  // because big vecs dramatically increase compile time... :(
  def loadMem(lines: Iterator[String], chunk: Int) = ((lines flatMap { line =>
    assert(line.length % (chunk / 4) == 0)
    ((line.length - (chunk / 4)) to 0 by -(chunk / 4)) map { i =>
      ((0 until (chunk / 4)) foldLeft BigInt(0)){ (inst, j) =>
        inst | (BigInt(parseNibble(line(i + j))) << (4 * ((chunk / 4) - (j + 1))))
      }
    }
  }) map (_.U(chunk.W)) sliding (1 << 8, 1 << 8)).toSeq
}


class TileTester(
                  tile: => TileBase,
                  loadmem: Iterator[String],
                  maxcycles: Long,
                  latency: Int = 8)
                (implicit val p: freechips.rocketchip.config.Parameters)
  extends BasicTester with HexUtils with CacheParams {
  val dut = Module(tile)
  // Connect black box clock
  dut match {
    case bbox: ExtModule =>
      bbox.clock := clock
    case bbox: BlackBox =>
      bbox.clock := clock
    case _ =>
  }
  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _hex = VecInit(loadMem(loadmem, nastiXDataBits) map (x => Cat(x.reverse)))
  val _mem = Mem(1 << 20, UInt(nastiXDataBits.W))
  val sInit :: sIdle :: sWrite :: sWrAck :: sRead :: Nil = Enum(5)
  val state = RegInit(sInit)
  val cycle = RegInit(0.U(32.W))
  val (cntr, done) = Counter(state === sInit, _hex.size * (1 << 8))

  val id = Reg(UInt(nastiXIdBits.W))
  val addr = Reg(UInt(nastiXAddrBits.W))
  val len = Reg(UInt(nastiXLenBits.W))
  val off = Reg(UInt(nastiXLenBits.W))
  val write = ((0 until (nastiXDataBits / 8)) foldLeft 0.U(nastiXDataBits.W)){ (write, i) => write |
    (Mux(dut.io.nasti.w.bits.strb(i), dut.io.nasti.w.bits.data, _mem(addr))(8*(i+1)-1, 8*i)) << (8*i).U
  }
  val bpipe = WireInit(dut.io.nasti.b)
  val rpipe = WireInit(dut.io.nasti.r)

  dut.reset := reset.toBool || state === sInit
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

  when(state =/= sInit) {
    cycle := cycle + 1.U
    assert(cycle < maxcycles.U)
    when(dut.io.host.tohost =/= 0.U) {
      isDone := true.B
    }
  }

  setDone := isDone
  when(setDone) {
    printf("cycles: %d\n", cycle)
    assert((dut.io.host.tohost >> 1.U) === 0.U,
      "* tohost: %d *\n", dut.io.host.tohost)
    stop(); stop()
  }

  val chunk = Wire(UInt(nastiXDataBits.W))
  chunk := _hex(cntr >> 8.U) >> (cntr(7, 0) * nastiXDataBits.U)

  switch(state) {
    is(sInit) {
      _mem(cntr) := chunk
      when(done) { state := sIdle }
      if (p(Trace)) printf("LOAMEM[%x] <= %x\n", cntr * (nastiXDataBits / 8).U, chunk)
    }
    is(sIdle) {
      when(dut.io.nasti.aw.valid) {
        assert((1.U << dut.io.nasti.aw.bits.size) === (nastiXDataBits / 8).U)
        addr := dut.io.nasti.aw.bits.addr / (nastiXDataBits / 8).U
        id := dut.io.nasti.aw.bits.id
        len := dut.io.nasti.aw.bits.len
        off := 0.U
        state := sWrite
      }.elsewhen(dut.io.nasti.ar.valid) {
        assert((1.U << dut.io.nasti.ar.bits.size) === (nastiXDataBits / 8).U)
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
