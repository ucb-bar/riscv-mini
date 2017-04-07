// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import junctions._
import config.Parameters

class MemArbiter(implicit p: Parameters) extends Module {
  val io = IO(new ParameterizedBundle {
    val icache = Flipped(new NastiIO)
    val dcache = Flipped(new NastiIO)
    val nasti  =  new NastiIO
  })

  val s_IDLE :: s_ICACHE_READ :: s_DCACHE_READ :: s_DCACHE_WRITE :: s_DCACHE_ACK :: Nil = Enum(UInt(), 5)
  val state = RegInit(s_IDLE)

  // Write Address
  io.nasti.aw.bits := io.dcache.aw.bits
  io.nasti.aw.valid := io.dcache.aw.valid && state === s_IDLE
  io.dcache.aw.ready := io.nasti.aw.ready && state === s_IDLE

  // Write Data 
  io.nasti.w.bits  := io.dcache.w.bits
  io.nasti.w.valid := io.dcache.w.valid && state === s_DCACHE_WRITE
  io.dcache.w.ready := io.nasti.w.ready && state === s_DCACHE_WRITE

  // Write Ack
  io.dcache.b.bits := io.nasti.b.bits
  io.dcache.b.valid := io.nasti.b.valid && state === s_DCACHE_ACK
  io.nasti.b.ready := io.dcache.b.ready && state === s_DCACHE_ACK

  // Read Address
  io.nasti.ar.bits := NastiReadAddressChannel(
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.id,   io.icache.ar.bits.id),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.addr, io.icache.ar.bits.addr),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.size, io.icache.ar.bits.size),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.len,  io.icache.ar.bits.len))
  io.nasti.ar.valid := (io.icache.ar.valid || io.dcache.ar.valid) && 
    !io.nasti.aw.valid && state === s_IDLE
  io.dcache.ar.ready := io.nasti.ar.ready && !io.nasti.aw.valid && state === s_IDLE
  io.icache.ar.ready := io.dcache.ar.ready && !io.dcache.ar.valid

  // Read Data
  io.icache.r.bits  := io.nasti.r.bits
  io.dcache.r.bits  := io.nasti.r.bits
  io.icache.r.valid := io.nasti.r.valid && state === s_ICACHE_READ
  io.dcache.r.valid := io.nasti.r.valid && state === s_DCACHE_READ
  io.nasti.r.ready := io.icache.r.ready && state === s_ICACHE_READ || 
                      io.dcache.r.ready && state === s_DCACHE_READ

  switch(state) {
    is(s_IDLE) {
      when(io.dcache.aw.fire()) {
        state := s_DCACHE_WRITE
      }.elsewhen(io.dcache.ar.fire()) {
        state := s_DCACHE_READ
      }.elsewhen(io.icache.ar.fire()) {
        state := s_ICACHE_READ
      }
    }
    is(s_ICACHE_READ) {
      when(io.nasti.r.fire() && io.nasti.r.bits.last) {
        state := s_IDLE
      }
    }
    is(s_DCACHE_READ) {
      when(io.nasti.r.fire() && io.nasti.r.bits.last) {
        state := s_IDLE
      }
    }
    is(s_DCACHE_WRITE) {
      when(io.dcache.w.fire() && io.dcache.w.bits.last) {
        state := s_DCACHE_ACK
      }
    }
    is(s_DCACHE_ACK) {
      when(io.nasti.b.fire()) {
        state := s_IDLE
      }
    }
  }
}

class TileIO(implicit p: Parameters) extends ParameterizedBundle {
  val host  = new HostIO
  val nasti = new NastiIO
}

class Tile(tileParams: Parameters) extends Module {
  implicit val p = tileParams
  val io     = IO(new TileIO)
  val core   = Module(new Core)
  val icache = Module(new Cache)
  val dcache = Module(new Cache)
  val arb    = Module(new MemArbiter)
  
  io.host <> core.io.host
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  arb.io.icache <> icache.io.nasti
  arb.io.dcache <> dcache.io.nasti
  io.nasti <> arb.io.nasti
}
