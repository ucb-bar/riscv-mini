package mini

import Chisel._
import junctions._
import cde.Parameters

class MemIOArbiter(implicit p: Parameters) extends Module {
  val io = new ParameterizedBundle {
    val icache = (new MemIO).flip
    val dcache = (new MemIO).flip
    val mem    =  new MemIO
  }

  val s_IDLE :: s_ICACHE_WAIT :: s_DCACHE_WAIT :: Nil = Enum(UInt(), 3)
  val state = RegInit(s_IDLE)

  // priority on dcache
  io.mem.req_cmd.valid      := state === s_IDLE && (io.icache.req_cmd.valid || io.dcache.req_cmd.valid)
  io.mem.req_cmd.bits.rw    := Mux(io.dcache.req_cmd.valid, io.dcache.req_cmd.bits.rw, io.icache.req_cmd.bits.rw)
  io.mem.req_cmd.bits.addr  := Mux(io.dcache.req_cmd.valid, io.dcache.req_cmd.bits.addr, io.icache.req_cmd.bits.addr)
  io.mem.req_cmd.bits.tag   := UInt(0)
  io.mem.req_data.valid     := io.dcache.req_cmd.valid && io.dcache.req_data.valid
  io.mem.req_data.bits.data := io.dcache.req_data.bits.data
  io.icache.req_cmd.ready   := io.mem.req_cmd.ready && state === s_IDLE && !io.dcache.req_cmd.valid
  io.dcache.req_cmd.ready   := io.mem.req_cmd.ready && state === s_IDLE
  io.icache.req_data.ready  := io.mem.req_data.ready && !io.dcache.req_cmd.valid
  io.dcache.req_data.ready  := io.mem.req_data.ready 
  
  io.icache.resp.bits.data  := io.mem.resp.bits.data
  io.icache.resp.bits.tag   := UInt(0)
  io.icache.resp.valid      := io.mem.resp.valid && state === s_ICACHE_WAIT
  io.dcache.resp.bits.data  := io.mem.resp.bits.data
  io.dcache.resp.bits.tag   := UInt(0)
  io.dcache.resp.valid      := io.mem.resp.valid && state === s_DCACHE_WAIT
  io.mem.resp.ready         := (state === s_ICACHE_WAIT && io.icache.resp.ready) || 
                               (state === s_DCACHE_WAIT && io.dcache.resp.ready)

  switch(state) {
    is(s_IDLE) {
      when(io.icache.req_cmd.fire()) {
        state := s_ICACHE_WAIT // Always read 
      }.elsewhen(io.dcache.req_cmd.fire() && !io.dcache.req_cmd.bits.rw) {
        state := s_DCACHE_WAIT
      }
    }
    is(s_ICACHE_WAIT) {
      when(io.mem.resp.valid) {
        state := s_IDLE
      }
    }
    is(s_DCACHE_WAIT) {
      when(io.mem.resp.valid) {
        state := s_IDLE
      }
    }
  }
}

class NastiIOArbiter(implicit p: Parameters) extends Module {
  val io = new ParameterizedBundle {
    val icache = (new NastiIO).flip
    val dcache = (new NastiIO).flip
    val nasti  =  new NastiIO
  }

  val s_IDLE :: s_ICACHE_READ :: s_DCACHE_READ :: s_DCACHE_WRITE :: Nil = Enum(UInt(), 4)
  val state = RegInit(s_IDLE)

  // Write Address
  io.nasti.aw.bits := io.dcache.aw.bits
  io.nasti.aw.valid := io.dcache.aw.valid && state === s_IDLE
  io.dcache.aw.ready := io.nasti.aw.ready && state === s_IDLE

  // Write Data 
  io.nasti.w.bits  := io.dcache.w.bits
  io.nasti.w.valid := io.dcache.w.valid && state === s_DCACHE_WRITE
  io.dcache.w.ready := io.nasti.w.ready && state === s_DCACHE_WRITE

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
        state := s_IDLE
      }
    }
  }
}
