package mini

import Chisel._
import junctions.MemIO

class MemArbiterIO extends Bundle {
  val icache = (new MemIO).flip
  val dcache = (new MemIO).flip
  val mem    =  new MemIO
}

class MemArbiter extends Module {
  val io = new MemArbiterIO

  val s_IDLE :: s_ICACHE_WAIT :: s_DCACHE_WAIT :: Nil = Enum(UInt(), 3)
  val state = RegInit(s_IDLE)

  // priority on dcache
  io.mem.req_cmd.valid      := state === s_IDLE && (io.icache.req_cmd.valid || io.dcache.req_cmd.valid)
  io.mem.req_cmd.bits.rw    := Mux(io.dcache.req_cmd.valid, io.dcache.req_cmd.bits.rw, io.icache.req_cmd.bits.rw)
  io.mem.req_cmd.bits.addr  := Mux(io.dcache.req_cmd.valid, io.dcache.req_cmd.bits.addr, io.icache.req_cmd.bits.addr)
  io.mem.req_cmd.bits.tag   := UInt(0)
  io.mem.req_data.valid     := io.dcache.req_cmd.valid && io.dcache.req_data.valid
  io.mem.req_data.bits.data := io.dcache.req_data.bits.data
  io.icache.req_cmd.ready   := io.mem.req_cmd.ready && !io.dcache.req_cmd.valid
  io.dcache.req_cmd.ready   := io.mem.req_cmd.ready 
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
