package mini

import Chisel._

class MemReqCmd extends MemBundle {
  val rw   = Bool()
  val addr = UInt(width=addrLen)
  val tag  = UInt(width=tagLen)
  val mask = UInt(width=4) // TODO:
}

class MemReqData extends MemBundle {
  val data = UInt(width=memLen)
}

class MemResp extends MemBundle {
  val data = UInt(width=memLen)
  val tag  = UInt(width=tagLen)
}

class MemIO extends Bundle {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemReqData)
  val resp     = Decoupled(new MemResp).flip
} 

class MemoryIO extends Bundle {
  val stall  = Bool(OUTPUT)
  val mem    = new MemIO
  val icache = (new CacheIO).flip
  val dcache = (new CacheIO).flip
}

class Memory extends Module with MemParams {
  val io = new MemoryIO
  val memReqCmdQueue = Module(new Queue(io.mem.req_cmd.bits.clone, 4))
  val memReqDataQueue = Module(new Queue(io.mem.req_data.bits.clone, 4))
  val s_READY :: s_WAIT :: Nil = Enum(UInt(), 2)
  val state  = RegInit(s_READY)
  val tag    = RegInit(UInt(0, tagLen))
  val cpuReq = (state === s_READY) && (io.icache.re || io.dcache.re || io.dcache.we.orR)
  val iaddr  = io.icache.addr(addrLen-1, 2)
  val daddr  = io.dcache.addr(addrLen-1, 2)
  val idata  = Reg(UInt())
  val ddata  = Reg(UInt())
  val ire    = Reg(Bool())
  val dre    = Reg(Bool())
  io.icache.dout := idata // io.mem.resp.bits.data
  io.dcache.dout := ddata // io.mem.resp.bits.data
  io.mem.req_cmd <> memReqCmdQueue.io.deq
  io.mem.req_data <> memReqDataQueue.io.deq
  io.mem.resp.ready := Bool(false)
  io.stall := state === s_WAIT || !memReqCmdQueue.io.enq.ready || !memReqDataQueue.io.enq.ready
  memReqCmdQueue.io.enq.bits.rw    := io.dcache.we.orR
  memReqCmdQueue.io.enq.bits.tag   := tag
  memReqCmdQueue.io.enq.bits.addr  := Mux(io.dcache.we.orR || !io.icache.re, daddr, iaddr)
  memReqCmdQueue.io.enq.bits.mask  := io.dcache.we
  memReqCmdQueue.io.enq.valid      := memReqDataQueue.io.enq.ready && cpuReq
  memReqDataQueue.io.enq.bits.data := io.dcache.din
  memReqDataQueue.io.enq.valid     := memReqCmdQueue.io.enq.ready && cpuReq && io.dcache.we.orR

  switch(state) {
    is(s_READY) {
      when((io.icache.re || io.dcache.re) && !io.dcache.we.orR && memReqCmdQueue.io.enq.ready) {
        ire := io.icache.re 
        dre := io.dcache.re 
        state := s_WAIT
      }
    }
    is(s_WAIT) {
      io.mem.resp.ready := Bool(true)
      when(io.mem.resp.valid && io.mem.resp.bits.tag === tag) {
        state := s_READY
        tag   := tag + UInt(1)
        io.mem.resp.ready := Bool(false)
        when(ire) { idata := io.mem.resp.bits.data }
        when(dre) { ddata := io.mem.resp.bits.data }
      }
    }
  }
} 
