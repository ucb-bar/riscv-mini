package mini

import Chisel._

abstract trait MemParams extends UsesParameters {
  val xlen = params(XLEN)
  val memLen = params(MemLen)
  val tagLen = params(TagLen)
}

abstract trait MemBundle extends Bundle with MemParams

class MemReqCmd extends MemBundle {
  val rw   = Bool()
  val addr = UInt(width=xlen)
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
  val icache = new CacheIO
  val dcache = new CacheIO
}

class Memory extends Module with MemParams {
  val io = new MemoryIO
  val memReqCmdQueue = Module(new Queue(io.mem.req_cmd.bits.clone, 4))
  val memReqDataQueue = Module(new Queue(io.mem.req_data.bits.clone, 4))
  val s_READY :: s_WAIT :: Nil = Enum(UInt(), 2)
  val state  = RegInit(s_READY)
  val tag    = RegInit(UInt(0, tagLen))
  val cpuReq = (state === s_READY) && (io.icache.req.valid || io.dcache.req.valid)
  val iaddr  = io.icache.req.bits.addr(xlen-1, 2)
  val daddr  = io.dcache.req.bits.addr(xlen-1, 2)
  val idata  = Reg(UInt())
  val ddata  = Reg(UInt())
  val ire    = Reg(Bool())
  val dre    = Reg(Bool())
  io.icache.resp.bits.data := idata // io.mem.resp.bits.data
  io.dcache.resp.bits.data := ddata // io.mem.resp.bits.data
  io.mem.req_cmd <> memReqCmdQueue.io.deq
  io.mem.req_data <> memReqDataQueue.io.deq
  io.mem.resp.ready := Bool(false)
  io.stall := state === s_WAIT || !memReqCmdQueue.io.enq.ready || !memReqDataQueue.io.enq.ready
  memReqCmdQueue.io.enq.bits.rw    := io.dcache.req.bits.mask.orR
  memReqCmdQueue.io.enq.bits.tag   := tag
  memReqCmdQueue.io.enq.bits.addr  := Mux(io.dcache.req.bits.mask.orR || !io.icache.req.valid, daddr, iaddr)
  memReqCmdQueue.io.enq.bits.mask  := io.dcache.req.bits.mask
  memReqCmdQueue.io.enq.valid      := memReqDataQueue.io.enq.ready && cpuReq
  memReqDataQueue.io.enq.bits.data := io.dcache.req.bits.data
  memReqDataQueue.io.enq.valid     := memReqCmdQueue.io.enq.ready && cpuReq && io.dcache.req.bits.mask.orR

  switch(state) {
    is(s_READY) {
      when((io.icache.req.valid || io.dcache.req.valid) && !io.dcache.req.bits.mask.orR && memReqCmdQueue.io.enq.ready) {
        ire := io.icache.req.valid
        dre := io.dcache.req.valid 
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
