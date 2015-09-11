package mini

import Chisel._
import junctions.MemIO

class CacheReq extends CoreBundle {
  val addr = UInt(width=xlen)
  val data = UInt(width=xlen)
  val mask = UInt(width=4)
}

class CacheResp extends CoreBundle {
  val data = UInt(width=xlen)
}

class CacheIO extends Bundle {
  val req  = Valid(new CacheReq).flip
  val resp = Valid(new CacheResp)
}

class CacheModuleIO extends Bundle {
  val cpu = new CacheIO
  val mem = new MemIO
}

trait CacheParams extends UsesParameters {
  val xlen  = params(XLEN) 
  val nWays = params(NWays) // Not used...
  val nSets = params(NSets)
  val bByte = params(CacheBlockBytes)
  val bBits = bByte << 3
  val blen  = log2Up(bByte)
  val slen  = log2Up(nSets)
  val tlen  = xlen - (slen + blen)
  val nWords = bBits / xlen
  val byteOffsetBits = log2Up(xlen / 8) 
} 

class MetaData extends Bundle with CacheParams {
  val valid = Bool()
  val dirty = Bool()
  val tag   = UInt(width=tlen)
}

class Cache extends Module with CacheParams {
  val io = new CacheModuleIO
  // cache states
  val s_IDLE :: s_READ_CACHE :: s_WRITE_CACHE :: s_WRITE_BACK :: s_REFILL :: Nil = Enum(UInt(), 5)
  val state = RegInit(s_IDLE)
  // memory
  val metaMem  = SeqMem(new MetaData,      nSets)
  val dataMem  = SeqMem(UInt(width=bBits), nSets)

  val idle   = state === s_IDLE
  val read   = state === s_READ_CACHE
  val write  = state === s_WRITE_CACHE
  val refill = state === s_REFILL && io.mem.resp.valid

  val addr_reg = Reg(io.cpu.req.bits.addr)
  val cpu_data = Reg(io.cpu.req.bits.data)
  val cpu_mask = Reg(io.cpu.req.bits.mask)

  val addr     = io.cpu.req.bits.addr
  val idx      = addr(slen+blen-1, blen)
  val ren      = idle || read
  val tag_reg  = addr_reg(xlen-1, slen+blen)
  val idx_reg  = addr_reg(slen+blen-1, blen)
  val off_reg  = addr_reg(blen-1, byteOffsetBits)

  val rmeta = metaMem.read(idx, ren)
  val rdata = Mux(!refill, dataMem.read(idx, ren), io.mem.resp.bits.data) // bypass refilled data
  val hit   = rmeta.valid && rmeta.tag === tag_reg

  when(ren || refill) {
    addr_reg := addr
    cpu_data := io.cpu.req.bits.data
    cpu_mask := io.cpu.req.bits.mask
  }

  // Read Mux
  io.cpu.resp.bits.data := MuxLookup(off_reg, rdata(bBits-1, bBits-xlen),
    (0 until (nWords - 1)) map (i => UInt(i) -> rdata((i+1)*xlen-1, i*xlen)))
  io.cpu.resp.valid := idle || (read && hit) || refill 

  val wmeta = Wire(new MetaData)
  wmeta.tag   := tag_reg
  wmeta.dirty := write || refill && cpu_mask.orR
  wmeta.valid := Bool(true)

  val wen = write || refill
  // Write Mux
  val wdata = Mux(!refill, Fill(nWords, cpu_data), Cat(((nWords-1) to 0 by -1) map (i =>
    Mux(off_reg === UInt(i) && cpu_mask.orR, cpu_data, io.mem.resp.bits.data((i+1)*xlen-1, i*xlen)))))
  val wmask = Mux(!refill, (cpu_mask << Cat(off_reg, UInt(0, byteOffsetBits))).zext, SInt(-1))
  when(wen) {
    metaMem.write(idx_reg, wmeta)
    dataMem.write(idx_reg, wdata, FillInterleaved(8, wmask))
  }
  io.mem.req_cmd.valid      := !hit && (read || write) || state === s_WRITE_BACK
  io.mem.req_cmd.bits.rw    := !hit && (read || write) && rmeta.valid && rmeta.dirty 
  io.mem.req_cmd.bits.addr  := Mux(io.mem.req_cmd.bits.rw, Cat(rmeta.tag, idx_reg), Cat(tag_reg, idx_reg))
  io.mem.req_cmd.bits.tag   := UInt(0) // Only one outstanding request
  io.mem.req_data.bits.data := rdata 
  io.mem.req_data.valid     := io.mem.req_cmd.bits.rw
  io.mem.resp.ready         := state === s_REFILL

  // Cache FSM
  switch(state) {
    is(s_IDLE) {
      when(io.cpu.req.valid) {
        state := Mux(io.cpu.req.bits.mask.orR, s_WRITE_CACHE, s_READ_CACHE)
      }
    }
    is(s_READ_CACHE) {
      when(hit) {
        when(io.cpu.req.valid) {
          state := Mux(io.cpu.req.bits.mask.orR, s_WRITE_CACHE, s_READ_CACHE)
        }.otherwise {
          state := s_IDLE
        }
      }.otherwise {
        when(io.mem.req_cmd.bits.rw && io.mem.req_cmd.ready && io.mem.req_data.ready) {
          state := s_WRITE_BACK
        }.elsewhen(io.mem.req_cmd.ready) {
          state := s_REFILL
        }
      }
    }
    is(s_WRITE_CACHE) {
      when(hit) {
        state := s_IDLE
      }.otherwise {
        when(io.mem.req_cmd.bits.rw && io.mem.req_cmd.ready && io.mem.req_data.ready) {
          state := s_WRITE_BACK
        }.elsewhen(!io.mem.req_cmd.bits.rw && io.mem.req_cmd.ready) {
          state := s_REFILL
        }
      }
    }
    is(s_WRITE_BACK) {
      when(io.mem.req_cmd.ready) {
        state := s_REFILL
      }
    }
    is(s_REFILL) {
      when(io.mem.resp.valid) {
        state := Mux(io.cpu.req.valid, s_READ_CACHE, s_IDLE)
      }
    }
  }
}
