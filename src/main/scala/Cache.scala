package mini

import Chisel._
import junctions._
import cde.{Parameters, Field}

case object NWays extends Field[Int]
case object NSets extends Field[Int]
case object CacheBlockBytes extends Field[Int]
case object UseNasti extends Field[Boolean]

class CacheReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width=xlen)
  val data = UInt(width=xlen)
  val mask = UInt(width=xlen/8)
}

class CacheResp(implicit p: Parameters) extends CoreBundle()(p) {
  val data = UInt(width=xlen)
}

class CacheIO (implicit p: Parameters) extends ParameterizedBundle()(p) {
  val abort = Bool(INPUT)
  val req   = Valid(new CacheReq).flip
  val resp  = Valid(new CacheResp)
}

class CacheModuleIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val cpu   = new CacheIO
  val mem   = new MemIO
  val nasti = new NastiIO
}

trait CacheParams extends CoreParams with HasNastiParameters {
  val nWays    = p(NWays) // Not used...
  val nSets    = p(NSets)
  val bBytes   = p(CacheBlockBytes)
  val bBits    = bBytes << 3
  val blen     = log2Up(bBytes)
  val slen     = log2Up(nSets)
  val tlen     = xlen - (slen + blen)
  val nWords   = bBits / xlen
  val wBytes   = xlen / 8
  val nBytes   = nastiXDataBits / 8
  val nsize    = log2Up(nBytes)
  val nlen     = bBytes / nBytes
  val byteOffsetBits = log2Up(wBytes) 
  require(nlen > 0)
} 

class MetaData(implicit val p: Parameters) extends ParameterizedBundle()(p) with CacheParams {
  val dirty = Bool()
  val tag   = UInt(width=tlen)
}

class Cache(implicit val p: Parameters) extends Module with CacheParams {
  val io = new CacheModuleIO
  // cache states
  val (s_IDLE :: s_READ_CACHE :: s_WRITE_CACHE :: s_WRITE_BACK :: 
       s_REFILL_READY :: s_REFILL :: Nil) = Enum(UInt(), 6)
  val state = RegInit(s_IDLE)
  // memory
  val v        = RegInit(UInt(0, nSets))
  val metaMem  = SeqMem(nSets, new MetaData)
  val dataMem  = Seq.fill(nWords)(SeqMem(nSets, Vec(wBytes, UInt(width=8))))

  val addr_reg = Reg(io.cpu.req.bits.addr)
  val cpu_data = Reg(io.cpu.req.bits.data)
  val cpu_mask = Reg(io.cpu.req.bits.mask)

  // counters for NastiIO
  val (read_count,  read_wrap_out)  = Counter(io.nasti.r.fire(), nlen)
  val (write_count, write_wrap_out) = Counter(io.nasti.w.fire(), nlen)

  val is_idle   = state === s_IDLE
  val is_read   = state === s_READ_CACHE
  val is_write  = state === s_WRITE_CACHE
  val is_alloc  = state === s_REFILL && (
    if (useNasti) read_wrap_out else io.mem.resp.valid)
  val is_allocd = RegNext(is_alloc)
  
  val hit = Wire(Bool())
  val wen = is_write && (hit || is_allocd) && !io.cpu.abort || is_alloc 
  val ren = !wen && (is_idle || is_read) && io.cpu.req.valid 

  val addr     = io.cpu.req.bits.addr
  val idx      = addr(slen+blen-1, blen)
  val tag_reg  = addr_reg(xlen-1, slen+blen)
  val idx_reg  = addr_reg(slen+blen-1, blen)
  val off_reg  = addr_reg(blen-1, byteOffsetBits)

  val rmeta = metaMem.read(idx, ren)
  val rdata_buf = Reg(Vec.fill(nlen)(UInt(width=nastiXDataBits))) 
  val rdata = Mux(!is_allocd, Cat(dataMem.map(_.read(idx, ren).toBits).reverse), 
    if (useNasti) rdata_buf.toBits else RegNext(io.mem.resp.bits.data)) // bypass refilled data
  
  hit := v(idx_reg) && rmeta.tag === tag_reg 

  // Read Mux
  io.cpu.resp.bits.data := Vec.tabulate(nWords)(i => rdata((i+1)*xlen-1, i*xlen))(off_reg)
  io.cpu.resp.valid     := is_idle || is_read && hit || is_allocd && !cpu_mask.orR

  when(io.cpu.resp.valid) { 
    addr_reg  := addr
    cpu_data  := io.cpu.req.bits.data
    cpu_mask  := io.cpu.req.bits.mask
  }

  val wmeta = Wire(new MetaData)
  wmeta.tag   := tag_reg
  wmeta.dirty := !is_alloc 

  val wmask = Mux(!is_alloc, (cpu_mask << Cat(off_reg, UInt(0, byteOffsetBits))).zext, SInt(-1))
  val wdata = Mux(!is_alloc, Fill(nWords, cpu_data), 
    if (useNasti) Cat(io.nasti.r.bits.data, Cat(rdata_buf.init.reverse)) else io.mem.resp.bits.data)
  when(wen) {
    v := v.bitSet(idx_reg, Bool(true))
    metaMem.write(idx_reg, wmeta)
    dataMem.zipWithIndex foreach { case (mem, i) =>
      val data = Vec.tabulate(wBytes)(k => wdata(i*xlen+(k+1)*8-1, i*xlen+k*8))
      mem.write(idx_reg, data, wmask((i+1)*wBytes-1, i*wBytes).toBools)
      mem setName s"dataMem_${i}"
    }
  }

  if (useNasti) {
    // read addr
    io.nasti.ar.bits := NastiReadAddressChannel(
      UInt(0), Cat(Cat(tag_reg, idx_reg), UInt(0, blen)), UInt(nsize), UInt(nlen))
    io.nasti.ar.valid := Bool(false)
    // read data
    io.nasti.r.ready := state === s_REFILL
    when(io.nasti.r.fire()) { rdata_buf(read_count) := io.nasti.r.bits.data }
    // write addr
    io.nasti.aw.bits := NastiWriteAddressChannel(
      UInt(0), Cat(Cat(rmeta.tag, idx_reg), UInt(0, blen)), UInt(nsize), UInt(nlen))
    io.nasti.aw.valid := Bool(false)
    // write data
    io.nasti.w.bits := NastiWriteDataChannel(
      Vec.tabulate(nlen)(i => rdata((i+1)*nastiXDataBits-1, i*nastiXDataBits))(write_count), write_wrap_out)
    io.nasti.w.valid := Bool(false)
    // write resp
    io.nasti.b.ready := Bool(true)
  } else {
    io.mem.req_cmd.valid      := Bool(false)
    io.mem.req_cmd.bits.rw    := Bool(false)
    io.mem.req_cmd.bits.addr  := Mux(io.mem.req_cmd.bits.rw, Cat(rmeta.tag, idx_reg), Cat(tag_reg, idx_reg))
    io.mem.req_cmd.bits.tag   := UInt(0) // Only one outstanding request
    io.mem.req_data.bits.data := Mux(io.mem.req_cmd.valid, rdata, UInt(0))
    io.mem.req_data.valid     := io.mem.req_cmd.bits.rw
    io.mem.resp.ready         := state === s_REFILL
  }

  // Cache FSM
  val is_dirty = v(idx_reg) && rmeta.dirty
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
        if (useNasti) {
          io.nasti.aw.valid := is_dirty 
          io.nasti.ar.valid := !is_dirty
          when(io.nasti.aw.fire()) {
            state := s_WRITE_BACK
          }.elsewhen(io.nasti.ar.fire()) {
            state := s_REFILL
          }
        } else {
          io.mem.req_cmd.valid := Bool(true)
          io.mem.req_cmd.bits.rw := is_dirty
          when(is_dirty && io.mem.req_cmd.ready && io.mem.req_data.ready) {
            state := s_REFILL_READY
          }.elsewhen(!is_dirty && io.mem.req_cmd.ready) {
            state := s_REFILL
          }
        }
      }
    }
    is(s_WRITE_CACHE) {
      when(hit || is_allocd || io.cpu.abort) {
        state := s_IDLE
      }.otherwise {
        if (useNasti) {
          io.nasti.aw.valid := is_dirty
          io.nasti.ar.valid := !is_dirty
          when(io.nasti.aw.fire()) {
            state := s_WRITE_BACK
          }.elsewhen(io.nasti.ar.fire()) {
            state := s_REFILL
          }
        } else {
          io.mem.req_cmd.valid := Bool(true)
          io.mem.req_cmd.bits.rw := is_dirty
          when(is_dirty && io.mem.req_cmd.ready && io.mem.req_data.ready) {
            state := s_REFILL_READY
          }.elsewhen(!is_dirty && io.mem.req_cmd.ready) {
            state := s_REFILL
          }
        }
      }
    }
    is(s_WRITE_BACK) {
      if (useNasti) {
        io.nasti.w.valid := Bool(true)
        when(write_wrap_out) {
          state := s_REFILL_READY
        }
      } 
    }
    is(s_REFILL_READY) {
      if (useNasti) {
        io.nasti.ar.valid := Bool(true)
        when(io.nasti.ar.fire()) {
          state := s_REFILL
        }
      } else {
        io.mem.req_cmd.valid := Bool(true)
        when(io.mem.req_cmd.ready) {
          state := s_REFILL
        }
      } 
    }
    is(s_REFILL) {
      if (useNasti) {
        when(read_wrap_out) {
          state := Mux(cpu_mask.orR, s_WRITE_CACHE, s_IDLE) 
        }
      } else {
        when(io.mem.resp.valid) {
          state := Mux(cpu_mask.orR, s_WRITE_CACHE, s_IDLE) 
        }
      }
    }
  }
}
