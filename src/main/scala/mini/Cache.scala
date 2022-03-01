// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

class CacheReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  val mask = UInt((dataWidth / 8).W)
}

class CacheResp(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
}

class CacheIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val abort = Input(Bool())
  val req = Flipped(Valid(new CacheReq(addrWidth, dataWidth)))
  val resp = Valid(new CacheResp(dataWidth))
}

class CacheModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cpu = new CacheIO(addrWidth, dataWidth)
  val nasti = new NastiBundle(nastiParams)
}

case class CacheConfig(nWays: Int, nSets: Int, bytesPerBlock: Int, cacheable: UInt => Bool, xlen: Int) {
  require(nWays == 1)
  val bitsPerBlock = bytesPerBlock * 8
  val blockBits = log2Ceil(bytesPerBlock)
  val setBits = log2Ceil(nSets)
  val tagBits = xlen - (setBits + blockBits)
  val bytesPerWord = xlen / 8
  val wordsPerBlock = bytesPerBlock / bytesPerWord
  val byteOffsetBits = log2Ceil(bytesPerWord)
  assert(blockBits + setBits + tagBits == xlen)
}

class Address(p: CacheConfig) extends Bundle {
  val tag = UInt(p.tagBits.W)
  val set = UInt(p.setBits.W)
  val wordOffset = UInt((p.blockBits - p.byteOffsetBits).W)
  val byteOffset = UInt(p.byteOffsetBits.W)
}

class MetaData(tagLength: Int) extends Bundle {
  val tag = UInt(tagLength.W)
}

object CacheState extends ChiselEnum {
  val sIdle, sReadCache, sWriteCache, sWriteBack, sWriteAck, sRefillReady, sRefill = Value
}

class Cache(val p: CacheConfig, val nasti: NastiBundleParameters) extends Module {
  val dataBeats = p.bitsPerBlock / nasti.dataBits
  require(dataBeats > 0)

  val io = IO(new CacheModuleIO(nasti, addrWidth = p.xlen, dataWidth = p.xlen))

  // cache states
  import CacheState._
  val state = RegInit(sIdle)
  // memory
  val valid = RegInit(0.U(p.nSets.W))
  val dirty = RegInit(0.U(p.nSets.W))
  val metaMem = SyncReadMem(p.nSets, new MetaData(tagLength=p.tagBits))
  val dataMem = Seq.fill(p.wordsPerBlock)(SyncReadMem(p.nSets, Vec(p.bytesPerWord, UInt(8.W))))

  val cpu_req_reg = Reg(chiselTypeOf(io.cpu.req.bits))

  // Counters
  val (read_count, read_wrap_out) = Counter(0 until dataBeats, io.nasti.r.fire,
    reset=uncached && io.cpu.req.valid && state === sRefill && io.nasti.r.fire)
  val (write_count, write_wrap_out) = Counter(io.nasti.w.fire, dataBeats)

  val is_idle = state === sIdle
  val is_read = state === sReadCache
  val is_write = state === sWriteCache
  val is_alloc = state === sRefill && read_wrap_out
  val is_alloc_reg = RegNext(is_alloc)

  val hit = Wire(Bool())
  val wen = is_write && (hit || is_alloc_reg) && !io.cpu.abort || is_alloc
  val ren = !wen && (is_idle || is_read) && io.cpu.req.valid
  val ren_reg = RegNext(ren)

  val uncached = !p.cacheable(io.cpu.req.bits.addr)
  val set_idx = io.cpu.req.bits.addr.asTypeOf(new Address(p)).set
  val tag_reg = cpu_req_reg.asTypeOf(new Address(p)).tag
  val idx_reg = cpu_req_reg.asTypeOf(new Address(p)).wordOffset
    cpu_req_reg.addr(slen + blen - 1, blen)
  val off_reg = cpu_req_reg.addr(blen - 1, byteOffsetBits)

  val rmeta = metaMem.read(set_idx, ren)
  val rdata = Cat((dataMem.map(_.read(set_idx, ren).asUInt)).reverse)
  val rdata_buf = RegEnable(rdata, ren_reg)
  val refill_buf = Reg(Vec(dataBeats, UInt(nasti.dataBits.W)))
  val read = Mux(is_alloc_reg, refill_buf.asUInt, Mux(ren_reg, rdata, rdata_buf))

  hit := valid(idx_reg) && rmeta.tag === tag_reg

  // Read Mux
  io.cpu.resp.bits.data := VecInit.tabulate(nWords)(i => read((i + 1) * xlen - 1, i * xlen))(off_reg)
  io.cpu.resp.valid := is_idle || is_read && hit || is_alloc_reg && !cpu_mask.orR

  // Hold the CPU request data as the cache is about to move out of idle
  when(io.cpu.resp.valid) {
    cpu_req_reg := io.cpu.req.bits
  }

  val wmeta = Wire(new MetaData(p.tagBits))
  wmeta.tag := tag_reg

  val wmask = Mux(!is_alloc, (cpu_mask << Cat(off_reg, 0.U(byteOffsetBits.W))).zext, (-1).S)
  val wdata = Mux(
    !is_alloc,
    Fill(nWords, cpu_data),
    if (refill_buf.size == 1) io.nasti.r.bits.data
    else Cat(io.nasti.r.bits.data, Cat(refill_buf.init.reverse))
  )
  when(wen) {
    valid := valid.bitSet(idx_reg, true.B)
    dirty := dirty.bitSet(idx_reg, !is_alloc)
    when(is_alloc) {
      metaMem.write(idx_reg, wmeta)
    }
    dataMem.zipWithIndex.foreach {
      case (mem, i) =>
        val data = VecInit.tabulate(wBytes)(k => wdata(i * xlen + (k + 1) * 8 - 1, i * xlen + k * 8))
        mem.write(idx_reg, data, wmask((i + 1) * wBytes - 1, i * wBytes).asBools)
        mem.suggestName(s"dataMem_${i}")
    }
  }

  io.nasti.ar.bits := NastiAddressBundle(nasti)(
    id=0.U,
    addr=(Cat(tag_reg, idx_reg) << blen.U).asUInt,
    size=log2Up(nasti.dataBits / 8).U,
    len=Mux(uncached, 0.U, (dataBeats - 1).U) // fetch only 1 beat for uncached accesses otherwise a full cache line
  )
  io.nasti.ar.valid := false.B
  // read data
  io.nasti.r.ready := state === sRefill
  when(io.nasti.r.fire) {
    refill_buf(read_count) := io.nasti.r.bits.data
  }

  // write addr
  io.nasti.aw.bits := NastiAddressBundle(nasti)(
    id=0.U,
    addr=(Cat(rmeta.tag, idx_reg) << blen.U).asUInt,
    size=log2Up(nasti.dataBits / 8).U,
    len=(dataBeats - 1).U
  )
  io.nasti.aw.valid := false.B
  // write data
  io.nasti.w.bits := NastiWriteDataBundle(nasti)(
    VecInit.tabulate(dataBeats)(i => read((i + 1) * nasti.dataBits - 1, i * nasti.dataBits))(write_count),
    None,
    write_wrap_out
  )
  io.nasti.w.valid := false.B
  // write resp
  io.nasti.b.ready := false.B

  // Cache FSM
  val is_dirty = valid(idx_reg) && dirty(idx_reg)
  switch(state) {
    is(sIdle) {
      when(io.cpu.req.valid) {
        state := Mux(io.cpu.req.bits.mask.orR, sWriteCache, sReadCache)
      }
    }
    is(sReadCache) {
      when(hit) {
        when(io.cpu.req.valid) { // Another request was received and we can service it next cycle
          state := Mux(io.cpu.req.bits.mask.orR, sWriteCache, sReadCache)
        }.otherwise {
          state := sIdle
        }
      }.elsewhen(uncached) {
        io.nasti.ar.valid := true.B
        when(io.nasti.ar.fire) {
          state := sRefill
        }
      }.otherwise {
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire) {
          state := sWriteBack
        }.elsewhen(io.nasti.ar.fire) {
          state := sRefill
        }
      }
    }
    is(sWriteCache) {
      when(hit || is_alloc_reg || io.cpu.abort) {
        state := sIdle
      }.otherwise {
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire) {
          state := sWriteBack
        }.elsewhen(io.nasti.ar.fire) {
          state := sRefill
        }
      }
    }
    is(sWriteBack) {
      io.nasti.w.valid := true.B
      when(write_wrap_out) {
        state := sWriteAck
      }
    }
    is(sWriteAck) {
      io.nasti.b.ready := true.B
      when(io.nasti.b.fire) {
        state := sRefillReady
      }
    }
    is(sRefillReady) {
      io.nasti.ar.valid := true.B
      when(io.nasti.ar.fire) {
        state := sRefill
      }
    }
    is(sRefill) {
      when(read_wrap_out) {
        state := Mux(cpu_mask.orR, sWriteCache, sIdle)
      }
    }
  }
}
