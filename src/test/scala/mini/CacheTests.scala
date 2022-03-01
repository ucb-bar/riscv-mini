// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import chisel3.testers._
import junctions._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class GoldCache()

class GoldCacheIO(p: CacheConfig, nastiParams: NastiBundleParameters, xlen: Int) extends Bundle {
  val req = Flipped(Decoupled(new CacheReq(xlen, xlen)))
  val resp = Decoupled(new CacheResp(xlen))
  val nasti = new NastiBundle(nastiParams)
}

object GoldCacheState extends ChiselEnum {
  val sIdle, sWrite, sWriteAck, sRead = Value
}

class GoldCache(p: CacheConfig, nasti: NastiBundleParameters, xlen: Int) extends Module {
  // local parameters
  val nSets = p.nSets
  val bBytes = p.bytesPerBlock
  val bBits = bBytes << 3
  val blen = log2Ceil(bBytes)
  val slen = log2Ceil(nSets)
  val tlen = xlen - (slen + blen)
  val nWords = bBits / xlen
  val wBytes = xlen / 8
  val byteOffsetBits = log2Ceil(wBytes)
  val dataBeats = bBits / nasti.dataBits

  val io = IO(new GoldCacheIO(p, nasti, xlen))
  val size = log2Ceil(nasti.dataBits / 8).U
  val len = (dataBeats - 1).U

  val dataMemory = Mem(nSets, UInt(bBits.W))
  val tags = Mem(nSets, UInt(tlen.W))
  val v = Mem(nSets, Bool())
  val d = Mem(nSets, Bool())

  val req = io.req.bits
  val tag = (req.addr >> (blen + slen).U).asUInt
  val idx = req.addr(blen + slen - 1, blen)
  val off = req.addr(blen - 1, 0)

  val readData = dataMemory(idx)
  val writeData = (0 until bBytes).foldLeft(0.U) { (write, i) =>
    write | {
      val condition = ((off / 4.U) === (i / 4).U) && (req.mask >> (i & 0x3).U)(0)
      val trueClause = (((req.data >> (8 * (i & 0x3)).U).asUInt & 0xff.U).asUInt << (8 * BigInt(i)).U).asUInt
      val falseClause = readData & (BigInt(0xff) << (8 * i)).U
      Mux(condition, trueClause, falseClause)
    }
  }(bBits - 1, 0)

  import GoldCacheState._
  val state = RegInit(sIdle)
  val (wCnt, wDone) = Counter(state === sWrite, dataBeats)
  val (rCnt, rDone) = Counter(state === sRead && io.nasti.r.valid, dataBeats)

  io.resp.bits.data := readData >> ((off / 4.U) * xlen.U)
  io.resp.valid := false.B
  io.req.ready := false.B
  io.nasti.ar.bits := NastiAddressBundle(nasti)(0.U, ((req.addr >> blen.U).asUInt << blen.U).asUInt, size, len)
  io.nasti.ar.valid := false.B
  io.nasti.aw.bits := NastiAddressBundle(nasti)(0.U, (Cat(tags(idx), idx) << blen.U).asUInt, size, len)
  io.nasti.aw.valid := false.B
  io.nasti.w.bits := NastiWriteDataBundle(nasti)((readData >> (wCnt * nasti.dataBits.U)).asUInt, None, wDone)
  io.nasti.w.valid := state === sWrite
  io.nasti.b.ready := state === sWriteAck
  io.nasti.r.ready := state === sRead

  switch(state) {
    is(sIdle) {
      when(io.req.valid && io.resp.ready) {
        when(v(idx) && (tags(idx) === tag)) {
          when(req.mask.orR) {
            d(idx) := true.B
            dataMemory(idx) := writeData
            printf(
              "[cache] data[%x] <= writeData %x, readData %x, off: %x, req: %x, mask: %b\n",
              idx,
              writeData,
              readData,
              off,
              io.req.bits.data,
              io.req.bits.mask
            )
          }.otherwise {
            printf("[cache] data[%x] => %x, off: %x, resp: %x\n", idx, readData, off, io.resp.bits.data)
          }
          io.req.ready := true.B
          io.resp.valid := true.B
        }.otherwise {
          when(d(idx)) {
            io.nasti.aw.valid := true.B
            state := sWrite
          }.otherwise {
            dataMemory(idx) := 0.U
            io.nasti.ar.valid := true.B
            state := sRead
          }
        }
      }
    }
    is(sWrite) {
      when(wDone) {
        state := sWriteAck
      }
    }
    is(sWriteAck) {
      when(io.nasti.b.valid) {
        dataMemory(idx) := 0.U
        io.nasti.ar.valid := true.B
        state := sRead
      }
    }
    is(sRead) {
      when(io.nasti.r.valid) {
        dataMemory(idx) := readData | (io.nasti.r.bits.data << (rCnt * nasti.dataBits.U)).asUInt
      }
      when(rDone) {
        assert(io.nasti.r.bits.last)
        tags(idx) := tag
        v(idx) := true.B
        state := sIdle
      }
    }
  }
}

object CacheTesterState extends ChiselEnum {
  val sInit, sStart, sWait, sDone = Value
}

object CacheTesterMemState extends ChiselEnum {
  val sMemIdle, sMemWrite, sMemWrAck, sMemRead = Value
}

class CacheTester(cache: => Cache) extends BasicTester {
  /* Target Design */
  val dut = Module(cache)
  // extract parameters from dut
  val p = dut.p
  val xlen = dut.xlen
  val nasti = dut.nasti
  val nSets = p.nSets
  val bBytes = p.bytesPerBlock
  val bBits = bBytes << 3
  val blen = log2Ceil(bBytes)
  val slen = log2Ceil(nSets)
  val tlen = xlen - (slen + blen)
  val nWords = bBits / xlen
  val wBytes = xlen / 8
  val byteOffsetBits = log2Ceil(wBytes)
  val dataBeats = bBits / nasti.dataBits
  val dut_mem = Wire(new NastiBundle(nasti))
  dut_mem.ar <> Queue(dut.io.nasti.ar, 32)
  dut_mem.aw <> Queue(dut.io.nasti.aw, 32)
  dut_mem.w <> Queue(dut.io.nasti.w, 32)
  dut.io.nasti.b <> Queue(dut_mem.b, 32)
  dut.io.nasti.r <> Queue(dut_mem.r, 32)

  /* Gold Model */
  val gold = Module(new GoldCache(p, nasti, xlen))
  val gold_req = WireInit(gold.io.req)
  val gold_resp = WireInit(gold.io.resp)
  val gold_mem = Wire(new NastiBundle(nasti))
  gold.io.req <> Queue(gold_req, 32)
  gold_resp <> Queue(gold.io.resp, 32)
  gold_mem.ar <> Queue(gold.io.nasti.ar, 32)
  gold_mem.aw <> Queue(gold.io.nasti.aw, 32)
  gold_mem.w <> Queue(gold.io.nasti.w, 32)
  gold.io.nasti.b <> Queue(gold_mem.b, 32)
  gold.io.nasti.r <> Queue(gold_mem.r, 32)

  val size = log2Ceil(nasti.dataBits / 8).U
  val len = (dataBeats - 1).U

  /* Main Memory */
  val mem = Mem(1 << 20, UInt(nasti.dataBits.W))
  import CacheTesterMemState._
  val memState = RegInit(sMemIdle)
  val (wCnt, wDone) = Counter(memState === sMemWrite && dut_mem.w.valid && gold_mem.w.valid, dataBeats)
  val (rCnt, rDone) = Counter(memState === sMemRead && dut_mem.r.ready && gold_mem.r.ready, dataBeats)

  dut_mem.ar.ready := false.B
  dut_mem.aw.ready := false.B
  dut_mem.w.ready := false.B
  dut_mem.b.valid := memState === sMemWrAck
  dut_mem.b.bits := NastiWriteResponseBundle(nasti)(0.U)
  dut_mem.r.valid := memState === sMemRead
  dut_mem.r.bits := NastiReadDataBundle(nasti)(0.U, mem((gold_mem.ar.bits.addr >> size).asUInt + rCnt), rDone)
  gold_mem.ar.ready := dut_mem.ar.ready
  gold_mem.aw.ready := dut_mem.aw.ready
  gold_mem.w.ready := dut_mem.w.ready
  gold_mem.b.valid := dut_mem.b.valid
  gold_mem.b.bits := dut_mem.b.bits
  gold_mem.r.valid := dut_mem.r.valid
  gold_mem.r.bits := dut_mem.r.bits

  switch(memState) {
    is(sMemIdle) {
      when(gold_mem.aw.valid && dut_mem.aw.valid) {
        assert(
          dut_mem.aw.bits.id === gold_mem.aw.bits.id,
          "* dut.io.nasti.aw.bits.id => %x != %x *\n",
          dut_mem.aw.bits.id,
          gold_mem.aw.bits.id
        )
        assert(
          gold_mem.aw.bits.addr === dut_mem.aw.bits.addr,
          "* dut.io.nasti.aw.bits.addr => %x != %x *\n",
          dut_mem.aw.bits.addr,
          gold_mem.aw.bits.addr
        )
        assert(
          gold_mem.aw.bits.size === dut_mem.aw.bits.size,
          "* dut.io.nasti.aw.bits.size => %x != %x *\n",
          dut_mem.aw.bits.size,
          gold_mem.aw.bits.size
        )
        assert(
          gold_mem.aw.bits.len === dut_mem.aw.bits.len,
          "* dut.io.nasti.aw.bits.len => %x != %x *\n",
          dut_mem.aw.bits.len,
          gold_mem.aw.bits.len
        )
        memState := sMemWrite
      }.elsewhen(gold_mem.ar.valid && dut_mem.ar.valid) {
        assert(
          dut_mem.ar.bits.id === gold_mem.ar.bits.id,
          "* dut.io.nasti.ar.bits.id => %x != %x *\n",
          dut_mem.ar.bits.id,
          gold_mem.ar.bits.id
        )
        assert(
          gold_mem.ar.bits.addr === dut_mem.ar.bits.addr,
          "* dut.io.nasti.ar.bits.addr => %x != %x *\n",
          dut_mem.ar.bits.addr,
          gold_mem.ar.bits.addr
        )
        assert(
          gold_mem.ar.bits.size === dut_mem.ar.bits.size,
          "* dut.io.nasti.ar.bits.size => %x != %x *\n",
          dut_mem.ar.bits.size,
          gold_mem.ar.bits.size
        )
        assert(
          gold_mem.ar.bits.len === dut_mem.ar.bits.len,
          "* dut.io.nasti.ar.bits.len => %x != %x *\nn",
          dut_mem.ar.bits.len,
          gold_mem.ar.bits.len
        )
        memState := sMemRead
      }
    }
    is(sMemWrite) {
      assert(dut_mem.aw.bits.size === size)
      assert(dut_mem.aw.bits.len === len)
      when(gold_mem.w.valid && dut_mem.w.valid) {
        assert(
          dut_mem.w.bits.data === gold_mem.w.bits.data,
          "* dut.io.nasti.w.bits.data => %x != %x *\n",
          dut_mem.w.bits.data,
          gold_mem.w.bits.data
        )
        assert(
          dut_mem.w.bits.strb === gold_mem.w.bits.strb,
          "* dut.io.nasti.w.bits.strb => %x != %x *\n",
          dut_mem.w.bits.strb,
          gold_mem.w.bits.strb
        )
        assert(
          dut_mem.w.bits.last === gold_mem.w.bits.last,
          "* dut.io.nasti.w.bits.last => %x != %x *\n",
          dut_mem.w.bits.last,
          gold_mem.w.bits.last
        )
        assert(dut_mem.w.bits.strb === ((1 << (nasti.dataBits / 8)) - 1).U) // TODO: release it?
        mem((dut_mem.aw.bits.addr >> size).asUInt + wCnt) := dut_mem.w.bits.data
        printf("[write] mem[%x] <= %x\n", (dut_mem.aw.bits.addr >> size).asUInt + wCnt, dut_mem.w.bits.data)
        dut_mem.w.ready := true.B
      }
      when(wDone) {
        dut_mem.aw.ready := true.B
        memState := sMemWrAck
      }
    }
    is(sMemWrAck) {
      when(gold_mem.b.ready && dut_mem.b.ready) {
        memState := sMemIdle
      }
    }
    is(sMemRead) {
      when(dut_mem.r.ready && gold_mem.r.ready) {
        printf("[read] mem[%x] => %x\n", (dut_mem.ar.bits.addr >> size).asUInt + rCnt, dut_mem.r.bits.data)
      }
      when(rDone) {
        dut_mem.ar.ready := true.B
        memState := sMemIdle
      }
    }
  }

  /* Tests */
  val seed = System.currentTimeMillis()
  val rnd = new scala.util.Random(seed)
  println(s"CacheTester using seed $seed")

  def rand_tag = rnd.nextInt(1 << tlen).U(tlen.W)
  def rand_idx = rnd.nextInt(1 << slen).U(slen.W)
  def rand_off = (rnd.nextInt(1 << blen) & -4).U(blen.W)
  def rand_data = (0 until (nasti.dataBits / 8))
    .foldLeft(BigInt(0))((r, i) => r | (BigInt(rnd.nextInt(0xff + 1)) << (8 * i)))
    .U(nasti.dataBits.W)
  def rand_mask = (rnd.nextInt((1 << (xlen / 8)) - 1) + 1).U((xlen / 8).W)
  def test(tag: UInt, idx: UInt, off: UInt, mask: UInt = 0.U((xlen / 8).W)) =
    Cat(mask, Cat(Seq.fill(bBits / nasti.dataBits)(rand_data)), tag, idx, off)

  val tags = Vector.fill(3)(rand_tag)
  val idxs = Vector.fill(2)(rand_idx)
  val offs = Vector.fill(6)(rand_off)

  val initAddr = for {
    tag <- tags
    idx <- idxs
    off <- 0 until dataBeats
  } yield Cat(tag, idx, off.U)
  val initData = Seq.fill(initAddr.size)(rand_data)
  val testVec = Seq(
    test(tags(0), idxs(0), offs(0)), // #0: read miss
    test(tags(0), idxs(0), offs(1)), // #1: read hit
    test(tags(1), idxs(0), offs(0)), // #2: read miss
    test(tags(1), idxs(0), offs(2)), // #3: read hit
    test(tags(1), idxs(0), offs(3)), // #4: read hit
    test(tags(1), idxs(0), offs(4), rand_mask), // #5: write hit
    test(tags(1), idxs(0), offs(4)), // #6: read hit
    test(tags(2), idxs(0), offs(5)), // #7: read miss & write back
    test(tags(0), idxs(1), offs(0), rand_mask), // #8: write miss
    test(tags(0), idxs(1), offs(0)), // #9: read hit
    test(tags(0), idxs(1), offs(1)), // #10: read hit
    test(tags(1), idxs(1), offs(2), rand_mask), // #11: write miss & write back
    test(tags(1), idxs(1), offs(3)), // #12: read hit
    test(tags(2), idxs(1), offs(4)), // #13: read write back
    test(tags(2), idxs(1), offs(5)) // #14: read hit
  )

  import CacheTesterState._
  val state = RegInit(sInit)
  val timeout = Reg(UInt(32.W))
  val (initCnt, initDone) = Counter(state === sInit, initAddr.size)
  val (testCnt, testDone) = Counter(state === sDone, testVec.size)
  val mask = (VecInit(testVec)(testCnt) >> (blen + slen + tlen + bBits)).asUInt
  val data = (VecInit(testVec)(testCnt) >> (blen + slen + tlen))(bBits - 1, 0)
  val tag = (VecInit(testVec)(testCnt) >> (blen + slen).U)(tlen - 1, 0)
  val idx = (VecInit(testVec)(testCnt) >> blen.U)(slen - 1, 0)
  val off = VecInit(testVec)(testCnt)(blen - 1, 0)
  dut.io.cpu.req.bits.addr := Cat(tag, idx, off)
  dut.io.cpu.req.bits.data := data
  dut.io.cpu.req.bits.mask := mask
  dut.io.cpu.req.valid := state === sWait
  dut.io.cpu.abort := DontCare
  gold_req.bits := dut.io.cpu.req.bits
  gold_req.valid := state === sStart
  gold_resp.ready := state === sDone

  switch(state) {
    is(sInit) {
      mem(VecInit(initAddr)(initCnt)) := VecInit(initData)(initCnt)
      printf("[init] mem[%x] <= %x\n", VecInit(initAddr)(initCnt), VecInit(initData)(initCnt))
      when(initDone) {
        state := sStart
      }
    }
    is(sStart) {
      when(gold_req.ready) {
        timeout := 0.U
        state := sWait
      }
    }
    is(sWait) {
      timeout := timeout + 1.U
      assert(timeout < 100.U)
      when(dut.io.cpu.resp.valid && gold_resp.valid) {
        when(!mask.orR) {
          assert(
            dut.io.cpu.resp.bits.data === gold_resp.bits.data,
            "* dut.io.cpu.resp.bits.data => %x ?= %x *\n",
            dut.io.cpu.resp.bits.data,
            gold_resp.bits.data
          )
        }
        state := sDone
      }
    }
    is(sDone) {
      state := sStart
    }
  }

  when(testDone) { stop() }
}

class CacheTests extends AnyFlatSpec with ChiselScalatestTester {
  val p = MiniConfig()

  // TODO: add a test to check CacheConfig has the right derived values via manual calculation

  "Cache" should "pass with verilator" in {
    test(new CacheTester(new Cache(p.cache, p.nasti, p.core.xlen)))
      .withAnnotations(Seq(VerilatorBackendAnnotation))
      .runUntilStop()
  }

  "Cache" should "pass with treadle" in {
    test(new CacheTester(new Cache(p.cache, p.nasti, p.core.xlen))).runUntilStop()
  }
}
