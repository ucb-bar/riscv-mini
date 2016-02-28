package mini

import Chisel._
import Chisel.AdvTester._
import junctions._
import scala.collection.mutable.{Queue => ScalaQueue}

class GoldCache(nSets: Int, bBytes: Int, xlen: Int, nBytes: Int, 
    useNasti: Boolean=false) extends Processable {
  val cpu_reqs = ScalaQueue[TestCacheReq]()
  val cpu_resps = ScalaQueue[TestCacheResp]()
  val mem_cmds = ScalaQueue[TestMemReq]() 
  val mem_data = ScalaQueue[TestMemData]()
  val mem_resps = ScalaQueue[TestMemResp]()
  val nasti_ar = ScalaQueue[TestNastiReadAddr]()
  val nasti_aw = ScalaQueue[TestNastiWriteAddr]()
  val nasti_r = ScalaQueue[TestNastiReadData]()
  val nasti_w = ScalaQueue[TestNastiWriteData]()
  val nasti_b = ScalaQueue[TestNastiWriteResp]()
  private val waitQ = ScalaQueue[TestCacheReq]()  
  private val data = Array.fill(nSets){BigInt(0)}
  private val tags = Array.fill(nSets){0}
  private val v    = Array.fill(nSets){false}
  private val d    = Array.fill(nSets){false}
  private val blen = log2Up(bBytes)
  private val slen = log2Up(nSets)
  private val nsize  = log2Up(nBytes)
  private val nlen   = bBytes/nBytes
  private val dataMask = (BigInt(1) << xlen) - 1
  require(nlen > 0)

  def process {
    if (!cpu_reqs.isEmpty) {
      val req = cpu_reqs.dequeue
      val off =  req.addr          & ((1 << blen)-1)
      val idx = (req.addr >> blen) & ((1 << slen)-1)
      val tag = (req.addr >> (blen + slen))
      val read = data(idx)
      if (v(idx) && tags(idx) == tag) {
        if (req.mask != 0) {
          data(idx) = ((0 until bBytes) foldLeft BigInt(0)){(write, i) =>
            if (i >> 2 == off >> 2 && (((req.mask >> (i&0x3))) & 0x1) == 1) {
              write | (req.data >> 8*(i&0x3)) << 8*i
            } else {
              write | read & (BigInt(0xff) << 8*i)
            }
          }
          v(idx) = true
          d(idx) = true 
          cpu_resps enqueue new TestCacheResp(BigInt(0))
        } else {
          cpu_resps enqueue new TestCacheResp((read >> (xlen * (off >> 2))) & dataMask)
        }
      } else {
        if (useNasti) {
          if (d(idx)) {
            val addr = ((tags(idx) << slen | idx)) << blen
            val size = 8 * (1 << nsize)
            val mask = (BigInt(1) << size) - 1
            nasti_aw enqueue new TestNastiWriteAddr(0, addr, nsize, nlen)
            (0 until nlen) foreach { i =>
              val w_data = (data(idx) >> (i * size)) & mask
              nasti_w enqueue new TestNastiWriteData(w_data, i == (nlen-1))
            }
          }
          nasti_ar enqueue new TestNastiReadAddr(0, (req.addr >>> blen) << blen, nsize, nlen)
        } else {
          if (d(idx)) {
            val addr = (tags(idx) << slen | idx) & ((1<<(xlen-blen))-1)
            mem_cmds enqueue new TestMemReq(addr, 0, true)
            mem_data enqueue new TestMemData(data(idx))
          }
          mem_cmds enqueue new TestMemReq(req.addr >>> blen, 0, false)
        }
        waitQ enqueue req
      }
    } 
    if (!mem_resps.isEmpty || nasti_r.size >= nlen) {
      assert(!waitQ.isEmpty, s"nasti_r.size = ${nasti_r.size}")
      assert(mem_resps.isEmpty && useNasti)
      val req = waitQ.dequeue
      val idx = (req.addr >> blen) & ((1 << slen)-1)
      val tag = (req.addr >> (blen + slen))
      v(idx)    = true
      tags(idx) = tag
      data(idx) = if (useNasti) {
        ((0 until nlen) foldLeft BigInt(0)){(res, i) =>
          val read = nasti_r.dequeue
          assert(i != (nlen-1) || read.last, s"[${i}] NastReadData: ${read}")
          res | (read.data << (i * 8 * (1 << nsize)))
        }
      } else mem_resps.dequeue.data
      cpu_reqs enqueue req
      process
    }
  } 
}

class MemIOMem(
    cacheCmdQ:  ScalaQueue[TestMemReq],  goldCmdQ: ScalaQueue[TestMemReq],
    cacheDataQ: ScalaQueue[TestMemData], goldDataQ: ScalaQueue[TestMemData],
    cacheRespQ: ScalaQueue[TestMemResp], goldRespQ: ScalaQueue[TestMemResp],
    log: Option[java.io.PrintStream] = None, word_width: Int = 16, depth: Int = 1 << 20) 
    extends SimMem(word_width, depth, log) {
  def process {
    if (!cacheCmdQ.isEmpty && !cacheDataQ.isEmpty && cacheCmdQ.front.rw &&
        !goldCmdQ.isEmpty  && !goldDataQ.isEmpty  && goldCmdQ.front.rw) {
      val cacheCmd  = cacheCmdQ.dequeue
      val goldCmd   = goldCmdQ.dequeue
      val cacheData = cacheDataQ.dequeue
      val goldData  = goldDataQ.dequeue
      assert(cacheCmd.addr == goldCmd.addr, 
        s"\n*Cache* => ${cacheCmd}\n*Gold*  => ${goldCmd}")
      assert(cacheData.data == cacheData.data, 
        s"\n*Cache => ${cacheData}\n*Gold*  => ${goldData}")
      write(cacheCmd.addr, cacheData.data)
    } else if (!cacheCmdQ.isEmpty && !cacheCmdQ.front.rw &&
               !goldCmdQ.isEmpty  && !goldCmdQ.front.rw) {
      val cacheCmd = cacheCmdQ.dequeue
      val goldCmd  = goldCmdQ.dequeue
      assert(cacheCmd.addr == goldCmd.addr, 
        s"\n*Cache* => ${cacheCmd}\n*Gold*  => ${goldCmd}")
      val data = read(cacheCmd.addr)
      val resp = new TestMemResp(data, cacheCmd.tag)
      cacheRespQ enqueue resp
      goldRespQ  enqueue resp
    }
  }
}

class NastiIOMem(
    cache_ar_Q: ScalaQueue[TestNastiReadAddr],  gold_ar_Q: ScalaQueue[TestNastiReadAddr],
    cache_aw_Q: ScalaQueue[TestNastiWriteAddr], gold_aw_Q: ScalaQueue[TestNastiWriteAddr],
    cache_r_Q: ScalaQueue[TestNastiReadData],   gold_r_Q: ScalaQueue[TestNastiReadData],
    cache_w_Q: ScalaQueue[TestNastiWriteData],  gold_w_Q: ScalaQueue[TestNastiWriteData],
    log: Option[java.io.PrintStream] = None, word_width: Int = 16, depth: Int = 1 << 20) 
    extends SimMem(word_width, depth, log) {
  var aw: Option[TestNastiWriteAddr] = None
  def process = aw match {
    case Some(p) if cache_w_Q.size >= p.len && gold_w_Q.size >= p.len =>
      val addr = p.addr >> off
      val size = 8 * (1 << p.size)
      write(addr, ((0 until p.len) foldLeft BigInt(0)){(data, i) =>
        val cache_w = cache_w_Q.dequeue
        val gold_w  = gold_w_Q.dequeue
        assert(cache_w == gold_w, 
          s"\n*Cache* => ${cache_w}\n*Gold*  => ${gold_w}")
        assert(i != p.len || cache_w.last, cache_w.toString)
        data | (cache_w.data << size)
      })
      aw = None
    case None if !cache_aw_Q.isEmpty && !gold_aw_Q.isEmpty =>
      val cache_aw = cache_aw_Q.dequeue
      val gold_aw  = gold_aw_Q.dequeue
      assert(cache_aw == gold_aw, 
        s"\n*Cache* => ${cache_aw}\n*Gold*  => ${gold_aw}")
      aw = Some(cache_aw)
    case None if !cache_ar_Q.isEmpty && !gold_ar_Q.isEmpty =>
      val cache_ar = cache_ar_Q.dequeue
      val gold_ar  = gold_ar_Q.dequeue
      assert(cache_ar == gold_ar, 
        s"\n*Cache* => ${cache_ar}\n*Gold*  => ${gold_ar}")
      val size = 8 * (1 << cache_ar.size)
      val addr = cache_ar.addr >> off
      val mask = (BigInt(1) << size) - 1
      val data = read(addr)
      (0 until cache_ar.len) foreach { i =>
        val r_data = new TestNastiReadData(
          cache_ar.id, (data >> (i * size)) & mask, i == (cache_ar.len-1))
        cache_r_Q enqueue r_data
        gold_r_Q  enqueue r_data
      }
    case _ =>
  }
}

class CacheTests(c: Cache, log: Option[java.io.PrintStream] = None) extends AdvTester(c, log == None) {
  implicit def bigIntToInt(b: BigInt) = b.toInt
  implicit def bigIntToBoolean(b: BigInt) = b != BigInt(0)
  implicit def booleanToBigInt(b: Boolean) = if (b) BigInt(1) else BigInt(0)
  // CacheIO
  val req_h = new ValidSource(c.io.cpu.req, (req: CacheReq, in: TestCacheReq) => { 
    reg_poke(req.addr, in.addr) ; reg_poke(req.data, in.data) ; reg_poke(req.mask, in.mask)})
  val resp_h = new ValidSink(c.io.cpu.resp,
    (resp: CacheResp) => new TestCacheResp(peek(resp.data)))
  // MemIO
  lazy val cmd_h = new DecoupledSink(c.io.mem.req_cmd,
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  lazy val data_h = new DecoupledSink(c.io.mem.req_data,
    (cmd: MemData) => new TestMemData(peek(cmd.data)))
  lazy val mem_resp_h = new DecoupledSource(c.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  // NastiIO
  lazy val ar_h = new DecoupledSink(c.io.nasti.ar, 
    (ar: NastiReadAddressChannel) => new TestNastiReadAddr(
      peek(ar.id), peek(ar.addr), peek(ar.size), peek(ar.len)))
  lazy val aw_h = new DecoupledSink(c.io.nasti.aw, 
    (aw: NastiWriteAddressChannel) => new TestNastiWriteAddr(
      peek(aw.id), peek(aw.addr), peek(aw.size), peek(aw.len)))
  lazy val w_h = new DecoupledSink(c.io.nasti.w, (w: NastiWriteDataChannel) => 
    new TestNastiWriteData(peek(w.data), peek(w.last)))
  lazy val r_h = new DecoupledSource(c.io.nasti.r, (r: NastiReadDataChannel, in: TestNastiReadData) => {
    reg_poke(r.id, in.id) ; reg_poke(r.data, in.data) ; reg_poke(r.last, in.last)})
 
  val gold = new GoldCache(c.nSets, c.bBytes, c.xlen, c.nastiXDataBits/8, c.useNasti)
  lazy val mem = new MemIOMem(
    cmd_h.outputs,     gold.mem_cmds, 
    data_h.outputs,    gold.mem_data,
    mem_resp_h.inputs, gold.mem_resps, 
    log, c.bBytes)
  lazy val nasti = new NastiIOMem(
    ar_h.outputs, gold.nasti_ar,
    aw_h.outputs, gold.nasti_aw,
    r_h.inputs,   gold.nasti_r,
    w_h.outputs,  gold.nasti_w,
    log, c.bBytes)
  preprocessors += gold
  preprocessors += (if (c.useNasti) nasti else mem)
  
  log match {
    case None =>
    case Some(f) => addObserver(new Observer(file=f))
  }

  def rand_tag = rnd.nextInt(1 << c.tlen)
  def rand_idx = rnd.nextInt(1 << c.slen)
  def rand_off = rnd.nextInt(1 << c.blen) & -4
  def rand_data = ((0 until c.bBytes) foldLeft BigInt(0))((r, i) => 
                    r | (BigInt(rnd.nextInt() & 0xff) << 8*i))
  def rand_mask = (1 << (c.xlen / 8)) - 1

  def addr(tag: Int, idx: Int, off: Int) = ((tag << c.slen | idx) << c.blen) | off

  def init(tags: Vector[Int], idxs: Vector[Int]) {
    for (tag <- tags ; idx <- idxs) {
      if (c.useNasti) {
        nasti.write((tag << c.slen | idx), rand_data)
      } else {
        mem.write(tag << c.slen | idx, rand_data)
      }
    }
  }

  private var testCnt = 0
  def test(tag: Int, idx: Int, off: Int, mask: Int = 0) = {
    addEvent(new DumpEvent(s"***** TEST ${testCnt} *****"))
    val req = new TestCacheReq(addr(tag, idx, off), int(rnd.nextInt), mask)
    gold.cpu_reqs enqueue req
    req_h.inputs  enqueue req
    addEvent(new DumpEvent(req.toString))

    // cache access
    req_h.process
    takestep{resp_h.outputs.clear}    

    if (eventually(!resp_h.outputs.isEmpty && !gold.cpu_resps.isEmpty, 10)) {
      val cacheResp = resp_h.outputs.dequeue
      val goldResp  = gold.cpu_resps.dequeue
      if (mask == 0) {
        assert(cacheResp.data == goldResp.data, 
          s"\n*Cache* => ${cacheResp}\n*Gold*  => ${goldResp}")
        addEvent(new DumpEvent(cacheResp.toString))
      }
    }
    resp_h.outputs.clear
    gold.cpu_resps.clear
    testCnt += 1
  }

  val tags = Vector(rand_tag, rand_tag, rand_tag)
  val idxs = Vector(rand_idx, rand_idx)
  val offs = Vector(rand_off, rand_off, rand_off, rand_off, rand_off, rand_off)

  init(tags, idxs)
  test(tags(0), idxs(0), offs(0)) // #0: read miss
  test(tags(0), idxs(0), offs(1)) // #1: read hit
  test(tags(1), idxs(0), offs(0)) // #2: read hit
  test(tags(1), idxs(0), offs(2)) // #3: read miss
  test(tags(1), idxs(0), offs(3)) // #4: read hit
  test(tags(1), idxs(0), offs(4), rand_mask) // #5: write hit
  test(tags(1), idxs(0), offs(4)) // #6: read hit
  test(tags(2), idxs(0), offs(5)) // #7: read miss & write back
  test(tags(0), idxs(1), offs(0), rand_mask) // #8: write miss
  test(tags(0), idxs(1), offs(0)) // #9: read hit
  test(tags(0), idxs(1), offs(1)) // #10: read hit
  test(tags(1), idxs(1), offs(2), rand_mask) // #11: write miss & write back
  test(tags(1), idxs(1), offs(3)) // #12: read hit
  test(tags(2), idxs(1), offs(4)) // #13: read write back
  test(tags(2), idxs(1), offs(5)) // #14: read hit
}
