package mini

import Chisel._
import Chisel.swtesters._
import junctions._
import scala.collection.mutable.{Queue => ScalaQueue}

class GoldCache(nSets: Int, bBytes: Int, xlen: Int, nastiBytes: Int) extends Processable {
  val cpu_reqs = ScalaQueue[TestCacheReq]()
  val cpu_resps = ScalaQueue[TestCacheResp]()
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
  private val nastiSize = log2Up(nastiBytes)
  private val nastiLen  = bBytes/nastiBytes - 1
  private val dataMask = (BigInt(1) << xlen) - 1
  require(nastiLen >= 0)

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
        if (d(idx)) {
          val addr = ((tags(idx) << slen | idx)) << blen
          val size = 8 * (1 << nastiSize)
          val mask = (BigInt(1) << size) - 1
          nasti_aw enqueue new TestNastiWriteAddr(0, addr, nastiSize, nastiLen)
          (0 to nastiLen) foreach { i =>
            val w_data = (data(idx) >> (i * size)) & mask
            nasti_w enqueue new TestNastiWriteData(w_data, i == nastiLen)
          }
        }
        nasti_ar enqueue new TestNastiReadAddr(0, (req.addr >>> blen) << blen, nastiSize, nastiLen)
        waitQ enqueue req
      }
    } 
    if (nasti_r.size > nastiLen) {
      assert(!waitQ.isEmpty)
      val req = waitQ.dequeue
      val idx = (req.addr >> blen) & ((1 << slen)-1)
      val tag = (req.addr >> (blen + slen))
      v(idx)    = true
      tags(idx) = tag
      data(idx) = ((0 to nastiLen) foldLeft BigInt(0)){(res, i) =>
        val read = nasti_r.dequeue
        assert(i != nastiLen || read.last, s"[${i}] NastReadData: ${read}")
        res | (read.data << (i * 8 * (1 << nastiSize)))
      }
      cpu_reqs enqueue req
      process
    }
  } 
}

class Mem(
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
      (0 to cache_ar.len) foreach { i =>
        val r_data = new TestNastiReadData(
          cache_ar.id, (data >> (i * size)) & mask, i == cache_ar.len)
        cache_r_Q enqueue r_data
        gold_r_Q  enqueue r_data
      }
    case _ =>
  }
}

class CacheTests(c: Cache, log: Option[java.io.PrintStream] = None) extends AdvTester(c) {
  implicit def bigIntToInt(b: BigInt) = b.toInt
  implicit def bigIntToBoolean(b: BigInt) = b != BigInt(0)
  implicit def booleanToBigInt(b: Boolean) = if (b) BigInt(1) else BigInt(0)
  // CacheIO
  val req_h = new ValidSource(c.io.cpu.req, (req: CacheReq, in: TestCacheReq) => { 
    reg_poke(req.addr, in.addr) ; reg_poke(req.data, in.data) ; reg_poke(req.mask, in.mask)})
  val resp_h = new ValidSink(c.io.cpu.resp,
    (resp: CacheResp) => new TestCacheResp(peek(resp.data)))
  // NastiIO
  val ar_h = new DecoupledSink(c.io.nasti.ar, 
    (ar: NastiReadAddressChannel) => new TestNastiReadAddr(
      peek(ar.id), peek(ar.addr), peek(ar.size), peek(ar.len)))
  val aw_h = new DecoupledSink(c.io.nasti.aw, 
    (aw: NastiWriteAddressChannel) => new TestNastiWriteAddr(
      peek(aw.id), peek(aw.addr), peek(aw.size), peek(aw.len)))
  val w_h = new DecoupledSink(c.io.nasti.w, (w: NastiWriteDataChannel) => 
    new TestNastiWriteData(peek(w.data), peek(w.last)))
  val r_h = new DecoupledSource(c.io.nasti.r, (r: NastiReadDataChannel, in: TestNastiReadData) => {
    reg_poke(r.id, in.id) ; reg_poke(r.data, in.data) ; reg_poke(r.last, in.last)})
 
  val gold = new GoldCache(c.nSets, c.bBytes, c.xlen, c.nastiXDataBits/8)
  val mem = new Mem(
    ar_h.outputs, gold.nasti_ar,
    aw_h.outputs, gold.nasti_aw,
    r_h.inputs,   gold.nasti_r,
    w_h.outputs,  gold.nasti_w,
    log, c.bBytes)
  preprocessors += gold
  preprocessors += mem
  
  def rand_tag = rnd.nextInt(1 << c.tlen)
  def rand_idx = rnd.nextInt(1 << c.slen)
  def rand_off = rnd.nextInt(1 << c.blen) & -4
  def rand_data = ((0 until c.bBytes) foldLeft BigInt(0))((r, i) => 
                    r | (BigInt(rnd.nextInt() & 0xff) << 8*i))
  def rand_mask = (1 << (c.xlen / 8)) - 1

  def addr(tag: Int, idx: Int, off: Int) = ((tag << c.slen | idx) << c.blen) | off

  def init(tags: Vector[Int], idxs: Vector[Int]) {
    for (tag <- tags ; idx <- idxs) {
      mem.write(tag << c.slen | idx, rand_data)
    }
  }

  private var testCnt = 0
  def test(tag: Int, idx: Int, off: Int, mask: Int = 0) = {
    println(s"***** TEST ${testCnt} *****")
    val req = new TestCacheReq(addr(tag, idx, off), int(rnd.nextInt), mask)
    gold.cpu_reqs enqueue req
    req_h.inputs  enqueue req
    println(req.toString)

    // cache access
    req_h.process
    takestep{resp_h.outputs.clear}    

    if (eventually(!resp_h.outputs.isEmpty && !gold.cpu_resps.isEmpty, 10)) {
      val cacheResp = resp_h.outputs.dequeue
      val goldResp  = gold.cpu_resps.dequeue
      if (mask == 0) {
        assert(cacheResp.data == goldResp.data, 
          s"\n*Cache* => ${cacheResp}\n*Gold*  => ${goldResp}")
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
