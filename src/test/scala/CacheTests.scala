package mini

import Chisel._
import Chisel.AdvTester._
import junctions.{MemReqCmd, MemData, MemResp}
import scala.collection.mutable.{Queue => ScalaQueue}

class GoldCache(nSets: Int, bBytes: Int, xlen: Int) extends Processable {
  val cpu_reqs = ScalaQueue[TestCacheReq]()
  val cpu_resps = ScalaQueue[TestCacheResp]()
  val mem_cmds = ScalaQueue[TestMemReq]() 
  val mem_data = ScalaQueue[TestMemData]()
  val mem_resps = ScalaQueue[TestMemResp]()

  private val waitQ = ScalaQueue[TestCacheReq]()  
  private val data = Array.fill(nSets){BigInt(0)}
  private val tags = Array.fill(nSets){0}
  private val v    = Array.fill(nSets){false}
  private val d    = Array.fill(nSets){false}
  private val blen = log2Up(bBytes)
  private val slen = log2Up(nSets)
  private val dataMask = (BigInt(1) << xlen) - 1

  def process {
    if (!cpu_reqs.isEmpty) {
      val req = cpu_reqs.dequeue
      val off =  req.addr          & ((1 << blen)-1)
      val idx = (req.addr >> blen) & ((1 << slen)-1)
      val tag = (req.addr >> (blen + slen))
      val read = data(idx)
      if (v(idx) && tags(idx) == tag) {
        if (req.mask != 0) {
          var write = BigInt(0)
          for (i <- 0 until bBytes) {
            if (i >> 2 == off >> 2 && (((req.mask >> (i&0x3))) & 0x1) == 1) {
              write |= (req.data >> 8*(i&0x3)) << 8*i
            } else {
              write |= read & (BigInt(0xff) << 8*i)
            }
          }
          data(idx) = write
          v(idx)    = true
          d(idx)    = true 
          cpu_resps enqueue new TestCacheResp(BigInt(0))
        } else {
          cpu_resps enqueue new TestCacheResp((read >> (xlen * (off >> 2))) & dataMask)
        }
      } else {
        if (d(idx)) {
          val addr = (tags(idx) << slen | idx) & ((1<<(xlen-blen))-1)
          mem_cmds enqueue new TestMemReq(addr, 0, true)
          mem_data enqueue new TestMemData(data(idx))
        }
        mem_cmds enqueue new TestMemReq(req.addr >>> blen, 0, false)
        waitQ enqueue req
      }
    } 
    if (!mem_resps.isEmpty) {
      val resp = mem_resps.dequeue
      assert(!waitQ.isEmpty)
      val req = waitQ.dequeue
      val off =  req.addr          & ((1 << blen)-1)
      val idx = (req.addr >> blen) & ((1 << slen)-1)
      val tag = (req.addr >> (blen + slen))
      data(idx) = resp.data
      tags(idx) = tag
      v(idx)    = true
      cpu_reqs enqueue req
      process
    } 
  } 
}

class MainMem(
    cacheCmdQ: ScalaQueue[TestMemReq], goldCmdQ: ScalaQueue[TestMemReq],
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
      assert(cacheCmd.addr == goldCmd.addr, "\n*Cache* => %s\n*Gold* => %s".format(cacheCmd, goldCmd))
      assert(cacheData.data == cacheData.data, "\n*Cache => %s\n*Gold* => %s".format(cacheData, goldData))
      write(cacheCmd.addr, cacheData.data)
    } else if (!cacheCmdQ.isEmpty && !cacheCmdQ.front.rw &&
               !goldCmdQ.isEmpty  && !goldCmdQ.front.rw) {
      val cacheCmd  = cacheCmdQ.dequeue
      val goldCmd   = goldCmdQ.dequeue
      assert(cacheCmd.addr == goldCmd.addr, "\n*Cache* => %s\n*Gold* => %s".format(cacheCmd, goldCmd))
      val data = read(cacheCmd.addr)
      val resp = new TestMemResp(data, cacheCmd.tag)
      cacheRespQ enqueue resp
      goldRespQ enqueue resp
    }
  }
}

class CacheTests(c: Cache, log: Option[java.io.PrintStream] = None) extends AdvTester(c, log == None) {
  val req_h = new ValidSource(c.io.cpu.req,
    (req: CacheReq, in: TestCacheReq) => { 
      reg_poke(req.addr, in.addr) 
      reg_poke(req.data, in.data)
      reg_poke(req.mask, in.mask)})
  val resp_h = new ValidSink(c.io.cpu.resp,
    (resp: CacheResp) => new TestCacheResp(peek(resp.data)))
  val cmd_h = new DecoupledSink(c.io.mem.req_cmd,
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  val data_h = new DecoupledSink(c.io.mem.req_data,
    (cmd: MemData) => new TestMemData(peek(cmd.data)))
  val mem_resp_h = new DecoupledSource(c.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
 
  val gold = new GoldCache(c.nSets, c.bBytes, c.xlen)
  val mem = new MainMem(cmd_h.outputs, gold.mem_cmds, data_h.outputs, gold.mem_data,
                        mem_resp_h.inputs, gold.mem_resps, log, c.bBytes)
  preprocessors += gold
  preprocessors += mem
  
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
      mem.write(tag << c.slen | idx, rand_data)
    }
  }

  private var testCnt = 0
  def test(tag: Int, idx: Int, off: Int, mask: Int = 0) = {
    addEvent(new DumpEvent(s"***** TEST ${testCnt} *****"))
    val req = new TestCacheReq(addr(tag, idx, off), int(rnd.nextInt), mask)
    req_h.inputs enqueue req
    gold.cpu_reqs enqueue req
    addEvent(new DumpEvent(req.toString))

    // cache access
    req_h.process
    takestep{resp_h.outputs.clear}    

    if (eventually(!resp_h.outputs.isEmpty && !gold.cpu_resps.isEmpty, 10)) {
      val cacheResp = resp_h.outputs.dequeue
      val goldResp  = gold.cpu_resps.dequeue
      if (mask == 0) {
        assert(cacheResp.data == goldResp.data, 
          s"\n*Cache* => ${cacheResp}\n*Gold* => ${goldResp}")
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
  test(tags(0), idxs(0), offs(0)) // read miss
  test(tags(0), idxs(0), offs(1)) // read hit
  test(tags(1), idxs(0), offs(0)) // read hit
  test(tags(1), idxs(0), offs(2)) // read miss
  test(tags(1), idxs(0), offs(3)) // read hit
  test(tags(1), idxs(0), offs(4), rand_mask) // write hit
  test(tags(1), idxs(0), offs(4)) // read hit
  test(tags(2), idxs(0), offs(5)) // read miss & write back
  test(tags(0), idxs(1), offs(0), rand_mask) // write miss
  test(tags(0), idxs(1), offs(0)) // read hit
  test(tags(0), idxs(1), offs(1)) // read hit
  test(tags(1), idxs(1), offs(2), rand_mask) // write miss & write back
  test(tags(1), idxs(1), offs(3)) // read hit
  test(tags(2), idxs(1), offs(4)) // read write back
  test(tags(2), idxs(1), offs(5)) // read hit
}
