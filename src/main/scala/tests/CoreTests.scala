package mini

import Chisel._
import Chisel.AdvTester._
import scala.collection.mutable.{Queue => ScalaQueue}

case class TestCacheReq(addr: Int, data: BigInt, mask: BigInt) {
  override def toString = "[Cache Req] addr: %x, data: %x, mask: %x".format(addr, data, mask)
}
case class TestCacheResp(data: BigInt) {
  override def toString = "[Cache Resp] data: %x".format(data)
}

class CoreMem(ireqQ: ScalaQueue[TestCacheReq], irespQ: ScalaQueue[TestCacheResp],
              dreqQ: ScalaQueue[TestCacheReq], drespQ: ScalaQueue[TestCacheResp], abort: => BigInt,
              dasm: UInt => String, verbose: Boolean, word_width: Int = 4, depth: Int = 1 << 20) extends SimMem(word_width, depth) {
  var isWr = false
  var wrAddr = 0
  var wrData = BigInt(0)
  def process {
    if (isWr && abort == 0) {
      if (verbose) println("MEM[%x] <- %s".format(wrAddr, wrData))
      write(wrAddr, wrData)
    } 
    isWr = false

    if (!ireqQ.isEmpty) {
      val ireq = ireqQ.dequeue
      val inst = read(ireq.addr>>off)
      if (verbose) println("MEM[%x] -> %s".format(ireq.addr, dasm(UInt(inst))))
      irespQ.enqueue(new TestCacheResp(inst))
    } else {
      irespQ.enqueue(new TestCacheResp(BigInt(0)))
    }

    if (!dreqQ.isEmpty && dreqQ.front.mask != 0) {
      val dreq = dreqQ.dequeue
      wrAddr = dreq.addr >> off
      wrData = BigInt(0)
      for (i <- 0 until word_width) {
        if (((dreq.mask >> i) & 0x1) == 1) {
          wrData |= dreq.data & (BigInt(0xff) << 8*i)
        } else {
          wrData |= read(dreq.addr>>off) & (BigInt(0xff) << 8*i)
        }
      }
      isWr = true
      drespQ.enqueue(new TestCacheResp(BigInt(0)))
    } else if (!dreqQ.isEmpty) {
      val dreq = dreqQ.dequeue
      val data = read(dreq.addr>>off)
      if (verbose) println("MEM[%x] -> %s".format(dreq.addr, data))
      drespQ.enqueue(new TestCacheResp(data))
    } else {
      drespQ.enqueue(new TestCacheResp(BigInt(0)))
    }
  }
}

class CoreTester(c: Core, args: Array[String]) extends AdvTester(c, false) with MemTests {
  val (file, tests, maxcycles, verbose) = parseOpts(args)
  val ireqHandler = new ValidSink(c.io.icache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val dreqHandler = new ValidSink(c.io.dcache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val irespHandler = new ValidSource(c.io.icache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val drespHandler = new ValidSource(c.io.dcache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val mem = new CoreMem(ireqHandler.outputs, irespHandler.inputs, dreqHandler.outputs, drespHandler.inputs, peek(c.io.dcache.abort), dasm, verbose, 4)
  preprocessors += mem
  def regFile(x: Int) = peekAt(c.dpath.regFile.regs, x)
  def loadMem(testname: String) = mem.loadMem(testname)
  def loadMem(test: Seq[UInt]) = mem.loadMem(test)
  def runTests(maxcycles: Int, verbose: Boolean) {
    cycles = 0
    wire_poke(c.io.icache.resp.valid, true)
    wire_poke(c.io.dcache.resp.valid, true)
    ireqHandler.process()
    dreqHandler.process()
    mem.process()
    irespHandler.process()
    drespHandler.process()
    ok &= run(c.io.host, maxcycles, verbose)
  }
  start(file, tests, maxcycles, verbose)
}
