package mini

import Chisel._
import Chisel.AdvTester._
import scala.collection.mutable.{Queue => ScalaQueue}
import junctions.{MemReqCmd, MemData, MemResp}
import java.io.PrintStream

case class TestCacheReq(addr: Int, data: BigInt, mask: BigInt) {
  override def toString = "[Cache Req] addr: %x, data: %x, mask: %x".format(addr, data, mask)
}
case class TestCacheResp(data: BigInt) {
  override def toString = "[Cache Resp] data: %x".format(data)
}
case class TestMemReq(addr: Int, tag: BigInt, rw: Boolean) {
  override def toString = "[Mem Req] %s addr: %x, tag: %x".format(if (rw) "write" else "read", addr, tag)
}
case class TestMemData(data: BigInt) {
  override def toString = "[Mem Data] data: %x".format(data)
}
case class TestMemResp(data: BigInt, tag: BigInt) {
  override def toString = "[Mem Data] data: %x, tag: %x".format(data, tag)
}

abstract class SimMem(word_width: Int = 4, depth: Int = 1 << 20, 
    log: Option[PrintStream] = None) extends Processable {
  require(word_width % 4 == 0, "word_width should be divisible by 4")
  implicit def toBigInt(x: UInt) = x.litValue()
  private val addrMask = (1 << log2Up(depth))-1
  protected val off = log2Up(word_width)
  private val mem = Array.fill(depth){BigInt(0)}
  private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1
  private def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'
  protected val f = log getOrElse System.out

  def read(addr: Int) = {
    val data = mem(addr & addrMask)
    f.println("MEM[%x] => %x".format(addr & addrMask, data))
    data
  }
  def write(addr: Int, data: BigInt) { 
    f.println("MEM[%x] <= %x".format(addr & addrMask, data))
    mem(addr & addrMask) = data 
  }
  def loadMem(test: Seq[UInt]) {
    val chunk = word_width / 4
    (0 until (test.size / chunk)) foreach {i =>
      write(i, ((0 until chunk) foldLeft BigInt(0))((res, k) => 
                 res | (test(i*chunk+k) << 32*(chunk-1-k))))
    }
  }

  def loadMem(filename: String) {
    val lines = io.Source.fromFile(filename).getLines
    for ((line, i) <- lines.zipWithIndex) {
      val base = (i * line.length) / 2
      assert(base % word_width == 0)
      ((0 until line.length by 2) foldRight (BigInt(0), 0)){case (k, (data, offset)) =>
        val shift = 8 * (offset % word_width)
        val byte = ((parseNibble(line(k)) << 4) | parseNibble(line(k+1))).toByte
        if ((offset % word_width) == word_width - 1) {
          mem((base+offset)>>off) = data | int(byte) << shift
          (BigInt(0), offset + 1)
        } else {
          (data | int(byte) << shift, offset + 1)
        }
      }
    }
  }
}

trait MiniTests extends AdvTests {
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  implicit def booleanToBigInt(x: Boolean) = if (x) BigInt(1) else BigInt(0)
  implicit def boolToBoolean(x: Bool) = x.isTrue
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def uintToBigInt(x: UInt) = x.litValue()

  def run(host: HostIO, maxcycles: Long, f: PrintStream) = {
    val startTime = System.nanoTime
    val ok = eventually(peek(host.tohost), maxcycles)
    val tohost = peek(host.tohost)
    val endTime = System.nanoTime
    val simTime = (endTime - startTime) / 1000000000.0
    val simSpeed = cycles / simTime
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    f.println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, cycles))
    f.println("Time elapsed = %.1f s, Simulation Speed = %.2f Hz".format(simTime, simSpeed))
    ok
  }
}

class CoreMem(
    ireqQ: ScalaQueue[TestCacheReq], irespQ: ScalaQueue[TestCacheResp],
    dreqQ: ScalaQueue[TestCacheReq], drespQ: ScalaQueue[TestCacheResp], abort: => BigInt, 
    log: Option[PrintStream] = None, word_width: Int = 4, depth: Int = 1 << 20) 
    extends SimMem(word_width, depth, log) with RISCVCommon {
  private var isWr = false
  private var wrAddr = 0
  private var wrData = BigInt(0)
  def process {
    if (isWr && abort == 0) {
      f.println("MEM[%x] <- %s".format(wrAddr, wrData))
      write(wrAddr, wrData)
    } 
    isWr = false

    if (!ireqQ.isEmpty) {
      val ireq = ireqQ.dequeue
      val inst = read(ireq.addr>>off)
      f.println("MEM[%x] -> %s".format(ireq.addr, dasm(UInt(inst))))
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
      f.println("MEM[%x] -> %s".format(dreq.addr, data))
      drespQ.enqueue(new TestCacheResp(data))
    } else {
      drespQ.enqueue(new TestCacheResp(BigInt(0)))
    }
  }
}

case class MiniTestArgs(
  loadmem: String, 
  maxcycles: Long = 500000, 
  testCmd: Option[String] = Driver.testCommand,
  log: Option[java.io.PrintStream] = None,
  dumpFile: Option[String] = None)

class CoreTester(c: Core, args: MiniTestArgs) extends AdvTester(
    c, testCmd=args.testCmd, dumpFile=args.dumpFile) with MiniTests {
  val ireqHandler = new ValidSink(c.io.icache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val dreqHandler = new ValidSink(c.io.dcache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val irespHandler = new ValidSource(c.io.icache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val drespHandler = new ValidSource(c.io.dcache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val mem = new CoreMem(ireqHandler.outputs, irespHandler.inputs, 
    dreqHandler.outputs, drespHandler.inputs, peek(c.io.dcache.abort), args.log, 4)

  args.log match {
    case None =>
    case Some(f) => addObserver(new Observer(file=f))
  }

  mem loadMem args.loadmem
  preprocessors += mem
  wire_poke(c.io.icache.resp.valid, true)
  wire_poke(c.io.dcache.resp.valid, true)
  ireqHandler.process()
  dreqHandler.process()
  mem.process()
  irespHandler.process()
  drespHandler.process()
  if (!run(c.io.host, args.maxcycles, args.log getOrElse System.out)) fail
}

class TileMem(
    cmdQ: ScalaQueue[TestMemReq], dataQ: ScalaQueue[TestMemData], respQ: ScalaQueue[TestMemResp],
    log: Option[PrintStream], word_width: Int = 16, depth: Int = 1 << 20) 
    extends SimMem(word_width, depth, log) {
  def process {
    if (!cmdQ.isEmpty && !dataQ.isEmpty && cmdQ.front.rw) {
      val cmd = cmdQ.dequeue
      val data = dataQ.dequeue
      write(cmd.addr, data.data)
    } else if (!cmdQ.isEmpty && !cmdQ.front.rw) {
      val cmd = cmdQ.dequeue
      respQ enqueue new TestMemResp(read(cmd.addr), cmd.tag)
    } 
  }
}

class TileSlowMem(
    cmdQ: ScalaQueue[TestMemReq], dataQ: ScalaQueue[TestMemData], respQ: ScalaQueue[TestMemResp],
    log: Option[PrintStream], latency: Int, word_width: Int = 16, depth: Int = 1 << 20) 
    extends SimMem(word_width, depth, log) {
  private val schedule = Array.fill(latency){ScalaQueue[TestMemResp]()}
  private var cur_cycle = 0
  def process {
    if (!cmdQ.isEmpty && !dataQ.isEmpty && cmdQ.front.rw) {
      val cmd = cmdQ.dequeue
      val data = dataQ.dequeue
      write(cmd.addr, data.data)
    } else if (!cmdQ.isEmpty && !cmdQ.front.rw) {
      val cmd = cmdQ.dequeue
      val resp = new TestMemResp(read(cmd.addr), cmd.tag)
      schedule((cur_cycle+latency-1) % latency) enqueue resp
    }
    while (!schedule(cur_cycle).isEmpty) { 
      respQ enqueue schedule(cur_cycle).dequeue 
    }
    cur_cycle = (cur_cycle + 1) % latency
  }
}
     
class TileTester(c: Tile, args: MiniTestArgs) extends AdvTester(
    c, testCmd=args.testCmd, dumpFile=args.dumpFile) with MiniTests {
  val cmdHandler = new DecoupledSink(c.io.mem.req_cmd, 
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  val dataHandler = new DecoupledSink(c.io.mem.req_data, 
    (data: MemData) => new TestMemData(peek(data.data)))
  val respHandler = new DecoupledSource(c.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  val mem = new TileSlowMem(cmdHandler.outputs, dataHandler.outputs, respHandler.inputs, args.log, 5, 16)

  args.log match {
    case None =>
    case Some(f) => addObserver(new Observer(file=f))
  }

  mem loadMem args.loadmem
  preprocessors += mem
  cmdHandler.process()
  dataHandler.process()
  respHandler.process()
  if (!run(c.io.htif.host, args.maxcycles, args.log getOrElse System.out)) fail
}
