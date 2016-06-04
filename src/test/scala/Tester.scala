package mini

import Chisel._
import Chisel.iotesters._
import junctions._
import scala.collection.mutable.{Queue => ScalaQueue}
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
case class TestNastiReadAddr(id: Int, addr: Int, size: Int, len: Int) {
  override def toString = "[NastiReadAddr] id: %x, addr: %x, size: %x, len: %x".format(id, addr, size, len)
}
case class TestNastiWriteAddr(id: Int, addr: Int, size: Int, len: Int) {
  override def toString = "[NastiWriteAddr] id: %x, addr: %x, size: %x, len: %x".format(id, addr, size, len)
}
case class TestNastiReadData(id: Int, data: BigInt, last: Boolean) {
  override def toString = "[NastiReadData] id: %x, data: %x, last: %s".format(id, data, last)
}
case class TestNastiWriteData(data: BigInt, last: Boolean) {
  override def toString = "[NastiWriteData] data: %x, last: %s".format(data, last)
}
case class TestNastiWriteResp(id: Int, resp: Int) {
  override def toString = "[Nasti Write Resp] id: %x, resp: %x".format(id, resp)
}

abstract class SimMem(word_width: Int = 4, depth: Int = 1 << 20, verbose: Boolean = true)(
    implicit logger: PrintStream) extends Processable {
  require(word_width % 4 == 0, "word_width should be divisible by 4")
  implicit def toBigInt(x: UInt) = x.litValue()
  private val addrMask = (1 << log2Up(depth))-1
  private val mem = Array.fill(depth){BigInt(0)}
  private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1
  private def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'
  protected val off = log2Up(word_width)

  def read(addr: Int) = {
    val data = mem(addr & addrMask)
    if (verbose) logger println "MEM[%x] => %x".format(addr & addrMask, data)
    data
  }
  def write(addr: Int, data: BigInt) {
    if (verbose) logger println "MEM[%x] <= %x".format(addr & addrMask, data)
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
  implicit def boolToBoolean(x: Bool) = x.litValue() == 1
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def uintToBigInt(x: UInt) = x.litValue()

  def run(host: HostIO, maxcycles: Long) = {
    val startTime = System.nanoTime
    val ok = eventually(peek(host.tohost), maxcycles)
    val tohost = peek(host.tohost)
    val endTime = System.nanoTime
    val simTime = (endTime - startTime) / 1000000000.0
    val simSpeed = cycles / simTime
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok && tohost == 1) "PASSED" else "FAILED", reason, cycles))
    println("Time elapsed = %.1f s, Simulation Speed = %.2f Hz".format(simTime, simSpeed))
    ok && tohost == 1
  }
}

class CoreMem(
    ireqQ: ScalaQueue[TestCacheReq], irespQ: ScalaQueue[TestCacheResp],
    dreqQ: ScalaQueue[TestCacheReq], drespQ: ScalaQueue[TestCacheResp], abort: => BigInt, 
    word_width: Int = 4, depth: Int = 1 << 20, verbose: Boolean = true)(implicit logger: PrintStream)
    extends SimMem(word_width, depth, verbose)(logger) with RISCVCommon {
  private var isWr = false
  private var wrAddr = 0
  private var wrData = BigInt(0)
  def process {
    if (isWr && abort == 0) {
      if (verbose) logger println "MEM[%x] <- %s".format(wrAddr, wrData)
      write(wrAddr, wrData)
    } 
    isWr = false

    if (!ireqQ.isEmpty) {
      val ireq = ireqQ.dequeue
      val inst = read(ireq.addr>>off)
      if (verbose) logger println "MEM[%x] -> %s".format(ireq.addr, dasm(UInt(inst)))
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
      logger println "MEM[%x] -> %s".format(dreq.addr, data)
      drespQ.enqueue(new TestCacheResp(data))
    } else {
      drespQ.enqueue(new TestCacheResp(BigInt(0)))
    }
  }
}

case class MiniTestArgs(
  loadmem: String, 
  maxcycles: Long = 500000, 
  logFile: Option[String] = None,
  waveform: Option[String] = None,
  testCmd: Option[String] = None,
  verbose: Boolean = false)

class CoreTester(c: Core, args: MiniTestArgs) extends AdvTester(c, args.verbose, 
    logFile=args.logFile, waveform=args.waveform) with MiniTests {
  val ireqHandler = new ValidSink(c.io.icache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val dreqHandler = new ValidSink(c.io.dcache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val irespHandler = new ValidSource(c.io.icache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val drespHandler = new ValidSource(c.io.dcache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val mem = new CoreMem(ireqHandler.outputs, irespHandler.inputs, 
    dreqHandler.outputs, drespHandler.inputs, peek(c.io.dcache.abort), 
    4, verbose=args.verbose)

  mem loadMem args.loadmem
  preprocessors += mem
  wire_poke(c.io.icache.resp.valid, true)
  wire_poke(c.io.dcache.resp.valid, true)
  ireqHandler.process()
  dreqHandler.process()
  mem.process()
  irespHandler.process()
  drespHandler.process()
  if (!run(c.io.host, args.maxcycles)) fail
}

class TileMagicMem(
    cmdQ: ScalaQueue[TestMemReq], dataQ: ScalaQueue[TestMemData], respQ: ScalaQueue[TestMemResp],
    beats: Int, databits: Int, depth: Int = 1 << 20, verbose: Boolean = true)(
    implicit logger: PrintStream) extends SimMem(beats*databits/8, depth, verbose)(logger) {
  private val mask = (BigInt(1) << databits) - 1
  def process {
    if (!cmdQ.isEmpty && cmdQ.front.rw && dataQ.size >= beats) {
      val cmd  = cmdQ.dequeue
      write(cmd.addr, ((0 until beats) foldLeft BigInt(0))((data, i) =>
        data | dataQ.dequeue.data << i*databits))
    } else if (!cmdQ.isEmpty && !cmdQ.front.rw) {
      val cmd  = cmdQ.dequeue
      val data = read(cmd.addr)
      (0 until beats) foreach (i => respQ enqueue 
        new TestMemResp((data >> i*databits) & mask, cmd.tag))
    }
  }
}

class NastiMagicMem(
    arQ: ScalaQueue[TestNastiReadAddr],  rQ: ScalaQueue[TestNastiReadData],
    awQ: ScalaQueue[TestNastiWriteAddr], wQ: ScalaQueue[TestNastiWriteData],
    word_width: Int = 16, depth: Int = 1 << 20, verbose: Boolean = true)(
    implicit logger: PrintStream) extends SimMem(word_width, depth, verbose)(logger) {
  private var aw: Option[TestNastiWriteAddr] = None
  def process = aw match {
    case Some(p) if wQ.size > p.len =>
      assert((1 << p.size) == word_width)
      (0 to p.len) foreach (i => 
        write((p.addr >> off) + i, wQ.dequeue.data)) 
      aw = None
    case None if !awQ.isEmpty => aw = Some(awQ.dequeue)
    case None if !arQ.isEmpty =>
      val ar = arQ.dequeue
      (0 to ar.len) foreach (i =>
        rQ enqueue new TestNastiReadData(
          ar.id, read((ar.addr >> off) + i), i == ar.len))
    case _ =>
  }
}
     
class TileMem(
    cmdQ: ScalaQueue[TestMemReq], dataQ: ScalaQueue[TestMemData], respQ: ScalaQueue[TestMemResp],
    latency: Int, beats: Int, databits: Int, depth: Int = 1 << 20, verbose: Boolean = true)(
    implicit logger: PrintStream) extends SimMem(beats*databits/8, depth, verbose)(logger) {
  private val mask = (BigInt(1) << databits) - 1
  private val schedule = Array.fill(latency){ScalaQueue[TestMemResp]()}
  private var cur_cycle = 0
  def process {
    if (!cmdQ.isEmpty && cmdQ.front.rw && dataQ.size >= beats) {
      val cmd = cmdQ.dequeue
      write(cmd.addr, ((0 until beats) foldLeft BigInt(0))((data, i) =>
        data | dataQ.dequeue.data << i*databits))
    } else if (!cmdQ.isEmpty && !cmdQ.front.rw) {
      val cmd = cmdQ.dequeue
      val data = read(cmd.addr)
      (0 until beats) foreach (i =>
        schedule((cur_cycle+latency-1) % latency) enqueue 
          new TestMemResp((data >> i*databits) & mask, cmd.tag))
    }
    while (!schedule(cur_cycle).isEmpty) { 
      respQ enqueue schedule(cur_cycle).dequeue 
    }
    cur_cycle = (cur_cycle + 1) % latency
  }
}

class NastiMem(
    arQ: ScalaQueue[TestNastiReadAddr],  rQ: ScalaQueue[TestNastiReadData],
    awQ: ScalaQueue[TestNastiWriteAddr], wQ: ScalaQueue[TestNastiWriteData],
    latency: Int, word_width: Int = 16, depth: Int = 1 << 20, verbose: Boolean = true)(
    implicit logger: PrintStream) extends SimMem(word_width, depth, verbose)(logger) {
  private var aw: Option[TestNastiWriteAddr] = None
  private val schedule = Array.fill(latency){ScalaQueue[TestNastiReadData]()}
  private var cur_cycle = 0
  def process { 
    aw match {
      case Some(p) if wQ.size > p.len =>
        assert((1 << p.size) == word_width)
        (0 to p.len) foreach (i => 
          write((p.addr >> off) + i, wQ.dequeue.data)) 
        aw = None
      case None if !awQ.isEmpty => aw = Some(awQ.dequeue)
      case None if !arQ.isEmpty =>
        val ar = arQ.dequeue
        (0 to ar.len) foreach (i =>
          schedule((cur_cycle+latency-1) % latency) enqueue 
            new TestNastiReadData(ar.id, read((ar.addr >> off) + i), i == ar.len))
      case _ =>
    }
    while (!schedule(cur_cycle).isEmpty) {
      rQ enqueue schedule(cur_cycle).dequeue
    }
    cur_cycle = (cur_cycle + 1) % latency
  }
}

class TileTester(c: Tile, args: MiniTestArgs) extends AdvTester(c, args.verbose,
    logFile=args.logFile, waveform=args.waveform) with MiniTests {
  lazy val cmdHandler = new DecoupledSink(c.io.mem.req_cmd, (cmd: MemReqCmd) => 
    new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  lazy val dataHandler = new DecoupledSink(c.io.mem.req_data, (data: MemData) => 
    new TestMemData(peek(data.data)))
  lazy val respHandler = new DecoupledSource(c.io.mem.resp, (resp: MemResp, in: TestMemResp) => 
    {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  lazy val mem = new TileMem(
    cmdHandler.outputs, dataHandler.outputs, respHandler.inputs, 
    5, c.icache.mifDataBeats, c.icache.mifDataBits, verbose=args.verbose)
  
  lazy val arHandler = new DecoupledSink(c.io.nasti.ar, (ar: NastiReadAddressChannel) =>
    new TestNastiReadAddr(peek(ar.id), peek(ar.addr), peek(ar.size), peek(ar.len)))
  lazy val awHandler = new DecoupledSink(c.io.nasti.aw, (aw: NastiWriteAddressChannel) =>
    new TestNastiWriteAddr(peek(aw.id), peek(aw.addr), peek(aw.size), peek(aw.len)))
  lazy val wHandler = new DecoupledSink(c.io.nasti.w, (w: NastiWriteDataChannel) =>
    new TestNastiWriteData(peek(w.data), peek(w.last)))
  lazy val rHandler = new DecoupledSource(c.io.nasti.r, 
    (r: NastiReadDataChannel, in: TestNastiReadData) => 
      {reg_poke(r.id, in.id) ; reg_poke(r.data, in.data) ; reg_poke(r.last, in.last)})
  lazy val nasti = new NastiMem(
    arHandler.outputs, rHandler.inputs,
    awHandler.outputs, wHandler.outputs,
    5, c.icache.nastiXDataBits/8, verbose=args.verbose)

  if (c.core.useNasti) {
    nasti loadMem args.loadmem
    preprocessors += nasti
    arHandler.process()
    awHandler.process()
    rHandler.process()
    wHandler.process()
  } else {
    mem loadMem args.loadmem
    preprocessors += mem
    cmdHandler.process()
    dataHandler.process()
    respHandler.process()
  }
  if (!run(c.io.host, args.maxcycles)) fail
}
