package mini

import Chisel._
import Chisel.AdvTester._
import scala.collection.mutable.{Queue => ScalaQueue}
import junctions.{MemReqCmd, MemData, MemResp}

object RISCVCommon {
  import Instructions._
  implicit def boolToBoolean(x: Bool) = x.isTrue
  def rs1(inst: UInt) = ((inst.litValue() >> 15) & 0x1f).toInt
  def rs2(inst: UInt) = ((inst.litValue() >> 20) & 0x1f).toInt
  def rd (inst: UInt) = ((inst.litValue() >> 7)  & 0x1f).toInt
  def csr(inst: UInt) =  (inst.litValue() >> 20)
  def reg(x: Int) = UInt(x, 5)
  def imm(x: Int) = SInt(x, 21)
  val fin   = Cat(CSR.mtohost, reg(1), Funct3.CSRRWI, reg(0), Opcode.SYSTEM)
  val fence = Cat(UInt(0, 4), UInt(0xf, 4), UInt(0xf, 4), UInt(0, 13), Opcode.MEMORY)
  val nop   = Cat(UInt(0, 12), reg(0), Funct3.ADD, reg(0), Opcode.ITYPE)
  val csrRegs = CSR.regs map (_.litValue())
  private val csrMap  = (csrRegs zip List(
    "cycle", "time", "instret", "cycleh", "timeh", "instreth",
    "cyclew", "timew", "instretw", "cyclehw", "timehw", "instrethw",
    "mcpuid", "mimpid","mhartid", "mtvec", "mtdeleg", "mie",
    "mtimecmp", "mtime", "mtimeh", "mscratch", "mepc", "mcause", "mbadaddr", "mip",
    "mtohost", "mfromhost", "mstatus"
  )).toMap
  def csrName(csr: BigInt) = csrMap getOrElse (csr, csr.toString(16))

  private def inst_31(inst: UInt)    = UInt((inst.litValue() >> 31) & 0x1,  1)
  private def inst_30_25(inst: UInt) = UInt((inst.litValue() >> 25) & 0x3f, 6)
  private def inst_24_21(inst: UInt) = UInt((inst.litValue() >> 21) & 0xf,  4)
  private def inst_20(inst: UInt)    = UInt((inst.litValue() >> 20) & 0x1,  1)
  private def inst_19_12(inst: UInt) = UInt((inst.litValue() >> 12) & 0xff, 8)
  private def inst_11_8(inst: UInt)  = UInt((inst.litValue() >> 8)  & 0xf,  4)
  private def inst_7(inst: UInt)     = UInt((inst.litValue() >> 7)  & 0x1,  1)

  def iimm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_24_21(inst), inst_20(inst)).litValue()
  def simm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_11_8(inst), inst_7(inst)).litValue()
  def bimm(inst: UInt) = Cat(Cat(Seq.fill(20){inst_31(inst)}),
                             inst_7(inst), inst_30_25(inst), inst_11_8(inst), UInt(0, 1)).litValue()
  def uimm(inst: UInt) = Cat(inst_31(inst), inst_30_25(inst), inst_24_21(inst),
                             inst_20(inst), inst_19_12(inst), UInt(0, 12)).litValue()
  def jimm(inst: UInt) = Cat(Cat(Seq.fill(12){inst_31(inst)}), inst_19_12(inst),
                             inst_20(inst), inst_30_25(inst), inst_24_21(inst), UInt(0, 1)).litValue()
  def zimm(inst: UInt) = (inst.litValue() >> 15) & 0x1f

  private val instPats = List(AUIPC, LUI, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI,
    ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA, FENCE, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI)
  private val instFmts = List(
    (x: UInt) => "AUIPC x%d, %x".format(rd(x), uimm(x)),
    (x: UInt) => "LUI x%d, %x".format(rd(x), uimm(x)),
    (x: UInt) => "JAL x%d, %x".format(rd(x), jimm(x)),
    (x: UInt) => "JALR x%d, x%d, %x".format(rd(x), rs2(x), iimm(x)),
    (x: UInt) => "BEQ x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "BNE x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "BLT x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "BGE x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "BLTU x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "BGEU x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x)),
    (x: UInt) => "LB x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "LH x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "LW x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "LBU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "LHU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SB x%d, x%d, %x".format(rs2(x), rs1(x), simm(x)),
    (x: UInt) => "SH x%d, x%d, %x".format(rs2(x), rs1(x), simm(x)),
    (x: UInt) => "SW x%d, x%d, %x".format(rs2(x), rs1(x), simm(x)),
    (x: UInt) => "ADDI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SLTI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SLTIU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "XORI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "ORI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "ANDI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SLLI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SRLI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "SRAI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x)),
    (x: UInt) => "ADD x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SUB x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SLT x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SLTU x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "XOR x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "OR x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "AND x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SLL x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SRL x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "SRA x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x)),
    (x: UInt) => "FENCE",
    (x: UInt) => "CSRRW x%d, %s, x%d".format(rd(x), csrName(csr(x)), rs1(x)),
    (x: UInt) => "CSRRS x%d, %s, x%d".format(rd(x), csrName(csr(x)), rs1(x)),
    (x: UInt) => "CSRRC x%d, %s, x%d".format(rd(x), csrName(csr(x)), rs1(x)),
    (x: UInt) => "CSRRWI x%d, %s, %d".format(rd(x), csrName(csr(x)), rs1(x)),
    (x: UInt) => "CSRRSI x%d, %s, %d".format(rd(x), csrName(csr(x)), rs1(x)),
    (x: UInt) => "CSRRCI x%d, %s, %d".format(rd(x), csrName(csr(x)), rs1(x))
  )

  def dasm(x: UInt) = {
    def iter(l: List[(BitPat, UInt => String)]): String = l match {
      case Nil => "???(%s)".format(x.litValue().toString(16))
      case (p, f) :: tail => if (x === p) f(x) else iter(tail)
    }
    if (x === FENCEI) "FENCEI"
    else if (x === ECALL) "ECALL"
    else if (x === EBREAK) "EBREAK"
    else if (x === ERET) "ERET"
    else if (x === NOP) "NOP"
    else iter(instPats zip instFmts)
  }

  val isaTests = (List("simple", "add", "addi", "auipc", "and", "andi", // TODO: "fence_i",
    "sb", "sh", "sw", "lb", "lbu", "lh", "lhu", "lui", "lw",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "j", "jal", "jalr",
    "or", "ori", "sll", "slli", "slt", "slti", "sra", "srai", "sub", "xor", "xori"
  ) map (t => s"rv32ui-p-${t}")) ++ (List(
    "sbreak", "scall", "illegal", "ma_fetch", "ma_addr", "csr" //, TODO: "timer" 
  ) map (t => s"rv32mi-p-${t}"))

  val bmarksTest = List(
    "median.riscv", "multiply.riscv", "qsort.riscv", "towers.riscv", "vvadd.riscv"
  )
}

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

abstract class SimMem(word_width: Int = 4, depth: Int = 1 << 20, verbose: Boolean = false) extends Processable {
  require(word_width % 4 == 0, "word_width should be divisible by 4")
  implicit def toBigInt(x: UInt) = x.litValue()
  private val addrMask = (1 << log2Up(depth))-1
  protected val off = log2Up(word_width)
  private val mem = Array.fill(depth){BigInt(0)}
  private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1
  private def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'

  def read(addr: Int) = {
    val data = mem(addr & addrMask)
    if (verbose) println("MEM[%x] => %x".format(addr & addrMask, data))
    data
  }
  def write(addr: Int, data: BigInt) { 
    if (verbose) println("MEM[%x] <= %x".format(addr & addrMask, data))
    mem(addr & addrMask) = data 
  }
  def loadMem(test: Seq[UInt]) {
    val chunk = word_width / 4
    for (i <- 0 until (test.size / chunk)) {
      val data = ((0 until chunk) foldLeft BigInt(0))((res, k) => 
                   res | (test(i*chunk+k) << 32*(chunk-1-k)))
      write(i, data)
    }
  }

  def loadMem(filename: String) {
    val lines = io.Source.fromFile(filename).getLines
    for ((line, i) <- lines.zipWithIndex) {
      val base = (i * line.length) / 2
      assert(base % word_width == 0)
      var offset = 0
      var data = BigInt(0)
      for (k <- (line.length - 2) to 0 by -2) {
        val shift = 8 * (offset % word_width)
        val byte = ((parseNibble(line(k)) << 4) | parseNibble(line(k+1))).toByte
        data |= int(byte) << shift
        if ((offset % word_width) == word_width - 1) {
          mem((base+offset)>>off) = data
          data = BigInt(0)
        }
        offset += 1
      }
    }
  }
}

trait MemTests extends AdvTests {
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  implicit def booleanToBigInt(x: Boolean) = if (x) BigInt(1) else BigInt(0)
  implicit def boolToBoolean(x: Bool) = x.isTrue
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def uintToBigInt(x: UInt) = x.litValue()

  abstract class Tests
  case object ISATests extends Tests
  case object Benchmarks extends Tests
  case object LoadMem extends Tests

  def genTests(tests: List[String], dir: String) {
    for (test <- tests if !(new java.io.File(dir + "/" + test + ".hex").exists)) {
      run(List("make", "-C", dir, test + ".hex", """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""") mkString " ")
    }
  }

  def loadMem(testname: String): Unit
  def runTests(maxCycles: Int, verbose: Boolean): Unit

  def parseArgs(args: Array[String]) = {
    var tests: Tests = LoadMem
    var file = ""
    var maxcycles = 0
    var verbose = false
    args foreach {
      case "+verbose" => verbose = true 
      case arg if arg.slice(0, 5) == "+isa=" =>
        tests = ISATests
        file = arg.substring(5)
        genTests(RISCVCommon.isaTests, file)
      case arg if arg.slice(0, 8) == "+bmarks=" =>
        tests = Benchmarks
        file = arg.substring(8)
        genTests(RISCVCommon.bmarksTest, file)
      case arg if arg.slice(0, 12) == "+max-cycles=" =>
        maxcycles = arg.substring(12).toInt
      case arg if arg.slice(0, 9) == "+loadmem=" =>
        tests = LoadMem
        file = arg.substring(9)
      case _ => 
    }
    (tests, file, maxcycles, verbose) 
  }

  def start(tests: Tests, file: String, maxcycles: Int, verbose: Boolean) {
    tests match {
      case ISATests => for (test <- RISCVCommon.isaTests) {
        println("\n***** ISA Test: %s ******".format(test))
        loadMem(file + "/" + test + ".hex")
        runTests(maxcycles, verbose)
        reset(5)
      }
      case Benchmarks => for (test <- RISCVCommon.bmarksTest) {
        println("\n***** Benchmark: %s ******".format(test))
        loadMem(file + "/" + test + ".hex")
        runTests(maxcycles, verbose)
        reset(5)
      }
      case LoadMem =>
        println("\n***** LoadMem: %s ******".format(file))
        loadMem(file)
        runTests(maxcycles, verbose)
    }
  }

  def run(host: HostIO, maxcycles: Int, verbose: Boolean) = {
    val startTime = System.nanoTime
    val ok = eventually(peek(host.tohost), maxcycles)
    val tohost = peek(host.tohost)
    val endTime = System.nanoTime
    val simTime = (endTime - startTime) / 1000000000.0
    val simSpeed = cycles / simTime
    val reason = if (cycles < maxcycles) "tohost = " + tohost else "timeout"
    println("*** %s *** (%s) after %d simulation cycles".format(
            if (ok) "PASSED" else "FAILED", reason, cycles))
    println("Time elapsed = %.1f s, Simulation Speed = %.2f Hz".format(simTime, simSpeed))
    ok
  }
}

class CoreMem(ireqQ: ScalaQueue[TestCacheReq], irespQ: ScalaQueue[TestCacheResp],
              dreqQ: ScalaQueue[TestCacheReq], drespQ: ScalaQueue[TestCacheResp], abort: => BigInt,
              verbose: Boolean, word_width: Int = 4, depth: Int = 1 << 20) extends SimMem(word_width, depth) {
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
      if (verbose) println("MEM[%x] -> %s".format(ireq.addr, RISCVCommon.dasm(UInt(inst))))
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
  val (tests, file, maxcycles, verbose) = parseArgs(args)
  val ireqHandler = new ValidSink(c.io.icache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val dreqHandler = new ValidSink(c.io.dcache.req, 
    (req: CacheReq) => new TestCacheReq(peek(req.addr), peek(req.data), peek(req.mask)))
  val irespHandler = new ValidSource(c.io.icache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val drespHandler = new ValidSource(c.io.dcache.resp,
    (resp: CacheResp, in: TestCacheResp) => reg_poke(resp.data, in.data))
  val mem = new CoreMem(ireqHandler.outputs, irespHandler.inputs, dreqHandler.outputs, drespHandler.inputs, peek(c.io.dcache.abort), verbose, 4)
  def loadMem(testname: String) = mem.loadMem(testname)
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
  preprocessors += mem
  start(tests, file, maxcycles, verbose) 
}

class TileMem(cmdQ: ScalaQueue[TestMemReq],
             dataQ: ScalaQueue[TestMemData],
             respQ: ScalaQueue[TestMemResp],
             word_width: Int = 16, depth: Int = 1 << 20) extends SimMem(word_width, depth) {
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

class TileSlowMem(cmdQ: ScalaQueue[TestMemReq],
             dataQ: ScalaQueue[TestMemData],
             respQ: ScalaQueue[TestMemResp],
    latency: Int, word_width: Int = 16, depth: Int = 1 << 20) extends SimMem(word_width, depth) {
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
     
class TileTester(c: Tile, args: Array[String]) extends AdvTester(c, false) with MemTests {
  val (tests, file, maxcycles, verbose) = parseArgs(args)
  val cmdHandler = new DecoupledSink(c.io.mem.req_cmd, 
    (cmd: MemReqCmd) => new TestMemReq(peek(cmd.addr).toInt, peek(cmd.tag), peek(cmd.rw) != 0))
  val dataHandler = new DecoupledSink(c.io.mem.req_data, 
    (data: MemData) => new TestMemData(peek(data.data)))
  val respHandler = new DecoupledSource(c.io.mem.resp,
    (resp: MemResp, in: TestMemResp) => {reg_poke(resp.data, in.data) ; reg_poke(resp.tag, in.tag)})
  val mem = new TileSlowMem(cmdHandler.outputs, dataHandler.outputs, respHandler.inputs, 5, 16)
  preprocessors += mem
  cmdHandler.process()
  dataHandler.process()
  respHandler.process()
  def loadMem(testname: String) = mem.loadMem(testname)
  def runTests(maxcycles: Int, verbose: Boolean) {
    cycles = 0
    ok &= run(c.io.htif.host, maxcycles, verbose)
  }
  start(tests, file, maxcycles, verbose) 
}
