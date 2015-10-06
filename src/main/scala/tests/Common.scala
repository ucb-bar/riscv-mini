package mini

import Chisel._
import Chisel.AdvTester._
import Instructions._
import RISCVCommon._
import scala.io.Source
import scala.collection.mutable.HashMap

object RISCVCommon {
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

  private val instPats = List(AUIPC, LUI, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU, 
    LB, LH, LW, LBU, LHU, SB, SH, SW, ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI,
    ADD, SUB, SLT, SLTU, XOR, OR, AND, SLL, SRL, SRA, FENCE, CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI)
  private val instFmts = List(
    (x: UInt) => "AUIPC x%d, %x".format(rd(x), GoldImmGen.uimm(x)),
    (x: UInt) => "LUI x%d, %x".format(rd(x), GoldImmGen.uimm(x)),
    (x: UInt) => "JAL x%d, %x".format(rd(x), GoldImmGen.jimm(x)),
    (x: UInt) => "JALR x%d, x%d, %x".format(rd(x), rs2(x), GoldImmGen.iimm(x)),
    (x: UInt) => "BEQ x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "BNE x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "BLT x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "BGE x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "BLTU x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "BGEU x%d, x%d, %x".format(rs1(x), rs2(x), GoldImmGen.bimm(x)),
    (x: UInt) => "LB x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "LH x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "LW x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "LBU x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "LHU x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SB x%d, x%d, %x".format(rs2(x), rs1(x), GoldImmGen.simm(x)),
    (x: UInt) => "SH x%d, x%d, %x".format(rs2(x), rs1(x), GoldImmGen.simm(x)),
    (x: UInt) => "SW x%d, x%d, %x".format(rs2(x), rs1(x), GoldImmGen.simm(x)),
    (x: UInt) => "ADDI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SLTI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SLTIU x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "XORI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "ORI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "ANDI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SLLI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SRLI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
    (x: UInt) => "SRAI x%d, x%d, %x".format(rd(x), rs1(x), GoldImmGen.iimm(x)),
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
  def RU(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.U, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def RS(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.S, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  def I(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.ITYPE)
  def L(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.LOAD)
  def S(funct3: UInt, rs2: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 0), Opcode.STORE)
  def B(funct3: UInt, rs1: Int, rs2: Int, i: Int) =
    Cat(imm(i)(12), imm(i)(10, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 1), imm(i)(11), Opcode.BRANCH)
  def U(op: UInt, rd: Int, i: Int) = 
    Cat(imm(i), reg(rd), op)
  def J(rd: Int, i: Int) = 
    Cat(imm(i)(20), imm(i)(10, 1), imm(i)(11), imm(i)(19, 12), reg(rd), Opcode.JAL)
  def JR(rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), UInt(0, 3), reg(rd), Opcode.JALR)
  def SYS(funct3: UInt, rd: Int, csr: UInt, rs1: Int) = 
    Cat(csr, reg(rs1), funct3, reg(rd), Opcode.SYSTEM)
}

trait RandInsts {
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  implicit def booleanToBigInt(x: Boolean) = if (x) BigInt(1) else BigInt(0)
  implicit def boolToBoolean(x: Bool) = x.isTrue
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def uintToBigInt(x: UInt) = x.litValue()

  /* Define tests */
  def rnd: scala.util.Random
  def rand_fn7 = UInt(rnd.nextInt(1 << 7), 7)
  def rand_rs2 = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_rs1 = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_fn3 = UInt(rnd.nextInt(1 << 3), 3) 
  def rand_rd  = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_csr = UInt(csrRegs(rnd.nextInt(csrRegs.size-1)))
  def rand_inst = UInt(rnd.nextInt())
  def rand_addr = UInt(rnd.nextInt())

  def insts = List(
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.LUI),
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.AUIPC), 
    Cat(rand_fn7, rand_rs2, rand_rs1, rand_fn3, rand_rd, Opcode.JAL),
    Cat(rand_fn7, rand_rs2, rand_rs1, UInt(0,3), rand_rd, Opcode.JALR),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BEQ, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BNE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLT, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGE, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BLTU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.BGEU, rand_rd, Opcode.BRANCH),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LB, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LH, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LW, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LBU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.LHU, rand_rd, Opcode.LOAD),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SB, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SH, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SW, rand_rd, Opcode.STORE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.ITYPE),
    Cat(rand_fn7, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.ITYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.ADD, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLL, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLT, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SLTU, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.XOR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.S, rand_rs2, rand_rs1, Funct3.SR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.OR, rand_rd, Opcode.RTYPE),
    Cat(Funct7.U, rand_rs2, rand_rs1, Funct3.AND, rand_rd, Opcode.RTYPE),
    fence, FENCEI,
    Cat(rand_csr, rand_rs1, Funct3.CSRRW, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRS, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRC, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRWI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRSI, rand_rd, Opcode.SYSTEM),
    Cat(rand_csr, rand_rs1, Funct3.CSRRCI, rand_rd, Opcode.SYSTEM),
    ECALL, EBREAK, ERET, nop, rand_inst
  )
  def dasm(x: UInt) = RISCVCommon.dasm(x)
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
    val lines = Source.fromFile(filename).getLines
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

trait MemTests extends AdvTests with RandInsts {
  abstract class Tests
  case object SimpleTests extends Tests
  case object ISATests extends Tests
  case object Benchmarks extends Tests
  case object LoadMem extends Tests
  val pc_start = Const.PC_START.litValue().toInt
  val bypassTest = List.fill(pc_start/4){fin} ++ List(
    I(Funct3.ADD, 1, 0, 1),  // ADDI x1, x0, 1   # x1 <- 1
    S(Funct3.SW, 1, 0, 12),  // SW   x1, x0, 12  # Mem[12] <- 1
    L(Funct3.LW, 2, 0, 12),  // LW   x2, x0, 12  # x2 <- 1
    RU(Funct3.ADD, 3, 2, 2), // ADD  x3, x2, x2  # x3 <- 2
    RS(Funct3.ADD, 4, 3, 2), // SUB  x4, x2, x3  # x4 <- 1
    RU(Funct3.SLL, 5, 3, 4), // SLL  x5, x2, x4  # x5 <- 4
    RU(Funct3.SLT, 6, 4, 5), // SLT  x6, x4, x5  # x6 <- 1
    B(Funct3.BEQ, 1, 6, 8),  // BEQ  x1, x6, 8   # go to the BGE branch
    J(0, 12),                // JAL  x0, 8       # skip nop, scrrw
    B(Funct3.BGE, 4, 1, -4), // BGE  x4, x1, -4  # go to the jump
    nop, nop, fin            // Finish
  )
  val exceptionTest = List.fill(pc_start/4){fin} ++ List(
    fence,
    I(Funct3.ADD, 1, 0, 2),  // ADDI x1, x0, 1   # x1 <- 2
    I(Funct3.ADD, 2, 1, 1),  // ADDI x2, x1, 1   # x2 <- 3
    I(Funct3.ADD, 3, 2, 1),  // ADDI x3, x2, 1   # x3 <- 4
    rand_inst,               // excpetion
    I(Funct3.ADD, 1, 1, 1),  // ADDI x1, x1, 1   # x1 <- 3
    I(Funct3.ADD, 2, 1, 1),  // ADDI x1, x1, 1   # x1 <- 4
    I(Funct3.ADD, 3, 2, 1),  // ADDI x1, x1, 1   # x1 <- 5
    fin                      // fin
  )
  val testResults = Map(
    bypassTest    -> Array((1, 1), (2, 1), (3, 2), (4, 1), (5, 4), (6, 1)),
    exceptionTest -> Array((1, 2), (2, 3), (3, 4))
  )

  val isaTests = List(
    "rv32ui-p-simple",
    "rv32ui-p-add",
    "rv32ui-p-addi",
    "rv32ui-p-auipc",
    // TODO: "rv32ui-p-fence_i",
    "rv32ui-p-sb",
    "rv32ui-p-sh",
    "rv32ui-p-sw",
    "rv32ui-p-and",
    "rv32ui-p-andi",
    "rv32ui-p-beq",
    "rv32ui-p-bge",
    "rv32ui-p-bgeu",
    "rv32ui-p-blt",
    "rv32ui-p-bltu",
    "rv32ui-p-bne",
    "rv32ui-p-j",
    "rv32ui-p-jal",
    "rv32ui-p-jalr",
    "rv32ui-p-lb",
    "rv32ui-p-lbu",
    "rv32ui-p-lh",
    "rv32ui-p-lhu",
    "rv32ui-p-lui",
    "rv32ui-p-lw",
    "rv32ui-p-or",
    "rv32ui-p-ori",
    "rv32ui-p-sll",
    "rv32ui-p-slli",
    "rv32ui-p-slt",
    "rv32ui-p-slti",
    "rv32ui-p-sra",
    "rv32ui-p-srai",
    "rv32ui-p-sub",
    "rv32ui-p-xor",
    "rv32ui-p-xori",

    "rv32mi-p-sbreak",
    "rv32mi-p-scall",
    "rv32mi-p-illegal",
    "rv32mi-p-ma_fetch", 
    "rv32mi-p-ma_addr", 
    // TODO: "rv32mi-p-timer",
    "rv32mi-p-csr"
  ) 

  val bmarksTest = List(
    "median.riscv",
    "multiply.riscv",
    "qsort.riscv",
    "towers.riscv",
    "vvadd.riscv"
  )

  def genTests(tests: List[String], dir: String) {
    for (test <- tests if !(new java.io.File(dir + "/" + test + ".hex").exists)) {
      run(List("make", "-C", dir, test + ".hex", """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""") mkString " ")
    }
  }

  def parseOpts(args: Array[String]) = {
    var tests: Tests = SimpleTests
    var file = ""
    var loadmem: Option[String] = None
    var maxcycles = 0
    var verbose = false
    args foreach {
      case "+verbose" => verbose = true 
      case "+simple" => tests = SimpleTests
      case arg if arg.substring(0, 5) == "+isa=" =>
        tests = ISATests
        file = arg.substring(5)
        genTests(isaTests, file)
      case arg if arg.substring(0, 8) == "+bmarks=" =>
        tests = Benchmarks
        file = arg.substring(8)
        genTests(bmarksTest, file)
      case arg if arg.substring(0, 12) == "+max-cycles=" =>
        maxcycles = arg.substring(12).toInt
      case arg if arg.substring(0, 9) == "+loadmem=" =>
        tests = LoadMem
        file = arg.substring(9)
      case _ => 
    }
    (file, tests, maxcycles, verbose)
  }

  def loadMem(test: Seq[UInt]): Unit
  def loadMem(testname: String): Unit
  def runTests(maxCycles: Int, verbose: Boolean): Unit
  def regFile(x: Int): BigInt

  def start(file: String, tests: Tests, maxcycles: Int, verbose: Boolean) {
    tests match {
      case SimpleTests =>
        reset(5)
        loadMem(bypassTest)
        runTests(maxcycles, verbose)
        for ((rd, expected) <- testResults(bypassTest)) {
          val result = regFile(rd) 
          println("[%s] RegFile[%d] = %d == %d".format(
                  if (result == expected) "PASS" else "FAIL", rd, result, expected))
        }
        reset(5)
        loadMem(exceptionTest)
        runTests(maxcycles, verbose)
        for ((rd, expected) <- testResults(exceptionTest)) {
          val result = regFile(rd) 
          println("[%s] RegFile[%d] = %d == %d".format(
                  if (result == expected) "PASS" else "FAIL", rd, result, expected))
        }
      case ISATests => for (test <- isaTests) {
        println("\n***** ISA Test: %s ******".format(test))
        loadMem(file + "/" + test + ".hex")
        runTests(maxcycles, verbose)
        reset(5)
      }
      case Benchmarks => for (test <- bmarksTest) {
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
    val ok = do_until {
      val log = testOutputString
      if (verbose && !log.isEmpty) println(log)
    } (peek(host.tohost), maxcycles)
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
