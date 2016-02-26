package mini

import Chisel._
import Chisel.AdvTester._
import scala.collection.mutable.HashMap

trait RISCVCommon {
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
}

trait RandInsts extends Tests with RISCVCommon {
  import Instructions._
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  implicit def booleanToBigInt(x: Boolean) = if (x) BigInt(1) else BigInt(0)
  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def uintToBigInt(x: UInt) = x.litValue()

  /* Define tests */
  def rand_fn7 = UInt(rnd.nextInt(1 << 7), 7)
  def rand_rs2 = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_rs1 = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_fn3 = UInt(rnd.nextInt(1 << 3), 3) 
  def rand_rd  = UInt(rnd.nextInt((1 << 5) - 1) + 1, 5)
  def rand_csr = UInt(csrRegs(rnd.nextInt(csrRegs.size-1)))
  def rand_inst = UInt(rnd.nextInt())
  def rand_addr = UInt(rnd.nextInt())
  def rand_data = int(rnd.nextInt())

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

  val bypassTest = List(
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
  val exceptionTest = List(
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
}

class LogTester[+T <: Module](c: T, log: Option[java.io.PrintStream]) 
    extends Tester(c, log == None) {
  log match {
    case None =>
    case Some(f) => addObserver(new Observer(file=f))
  }
}
