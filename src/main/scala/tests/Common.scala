package mini

import Chisel._
import Instructions._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.HashMap

object TestCommon extends FileSystemUtilities {
  /* Define tests */
  private def rand_fn7 = UInt(Random.nextInt(1 << 7), 7)
  private def rand_rs2 = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)
  private def rand_rs1 = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)
  private def rand_fn3 = UInt(Random.nextInt(1 << 3), 3) 
  private def rand_rd  = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)
  private def rand_csr = UInt(csrRegs(Random.nextInt(csrRegs.size-1)))
  private def rand_inst = UInt(Random.nextInt())

  private def reg(x: Int) = UInt(x, 5)
  private def imm(x: Int) = SInt(x, 21)
  private def RU(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.U, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  private def RS(funct3: UInt, rd: Int, rs1: Int, rs2: Int) = 
    Cat(Funct7.S, reg(rs2), reg(rs1), funct3, reg(rd), Opcode.RTYPE)
  private def I(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.ITYPE)
  private def L(funct3: UInt, rd: Int, rs1: Int, i: Int) = 
    Cat(imm(i)(11, 0), reg(rs1), funct3, reg(rd), Opcode.LOAD)
  private def S(funct3: UInt, rs2: Int, rs1: Int, i: Int) =
    Cat(imm(i)(11, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 0), Opcode.STORE)
  private def B(funct3: UInt, rs1: Int, rs2: Int, i: Int) =
    Cat(imm(i)(12), imm(i)(10, 5), reg(rs2), reg(rs1), funct3, imm(i)(4, 1), imm(i)(11), Opcode.BRANCH)
  private def U(op: UInt, rd: Int, i: Int) = 
    Cat(imm(i), reg(rd), op)
  private def J(op: UInt, rd: Int, i: Int) = 
    Cat(imm(i)(20), imm(i)(10, 1), imm(i)(11), imm(i)(19, 12), reg(rd), op)
  private def SYS(funct3: UInt, rd: Int, csr: UInt, rs1: Int) = 
    Cat(csr, reg(rs1), funct3, reg(rd), Opcode.SYSTEM)

  val fin   = Cat(CSR.mtohost, reg(1), Funct3.CSRRWI, reg(0), Opcode.SYSTEM)
  val fence = Cat(UInt(0, 4), UInt(0xf, 4), UInt(0xf, 4), UInt(0, 13), Opcode.MEMORY)
  val nop   = Cat(UInt(0, 12), reg(0), Funct3.ADD, reg(0), Opcode.ITYPE)

  def insts = Array(
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

  val y      = BigInt(1)
  val n      = BigInt(0)

  val pc_4   = BigInt(0)
  val pc_alu = BigInt(1)
  val pc_0   = BigInt(2)
  val pc_xxx = BigInt(3)

  val a_rs1  = BigInt(0)
  val a_pc   = BigInt(1)
  val a_xxx  = BigInt(1)

  val b_rs2  = BigInt(0)
  val b_imm  = BigInt(1)
  val b_xxx  = BigInt(1)

  val imm_i  = BigInt(0)
  val imm_s  = BigInt(1)
  val imm_u  = BigInt(2)
  val imm_j  = BigInt(3)
  val imm_b  = BigInt(4)
  val imm_z  = BigInt(5)
  val imm_x  = BigInt(7)

  val alu_add    = BigInt(0)
  val alu_sub    = BigInt(1)
  val alu_and    = BigInt(2)
  val alu_or     = BigInt(3)
  val alu_xor    = BigInt(4)
  val alu_slt    = BigInt(5)
  val alu_sll    = BigInt(6)
  val alu_sltu   = BigInt(7)
  val alu_srl    = BigInt(8)
  val alu_sra    = BigInt(9)
  val alu_copy_a = BigInt(10)
  val alu_copy_b = BigInt(11)
  val alu_xxx    = BigInt(15)

  val br_ltu = BigInt(0)
  val br_lt  = BigInt(1)
  val br_eq  = BigInt(2)
  val br_geu = BigInt(4)
  val br_ge  = BigInt(5)
  val br_ne  = BigInt(6)
  val br_xxx = BigInt(7)

  val st_sw  = BigInt(0)
  val st_sh  = BigInt(1)
  val st_sb  = BigInt(2)
  val st_xxx = BigInt(3)

  val ld_lw  = BigInt(0)
  val ld_lh  = BigInt(1)
  val ld_lb  = BigInt(2)
  val ld_lhu = BigInt(3)
  val ld_lbu = BigInt(4)
  val ld_xxx = BigInt(7)

  val wb_alu = BigInt(0)
  val wb_mem = BigInt(1)
  val wb_pc4 = BigInt(2)
  val wb_csr = BigInt(3)

  val csr_n = BigInt(0)
  val csr_w = BigInt(1)
  val csr_s = BigInt(2)
  val csr_c = BigInt(3)
  val csr_p = BigInt(4)

  def rs1(inst: UInt) = ((inst.litValue() >> 15) & 0x1f).toInt
  def rs2(inst: UInt) = ((inst.litValue() >> 20) & 0x1f).toInt
  def rd (inst: UInt) = ((inst.litValue() >> 7)  & 0x1f).toInt
  def csr(inst: UInt) =  (inst.litValue() >> 20)

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
  def zimm(inst: UInt) = BigInt(rs1(inst))

  implicit def toBoolean(x: Bool) = x.isTrue

  val csrRegs = List(
    CSR.cycle, CSR.time, CSR.instret, CSR.cycleh, CSR.timeh, CSR.instreth,
    CSR.cyclew, CSR.timew, CSR.instretw, CSR.cyclehw, CSR.timehw, CSR.instrethw,
    CSR.mcpuid, CSR.mimpid, CSR.mhartid, CSR.mtvec, CSR.mtdeleg, CSR.mie,
    CSR.mtimecmp, CSR.mtime, CSR.mtimeh, CSR.mscratch, CSR.mepc, CSR.mcause, CSR.mbadaddr, CSR.mip,
    CSR.mtohost, CSR.mfromhost, CSR.mstatus
  ) map (_.litValue())

  val csrNames = (csrRegs zip List(
    "cycle", "time", "instret", "cycleh", "timeh", "instreth",
    "cyclew", "timew", "instretw", "cyclehw", "timehw", "instrethw",
    "mcpuid", "mimpid","mhartid", "mtvec", "mtdeleg", "mie",
    "mtimecmp", "mtime", "mtimeh", "mscratch", "mepc", "mcause", "mbadaddr", "mip",
    "mtohost", "mfromhost", "mstatus"
  )).toMap

  def csrPrv(csr: BigInt, prv: BigInt) = ((csr >> 8) & 0x3) <= prv
  def csrVal(csr: BigInt) = csrRegs exists (_ == csr)
  def csrRO(csr: BigInt) = ((csr >> 10) & 0x3) == 0x3 || 
      csr == CSR.mtvec.litValue() || csr == CSR.mtdeleg.litValue()    
  def csrTime(csr: BigInt) = 
      csr == CSR.time.litValue() || csr == CSR.timew.litValue() || csr == CSR.mtime.litValue()
  def csrCycle(csr: BigInt) = 
      csr == CSR.cycle.litValue() || csr == CSR.cyclew.litValue()
  def csrInstret(csr: BigInt) =
      csr == CSR.instret.litValue() || csr == CSR.instretw.litValue() 
  def csrTimeh(csr: BigInt) = 
      csr == CSR.timeh.litValue() || csr == CSR.timehw.litValue() || csr == CSR.mtimeh.litValue()
  def csrCycleh(csr: BigInt) = 
      csr == CSR.cycleh.litValue() || csr == CSR.cyclehw.litValue()
  def csrInstreth(csr: BigInt) =
      csr == CSR.instreth.litValue() || csr == CSR.instrethw.litValue() 

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
    (x: UInt) => "CSRRW x%d, %s, x%d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x)),
    (x: UInt) => "CSRRS x%d, %s, x%d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x)),
    (x: UInt) => "CSRRC x%d, %s, x%d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x)),
    (x: UInt) => "CSRRWI x%d, %s, %d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x)),
    (x: UInt) => "CSRRSI x%d, %s, %d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x)),
    (x: UInt) => "CSRRCI x%d, %s, %d".format(rd(x), csrNames getOrElse (csr(x), csr(x).toString(16)), rs1(x))
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

  val instCtrls = List(
    //                                                                      kill                    wb_en   illiegal?
    //                pc_sel   A_sel   B_sel  imm_sel alu_op     br_type |  st_type ld_type wb_sel  |  csr_cmd _|
    //                   |       |      |      |       |            |    |       |       |          |  |      |
    /* AUIPC  */ Array(pc_4,   a_pc,   b_imm, imm_u, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* LUI    */ Array(pc_4,   a_pc,   b_imm, imm_u, alu_copy_b, br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* JAL    */ Array(pc_alu, a_pc,   b_imm, imm_j, alu_add,    br_xxx, y, st_xxx, ld_xxx, wb_pc4, y, csr_n, n),
    /* JALR   */ Array(pc_alu, a_rs1,  b_imm, imm_i, alu_add,    br_xxx, y, st_xxx, ld_xxx, wb_pc4, y, csr_n, n),
    /* BEQ    */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_eq,  n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* BNE    */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ne,  n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* BLT    */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_lt,  n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* BGE    */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ge,  n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* BLTU   */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ltu, n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* BGEU   */ Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_geu, n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* LB     */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lb,  wb_mem, y, csr_n, n),
    /* LH     */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lh,  wb_mem, y, csr_n, n),
    /* LW     */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lw,  wb_mem, y, csr_n, n),
    /* LBU    */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lbu, wb_mem, y, csr_n, n),
    /* LHU    */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lhu, wb_mem, y, csr_n, n),
    /* SB     */ Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sb,  ld_xxx, wb_alu, n, csr_n, n),
    /* SH     */ Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sh,  ld_xxx, wb_alu, n, csr_n, n),
    /* SW     */ Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sw,  ld_xxx, wb_alu, n, csr_n, n),
    /* ADDI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLTI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_slt,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLTIU  */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_sltu,   br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* XORI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_xor,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* ORI    */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_or,     br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* ANDI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_and,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLLI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_sll,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SRLI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_srl,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SRAI   */ Array(pc_4,   a_rs1,  b_imm, imm_i, alu_sra,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* ADD    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SUB    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sub,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLT    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_slt,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLTU   */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sltu,   br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* XOR    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_xor,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* OR     */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_or,     br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* AND    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_and,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SLL    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sll,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SRL    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_srl,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* SRA    */ Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sra,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n, n),
    /* FENCE  */ Array(pc_4,   a_xxx,  b_xxx, imm_x, alu_xxx,    br_xxx, n, st_xxx, ld_xxx, wb_alu, n, csr_n, n),
    /* CSRRW  */ Array(pc_4,   a_rs1,  b_xxx, imm_x, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_w, n),
    /* CSRRS  */ Array(pc_4,   a_rs1,  b_xxx, imm_x, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_s, n),
    /* CSRRC  */ Array(pc_4,   a_rs1,  b_xxx, imm_x, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_c, n),
    /* CSRRWI */ Array(pc_4,   a_xxx,  b_xxx, imm_z, alu_xxx,    br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_w, n),
    /* CSRRSI */ Array(pc_4,   a_xxx,  b_xxx, imm_z, alu_xxx,    br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_s, n),
    /* CSRRCI */ Array(pc_4,   a_xxx,  b_xxx, imm_z, alu_xxx,    br_xxx, n, st_xxx, ld_xxx, wb_csr, y, csr_c, n)
    )

  def decode(x: UInt) = {
    def iter(l: List[(BitPat, Array[BigInt])]): Array[BigInt] = l match {
      case Nil => Array(pc_4, a_xxx, b_xxx, imm_x, alu_xxx, br_xxx, n, st_xxx, ld_xxx, wb_alu, n, csr_n, y)
      case (p, s) :: tail => if (x === p) s else iter(tail)  
    }
    if (x === FENCEI)
      Array(pc_0, a_xxx, b_xxx, imm_x, alu_xxx, br_xxx, y, st_xxx, ld_xxx, wb_alu, n, csr_n, n)
    else if (x === ERET)
      Array(pc_4, a_xxx, b_xxx, imm_x, alu_xxx, br_xxx, y, st_xxx, ld_xxx, wb_csr, n, csr_p, n)
    else if (x === ECALL || x === EBREAK)
      Array(pc_4, a_xxx, b_xxx, imm_x, alu_xxx, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_p, n)
    else iter(instPats zip instCtrls)
  }

  val pc_start = Const.PC_START.litValue().toInt
  val pc_utvec = Const.PC_EVEC.litValue().toInt + CSR.PRV_U.litValue().toInt * 0x40
  val pc_mtvec = Const.PC_EVEC.litValue().toInt + CSR.PRV_M.litValue().toInt * 0x40

  abstract class Tests
  case object SimpleTests extends Tests
  case object ISATests extends Tests
  case object Benchmarks extends Tests

  def genTests(tests: List[String], dir: String) {
    for (test <- tests if !(new java.io.File(dir + "/" + test + ".hex").exists)) {
      run(List("make", "-C", dir, test + ".hex", """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""") mkString " ")
    }
  }

  def parseOpts(args: Array[String]) = {
    var tests: Tests = SimpleTests
    var dir = ""
    var maxcycles = 0
    var verbose = false
    args foreach {
      case "+verbose" => verbose = true 
      case "+simple" => tests = SimpleTests
      case arg if arg.substring(0, 5) == "+isa=" =>
        tests = ISATests
        dir = arg.substring(5)
        genTests(isaTests, dir)
      case arg if arg.substring(0, 7) == "+bmark=" =>
        tests = Benchmarks
        dir = arg.substring(7)
        genTests(bmarksTest, dir)
      case arg if arg.substring(0, 12) == "+max-cycles=" =>
        maxcycles = arg.substring(12).toInt
      case _ => 
    }
    (dir, tests, maxcycles, verbose)
  }
  
  val bypassTest = List(
    I(Funct3.ADD, 1, 0, 1),  // ADDI x1, x0, 1   # x1 <- 1
    S(Funct3.SW, 1, 0, 12),  // SW   x1, x0, 12  # Mem[12] <- 1
    L(Funct3.LW, 2, 0, 12),  // LW   x2, x0, 12  # x2 <- 1
    RU(Funct3.ADD, 3, 2, 2), // ADD  x3, x2, x2  # x3 <- 2
    RS(Funct3.ADD, 4, 3, 2), // SUB  x4, x2, x3  # x4 <- 1
    RU(Funct3.SLL, 5, 3, 4), // SLL  x5, x2, x4  # x5 <- 4
    RU(Funct3.SLT, 6, 4, 5), // SLT  x6, x4, x5  # x6 <- 1
    B(Funct3.BEQ, 1, 6, 8),  // BEQ  x1, x6, 8   # go to the BGE branch
    J(Opcode.JAL, 0, 12),    // JAL  x0, 8       # skip nop, scrrw
    B(Funct3.BGE, 4, 1, -4), // BGE  x4, x1, -4  # go to the jump
    nop, nop, fin            // Finish
  )
  val exceptionTest = List(
    fence,
    I(Funct3.ADD, 1, 0, 1),  // ADDI x1, x0, 1   # x1 <- 1
    I(Funct3.ADD, 2, 1, 1),  // ADDI x2, x1, 1   # x2 <- 2
    I(Funct3.ADD, 3, 2, 1),  // ADDI x3, x2, 1   # x3 <- 3
    rand_inst,               // excpetion
    I(Funct3.ADD, 1, 1, 1),  // ADDI x1, x1, 1   # x1 <- 2
    I(Funct3.ADD, 2, 1, 1),  // ADDI x1, x1, 1   # x1 <- 3
    I(Funct3.ADD, 3, 2, 1),  // ADDI x1, x1, 1   # x1 <- 4
    fin                      // fin
  )
  val testResults = Map(
    bypassTest    -> Array((1, 1), (2, 1), (3, 2), (4, 1), (5, 4), (6, 1)),
    exceptionTest -> Array((1, 1), (2, 2), (3, 3))
  )

  val isaTests = List(
    "rv32ui-p-simple",
    "rv32ui-p-add",
    "rv32ui-p-addi",
    "rv32ui-p-auipc",
    "rv32ui-p-fence_i",
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
    // TODO: "rv32mi-p-ma_fetch", 
    // TODO: "rv32mi-p-ma_addr", 
    // TODO: "rv32mi-p-timer",
    "rv32mi-p-csr"
  ) 

  val bmarksTest = List(
  )
}

class MagicMem(blockSize: Int = 4, size: Int = 1 << 23) {
  import TestCommon._
  implicit def toBigInt(x: UInt) = x.litValue()
  private val mem = Array.fill(size){0.toByte} 
  private def int(b: Byte) = (BigInt((b >>> 1) & 0x7f) << 1) | b & 0x1

  def read(addr: Int) = {
    val a = math.min(addr & (size - 1), size - 4)
    ((0 until blockSize) foldLeft BigInt(0)){case (res, i) => res | (int(mem(a + i)) << (8 * i))}
  }

  def write(addr: Int, data: BigInt, mask: BigInt = (1 << blockSize) - 1) {
    val a = addr & (size - 1)
    for (i <- blockSize to 0 by -1 if ((mask >> i) & 0x1) > 0) {
      mem(a+i) = (data >> (8 * i)).toByte
    }
  }

  def loadMem(test: Seq[UInt]) {
    write(pc_mtvec, fin)
    write(pc_utvec, fin)
    for((inst, i) <- test.zipWithIndex) {
      write(pc_start + i * blockSize, inst)
    }
  }

  def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'

  def loadMem(testname: String) {
    val lines = Source.fromFile(testname + ".hex").getLines
    for ((line, i) <- lines.zipWithIndex) {
      val base = (i * line.length) / 2
      var offset = 0
      for (k <- (line.length - 2) to 0 by -2) {
        val addr = base+offset
        val data = (parseNibble(line(k)) << 4) | parseNibble(line(k+1))
        mem(addr) = data.toByte
        offset += 1
      }
    }
  }
}

object HexCommon {
  private val mem = HashMap[BigInt, BigInt]()
  private var filename = ""
  private var maxcycles = 0
  private var readcycles = 2
  private var writecycles = 1


  def parseNibble(hex: Int) = if (hex >= 'a') hex - 'a' + 10 else hex - '0'

  def loadMem(filename: String) {
    val lines = Source.fromFile(filename).getLines
    for ((line, i) <- lines.zipWithIndex) {
      val base = (i * line.length) / 2
      var offset = 0
      for (k <- (line.length - 2) to 0 by -2) {
        val addr = BigInt(base+offset)
        val data = BigInt((parseNibble(line(k)) << 4) | parseNibble(line(k+1)))
        mem(addr) = data
        offset += 1
      }
    }
  }

  def readMem(addr: BigInt) = {
    var data = BigInt(0)
    for (i <- 0 until 4) {
      data |= mem(addr + i) << (8 * i)
    }
    data
  }
  def readCycles = readcycles

  def writeMem(addr: BigInt, data: BigInt, mask: BigInt = 0xf) {
    for (i <- 3 to 0 by -1 ; if ((mask >> i) & 0x1) > 0) {
      mem(addr+i) = (data >> (8 * i)) & 0xff
    }
  }
  def writeCycles = writecycles

  def clearMem {
    mem.clear
  }
}
