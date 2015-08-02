package mini

import Chisel._
import Instructions._
import scala.io.Source
import scala.util.Random
import scala.collection.mutable.HashMap

object TestCommon {
  /* Define tests */
  private def rand_fn7 = UInt(Random.nextInt(1 << 7), 7)
  private def rand_rs2 = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)
  private def rand_rs1 = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)
  private def rand_fn3 = UInt(Random.nextInt(1 << 3), 3) 
  private def rand_rd  = UInt(Random.nextInt((1 << 5) - 1) + 1, 5)

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

  val fin  = Cat(CSR.TOHOST, reg(1), Funct3.CSRRWI, reg(0), Opcode.SYSTEM)
  val nop  = Cat(UInt(0, 12), reg(0), Funct3.ADD, reg(0), Opcode.ITYPE)

  def isaTest = Array(
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
    fin
  )

  val bypassTest = Array(
    I(Funct3.ADD, 1, 0, 1),  // ADDI x1, x0, 1   # x1 <- 1
    S(Funct3.SW, 1, 0, 4),   // SW   x1, x0, 12  # Mem[12] <- 1
    L(Funct3.LW, 2, 0, 4),   // LW   x2, x0, 12  # x2 <- 1
    RU(Funct3.ADD, 3, 2, 2), // ADD  x3, x2, x2  # x3 <- 2
    RS(Funct3.ADD, 4, 3, 2), // SUB  x4, x2, x3  # x4 <- 1
    RU(Funct3.SLL, 5, 3, 4), // SLL  x5, x2, x4  # x5 <- 4
    RU(Funct3.SLT, 6, 4, 5), // SLT  x6, x4, x5  # x6 <- 1
    B(Funct3.BEQ, 1, 6, 8),  // BEQ  x1, x6, 8   # go to the BGE branch
    J(Opcode.JAL, 0, 12),    // JAL  x0, 8       # skip nop, scrrw
    B(Funct3.BGE, 4, 1, -4), // BGE  x4, x1, -4  # go to the jump
    nop, nop, fin            // Finish
  )
  val testResults = Map(
    bypassTest -> Array((1, 1), (2, 1), (3, 2), (4, 1), (5, 4), (6, 1))
  )
  
  val y      = BigInt(1)
  val n      = BigInt(0)

  val pc_4   = BigInt(0)
  val pc_alu = BigInt(1)

  val i_next = BigInt(0)
  val i_kill = BigInt(1)

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

  val wb_alu  = BigInt(0)
  val wb_mem  = BigInt(1)
  val wb_pc_4 = BigInt(2)
  val wb_csr  = BigInt(3)

  val csr_n = BigInt(0)
  val csr_w = BigInt(1)
  val csr_s = BigInt(2)
  val csr_c = BigInt(3)

  def rs1(inst: UInt) = ((inst & (UInt(0x1f) << UInt(15))) >> UInt(15)).litValue().toInt
  def rs2(inst: UInt) = ((inst & (UInt(0x1f) << UInt(20))) >> UInt(20)).litValue().toInt
  def rd(inst: UInt) = ((inst & (UInt(0x1f) << UInt(7))) >> UInt(7)).litValue().toInt

  private def inst_31(inst: UInt)    = UInt(((inst & (UInt(0x1) << UInt(31))) >> UInt(31)).litValue(), 1)
  private def inst_30_25(inst: UInt) = UInt(((inst & (UInt(0x3f) << UInt(25))) >> UInt(25)).litValue(), 6)
  private def inst_24_21(inst: UInt) = UInt(((inst & (UInt(0xf) << UInt(21))) >> UInt(21)).litValue(), 4)
  private def inst_20(inst: UInt)    = UInt(((inst & (UInt(0x1) << UInt(20))) >> UInt(20)).litValue(), 1)
  private def inst_19_12(inst: UInt) = UInt(((inst & (UInt(0xff) << UInt(12))) >> UInt(12)).litValue(), 8)
  private def inst_11_8(inst: UInt)  = UInt(((inst & (UInt(0xf) << UInt(8))) >> UInt(8)).litValue(), 4)
  private def inst_7(inst: UInt)     = UInt(((inst & (UInt(0x1) << UInt(7))) >> UInt(7)).litValue(), 1)

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

  def dasm(x: UInt) =
    if (x === AUIPC)      "AUIPC x%d, %x".format(rd(x), uimm(x)) 
    else if (x === LUI)   "LUI x%d, %x".format(rd(x), uimm(x))
    else if (x === JAL)   "JAL x%d, %x".format(rd(x), jimm(x))
    else if (x === JALR)  "JALR x%d, x%d, %x".format(rd(x), rs2(x), iimm(x))
    else if (x === BEQ)   "BEQ x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === BNE)   "BNE x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === BLT)   "BLT x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === BGE)   "BGE x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === BLTU)  "BLTU x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === BGEU)  "BGEU x%d, x%d, %x".format(rs1(x), rs2(x), bimm(x))
    else if (x === LB)    "LB x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === LH)    "LH x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === LW)    "LW x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === LBU)   "LBU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === LHU)   "LHU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SB)    "SB x%d, x%d, %x".format(rs2(x), rs1(x), simm(x))
    else if (x === SH)    "SH x%d, x%d, %x".format(rs2(x), rs1(x), simm(x))
    else if (x === SW)    "SW x%d, x%d, %x".format(rs2(x), rs1(x), simm(x))
    else if (x === ADDI)  "ADDI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SLTI)  "SLTI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SLTIU) "SLTIU x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === XORI)  "XORI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === ORI)   "ORI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === ANDI)  "ANDI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SLLI)  "SLLI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SRLI)  "SRLI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === SRAI)  "SRAI x%d, x%d, %x".format(rd(x), rs1(x), iimm(x))
    else if (x === ADD)   "ADD x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SUB)   "SUB x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SLT)   "SLT x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SLTU)  "SLTU x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === XOR)   "XOR x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === OR)    "OR x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === AND)   "AND x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SLL)   "SLL x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SRL)   "SRL x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === SRA)   "SRA x%d, x%d, x%d".format(rd(x), rs1(x), rs2(x))
    else if (x === CSRRW)  "CSRRW x%d, %s, %d".format(rd(x), csr_addr(x), rs1(x))
    else if (x === CSRRWI) "CSRRWI x%d, %s, %d".format(rd(x), csr_addr(x), rs1(x))
    else if (x === NOP)   "NOP"
    else "????"
  def csr_addr(x: UInt) = {
    val inst_31_20 = Cat(inst_31(x), inst_30_25(x), inst_24_21(x), inst_20(x))
    if (inst_31_20 === CSR.TOHOST) "tohost"
    else if (inst_31_20 === CSR.HARTID) "hartid"
    else if (inst_31_20 === CSR.STATUS) "status"
    else x.litValue().toString 
  }

  def decode(x: UInt) =
    //                                                                   kill                    wb_en
    //                         pc_sel   A_sel   B_sel   alu_op     br_type |  st_type ld_type wb_sel |  csr_cmd
    //                           |        |       |      |          |      |  |       |       |      |  |
    if (x === AUIPC)     Array(pc_4,   a_pc,   b_imm, imm_u, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === LUI)  Array(pc_4,   a_pc,   b_imm, imm_u, alu_copy_b, br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === JAL)  Array(pc_alu, a_pc,   b_imm, imm_j, alu_add,    br_xxx, y, st_xxx, ld_xxx, wb_pc_4, y, csr_n)
    else if (x === JALR) Array(pc_alu, a_rs1,  b_imm, imm_i, alu_add,    br_xxx, y, st_xxx, ld_xxx, wb_pc_4, y, csr_n)
    else if (x === BEQ)  Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_eq,  n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === BNE)  Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ne,  n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === BLT)  Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_lt,  n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === BGE)  Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ge,  n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === BLTU) Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_ltu, n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === BGEU) Array(pc_4,   a_pc,   b_imm, imm_b, alu_add,    br_geu, n, st_xxx, ld_xxx, wb_alu, n, csr_n)
    else if (x === LB)   Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lb,  wb_mem, y, csr_n)
    else if (x === LH)   Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lh,  wb_mem, y, csr_n)
    else if (x === LW)   Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lw,  wb_mem, y, csr_n)
    else if (x === LBU)  Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lbu, wb_mem, y, csr_n)
    else if (x === LHU)  Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_lhu, wb_mem, y, csr_n)
    else if (x === SB)   Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sb,  ld_xxx, wb_alu, n, csr_n)
    else if (x === SH)   Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sh,  ld_xxx, wb_alu, n, csr_n)
    else if (x === SW)   Array(pc_4,   a_rs1,  b_imm, imm_s, alu_add,    br_xxx, n, st_sw,  ld_xxx, wb_alu, n, csr_n)
    else if (x === ADDI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLTI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_slt,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLTIU) Array(pc_4,  a_rs1,  b_imm, imm_i, alu_sltu,   br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === XORI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_xor,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === ORI)  Array(pc_4,   a_rs1,  b_imm, imm_i, alu_or,     br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === ANDI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_and,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLLI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_sll,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SRLI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_srl,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SRAI) Array(pc_4,   a_rs1,  b_imm, imm_i, alu_sra,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === ADD)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_add,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SUB)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sub,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLT)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_slt,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLTU) Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sltu,   br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === XOR)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_xor,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === OR)   Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_or,     br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === AND)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_and,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SLL)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sll,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SRL)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_srl,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === SRA)  Array(pc_4,   a_rs1,  b_rs2, imm_x, alu_sra,    br_xxx, n, st_xxx, ld_xxx, wb_alu, y, csr_n)
    else if (x === CSRRW) Array(pc_4,  a_rs1,  b_xxx, imm_z, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_w)
    else if (x === CSRRS) Array(pc_4,  a_rs1,  b_xxx, imm_z, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_s)
    else if (x === CSRRC) Array(pc_4,  a_rs1,  b_xxx, imm_z, alu_copy_a, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_c)
    else if (x === CSRRWI) Array(pc_4, a_xxx,  b_xxx, imm_z, alu_copy_b, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_w)
    else if (x === CSRRSI) Array(pc_4, a_xxx,  b_xxx, imm_z, alu_copy_b, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_s)
    else if (x === CSRRCI) Array(pc_4, a_xxx,  b_xxx, imm_z, alu_copy_b, br_xxx, n, st_xxx, ld_xxx, wb_csr, n, csr_c)
    else                  Array(pc_4,   a_xxx,  b_xxx, imm_i, alu_xxx,    br_xxx, n, st_xxx, ld_xxx, wb_alu, n, csr_n)
}

object HexCommon {
  private val mem = HashMap[BigInt, BigInt]()
  private var filename = ""
  private var maxcycles = 0
  private var readcycles = 2
  private var writecycles = 1

  def parseOpts(args: Array[String]) = {
    var filename = ""
    var maxcycles = 0
    var verbose = false
    for (arg <- args) {
      if (arg.substring(0, 8) == "+verbose") {
        verbose = true
      } else if (arg.substring(0, 9) == "+loadmem=") {
        filename = arg.substring(9)
      } else if (arg.substring(0, 12) == "+max-cycles=") {
        maxcycles = arg.substring(12).toInt
      } else if (arg.substring(0, 13) == "+read-cycles=") {
        readcycles = arg.substring(13).toInt
      } else if (arg.substring(0, 14) == "+write-cycles=") {
        readcycles = arg.substring(14).toInt
      } 
    }
    (filename, maxcycles, verbose)
  }

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
