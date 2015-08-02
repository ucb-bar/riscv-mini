package mini

import Chisel._
import ALU._

class ALUdecIo extends Bundle {
  val opcode = Bits(INPUT, 7)
  val funct  = Bits(INPUT, 3)
  val add_rshift_type = Bool(INPUT)
  val alu_op = Bits(OUTPUT, 4)
}

class ALUdec extends Module {
  val io = new ALUdecIo

  val alu_op1 = MuxLookup(io.funct, ALU_XXX, Seq(
    Funct3.ADD_SUB -> Mux(io.add_rshift_type, ALU_SUB, ALU_ADD),
    Funct3.SLL     -> ALU_SLL,
    Funct3.SLT     -> ALU_SLT,
    Funct3.SLTU    -> ALU_SLTU,
    Funct3.XOR     -> ALU_XOR,
    Funct3.OR      -> ALU_OR,
    Funct3.AND     -> ALU_AND,
    Funct3.SRL_SRA -> Mux(io.add_rshift_type, ALU_SRA, ALU_SRL)) )

  val alu_op2 = MuxLookup(io.opcode, ALU_XXX, Seq(
    Opcode.LUI       -> ALU_COPY_B,
    Opcode.AUIPC     -> ALU_ADD,
    Opcode.JAL       -> ALU_ADD,
    Opcode.JALR      -> ALU_ADD,
    Opcode.BRANCH    -> ALU_ADD,
    Opcode.STORE     -> ALU_ADD,
    Opcode.LOAD      -> ALU_ADD,
    Opcode.ARI_RTYPE -> alu_op1,
    Opcode.ARI_ITYPE -> alu_op1) )

  io.alu_op := alu_op2  
} 

class ALUTopIO extends CoreBundle {
  val opcode = UInt(INPUT, 7)
  val funct = UInt(INPUT, 3)
  val add_rshift_type = Bool(INPUT)

  val A = UInt(INPUT, instLen)
  val B = UInt(INPUT, instLen)
  val out = UInt(OUTPUT, instLen)
}

class ALUTop extends Module {
  val io = new ALUTopIO
  val alu     = Module(new ALU)
  val alu_dec = Module(new ALUdec)

  io.opcode <> alu_dec.io.opcode
  io.funct <> alu_dec.io.funct
  io.add_rshift_type <> alu_dec.io.add_rshift_type

  io.A <> alu.io.A
  io.B <> alu.io.B
  io.out <> alu.io.out

  alu_dec.io.alu_op <> alu.io.alu_op
}
