package mini

import Chisel._

object ALU {
  val ALU_ADD    = UInt(0, 4)
  val ALU_SUB    = UInt(1, 4)
  val ALU_AND    = UInt(2, 4)
  val ALU_OR     = UInt(3, 4)
  val ALU_XOR    = UInt(4, 4)
  val ALU_SLT    = UInt(5, 4)
  val ALU_SLL    = UInt(6, 4)
  val ALU_SLTU   = UInt(7, 4)
  val ALU_SRL    = UInt(8, 4)
  val ALU_SRA    = UInt(9, 4)
  val ALU_COPY_A = UInt(10, 4)
  val ALU_COPY_B = UInt(11, 4)
  val ALU_XXX    = UInt(15, 4)
}

class ALUIo extends CoreBundle {
  val A = UInt(INPUT, xlen)
  val B = UInt(INPUT, xlen)
  val alu_op = UInt(INPUT, 4)
  val out = UInt(OUTPUT, xlen)
  val sum = UInt(OUTPUT, xlen)
}

import ALU._

abstract class ALU extends Module with CoreParams {
  val io = new ALUIo
}

class ALUSimple extends ALU {
  val shamt = io.B(4,0).toUInt

  io.out := MuxLookup(io.alu_op, io.B, Seq(
      ALU_ADD  -> (io.A + io.B),
      ALU_SUB  -> (io.A - io.B),
      ALU_SRA  -> (io.A.toSInt >> shamt).toUInt,
      ALU_SRL  -> (io.A >> shamt),
      ALU_SLL  -> (io.A << shamt),
      ALU_SLT  -> (io.A.toSInt < io.B.toSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND  -> (io.A & io.B),
      ALU_OR   -> (io.A | io.B),
      ALU_XOR  -> (io.A ^ io.B),
      ALU_COPY_A -> io.A))

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B)
}

class ALUArea extends ALU { 
  val sum = io.A + Mux(io.alu_op(0), -io.B, io.B)
  val cmp = Mux(io.A(xlen-1) === io.B(xlen-1), sum(xlen-1),
            Mux(io.alu_op(1), io.B(xlen-1), io.A(xlen-1)))
  val shamt  = io.B(4,0).toUInt
  val shin   = Mux(io.alu_op(3), io.A, Reverse(io.A))
  val shiftr = (Cat(io.alu_op(0) && shin(xlen-1), shin).toSInt >> shamt)(xlen-1, 0)
  val shiftl = Reverse(shiftr)

  val out = 
    Mux(io.alu_op === ALU_ADD || io.alu_op === ALU_SUB, sum,
    Mux(io.alu_op === ALU_SLT || io.alu_op === ALU_SLTU, cmp,
    Mux(io.alu_op === ALU_SRA || io.alu_op === ALU_SRL, shiftr,
    Mux(io.alu_op === ALU_SLL, shiftl,
    Mux(io.alu_op === ALU_AND, (io.A & io.B),
    Mux(io.alu_op === ALU_OR,  (io.A | io.B),
    Mux(io.alu_op === ALU_XOR, (io.A ^ io.B), 
    Mux(io.alu_op === ALU_COPY_A, io.A, io.B))))))))


  io.out := out
  io.sum := sum
}

class ALUdecIo extends Bundle {
  val opcode = Bits(INPUT, 7)
  val funct  = Bits(INPUT, 3)
  val add_rshift_type = Bool(INPUT)
  val alu_op = Bits(OUTPUT, 4)
}

class ALUdec extends Module {
  val io = new ALUdecIo

  val alu_op1 = MuxLookup(io.funct, ALU_XXX, Seq(
    Funct3.ADD  -> Mux(io.add_rshift_type, ALU_SUB, ALU_ADD),
    Funct3.SLL  -> ALU_SLL,
    Funct3.SLT  -> ALU_SLT,
    Funct3.SLTU -> ALU_SLTU,
    Funct3.XOR  -> ALU_XOR,
    Funct3.OR   -> ALU_OR,
    Funct3.AND  -> ALU_AND,
    Funct3.SR   -> Mux(io.add_rshift_type, ALU_SRA, ALU_SRL)) )

  val alu_op2 = MuxLookup(io.opcode, ALU_XXX, Seq(
    Opcode.LUI    -> ALU_COPY_B,
    Opcode.AUIPC  -> ALU_ADD,
    Opcode.JAL    -> ALU_ADD,
    Opcode.JALR   -> ALU_ADD,
    Opcode.BRANCH -> ALU_ADD,
    Opcode.STORE  -> ALU_ADD,
    Opcode.LOAD   -> ALU_ADD,
    Opcode.RTYPE  -> alu_op1,
    Opcode.ITYPE  -> alu_op1) )

  io.alu_op := alu_op2  
} 

class ALUTopIO extends CoreBundle {
  val opcode = UInt(INPUT, 7)
  val funct = UInt(INPUT, 3)
  val add_rshift_type = Bool(INPUT)

  val A = UInt(INPUT, xlen)
  val B = UInt(INPUT, xlen)
  val out = UInt(OUTPUT, xlen)
}

class ALUTop extends Module {
  val io = new ALUTopIO
  val alu     = params(BuildALU)()
  val alu_dec = Module(new ALUdec)

  io.opcode <> alu_dec.io.opcode
  io.funct <> alu_dec.io.funct
  io.add_rshift_type <> alu_dec.io.add_rshift_type

  io.A <> alu.io.A
  io.B <> alu.io.B
  io.out <> alu.io.out

  alu_dec.io.alu_op <> alu.io.alu_op
}
