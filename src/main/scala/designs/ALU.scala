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
  val A = Bits(INPUT, instLen)
  val B = Bits(INPUT, instLen)
  val alu_op = Bits(INPUT, 4)
  val out = Bits(OUTPUT, instLen)
  val sum = Bits(OUTPUT, instLen)
}

import ALU._

class ALU extends Module {
  val io = new ALUIo
  val shamt = io.B(4,0).toUInt

  io.out := MuxLookup(io.alu_op, io.B, Seq(
      ALU_ADD  -> (io.A + io.B),
      ALU_SUB  -> (io.A - io.B),
      ALU_SRA  -> (io.A.toSInt >> shamt),
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

class ALUArea extends Module with CoreParams {
  val io = new ALUIo
  val sum = io.A + Mux(io.alu_op(0), -io.B, io.B)
  val cmp = Mux(io.A(instLen-1) === io.B(instLen-1), sum(instLen-1),
            Mux(io.alu_op(1), io.A(instLen-1), io.B(instLen-1)))
  val shamt  = io.B(4,0).toUInt
  val shin   = Mux(io.alu_op(3), io.A, Reverse(io.A))
  val shiftr = (Cat(!io.alu_op(0) && shin(instLen-1), shin).toSInt >> shamt)(instLen-1, 0)
  val shiftl = Reverse(shiftr)

  val out = 
    Mux(io.alu_op === ALU_ADD || io.alu_op === ALU_SUB, sum,
    Mux(io.alu_op === ALU_SLT || io.alu_op === ALU_SLTU, cmp,
    Mux(io.alu_op === ALU_SRA || io.alu_op === ALU_SRL, shiftr,
    Mux(io.alu_op === ALU_SLL, shiftl,
    Mux(io.alu_op === ALU_AND, (io.A & io.B).zext,
    Mux(io.alu_op === ALU_OR,  (io.A | io.B).zext,
    Mux(io.alu_op === ALU_XOR, (io.A ^ io.B).zext, 
    Mux(io.alu_op === ALU_COPY_A, io.A, io.B.zext))))))))

  io.out := out
  io.sum := sum
}
