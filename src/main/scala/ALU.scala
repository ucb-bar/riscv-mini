package mini

import Chisel._
import cde.Parameters

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

class ALUIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A = UInt(INPUT, xlen)
  val B = UInt(INPUT, xlen)
  val alu_op = UInt(INPUT, 4)
  val out = UInt(OUTPUT, xlen)
  val sum = UInt(OUTPUT, xlen)
}

import ALU._

abstract class ALU(implicit val p: Parameters) extends Module with CoreParams {
  val io = new ALUIo
}

class ALUSimple(implicit p: Parameters) extends ALU()(p) {
  val shamt = io.B(4,0).asUInt

  io.out := MuxLookup(io.alu_op, io.B, Seq(
      ALU_ADD  -> (io.A + io.B),
      ALU_SUB  -> (io.A - io.B),
      ALU_SRA  -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL  -> (io.A >> shamt),
      ALU_SLL  -> (io.A << shamt),
      ALU_SLT  -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND  -> (io.A & io.B),
      ALU_OR   -> (io.A | io.B),
      ALU_XOR  -> (io.A ^ io.B),
      ALU_COPY_A -> io.A))

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B)
}

class ALUArea(implicit p: Parameters) extends ALU()(p) { 
  val sum = io.A + Mux(io.alu_op(0), -io.B, io.B)
  val cmp = Mux(io.A(xlen-1) === io.B(xlen-1), sum(xlen-1),
            Mux(io.alu_op(1), io.B(xlen-1), io.A(xlen-1)))
  val shamt  = io.B(4,0).asUInt
  val shin   = Mux(io.alu_op(3), io.A, Reverse(io.A))
  val shiftr = (Cat(io.alu_op(0) && shin(xlen-1), shin).asSInt >> shamt)(xlen-1, 0)
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
