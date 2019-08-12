// See LICENSE for license details.

package mini

import aoplib.histogram.HistogramSignal
import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

object ALU {
  val ALU_ADD    = 0.U(4.W)
  val ALU_SUB    = 1.U(4.W)
  val ALU_AND    = 2.U(4.W)
  val ALU_OR     = 3.U(4.W)
  val ALU_XOR    = 4.U(4.W)
  val ALU_SLT    = 5.U(4.W)
  val ALU_SLL    = 6.U(4.W)
  val ALU_SLTU   = 7.U(4.W)
  val ALU_SRL    = 8.U(4.W)
  val ALU_SRA    = 9.U(4.W)
  val ALU_COPY_A = 10.U(4.W)
  val ALU_COPY_B = 11.U(4.W)
  val ALU_XXX    = 15.U(4.W)

  def histogram(alu: ALU): Seq[HistogramSignal] = {
    Seq(
      new HistogramSignal(alu.io.A) {
        override def untilMax: Int = 1000
        override def nBins: Int = 10
      },
      new HistogramSignal(alu.io.B) {
        override def untilMax: Int = 1000
        override def nBins: Int = 10
      }
    )
  }

  def logALUOp(alu: ALU, op: Int): Unit = {
    when(alu.io.alu_op === op.U) {
      printf("A == %d, B == %d, opcode == %d\n", alu.io.A, alu.io.B, alu.io.alu_op)
    }
  }
}

class ALUIo(implicit p: Parameters) extends CoreBundle()(p) {
  val A = Input(UInt(xlen.W))
  val B = Input(UInt(xlen.W))
  val alu_op = Input(UInt(4.W))
  val out = Output(UInt(xlen.W))
  val sum = Output(UInt(xlen.W))
}

import mini.ALU._

abstract class ALU(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new ALUIo)
  def logALUOp(op: Int): Unit = {
    when(io.alu_op === op.U) {
      printf("A == %d, B == %d, opcode == %d\n", io.A, io.B, io.alu_op)
    }
  }
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
