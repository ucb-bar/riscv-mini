// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.testers._
import ALU._

class ALUTester(alu: => ALU)(implicit p: config.Parameters) extends BasicTester with TestUtils {
  val dut = Module(alu)
  val ctrl = Module(new Control)
  val xlen = p(XLEN)

  val (cntr, done) = Counter(true.B, insts.size)
  val rs1  = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val rs2  = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val sum  = Vec((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt + b.toInt).U(xlen.W) })
  val diff = Vec((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt - b.toInt).U(xlen.W) })
  val and  = Vec((rs1 zip rs2) map { case (a, b) => (a & b).U(xlen.W) })
  val or   = Vec((rs1 zip rs2) map { case (a, b) => (a | b).U(xlen.W) })
  val xor  = Vec((rs1 zip rs2) map { case (a, b) => (a ^ b).U(xlen.W) })
  val slt  = Vec((rs1 zip rs2) map { case (a, b) => (if (a.toInt < b.toInt) 1 else 0).U(xlen.W) })
  val sltu = Vec((rs1 zip rs2) map { case (a, b) => (if (a < b) 1 else 0).U(xlen.W) })
  val sll  = Vec((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt << (b.toInt & 0x1f)).U(xlen.W) })
  val srl  = Vec((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt >>> (b.toInt & 0x1f)).U(xlen.W) })
  val sra  = Vec((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt >> (b.toInt & 0x1f)).U(xlen.W) })
  val out = (Mux(dut.io.alu_op === ALU_ADD,  sum(cntr),
             Mux(dut.io.alu_op === ALU_SUB,  diff(cntr),
             Mux(dut.io.alu_op === ALU_AND,  and(cntr),
             Mux(dut.io.alu_op === ALU_OR,   or(cntr),
             Mux(dut.io.alu_op === ALU_XOR,  xor(cntr),
             Mux(dut.io.alu_op === ALU_SLT,  slt(cntr),
             Mux(dut.io.alu_op === ALU_SLTU, sltu(cntr),
             Mux(dut.io.alu_op === ALU_SLL,  sll(cntr),
             Mux(dut.io.alu_op === ALU_SRL,  srl(cntr),
             Mux(dut.io.alu_op === ALU_SRA,  sra(cntr),
             Mux(dut.io.alu_op === ALU_COPY_A, dut.io.A, dut.io.B))))))))))),
             Mux(dut.io.alu_op(0), diff(cntr), sum(cntr)))

  ctrl.io.inst := Vec(insts)(cntr)
  dut.io.alu_op := ctrl.io.alu_op
  dut.io.A := Vec(rs1 map (_.U))(cntr)
  dut.io.B := Vec(rs2 map (_.U))(cntr)

  when(done) { stop(); stop() } // from VendingMachine example...
  assert(dut.io.out === out._1)
  assert(dut.io.sum === out._2)
  printf("Counter: %d, OP: 0x%x, A: 0x%x, B: 0x%x, OUT: 0x%x ?= 0x%x, SUM: 0x%x ?= 0x%x\n",
         cntr, dut.io.alu_op, dut.io.A, dut.io.B, dut.io.out, out._1, dut.io.sum, out._2) 
}

class ALUTests extends org.scalatest.FlatSpec {
  implicit val p = config.Parameters.root((new MiniConfig).toInstance)
  "ALUSimple" should "pass" in {
    assert(TesterDriver execute (() => new ALUTester(new ALUSimple)))
  }
  "ALUArea" should "pass" in {
    assert(TesterDriver execute (() => new ALUTester(new ALUArea)))
  }
}
