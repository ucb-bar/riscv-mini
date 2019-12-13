// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import mini._

class BrCondTester(br: => BrCond)(implicit p: freechips.rocketchip.config.Parameters) extends BasicTester with TestUtils {
  import Control._
  val dut = Module(br)
  val ctrl = Module(new Control)
  val xlen = p(XLEN)

  override val insts = Seq.fill(10)(Seq(
    B(Funct3.BEQ, 0, 0, 0),
    B(Funct3.BNE, 0, 0, 0),
    B(Funct3.BLT, 0, 0, 0),
    B(Funct3.BGE, 0, 0, 0),
    B(Funct3.BLTU, 0, 0, 0),
    B(Funct3.BGEU, 0, 0, 0))).flatten

  val (cntr, done) = Counter(true.B, insts.size)
  val rs1 = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val rs2 = Seq.fill(insts.size)(rnd.nextInt()) map toBigInt
  val eq  = VecInit((rs1 zip rs2) map { case (a, b) => (a == b).B })
  val ne  = VecInit((rs1 zip rs2) map { case (a, b) => (a != b).B })
  val lt  = VecInit((rs1 zip rs2) map { case (a, b) => (a.toInt < b.toInt).B })
  val ge  = VecInit((rs1 zip rs2) map { case (a, b) => (a.toInt >= b.toInt).B })
  val ltu = VecInit((rs1 zip rs2) map { case (a, b) => (a < b).B })
  val geu = VecInit((rs1 zip rs2) map { case (a, b) => (a >= b).B })
  val out = Mux(dut.io.br_type === BR_EQ,  eq(cntr),
            Mux(dut.io.br_type === BR_NE,  ne(cntr),
            Mux(dut.io.br_type === BR_LT,  lt(cntr),
            Mux(dut.io.br_type === BR_GE,  ge(cntr),
            Mux(dut.io.br_type === BR_LTU, ltu(cntr),
            Mux(dut.io.br_type === BR_GEU, geu(cntr), false.B))))))

  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.br_type := ctrl.io.br_type
  dut.io.rs1 := VecInit(rs1 map (_.U))(cntr)
  dut.io.rs2 := VecInit(rs2 map (_.U))(cntr)

  when(done) { stop(); stop() } // from VendingMachine example...
  assert(dut.io.taken === out)
  printf("Counter: %d, BrType: 0x%x, rs1: 0x%x, rs2: 0x%x, Taken: %d ?= %d\n",
         cntr, dut.io.br_type, dut.io.rs1, dut.io.rs2, dut.io.taken, out)
}

class BrCondTests extends org.scalatest.FlatSpec {
  implicit val p = (new MiniConfig).toInstance
  "BrCondSimple" should "pass" in {
    assert(TesterDriver execute (() => new BrCondTester(new BrCondSimple)))
  }
  "BrCondArea" should "pass" in {
    assert(TesterDriver execute (() => new BrCondTester(new BrCondArea)))
  }
}
