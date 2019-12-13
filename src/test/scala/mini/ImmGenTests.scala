// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import mini._

class ImmGenTester(imm: => ImmGen)(implicit p: freechips.rocketchip.config.Parameters) extends BasicTester with TestUtils {
  import Control._
  val dut = Module(imm)
  val ctrl = Module(new Control)
  val xlen = p(XLEN)

  val (cntr, done) = Counter(true.B, insts.size)
  val i = VecInit(insts map iimm)
  val s = VecInit(insts map simm)
  val b = VecInit(insts map bimm)
  val u = VecInit(insts map uimm)
  val j = VecInit(insts map jimm)
  val z = VecInit(insts map zimm)
  val x = VecInit(insts map iimm map (x => (x.litValue() & -2).U))
  val out = Mux(dut.io.sel === IMM_I, i(cntr),
            Mux(dut.io.sel === IMM_S, s(cntr),
            Mux(dut.io.sel === IMM_B, b(cntr),
            Mux(dut.io.sel === IMM_U, u(cntr),
            Mux(dut.io.sel === IMM_J, j(cntr),
            Mux(dut.io.sel === IMM_Z, z(cntr), x(cntr)))))))

  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.inst  := ctrl.io.inst
  dut.io.sel   := ctrl.io.imm_sel

  when(done) { stop(); stop() } // from VendingMachine example...
  assert(dut.io.out === out)
  printf("Counter: %d, Type: 0x%x, Out: %x ?= %x\n",
         cntr, dut.io.sel, dut.io.out, out)
}

class ImmGenTests extends org.scalatest.FlatSpec {
  implicit val p = (new MiniConfig).toInstance
  "ImmGenWire" should "pass" in {
    assert(TesterDriver execute (() => new ImmGenTester(new ImmGenWire)))
  }
  "ImmGenMux" should "pass" in {
    assert(TesterDriver execute (() => new ImmGenTester(new ImmGenMux)))
  }
}
