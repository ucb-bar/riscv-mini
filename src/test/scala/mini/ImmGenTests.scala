// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util._
import chiseltest._
import chiseltest.formal._
import org.scalatest.flatspec.AnyFlatSpec

class ImmGenTester(imm: => ImmGen) extends BasicTester with TestUtils {
  import Control._
  val dut = Module(imm)
  val ctrl = Module(new Control)

  val (cntr, done) = Counter(true.B, insts.size)
  val i = VecInit(insts.map(iimm))
  val s = VecInit(insts.map(simm))
  val b = VecInit(insts.map(bimm))
  val u = VecInit(insts.map(uimm))
  val j = VecInit(insts.map(jimm))
  val z = VecInit(insts.map(zimm))
  val x = VecInit(insts.map(iimm).map(x => (x.litValue & -2).U))
  val out = Mux(
    dut.io.sel === IMM_I,
    i(cntr),
    Mux(
      dut.io.sel === IMM_S,
      s(cntr),
      Mux(
        dut.io.sel === IMM_B,
        b(cntr),
        Mux(
          dut.io.sel === IMM_U,
          u(cntr),
          Mux(dut.io.sel === IMM_J, j(cntr), Mux(dut.io.sel === IMM_Z, z(cntr), x(cntr)))
        )
      )
    )
  )

  ctrl.io.inst := VecInit(insts)(cntr)
  dut.io.inst := ctrl.io.inst
  dut.io.sel := ctrl.io.imm_sel

  when(done) { stop() }
  assert(dut.io.out === out)
  printf("Counter: %d, Type: 0x%x, Out: %x ?= %x\n", cntr, dut.io.sel, dut.io.out, out)
}

class ImmGenTests extends AnyFlatSpec with ChiselScalatestTester with Formal {
  "ImmGenWire" should "pass" in {
    test(new ImmGenTester(new ImmGenWire(32))).runUntilStop()
  }
  "ImmGenMux" should "pass" in {
    test(new ImmGenTester(new ImmGenMux(32))).runUntilStop()
  }
  "ImmGenMux" should "be equivalent to ImmGenWire" in {
    // since there is no state (registers/memory) in the ImmGen, a single cycle check is enough to prove equivalence
    verify(new ImmGenEquivalenceCheck(new ImmGenMux(32)), Seq(BoundedCheck(1)))
  }
}

class ImmGenEquivalenceCheck(other: => ImmGen) extends Module {
  val dut = Module(other)
  val ref = Module(new ImmGenWire(dut.xlen))

  // arbitrary inputs
  val io = IO(chiselTypeOf(dut.io))

  // connect the same inputs to both modules (the outputs will be overwritten to always connect to the reference)
  dut.io <> io; ref.io <> io

  // check to ensure that outputs are the same
  assert(ref.io.out === dut.io.out, "out: expected: %d, actual: %d", ref.io.out, dut.io.out)
}
