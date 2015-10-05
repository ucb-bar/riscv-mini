package mini

import Chisel._

object GoldControl {
  private val default = Control.default map (_.litValue())
  private val map = Control.map map (x => x._1 -> (x._2 map (_.litValue())))
  def apply(inst: UInt) = {
    map find {
      case (x: BitPat, y) => (x === inst).isTrue
      case (x: UInt,   y) => (x === inst).isTrue
      case _ => false
    } match {
      case None => default
      case Some(p) => p._2
    }
  }
}

class ControlTests(c: Control) extends RISCVTester(c) {
  def checkCtrl(ctrl: Seq[BigInt]) {
    expect(c.io.ctrl.pc_sel,    ctrl(0))
    expect(c.io.ctrl.A_sel,     ctrl(1))
    expect(c.io.ctrl.B_sel,     ctrl(2))
    expect(c.io.ctrl.imm_sel,   ctrl(3))
    expect(c.io.ctrl.alu_op,    ctrl(4))
    expect(c.io.ctrl.br_type,   ctrl(5))
    expect(c.io.ctrl.inst_kill, ctrl(6))
    expect(c.io.ctrl.st_type,   ctrl(7))
    step(1)
    expect(c.io.ctrl.st_type_r, ctrl(7))
    expect(c.io.ctrl.ld_type,   ctrl(8))
    expect(c.io.ctrl.wb_sel,    ctrl(9))
    expect(c.io.ctrl.wb_en,     ctrl(10))
    expect(c.io.ctrl.csr_cmd,   ctrl(11))
    expect(c.io.ctrl.illegal,   ctrl(12))
    expect(c.io.ctrl.pc_check,  ctrl(0) == Control.PC_ALU.litValue())
  }

  poke(c.io.ctrl.stall, 0)
  poke(c.io.ctrl.flush, 0)
  for ((inst, i) <- insts.zipWithIndex) {
    println("***** %s *****".format(dasm(inst)))
    poke(c.io.ctrl.inst, inst.litValue())
    checkCtrl(GoldControl(inst))
  }
}
