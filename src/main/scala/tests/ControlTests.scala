package mini

import Chisel._

case class ControlIn(inst: UInt, stall: Boolean) 

class ControlOut(ctrl: List[BigInt], stall: Boolean) {
  implicit def toBoolean(x: BigInt) = x != 0
  val (pc_sel :: a_sel :: b_sel :: imm_sel :: alu_op :: br_type :: inst_kill :: 
       st_type :: ld_type :: wb_sel :: wb_en :: csr_cmd :: illegal :: Nil) = ctrl
  val pc_check = pc_sel == Control.PC_ALU.litValue()
  val data_en  = !stall && (st_type != 0 || ld_type != 0)
}

object GoldControl {
  private val default = Control.default map (_.litValue())
  private val map = Control.map map (x => x._1 -> (x._2 map (_.litValue())))
  def apply(in: ControlIn) = new ControlOut(
    map find {
      case (x: BitPat, y) => (x === in.inst).isTrue
      case (x: UInt,   y) => (x === in.inst).isTrue
      case _ => false
    } match {
      case None => default
      case Some(p) => p._2
    }, in.stall)
}

class ControlTests(c: Control) extends Tester(c) with RandInsts { 
  def expect(ctrl: ControlOut) {
    expect(c.io.ctrl.pc_sel,    ctrl.pc_sel)
    expect(c.io.ctrl.A_sel,     ctrl.a_sel)
    expect(c.io.ctrl.B_sel,     ctrl.b_sel)
    expect(c.io.ctrl.imm_sel,   ctrl.imm_sel)
    expect(c.io.ctrl.alu_op,    ctrl.alu_op)
    expect(c.io.ctrl.br_type,   ctrl.br_type)
    expect(c.io.ctrl.inst_kill, ctrl.inst_kill)
    expect(c.io.ctrl.st_type,   ctrl.st_type)
    expect(c.io.ctrl.data_en,   ctrl.data_en)
    step(1)
    expect(c.io.ctrl.st_type_r, ctrl.st_type)
    expect(c.io.ctrl.ld_type,   ctrl.ld_type)
    expect(c.io.ctrl.wb_sel,    ctrl.wb_sel)
    expect(c.io.ctrl.wb_en,     ctrl.wb_en)
    expect(c.io.ctrl.csr_cmd,   ctrl.csr_cmd)
    expect(c.io.ctrl.illegal,   ctrl.illegal)
    expect(c.io.ctrl.pc_check,  ctrl.pc_check)
  }

  poke(c.io.ctrl.stall, 0)
  poke(c.io.ctrl.flush, 0)
  for ((inst, i) <- insts.zipWithIndex) {
    println("***** %s *****".format(dasm(inst)))
    poke(c.io.ctrl.inst, inst.litValue())
    expect(GoldControl(new ControlIn(inst, false)))
  }
}
