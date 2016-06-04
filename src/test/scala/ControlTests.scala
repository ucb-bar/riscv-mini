package mini

import Chisel._
import Chisel.iotesters.PeekPokeTester

case class ControlIn(inst: UInt)

class ControlOut(ctrl: List[BigInt]) {
  val (pc_sel    :: 
       a_sel     :: 
       b_sel     :: 
       imm_sel   :: 
       alu_op    :: 
       br_type   :: 
       inst_kill :: 
       st_type   :: 
       ld_type   :: 
       wb_sel    :: 
       wb_en     :: 
       csr_cmd   :: 
       illegal   :: 
       Nil) = ctrl
}

object GoldControl {
  private val default = Control.default map (_.litValue())
  private val map = Control.map map (x => x._1 -> (x._2 map (_.litValue())))
  def apply(in: ControlIn) = new ControlOut(
    map find (x => x._1.value == (x._1.mask & in.inst.litValue())) match {
      case None => default
      case Some(p) => p._2
    })
}

class ControlTests(c: Control) extends PeekPokeTester(c) with RandInsts {
  def expect(ctrl: ControlOut) {
    expect(c.io.pc_sel,    ctrl.pc_sel)
    expect(c.io.A_sel,     ctrl.a_sel)
    expect(c.io.B_sel,     ctrl.b_sel)
    expect(c.io.imm_sel,   ctrl.imm_sel)
    expect(c.io.alu_op,    ctrl.alu_op)
    expect(c.io.br_type,   ctrl.br_type)
    expect(c.io.inst_kill, ctrl.inst_kill)
    expect(c.io.st_type,   ctrl.st_type)
    expect(c.io.ld_type,   ctrl.ld_type)
    expect(c.io.wb_sel,    ctrl.wb_sel)
    expect(c.io.wb_en,     ctrl.wb_en)
    expect(c.io.csr_cmd,   ctrl.csr_cmd)
    expect(c.io.illegal,   ctrl.illegal)
  }

  for ((inst, i) <- insts.zipWithIndex) {
    println(s"***** ${dasm(inst)} *****")
    poke(c.io.inst, inst.litValue())
    expect(GoldControl(new ControlIn(inst)))
  }
}
