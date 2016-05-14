package mini

import Chisel._
import Chisel.iotesters.ClassicTester

case class ImmGenIn(inst: UInt, sel: BigInt)
case class ImmGenOut(out: BigInt)

object GoldImmGen extends RISCVCommon {
  import Control._
  def apply(in: ImmGenIn) = new ImmGenOut(if (in.sel == IMM_I.litValue()) iimm(in.inst)
    else if (in.sel == IMM_S.litValue()) simm(in.inst)
    else if (in.sel == IMM_B.litValue()) bimm(in.inst)
    else if (in.sel == IMM_U.litValue()) uimm(in.inst)
    else if (in.sel == IMM_J.litValue()) jimm(in.inst)
    else if (in.sel == IMM_Z.litValue()) zimm(in.inst) else iimm(in.inst) & -2)
}


class ImmGenTests[+T <: ImmGen](c: T) extends ClassicTester(c) with RandInsts {
  type DUT = ImmGen
  for (inst <- insts) {
    val ctrl = GoldControl(new ControlIn(inst))
    val gold = GoldImmGen(new ImmGenIn(inst, ctrl.imm_sel))
    println(s"*** ${dasm(inst)} ***")
    poke(c.io.inst,  inst)
    poke(c.io.sel,   ctrl.imm_sel)
    expect(c.io.out, gold.out) 
  }
}
