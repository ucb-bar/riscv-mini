package mini

import Chisel._

case class ImmGenIn(inst: UInt, sel: BigInt)
case class ImmGenOut(out: BigInt)

object GoldImmGen {
  import Control._
  def apply(in: ImmGenIn) = new ImmGenOut(if (in.sel == IMM_I.litValue()) RISCVCommon.iimm(in.inst)
    else if (in.sel == IMM_S.litValue()) RISCVCommon.simm(in.inst)
    else if (in.sel == IMM_B.litValue()) RISCVCommon.bimm(in.inst)
    else if (in.sel == IMM_U.litValue()) RISCVCommon.uimm(in.inst)
    else if (in.sel == IMM_J.litValue()) RISCVCommon.jimm(in.inst)
    else if (in.sel == IMM_Z.litValue()) RISCVCommon.zimm(in.inst) else RISCVCommon.iimm(in.inst) & -2)
}


class ImmGenTests[+T <: ImmGen](c: T) extends Tester(c) with RandInsts {
  for (inst <- insts) {
    val ctrl = GoldControl(new ControlIn(inst))
    val gold = GoldImmGen(new ImmGenIn(inst, ctrl.imm_sel))
    println("*** %s ***".format(dasm(inst)))
    poke(c.io.inst,  inst)
    poke(c.io.sel,   ctrl.imm_sel)
    expect(c.io.out, gold.out) 
  }
}
