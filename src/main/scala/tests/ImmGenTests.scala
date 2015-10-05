package mini

import Chisel._

case class ImmGenIn(inst: UInt, sel: BigInt)
case class ImmGenOut(out: BigInt)

object GoldImmGen {
  private def inst_31(inst: UInt)    = UInt((inst.litValue() >> 31) & 0x1,  1)
  private def inst_30_25(inst: UInt) = UInt((inst.litValue() >> 25) & 0x3f, 6)
  private def inst_24_21(inst: UInt) = UInt((inst.litValue() >> 21) & 0xf,  4)
  private def inst_20(inst: UInt)    = UInt((inst.litValue() >> 20) & 0x1,  1)
  private def inst_19_12(inst: UInt) = UInt((inst.litValue() >> 12) & 0xff, 8)
  private def inst_11_8(inst: UInt)  = UInt((inst.litValue() >> 8)  & 0xf,  4)
  private def inst_7(inst: UInt)     = UInt((inst.litValue() >> 7)  & 0x1,  1)

  def iimm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_24_21(inst), inst_20(inst)).litValue()
  def simm(inst: UInt) = Cat(Cat(Seq.fill(21){inst_31(inst)}),
                             inst_30_25(inst), inst_11_8(inst), inst_7(inst)).litValue()
  def bimm(inst: UInt) = Cat(Cat(Seq.fill(20){inst_31(inst)}),
                             inst_7(inst), inst_30_25(inst), inst_11_8(inst), UInt(0, 1)).litValue()
  def uimm(inst: UInt) = Cat(inst_31(inst), inst_30_25(inst), inst_24_21(inst),
                             inst_20(inst), inst_19_12(inst), UInt(0, 12)).litValue()
  def jimm(inst: UInt) = Cat(Cat(Seq.fill(12){inst_31(inst)}), inst_19_12(inst),
                             inst_20(inst), inst_30_25(inst), inst_24_21(inst), UInt(0, 1)).litValue()
  def zimm(inst: UInt) = (inst.litValue() >> 15) & 0x1f

  import Control._
  def apply(in: ImmGenIn) = new ImmGenOut(if (in.sel == IMM_I.litValue()) iimm(in.inst)
    else if (in.sel == IMM_S.litValue()) simm(in.inst)
    else if (in.sel == IMM_B.litValue()) bimm(in.inst)
    else if (in.sel == IMM_U.litValue()) uimm(in.inst)
    else if (in.sel == IMM_J.litValue()) jimm(in.inst)
    else if (in.sel == IMM_Z.litValue()) zimm(in.inst) else iimm(in.inst) & -2)
}

class ImmGenTests[+T <: ImmGen](c: T) extends Tester(c) with RISCVCommon {
  for (inst <- insts) {
    val ctrl = GoldControl(inst)
    val gold = GoldImmGen(new ImmGenIn(inst, ctrl(3)))
    println("*** %s ***".format(dasm(inst)))
    poke(c.io.inst, inst)
    poke(c.io.sel,  ctrl(3))
    expect(c.io.out, gold.out) 
  }
}
