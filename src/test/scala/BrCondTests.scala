package mini

import chisel3.iotesters.PeekPokeTester

case class BrCondIn(brType: BigInt, rs1: BigInt, rs2: BigInt)
case class BrCondOut(taken: Boolean)

object GoldBrCond {
  import Control._
  def toBigInt(x: Int) = (BigInt(x >>> 1) << 1) | (x & 0x1)
  def apply(in: BrCondIn) = new BrCondOut(if (in.brType == BR_EQ.litValue()) in.rs1 == in.rs2
    else if (in.brType == BR_NE.litValue()) in.rs1 != in.rs2
    else if (in.brType == BR_LT.litValue()) in.rs1.toInt < in.rs2.toInt
    else if (in.brType == BR_GE.litValue()) in.rs1.toInt >= in.rs2.toInt
    else if (in.brType == BR_LTU.litValue()) in.rs1 < in.rs2
    else if (in.brType == BR_GEU.litValue()) in.rs1 >= in.rs2 else false)
}

class BrCondTests[+T <: BrCond](c: T) extends PeekPokeTester(c) with RandInsts {
  override val insts = (List.fill(10){List(
    B(Funct3.BEQ, 0, 0, 0),
    B(Funct3.BNE, 0, 0, 0),
    B(Funct3.BLT, 0, 0, 0),
    B(Funct3.BGE, 0, 0, 0),
    B(Funct3.BLTU, 0, 0, 0),
    B(Funct3.BGEU, 0, 0, 0))}).flatten
  for (inst <- insts) {
    val a = rand_data
    val b = rand_data
    val ctrl = GoldControl(new ControlIn(inst))
    val gold = GoldBrCond(new BrCondIn(ctrl.br_type, a, b))
    println(s"*** ${dasm(inst)} -> A: %x, B: %x ***".format(a, b))
    poke(c.io.br_type, ctrl.br_type)
    poke(c.io.rs1, a)
    poke(c.io.rs2, b)
    expect(c.io.taken, gold.taken) 
  }
}
