package mini

import Chisel._

case class ALUIn(op: BigInt, A: BigInt, B: BigInt)
case class ALUOut(out: BigInt, sum: BigInt)

object GoldALU {
  import ALU._
  def toBigInt(x: Int) = (BigInt(x >>> 1) << 1) | (x & 0x1)
  def apply(in: ALUIn) = {
    val sum  = toBigInt(in.A.toInt + in.B.toInt)
    val diff = toBigInt(in.A.toInt - in.B.toInt)
    val slt  = if (in.A.toInt < in.B.toInt) toBigInt(1) else toBigInt(0)
    val sltu = if (in.A < in.B) toBigInt(1) else toBigInt(0)
    val sll  = toBigInt(in.A.toInt << (in.B.toInt & 0x1f))
    val srl  = toBigInt(in.A.toInt >>> (in.B.toInt & 0x1f))
    val sra  = toBigInt(in.A.toInt >> (in.B.toInt & 0x1f))
    new ALUOut(
      if (in.op == ALU_ADD.litValue()) sum
      else if (in.op == ALU_SUB.litValue()) diff
      else if (in.op == ALU_AND.litValue()) in.A & in.B
      else if (in.op == ALU_OR.litValue()) in.A | in.B
      else if (in.op == ALU_XOR.litValue()) in.A ^ in.B
      else if (in.op == ALU_SLT.litValue()) slt
      else if (in.op == ALU_SLTU.litValue()) sltu
      else if (in.op == ALU_SLL.litValue()) sll
      else if (in.op == ALU_SRL.litValue()) srl
      else if (in.op == ALU_SRA.litValue()) sra
      else if (in.op == ALU_COPY_A.litValue()) in.A
      else in.B, 
      if ((in.op & 0x1) == 1) diff else sum)
  } 
}

class ALUTests[+T <: ALU](c: T) extends Tester(c) with RandInsts {
  for (inst <- insts) {
    val a = int(rnd.nextInt)
    val b = int(rnd.nextInt)
    val ctrl = GoldControl(new ControlIn(inst, false))
    val gold = GoldALU(new ALUIn(ctrl.alu_op, a, b))
    println("*** %s -> A: %x, B: %x ***".format(dasm(inst), a, b))
    poke(c.io.A, a)
    poke(c.io.B, b)
    poke(c.io.alu_op, ctrl.alu_op)
    expect(c.io.out, gold.out) 
    expect(c.io.sum, gold.sum) 
  }
}
