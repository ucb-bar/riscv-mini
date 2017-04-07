// See LICENSE for license details.

package mini

import chisel3.Bundle
import config.Parameters

abstract class ParameterizedBundle(implicit p: config.Parameters) extends chisel3.Bundle {
  override def cloneType =
    this.getClass.getConstructors.head.newInstance(p).asInstanceOf[this.type]
}

trait TestType {
  def tests: List[String]
  def maxcycles: Long
}

case object SimpleTests extends TestType {
  val tests = List("rv32ui-p-simple")
  val maxcycles = 15000L
}

case object ISATests extends TestType {
  val tests = (List("simple", "add", "addi", "auipc", "and", "andi", // TODO: "fence_i",
    "sb", "sh", "sw", "lb", "lbu", "lh", "lhu", "lui", "lw",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "j", "jal", "jalr",
    "or", "ori", "sll", "slli", "slt", "slti", "sra", "srai", "sub", "xor", "xori"
  ) map (t => s"rv32ui-p-${t}")) ++ (List(
    "sbreak", "scall", "illegal", "ma_fetch", "ma_addr", "csr" //, TODO: "timer"
  ) map (t => s"rv32mi-p-${t}"))
  val maxcycles = 15000L
}

case object BmarkTests extends TestType {
  val tests = List(
    "median.riscv", "multiply.riscv", "qsort.riscv", "towers.riscv", "vvadd.riscv"
  )
  val maxcycles = 1500000L
}
