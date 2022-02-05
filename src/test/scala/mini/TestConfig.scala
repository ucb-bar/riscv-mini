// See LICENSE for license details.

package mini

trait TestConfig {
  def tests:      List[String]
  def maxcycles:  Int
  def namePrefix: String
}

case object ISATests extends TestConfig {
  val tests = (
    List(
      "simple",
      "add",
      "addi",
      "auipc",
      "and",
      "andi", // TODO: "fence_i",
      "sb",
      "sh",
      "sw",
      "lb",
      "lbu",
      "lh",
      "lhu",
      "lui",
      "lw",
      "beq",
      "bge",
      "bgeu",
      "blt",
      "bltu",
      "bne",
      "j",
      "jal",
      "jalr",
      "or",
      "ori",
      "sll",
      "slli",
      "slt",
      "slti",
      "sra",
      "srai",
      "sub",
      "xor",
      "xori"
    ).map(t => s"rv32ui-p-${t}")
  ) ++ (
    List(
      "sbreak",
      "scall",
      "illegal",
      "ma_fetch",
      "ma_addr",
      "csr" //, TODO: "timer"
    ).map(t => s"rv32mi-p-${t}")
  )
  val maxcycles = 15000
  val namePrefix = "ISATests"
}

case object BmarkTests extends TestConfig {
  val tests = List(
    "median.riscv",
    "multiply.riscv",
    "qsort.riscv",
    "towers.riscv",
    "vvadd.riscv"
  )
  val maxcycles = 1500000
  val namePrefix = "BmarkTests"
}

case object LargeBmarkTests extends TestConfig {
  val tests = List(
    "median.riscv-large",
    "multiply.riscv-large",
    "qsort.riscv-large",
    "towers.riscv-large",
    "vvadd.riscv-large"
  )
  val maxcycles = 5000000
  val namePrefix = "LargeBmarkTests"
}
