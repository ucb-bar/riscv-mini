package mini

import Chisel._

object Opcode {
  // Special immediate instructions
  val LUI    = UInt("b0110111")
  val AUIPC  = UInt("b0010111")

  // Jump instructions
  val JAL    = UInt("b1101111")
  val JALR   = UInt("b1100111")

  // Branch instructions
  val BRANCH = UInt("b1100011")

  // Load and store instrucdtions
  val STORE  = UInt("b0100011")
  val LOAD   = UInt("b0000011")

  // Arithmetic instructions
  val RTYPE  = UInt("b0110011")
  val ITYPE  = UInt("b0010011")

  val MEMORY = UInt("b0001111")
  val SYSTEM = UInt("b1110011")
}

object Funct3 {
  // Branch function codes
  val BEQ  = UInt("b000")
  val BNE  = UInt("b001")
  val BLT  = UInt("b100")
  val BGE  = UInt("b101")
  val BLTU = UInt("b110")
  val BGEU = UInt("b111")

  // Load and store function codes
  val LB   = UInt("b000")
  val LH   = UInt("b001")
  val LW   = UInt("b010")
  val LBU  = UInt("b100")
  val LHU  = UInt("b101")
  val SB   = UInt("b000")
  val SH   = UInt("b001")
  val SW   = UInt("b010")

  // Arithemetic R-type and I-type functions codes
  val ADD  = UInt("b000")
  val SLL  = UInt("b001")
  val SLT  = UInt("b010")
  val SLTU = UInt("b011")
  val XOR  = UInt("b100")
  val OR   = UInt("b110")
  val AND  = UInt("b111")
  val SR   = UInt("b101")

  val CSRRW   = UInt("b001")
  val CSRRS   = UInt("b010")
  val CSRRC   = UInt("b011")
  val CSRRWI  = UInt("b101")
  val CSRRSI  = UInt("b110")
  val CSRRCI  = UInt("b111")
}

object Funct7 {
  val U = UInt("b0000000")
  val S = UInt("b0100000")
}

object Funct12 {
  val ECALL  = UInt("b000000000000")
  val EBREAK = UInt("b000000000001")
  val ERET   = UInt("b000100000000")
}

object AddRshiftType {
  val ADD = UInt("b0")
  val SUB = UInt("b1")
  val SRL = UInt("b0")
  val SRA = UInt("b1")
}
