package mini

import Chisel._

object Opcode {
  // Special immediate instructions
  val LUI    = Bits("b0110111")
  val AUIPC  = Bits("b0010111")

  // Jump instructions
  val JAL    = Bits("b1101111")
  val JALR   = Bits("b1100111")

  // Branch instructions
  val BRANCH = Bits("b1100011")

  // Load and store instrucdtions
  val STORE  = Bits("b0100011")
  val LOAD   = Bits("b0000011")

  // Arithmetic instructions
  val RTYPE  = Bits("b0110011")
  val ITYPE  = Bits("b0010011")

  val SYSTEM = Bits("b1110011")
}

object Funct3 {
  // Branch function codes
  val BEQ  = Bits("b000")
  val BNE  = Bits("b001")
  val BLT  = Bits("b100")
  val BGE  = Bits("b101")
  val BLTU = Bits("b110")
  val BGEU = Bits("b111")

  // Load and store function codes
  val LB   = Bits("b000")
  val LH   = Bits("b001")
  val LW   = Bits("b010")
  val LBU  = Bits("b100")
  val LHU  = Bits("b101")
  val SB   = Bits("b000")
  val SH   = Bits("b001")
  val SW   = Bits("b010")

  // Arithemetic R-type and I-type functions codes
  val ADD  = Bits("b000")
  val SLL  = Bits("b001")
  val SLT  = Bits("b010")
  val SLTU = Bits("b011")
  val XOR  = Bits("b100")
  val OR   = Bits("b110")
  val AND  = Bits("b111")
  val SR   = Bits("b101")

  val CSRRW   = Bits("b001")
  val CSRRS   = Bits("b010")
  val CSRRC   = Bits("b011")
  val CSRRWI  = Bits("b101")
  val CSRRSI  = Bits("b110")
  val CSRRCI  = Bits("b111")
}

object Funct7 {
  val U = Bits("b0000000")
  val S = Bits("b0100000")
}

object AddRshiftType {
  val ADD = Bits("b0")
  val SUB = Bits("b1")
  val SRL = Bits("b0")
  val SRA = Bits("b1")
}
