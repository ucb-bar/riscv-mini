// See LICENSE for license details.

package mini

import chisel3._

object Opcode {
  // Special immediate instructions
  val LUI    = BigInt("0110111", 2).U(7.W)
  val AUIPC  = BigInt("0010111", 2).U(7.W)

  // Jump instructions
  val JAL    = BigInt("1101111", 2).U(7.W)
  val JALR   = BigInt("1100111", 2).U(7.W)

  // Branch instructions
  val BRANCH = BigInt("1100011", 2).U(7.W)

  // Load and store instrucdtions
  val STORE  = BigInt("0100011", 2).U(7.W)
  val LOAD   = BigInt("0000011", 2).U(7.W)

  // Arithmetic instructions
  val RTYPE  = BigInt("0110011", 2).U(7.W)
  val ITYPE  = BigInt("0010011", 2).U(7.W)

  val MEMORY = BigInt("0001111", 2).U(7.W)
  val SYSTEM = BigInt("1110011", 2).U(7.W)
}

object Funct3 {
  // Branch function codes
  val BEQ  = BigInt("000", 2).U(3.W)
  val BNE  = BigInt("001", 2).U(3.W)
  val BLT  = BigInt("100", 2).U(3.W)
  val BGE  = BigInt("101", 2).U(3.W)
  val BLTU = BigInt("110", 2).U(3.W)
  val BGEU = BigInt("111", 2).U(3.W)

  // Load and store function codes
  val LB   = BigInt("000", 2).U(3.W)
  val LH   = BigInt("001", 2).U(3.W)
  val LW   = BigInt("010", 2).U(3.W)
  val LBU  = BigInt("100", 2).U(3.W)
  val LHU  = BigInt("101", 2).U(3.W)
  val SB   = BigInt("000", 2).U(3.W)
  val SH   = BigInt("001", 2).U(3.W)
  val SW   = BigInt("010", 2).U(3.W)

  // Arithmetic R-type and I-type functions codes
  val ADD  = BigInt("000", 2).U(3.W)
  val SLL  = BigInt("001", 2).U(3.W)
  val SLT  = BigInt("010", 2).U(3.W)
  val SLTU = BigInt("011", 2).U(3.W)
  val XOR  = BigInt("100", 2).U(3.W)
  val OR   = BigInt("110", 2).U(3.W)
  val AND  = BigInt("111", 2).U(3.W)
  val SR   = BigInt("101", 2).U(3.W)

  val CSRRW  = BigInt("001", 2).U(3.W)
  val CSRRS  = BigInt("010", 2).U(3.W)
  val CSRRC  = BigInt("011", 2).U(3.W)
  val CSRRWI = BigInt("101", 2).U(3.W)
  val CSRRSI = BigInt("110", 2).U(3.W)
  val CSRRCI = BigInt("111", 2).U(3.W)
}

object Funct7 {
  val U = BigInt("0000000", 2).U(7.W)
  val S = BigInt("0100000", 2).U(7.W)
}

object Funct12 {
  val ECALL  = BigInt("000000000000", 2).U(12.W)
  val EBREAK = BigInt("000000000001", 2).U(12.W)
  val ERET   = BigInt("000100000000", 2).U(12.W)
}
