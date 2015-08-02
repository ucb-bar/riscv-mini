package mini

import Chisel._

class ALUTests(c: ALUTop) extends Tester(c) {
  for (i <- 0 until 25) {
    val rnd1 = (0x1 << 31) | (rnd.nextInt() & 0x7FFFFFFF)
    val rnd2 = (0xFFFF << 16) | (0x1 << 15) | (rnd.nextInt() & 0x7FFF)
    val A = BigInt(rnd1 >>> 1) << 1 | rnd1 & 1
    val B = BigInt(rnd2 >>> 1) << 1 | rnd2 & 1
    // gold results
    val sum  = A.toInt + B.toInt
    val diff = A.toInt - B.toInt
    val slt  = if (A.toInt < B.toInt) 1 else 0
    val sltu = if (A < B) 1 else 0
    val sll  = A.toInt << (B.toInt & 0x1f)
    val srl  = A.toInt >>> (B.toInt & 0x1f)
    val sra  = A.toInt >> (B.toInt & 0x1f)

    poke(c.io.A, A)
    poke(c.io.B, B)

    println("*** LUI ***")
    poke(c.io.opcode, Opcode.LUI.litValue())
    poke(c.io.funct, rnd.nextInt() & 0x7)
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, B)

    println("*** AUIPC ***")
    poke(c.io.opcode, Opcode.AUIPC.litValue())
    poke(c.io.funct, rnd.nextInt() & 0x7)
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** JAL ***")
    poke(c.io.opcode, Opcode.JAL.litValue())
    poke(c.io.funct, rnd.nextInt() & 0x7)
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** JALR ***")
    poke(c.io.opcode, Opcode.JALR.litValue())
    poke(c.io.funct, rnd.nextInt() & 0x7)
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)
    
    println("*** BEQ ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BEQ.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** BNE ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BNE.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** BLT ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BLT.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** BGE ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BGE.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** BLTU ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BLTU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** BGEU ***")
    poke(c.io.opcode, Opcode.BRANCH.litValue())
    poke(c.io.funct, Funct3.BGEU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** LB ***")
    poke(c.io.opcode, Opcode.LOAD.litValue())
    poke(c.io.funct, Funct3.LB.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** LH ***")
    poke(c.io.opcode, Opcode.LOAD.litValue())
    poke(c.io.funct, Funct3.LH.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** LW ***")
    poke(c.io.opcode, Opcode.LOAD.litValue())
    poke(c.io.funct, Funct3.LW.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** LBU ***")
    poke(c.io.opcode, Opcode.LOAD.litValue())
    poke(c.io.funct, Funct3.LBU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** LHU ***")
    poke(c.io.opcode, Opcode.LOAD.litValue())
    poke(c.io.funct, Funct3.LHU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)
 
    println("*** SB ***")
    poke(c.io.opcode, Opcode.STORE.litValue())
    poke(c.io.funct, Funct3.SB.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** SH ***")
    poke(c.io.opcode, Opcode.STORE.litValue())
    poke(c.io.funct, Funct3.SH.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** SW ***")
    poke(c.io.opcode, Opcode.STORE.litValue())
    poke(c.io.funct, Funct3.SW.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sum)

    println("*** ADD ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.ADD.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.ADD.litValue())
    expect(c.io.out, sum)

    println("*** SUB ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.ADD.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SUB.litValue())
    expect(c.io.out, diff)

    println("*** SLL ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.SLL.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sll)
    
    println("*** SLT ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.SLT.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, slt)

    println("*** SLTU ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.SLTU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sltu)

    println("*** XOR ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.XOR.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, A ^ B)
    
    println("*** SRL ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.SR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRL.litValue())
    expect(c.io.out, srl)
    
    println("*** SRA ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.SR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, sra)
    
    println("*** OR ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.OR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, A | B)

    println("*** AND ***")
    poke(c.io.opcode, Opcode.RTYPE.litValue())
    poke(c.io.funct, Funct3.AND.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, A & B)

    println("*** ADDI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.ADD.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.ADD.litValue())
    expect(c.io.out, sum)

    println("*** SUBI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.ADD.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SUB.litValue())
    expect(c.io.out, diff)

    println("*** SLLI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.SLL.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sll)
    
    println("*** SLTI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.SLT.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, slt)

    println("*** SLTIU ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.SLTU.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, sltu)

    println("*** XORI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.XOR.litValue())
    poke(c.io.add_rshift_type, rnd.nextInt() & 0x1)
    expect(c.io.out, A ^ B)
    
    println("*** SRLI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.SR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRL.litValue())
    expect(c.io.out, srl)
    
    println("*** SRAI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.SR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, sra)
    
    println("*** ORI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.OR.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, A | B)

    println("*** ANDI ***")
    poke(c.io.opcode, Opcode.ITYPE.litValue())
    poke(c.io.funct, Funct3.AND.litValue())
    poke(c.io.add_rshift_type, AddRshiftType.SRA.litValue())
    expect(c.io.out, A & B)
  }
}
