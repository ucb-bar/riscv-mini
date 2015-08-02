package mini

import Chisel._
import TestCommon._

class DatapathTests(c: Datapath) extends Tester(c) {
  def removeSign(x: Int) = BigInt(x >>> 1) << 1 | x & 1

  def pokeExCtrl(ctrl: Array[BigInt], br_cond: Boolean) {
    val inst_type = if (ctrl(8) != ld_xxx || ctrl(6) == y) i_kill else i_next
    val data_re   = if (ctrl(8) != ld_xxx) y else n
    println("=== Execute Control Signals ===")
    poke(c.io.ctrl.pc_sel,    ctrl(0))
    poke(c.io.ctrl.inst_type, inst_type)
    poke(c.io.ctrl.A_sel,     ctrl(1))
    poke(c.io.ctrl.B_sel,     ctrl(2))
    poke(c.io.ctrl.imm_sel,   ctrl(3))
    poke(c.io.ctrl.alu_op,    ctrl(4))
    poke(c.io.ctrl.br_type,   ctrl(5))
    poke(c.io.ctrl.st_type,   ctrl(7))
    poke(c.io.ctrl.data_re,   data_re)
    println("=======================")
  }

  def pokeWbCtrl(ctrl: Array[BigInt]) {
    val inst_type = if (ctrl(7) != st_xxx) i_kill else i_next
    println("=== Write-Back Control Signals ===")
    poke(c.io.ctrl.inst_type, inst_type)
    poke(c.io.ctrl.ld_type, ctrl(8))
    poke(c.io.ctrl.wb_sel,  ctrl(9))
    poke(c.io.ctrl.wb_en,   ctrl(10))
    poke(c.io.ctrl.csr_cmd, ctrl(11))
    println("=======================")
  }

  poke(c.io.stall, 1)
  poke(c.io.icache.dout, 0)
  poke(c.io.dcache.dout, 0)
  for (i <- 0 until c.regFile.regs.n) {
    if (i == 0)
      pokeAt(c.regFile.regs, 0, i)
    else
      pokeAt(c.regFile.regs, removeSign(rnd.nextInt() & 0xffffffff), i)
  }
  println("")
  step(1) 
  poke(c.io.stall, 0)

  for ((isa, i) <- isaTest.zipWithIndex) {
    println("*********************")
    println("  " + dasm(isa))
    println("*********************")

    poke(c.io.ctrl.inst_re, 1)
    poke(c.io.icache.dout,  0)
    poke(c.io.dcache.dout,  0)
    pokeExCtrl(decode(UInt(0)), false)
    pokeWbCtrl(decode(UInt(0)))
    val pc = peek(c.io.icache.addr)
    step(1)
    // Simulate fetch
    poke(c.io.ctrl.inst_re, 0)
    poke(c.io.icache.dout,  isa.litValue())
    poke(c.io.dcache.dout,  0)
    pokeExCtrl(decode(UInt(0)), false)
    pokeWbCtrl(decode(UInt(0)))
    step(1) 
    // Simulate decode & execute 
    val ctrl = decode(isa)
    val rs1_addr = rs1(isa)
    val rs2_addr = rs2(isa)
    val rd_addr  = rd(isa)
    val rs1_val = peekAt(c.regFile.regs, rs1_addr)
    val rs2_val = peekAt(c.regFile.regs, rs2_addr)
    val rd_val  = peekAt(c.regFile.regs, rd_addr)
    val imm_val = if (ctrl(3) == imm_i) iimm(isa)
      else if (ctrl(3) == imm_s) simm(isa)
      else if (ctrl(3) == imm_b) bimm(isa)
      else if (ctrl(3) == imm_u) uimm(isa)
      else if (ctrl(3) == imm_j) jimm(isa)
      else if (ctrl(3) == imm_z) zimm(isa)
      else BigInt(0)
    val a = if (ctrl(1) == a_rs1) rs1_val
      else if (ctrl(1) == a_pc) pc
      else BigInt(0)
    val b = if (ctrl(2) == b_rs2) rs2_val
      else if (ctrl(2) == b_imm) imm_val
      else BigInt(0)
    val alu_sum = 
      if ((ctrl(4) & 1) == 1) removeSign(a.toInt - b.toInt) 
      else removeSign(a.toInt + b.toInt)
    val alu_out = if (ctrl(4) == alu_copy_a) a
      else if (ctrl(4) == alu_copy_b) b
      else if (ctrl(4) == alu_add || ctrl(4) == alu_sub) alu_sum
      else if (ctrl(4) == alu_slt) if (a.toInt < b.toInt) BigInt(1) else BigInt(0)
      else if (ctrl(4) == alu_sltu) if (a < b) BigInt(1) else BigInt(0)
      else if (ctrl(4) == alu_sll) removeSign(a.toInt << (b.toInt & 0x1f))
      else if (ctrl(4) == alu_srl) removeSign(a.toInt >>> (b.toInt & 0x1f))
      else if (ctrl(4) == alu_sra) removeSign(a.toInt >> (b.toInt & 0x1f))
      else if (ctrl(4) == alu_xor) a ^ b
      else if (ctrl(4) == alu_or) a | b
      else if (ctrl(4) == alu_and) a & b
      else BigInt(0)
    val br_cond = if (ctrl(5) == br_eq) rs1_val == rs2_val
      else if (ctrl(5) == br_ne) rs1_val != rs2_val
      else if (ctrl(5) == br_lt) rs1_val.toInt < rs2_val.toInt
      else if (ctrl(5) == br_ge) rs1_val.toInt >= rs2_val.toInt
      else if (ctrl(5) == br_ltu) rs1_val < rs2_val
      else if (ctrl(5) == br_geu) rs1_val >= rs2_val
      else false
    val npc = if (br_cond) alu_out
      else if (ctrl(0) == pc_4) pc + 4
      else if (ctrl(0) == pc_alu) alu_out
      else pc
    val doffset = (8 * (alu_sum.toInt & 0x3)) & 0x1f
    val din = (rs2_val << doffset) & 0xffffffff
    val dwe = if (ctrl(7) == st_sw) BigInt(0xf)
      else if (ctrl(7) == st_sh) (BigInt(0x3) << (alu_out.toInt & 0x3)) & 0xf
      else if (ctrl(7) == st_sb) (BigInt(0x1) << (alu_out.toInt & 0x3)) & 0xf
      else BigInt(0)
    val dre = if (ctrl(8) != ld_xxx) y else n

    poke(c.io.ctrl.inst_re, y - dre)
    poke(c.io.icache.dout,  0)
    poke(c.io.dcache.dout,  0)
    pokeExCtrl(ctrl, br_cond)
    pokeWbCtrl(decode(UInt(0)))
    expect(c.alu.io.A,     a)
    expect(c.alu.io.B,     b)
    expect(c.alu.io.out,   alu_out)
    expect(c.pc,           pc)
    expect(c.io.icache.addr, npc)
    expect(c.io.dcache.addr, alu_sum)
    expect(c.io.dcache.din,  din)    
    expect(c.io.dcache.we,   dwe)
    step(1)

    // Simulate write back
    val r = rnd.nextInt() & 0xffffffff
    val lw = removeSign(r)
    val lhu = (r >>> doffset) & 0xffff
    val lbu = (r >>> doffset) & 0xff
    val load = if (ctrl(8) == ld_lw) lw
      else if (ctrl(8) == ld_lh) { 
        val s = if ((lhu >> 15) > 0) 0xffff << 16 else 0
        removeSign(s | lhu)
      }
      else if (ctrl(8) == ld_lb) {
        val s = if ((lbu >> 7) > 0) 0xffffff << 8 else 0
        removeSign(s | lbu)
      }
      else if (ctrl(8) == ld_lhu) lhu 
      else if (ctrl(8) == ld_lbu) lbu
      else BigInt(0)

    poke(c.io.ctrl.inst_re, 0)
    poke(c.io.icache.dout,  0)
    poke(c.io.dcache.dout,  lw)
    pokeWbCtrl(ctrl)
    step(1)

    // Check the results
    val wb_res = if (ctrl(10) == n) rd_val
      else if (ctrl(9) == wb_alu) alu_out
      else if (ctrl(9) == wb_mem) load
      else if (ctrl(9) == wb_pc_4) pc + 4
    val wb_rd_val = peekAt(c.regFile.regs, rd_addr)
    expect(wb_res == wb_rd_val, "Result Check: %d == %d".format(wb_res, wb_rd_val))
    println("")
  }
}
