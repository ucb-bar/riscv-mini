package mini

import Chisel._

class DatapathTests(c: Datapath) extends Tester(c) with RISCVCommon {
  def pokeExCtrl(ctrl: List[BigInt], br_cond: Boolean) {
    println("=== Execute Control Signals ===")
    poke(c.io.ctrl.pc_sel,    ctrl(0))
    poke(c.io.ctrl.inst_kill, ctrl(6))
    poke(c.io.ctrl.A_sel,     ctrl(1))
    poke(c.io.ctrl.B_sel,     ctrl(2))
    poke(c.io.ctrl.imm_sel,   ctrl(3))
    poke(c.io.ctrl.alu_op,    ctrl(4))
    poke(c.io.ctrl.br_type,   ctrl(5))
    poke(c.io.ctrl.st_type,   ctrl(7))
    poke(c.io.ctrl.data_en,   ctrl(8) != ld_xxx)
    println("=======================")
  }

  def pokeWbCtrl(ctrl: List[BigInt]) {
    println("=== Write-Back Control Signals ===")
    poke(c.io.ctrl.st_type_r, ctrl(7))
    poke(c.io.ctrl.ld_type,   ctrl(8))
    poke(c.io.ctrl.wb_sel,    ctrl(9))
    poke(c.io.ctrl.wb_en,     ctrl(10))
    poke(c.io.ctrl.csr_cmd,   ctrl(11))
    poke(c.io.ctrl.illegal,   ctrl(12))
    poke(c.io.ctrl.pc_check,  ctrl(0) == pc_alu)
    println("=======================")
  }

  poke(c.io.ctrl.stall, 0)
  poke(c.io.ctrl.flush, 0)
  poke(c.io.icache.resp.valid, 0)
  poke(c.io.dcache.resp.valid, 0)
  poke(c.io.icache.resp.bits.data, 0)
  poke(c.io.dcache.resp.bits.data, 0)
  for (i <- 0 until c.regFile.regs.n) {
    if (i == 0)
      pokeAt(c.regFile.regs, 0, i)
    else
      pokeAt(c.regFile.regs, int(rnd.nextInt() & 0xffffffff), i)
  }
  step(1)
  poke(c.io.icache.resp.valid, 1)
  poke(c.io.dcache.resp.valid, 1)

  /* Run ISA tests */
  var prv  = CSR.PRV_M.litValue()
  var prv1 = CSR.PRV_M.litValue()
  var epc  = BigInt(0)
  for ((inst, i) <- insts.zipWithIndex) {
    println("\n*********************")
    println("  %s (0x%s)".format(dasm(inst), inst.litValue().toString(16)))
    println("*********************")
    poke(c.io.icache.resp.bits.data, 0)
    poke(c.io.dcache.resp.bits.data, 0)
    pokeExCtrl(GoldControl(Instructions.NOP), false)
    pokeWbCtrl(GoldControl(Instructions.NOP))
    val pc = peek(c.io.icache.req.bits.addr) 
    step(1)

    // Emulate fetch
    poke(c.io.icache.resp.bits.data, inst)
    poke(c.io.dcache.resp.bits.data, 0)
    pokeExCtrl(GoldControl(Instructions.NOP), false)
    pokeWbCtrl(GoldControl(Instructions.NOP))
    step(1) 
    // Emulate GoldControl & execute 
    val ctrl = GoldControl(inst)
    val rs1_addr = rs1(inst)
    val rs2_addr = rs2(inst)
    val rd_addr  = rd(inst)
    val rs1_val = peekAt(c.regFile.regs, rs1_addr)
    val rs2_val = peekAt(c.regFile.regs, rs2_addr)
    val rd_val  = peekAt(c.regFile.regs, rd_addr)
    val imm_val = GoldImmGen(new ImmGenIn(inst, ctrl(3))).out
    val a = if (ctrl(1) == a_rs1) rs1_val else pc
    val b = if (ctrl(2) == b_rs2) rs2_val else imm_val
    val alu_gold = GoldALU(new ALUIn(ctrl(4), a, b))
    val alu_sum = alu_gold.sum
    val alu_out = alu_gold.out 
    val br_cond = GoldBrCond(new BrCondIn(ctrl(5), rs1_val, rs2_val)).taken 
    val cur_pc = pc + 4
    val npc = if (ctrl(0) == pc_epc) epc
      else if (ctrl(0) == pc_alu || br_cond) alu_out & int(-2)
      else if (ctrl(0) == pc_4) cur_pc + 4
      else cur_pc
    val doffset = (8 * (alu_sum.toInt & 0x3)) & 0x1f
    val din = (rs2_val << doffset) & 0xffffffff
    val dwe = if (ctrl(7) == st_sw) BigInt(0xf)
      else if (ctrl(7) == st_sh) (BigInt(0x3) << (alu_out.toInt & 0x3)) & 0xf
      else if (ctrl(7) == st_sb) (BigInt(0x1) << (alu_out.toInt & 0x3)) & 0xf
      else BigInt(0)
    val dre = if (ctrl(8) != ld_xxx) y else n
    poke(c.io.icache.resp.bits.data, 0)
    poke(c.io.dcache.resp.bits.data, 0)
    pokeExCtrl(ctrl, br_cond)
    pokeWbCtrl(GoldControl(Instructions.NOP))
    expect(c.alu.io.A,   a)
    expect(c.alu.io.B,   b)
    expect(c.pc,         cur_pc)
    expect(c.io.icache.req.bits.addr, npc)
    expect(c.io.dcache.req.bits.addr, alu_sum & int(-4))
    expect(c.io.dcache.req.bits.data, din)    
    expect(c.io.dcache.req.bits.mask, dwe)
    step(1)

    // Emulate write back
    pokeExCtrl(GoldControl(Instructions.NOP), false)
    pokeWbCtrl(ctrl)

    val csr_addr = csr(inst)
    val csr_file = (c.csr.csrFile map { case (k, v) => (k.litValue(), v) }).toMap
    val csr_out  = if (csr_file contains csr_addr) peek(csr_file(csr_addr)) else BigInt(0)
    val addr_invalid = ctrl(0) == pc_alu && (alu_sum & 0x2) || 
               ctrl(8) == ld_lw && (alu_sum & 0x3) || 
              (ctrl(8) == ld_lh || ctrl(8) == ld_lhu) && (alu_sum & 0x1) ||
               ctrl(7) == st_sw && (alu_sum & 0x3) ||
               ctrl(7) == st_sh && (alu_sum & 0x1)
    val eret = ctrl(11) == csr_p && csr_addr == Funct12.ERET.litValue()
    val expt = ctrl(11) == csr_p && csr_addr != Funct12.ERET.litValue() ||
              (ctrl(11) & 0x3) && (!csrVal(csr_addr) || !csrPrv(csr_addr, prv) || 
              (csrRO(csr_addr) && rs1_addr != 0)) || ctrl(12) || addr_invalid
    val evec = if (expt && prv) BigInt(pc_mtvec) else if (expt) BigInt(pc_utvec) else npc + 4 

    expect(c.io.icache.req.bits.addr, evec) 
    expect(c.csr.io.expt,             expt)
    expect(c.csr.io.out,              csr_out)

    val lw = rnd.nextInt() & 0xffffffff
    val lhu = (lw >>> doffset) & 0xffff
    val lbu = (lw >>> doffset) & 0xff
    val load = if (ctrl(8) == ld_lw) int(lw)
      else if (ctrl(8) == ld_lh) int(lhu | (if ((lhu >> 15) > 0) 0xffff << 16 else 0))
      else if (ctrl(8) == ld_lb) int(lbu | (if ((lbu >> 7) > 0) 0xffffff << 8 else 0))
      else if (ctrl(8) == ld_lhu) int(lhu)
      else if (ctrl(8) == ld_lbu) int(lbu)
      else BigInt(0)

    poke(c.io.icache.resp.bits.data, 0)
    poke(c.io.dcache.resp.bits.data, lw)
    step(1)

    // Check the results
    val wb_res = if (!ctrl(10) || expt) rd_val
      else if (ctrl(9) == wb_alu) alu_out
      else if (ctrl(9) == wb_mem) load
      else if (ctrl(9) == wb_pc4) pc + 4
      else if (ctrl(9) == wb_csr) csr_out
      else rd_val
    val wb_rd_val = peekAt(c.regFile.regs, rd_addr)
    expect(wb_res == wb_rd_val, "Result Check: %d == %d".format(wb_res, wb_rd_val))
    if (expt) {
      prv1 = prv
      prv  = CSR.PRV_M
      epc  = pc & int(-4)
    } else if (eret) {
      prv  = prv1
      prv1 = CSR.PRV_U
    }
  } 
}
