package mini

import Chisel._

case class DatapathIn(iresp: TestCacheResp, dresp: TestCacheResp)
case class DatapathOut(ireq: Option[TestCacheReq], dreq: Option[TestCacheReq], regs: List[BigInt], nop: Boolean)

class GoldDatapath extends RISCVCommon {
  // state
  private var pc   = Const.PC_START.litValue() - 4
  private val regs = Array.fill(32){BigInt(0)}

  // pipeline registers
  private var start     = true
  private var fe_inst   = nop
  private var fe_pc     = BigInt(0)

  private var ew_inst   = nop
  private var ew_pc     = BigInt(0)
  private var ew_alu    = BigInt(0)
  private var csr_in    = BigInt(0)

  private var st_type   = BigInt(0)
  private var ld_type   = BigInt(0)
  private var wb_sel    = BigInt(0)
  private var wb_en     = false
  private var csr_cmd   = BigInt(0)
  private var illegal   = false
  private var pc_check  = false

  private val goldCSR = new GoldCSR

  import Control._

  implicit def toBoolean(x: BigInt) = x != 0

  def apply(in: DatapathIn) = {
    // write back
    val loffset = ((ew_alu & 0x3) * 8).toInt
    val lbu = (in.dresp.data >> loffset) & 0xff
    val lhu = (in.dresp.data >> loffset) & 0xffff
    val lb = lbu | (if ((lbu >> 7)  > 0) BigInt(0xffffff) << 8  else BigInt(0))
    val lh = lhu | (if ((lhu >> 15) > 0) BigInt(0xffff)   << 16 else BigInt(0))
    val load = if (ld_type == LD_LH.litValue()) lh
          else if (ld_type == LD_LB.litValue()) lb
          else if (ld_type == LD_LHU.litValue()) lhu
          else if (ld_type == LD_LBU.litValue()) lbu
          else in.dresp.data
    val csr = goldCSR(new CSRIn(csr_cmd, csr_in, ew_inst, ew_pc, ew_alu, illegal, pc_check, st_type, ld_type))
    val reg_write = if (wb_sel == WB_MEM.litValue()) load
               else if (wb_sel == WB_PC4.litValue()) ew_pc + 4
               else if (wb_sel == WB_CSR.litValue()) csr.value
               else ew_alu

    // execute
    val ctrl = GoldControl(new ControlIn(fe_inst))
    val imm  = GoldImmGen(new ImmGenIn(fe_inst, ctrl.imm_sel))
    val rd_addr  = rd(fe_inst)
    val rs1_addr = rs1(fe_inst)
    val rs2_addr = rs2(fe_inst)
    val wb_rd_addr = rd(ew_inst)
    val rs1hazard = ctrl.wb_en && rs1_addr != 0 && rs1_addr == wb_rd_addr
    val rs2hazard = ctrl.wb_en && rs2_addr != 0 && rs2_addr == wb_rd_addr
    val rs1_val = if (wb_sel == WB_ALU.litValue() && rs1hazard) ew_alu else regs(rs1_addr)
    val rs2_val = if (wb_sel == WB_ALU.litValue() && rs2hazard) ew_alu else regs(rs2_addr)
    val alu = GoldALU(new ALUIn(ctrl.alu_op, 
        if (ctrl.a_sel == A_RS1.litValue()) rs1_val else fe_pc,
        if (ctrl.b_sel == B_RS2.litValue()) rs2_val else imm.out))
    val brcond = GoldBrCond(new BrCondIn(ctrl.br_type, rs1_val, rs2_val))
    val daddr  = alu.out & -4
    val ddata  = rs2_val << (8 * (alu.out & 0x3)).toInt
    val dmask  = if (ctrl.st_type == ST_SW.litValue()) BigInt(0xf)
             else if (ctrl.st_type == ST_SH.litValue()) BigInt(0x3) << (alu.out & 0x3).toInt
             else if (ctrl.st_type == ST_SB.litValue()) BigInt(0x1) << (alu.out & 0x3).toInt
             else BigInt(0)
    val dreq   = if (ctrl.ld_type == 0 && ctrl.st_type == 0) None 
             else Some(new TestCacheReq(daddr.toInt, ddata, dmask))
    
    // fetch
    val is_nop = start || ctrl.inst_kill || brcond.taken || csr.expt
    val npc = if (csr.expt) csr.evec
         else if (ctrl.pc_sel == PC_EPC.litValue()) csr.epc
         else if (ctrl.pc_sel == PC_ALU.litValue() || brcond.taken) alu.out & -2
         else if (ctrl.pc_sel == PC_0.litValue()) pc
         else pc + 4
    val inst = if (is_nop) nop else UInt(in.iresp.data)
    val ireq = Some(new TestCacheReq(npc.toInt, 0, 0))
    val out  = new DatapathOut(ireq, dreq, regs.toList, is_nop)

    // state update
    if (!csr.expt) {
      if (wb_en) regs(wb_rd_addr) = reg_write
      ew_pc    = fe_pc
      ew_inst  = fe_inst
      ew_alu   = alu.out
      csr_in   = if (ctrl.imm_sel == IMM_Z.litValue()) imm.out else rs1_val
      wb_sel   = ctrl.wb_sel
    }
    st_type  = if (!csr.expt) ctrl.st_type else BigInt(0)
    ld_type  = if (!csr.expt) ctrl.ld_type else BigInt(0)
    wb_en    = !csr.expt && ctrl.wb_en 
    csr_cmd  = if (!csr.expt) ctrl.csr_cmd else BigInt(0)
    illegal  = !csr.expt && ctrl.illegal 
    pc_check = !csr.expt && ctrl.pc_sel == PC_ALU.litValue() 
    fe_pc    = pc
    fe_inst  = inst
    pc       = npc
    start    = false
    out
  }
}

class DatapathTests(c: Datapath, log: Option[java.io.PrintStream] = None) 
    extends LogTester(c, log) with RandInsts {
  def poke(ctrl: ControlOut) {
    poke(c.io.ctrl.pc_sel,    ctrl.pc_sel)
    poke(c.io.ctrl.inst_kill, ctrl.inst_kill) 
    poke(c.io.ctrl.A_sel,     ctrl.a_sel)
    poke(c.io.ctrl.B_sel,     ctrl.b_sel)
    poke(c.io.ctrl.imm_sel,   ctrl.imm_sel)
    poke(c.io.ctrl.alu_op,    ctrl.alu_op)
    poke(c.io.ctrl.br_type,   ctrl.br_type)
    poke(c.io.ctrl.alu_op,    ctrl.alu_op)
    poke(c.io.ctrl.st_type,   ctrl.st_type)
    poke(c.io.ctrl.ld_type,   ctrl.ld_type)
    poke(c.io.ctrl.wb_sel,    ctrl.wb_sel)
    poke(c.io.ctrl.wb_en,     ctrl.wb_en)
    poke(c.io.ctrl.csr_cmd,   ctrl.csr_cmd)
    poke(c.io.ctrl.illegal,   ctrl.illegal)
  }

  def test(out: DatapathOut) {
    out.ireq match {
      case None =>
        expect(c.io.icache.req.valid, 0)
      case Some(req) =>
        expect(c.io.icache.req.valid, 1)
        expect(c.io.icache.req.bits.addr, req.addr)
        expect(c.io.icache.req.bits.data, req.data)
        expect(c.io.icache.req.bits.mask, req.mask)
    }
    out.dreq match {
      case None =>
        expect(c.io.dcache.req.valid, 0)
      case Some(req) =>
        expect(c.io.dcache.req.valid, 1)
        expect(c.io.dcache.req.bits.addr, req.addr)
        expect(c.io.dcache.req.bits.data, req.data)
        expect(c.io.dcache.req.bits.mask, req.mask)
    }
    for ((reg, i) <- out.regs.zipWithIndex) {
      val reg_i = peekAt(c.regFile.regs, i)
      expect(reg_i == reg, "regs[%d] => %x == %x".format(i, reg_i, reg))
    }
  }

  (0 until 32) foreach (pokeAt(c.regFile.regs, 0, _))
  poke(c.io.host.fromhost.bits,  rand_data)
  poke(c.io.host.fromhost.valid, 1)

  var i = 0
  val goldDatapath = new GoldDatapath
  while (i < insts.size) {
    val inst = insts(i)
    val data = rand_data
    val out = goldDatapath(new DatapathIn(new TestCacheResp(inst), new TestCacheResp(data)))
    addEvent(new DumpEvent(s"*** ${dasm(inst)} (%x) ***".format(inst.litValue())))
    poke(c.io.icache.resp.bits.data, inst)
    poke(c.io.icache.resp.valid,     1)
    poke(c.io.dcache.resp.bits.data, data)
    poke(c.io.dcache.resp.valid,     1)
    test(out)
    step(1)
    poke(GoldControl(new ControlIn(if (out.nop) nop else inst)))
    if (!out.nop) i += 1
  }
}
