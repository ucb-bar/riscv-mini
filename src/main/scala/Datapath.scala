package mini

import Chisel._
import cde.Parameters

object Const {
  val PC_START = UInt(0x200)
  val PC_EVEC  = UInt(0x100)
}

class DatapathIO(implicit p: Parameters) extends CoreBundle()(p) {
  val host = new HostIO
  val icache = (new CacheIO).flip
  val dcache = (new CacheIO).flip
  val ctrl = (new ControlSignals).flip
}

class Datapath(implicit val p: Parameters) extends Module with CoreParams {
  val io      = new DatapathIO
  val csr     = Module(new CSR)
  val regFile = Module(new RegFile) 
  val alu     = p(BuildALU)(p)
  val immGen  = p(BuildImmGen)(p)
  val brCond  = p(BuildBrCond)(p)

  import Control._

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(Instructions.NOP)
  val fe_pc   = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = RegInit(Instructions.NOP) 
  val ew_pc   = Reg(UInt())
  val ew_alu  = Reg(UInt())
  val csr_in  = Reg(UInt())

  /****** Control signals *****/
  val st_type  = Reg(io.ctrl.st_type)
  val ld_type  = Reg(io.ctrl.ld_type)
  val wb_sel   = Reg(io.ctrl.wb_sel)
  val wb_en    = Reg(Bool())
  val csr_cmd  = Reg(io.ctrl.csr_cmd)
  val illegal  = Reg(Bool())
  val pc_check = Reg(Bool())
 
  /****** Fetch *****/
  val started = RegNext(reset)
  val stall = !io.icache.resp.valid || !io.dcache.resp.valid
  val pc   = RegInit(Const.PC_START - UInt(4, xlen)) 
  val npc  = Mux(stall, pc, Mux(csr.io.expt, csr.io.evec,
             Mux(io.ctrl.pc_sel === PC_EPC,  csr.io.epc,
             Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum & SInt(-2), 
             Mux(io.ctrl.pc_sel === PC_0, pc, pc + UInt(4))))))
  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.resp.bits.data)
  pc                      := npc 
  io.icache.req.bits.addr := npc
  io.icache.req.bits.data := UInt(0)
  io.icache.req.bits.mask := UInt(0)
  io.icache.req.valid     := !stall
  io.icache.abort         := Bool(false)
 
  // Pipelining
  when (!stall) {
    fe_pc   := pc
    fe_inst := inst
  }

  /****** Execute *****/
  // Decode
  io.ctrl.inst  := fe_inst

  // regFile read
  val rd_addr  = fe_inst(11, 7)
  val rs1_addr = fe_inst(19, 15)
  val rs2_addr = fe_inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  immGen.io.inst := fe_inst
  immGen.io.sel  := io.ctrl.imm_sel

  // bypass
  val wb_rd_addr = ew_inst(11, 7)
  val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  val rs1 = Mux(wb_sel === WB_ALU && rs1hazard, ew_alu, regFile.io.rdata1) 
  val rs2 = Mux(wb_sel === WB_ALU && rs2hazard, ew_alu, regFile.io.rdata2)
 
  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1 
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  val daddr   = Mux(stall, ew_alu, alu.io.sum) & SInt(-4)
  val woffset = alu.io.sum(1) << UInt(4) | alu.io.sum(0) << UInt(3)
  io.dcache.req.valid     := !stall && (io.ctrl.st_type.orR || io.ctrl.ld_type.orR)
  io.dcache.req.bits.addr := daddr 
  io.dcache.req.bits.data := rs2 << woffset
  io.dcache.req.bits.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), 
              UInt("b0000"), Seq(
    ST_SW ->  UInt("b1111"),
    ST_SH -> (UInt("b11") << alu.io.sum(1,0)),
    ST_SB -> (UInt("b1")  << alu.io.sum(1,0)) ))
  
  // Pipelining
  when(!stall && !csr.io.expt) {
    ew_pc     := fe_pc
    ew_inst   := fe_inst
    ew_alu    := alu.io.out
    csr_in    := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
    st_type   := io.ctrl.st_type
    ld_type   := io.ctrl.ld_type
    wb_sel    := io.ctrl.wb_sel
    wb_en     := io.ctrl.wb_en
    csr_cmd   := io.ctrl.csr_cmd
    illegal   := io.ctrl.illegal
    pc_check  := io.ctrl.pc_sel === PC_ALU
  }.elsewhen(reset || !stall && csr.io.expt) {
    st_type   := UInt(0)
    ld_type   := UInt(0)
    wb_en     := Bool(false)
    csr_cmd   := UInt(0)
    illegal   := Bool(false) 
    pc_check  := Bool(false) 
  }

  // Load
  val loffset = ew_alu(1) << UInt(4) | ew_alu(0) << UInt(3)
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    = MuxLookup(ld_type, io.dcache.resp.bits.data.zext, Seq(
    LD_LH  -> lshift(15, 0).toSInt, LD_LB  -> lshift(7, 0).toSInt,
    LD_LHU -> lshift(15, 0).zext,   LD_LBU -> lshift(7, 0).zext) )
    
  // CSR access
  csr.io.stall    := stall
  csr.io.in       := csr_in
  csr.io.cmd      := csr_cmd
  csr.io.inst     := ew_inst
  csr.io.pc       := ew_pc
  csr.io.addr     := ew_alu
  csr.io.illegal  := illegal
  csr.io.pc_check := pc_check
  csr.io.ld_type  := ld_type
  csr.io.st_type  := st_type
  csr.io.host     <> io.host

  // Regfile Write
  val regWrite = MuxLookup(wb_sel, ew_alu.zext, Seq(
    WB_MEM -> load,
    WB_PC4 -> (ew_pc + UInt(4)).zext,
    WB_CSR -> csr.io.out.zext) ) 

  regFile.io.wen   := wb_en && !stall && !csr.io.expt 
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite

  // Abort store when there's an excpetion
  io.dcache.abort := csr.io.expt

  printf("PC: %x, INST: %x, REG[%d] <- %x\n", ew_pc, ew_inst, 
    Mux(regFile.io.wen, wb_rd_addr, UInt(0)),
    Mux(regFile.io.wen, regFile.io.wdata, UInt(0)))
}
