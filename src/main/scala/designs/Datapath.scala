package mini

import Chisel._

object Const {
  val PC_START = UInt(0x200)
  val PC_EVEC  = UInt(0x100)
}

class DatapathIO extends Bundle {
  val stall = Bool(INPUT)
  val host = new HostIO
  val icache = (new CacheIO).flip
  val dcache = (new CacheIO).flip
  val ctrl = (new ControlSignals).flip
}

class Datapath extends Module with CoreParams {
  val io      = new DatapathIO
  val alu     = Module(new ALU)
  val csr     = Module(new CSR)
  val regFile = Module(new RegFile) 
  val immGen  = params(BuildImmGen)()
  val brCond  = params(BuildBrCond)()

  import Control._

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(Instructions.NOP)
  val fe_pc   = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = Reg(UInt())
  val ew_pc   = Reg(UInt())
  val ew_alu  = Reg(UInt())
  val ew_csr  = Reg(UInt())
  val ew_expt = RegInit(Bool(false))
 
  /****** Fetch *****/
  val started = RegNext(reset)
  val pc    = RegInit(Const.PC_START - UInt(4, xlen)) 
  val iaddr = Mux(csr.io.expt || csr.io.eret, csr.io.evec,
              Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum & SInt(-2), 
              Mux(io.ctrl.pc_sel === PC_0, pc, pc + UInt(4))))
  val inst  = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, 
                  Instructions.NOP, io.icache.resp.bits.data)
  io.icache.req.bits.addr := iaddr 
  io.icache.req.valid     := io.ctrl.inst_re
  pc := Mux(io.ctrl.inst_re, iaddr, pc)
 
  // Pipelining
  when (!io.ctrl.stall) {
    fe_pc   := pc
    fe_inst := inst
  }

  /****** Execute *****/
  // Decode
  io.ctrl.inst  := fe_inst
  io.ctrl.stall := !io.icache.resp.valid || !io.dcache.resp.valid

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
  val ex_rd_addr = ew_inst(11, 7)
  val rs1hazard = io.ctrl.wb_en && rs1_addr.orR && (rs1_addr === ex_rd_addr)
  val rs2hazard = io.ctrl.wb_en && rs2_addr.orR && (rs2_addr === ex_rd_addr)
  val rs1 = Mux(io.ctrl.wb_sel === WB_ALU && rs1hazard, ew_alu, 
            Mux(io.ctrl.wb_sel === WB_CSR && rs1hazard, ew_csr, regFile.io.rdata1))
  val rs2 = Mux(io.ctrl.wb_sel === WB_ALU && rs2hazard, ew_alu, 
            Mux(io.ctrl.wb_sel === WB_CSR && rs2hazard, ew_csr, regFile.io.rdata2))
 
  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1 
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  val daddr   = Mux(io.stall, ew_alu, alu.io.sum) & SInt(-4)
  val woffset = alu.io.sum(1) << UInt(4) | alu.io.sum(0) << UInt(3)
  io.dcache.req.valid     := io.ctrl.data_re 
  io.dcache.req.bits.addr := daddr 
  io.dcache.req.bits.data := rs2 << woffset
  io.dcache.req.bits.mask := Mux(io.stall || csr.io.expt, UInt("b0000"), MuxLookup(io.ctrl.st_type, UInt("b0000"), 
     Seq(ST_SW -> UInt("b1111"),
         ST_SH -> (UInt("b11") << alu.io.sum(1,0)),
         ST_SB -> (UInt("b1")  << alu.io.sum(1,0)) )))
  
  // CSR access
  csr.io.in  := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
  csr.io.src := rs1_addr 
  csr.io.csr := fe_inst(31, 20) 
  csr.io.cmd := io.ctrl.csr_cmd
  csr.io.pc  := fe_pc
  csr.io.illegal_inst  := io.ctrl.xpt 
  csr.io.iaddr_invalid := Bool(false)
  csr.io.daddr_invalid := Bool(false) 
  csr.io.addr := Mux(csr.io.iaddr_invalid, iaddr, daddr)
  csr.io.host <> io.host

  // Pipelining
  when(!io.ctrl.stall) {
    ew_pc   := fe_pc
    ew_inst := fe_inst
    ew_alu  := alu.io.out
    ew_csr  := csr.io.out
    ew_expt := csr.io.expt
  }

  // Load
  val loffset = ew_alu(1) << UInt(4) | ew_alu(0) << UInt(3)
  val lshift  = io.dcache.resp.bits.data >> loffset
  val load    =  MuxLookup(io.ctrl.ld_type, io.dcache.resp.bits.data.zext, Seq(
    LD_LH  -> lshift(15, 0).toSInt,
    LD_LB  -> lshift(7, 0).toSInt,
    LD_LHU -> lshift(15, 0).zext,
    LD_LBU -> lshift(7, 0).zext) )
    
  // Regfile Write
  val regWrite = MuxLookup(io.ctrl.wb_sel, ew_alu.zext, Seq(
    WB_MEM -> load,
    WB_PC4 -> (ew_pc + UInt(4)).zext,
    WB_CSR -> ew_csr.zext) )

  regFile.io.wen   := io.ctrl.wb_en && !ew_expt
  regFile.io.waddr := ex_rd_addr
  regFile.io.wdata := regWrite
  csr.io.instret   := !io.stall && !ew_expt && ew_inst != Instructions.NOP
}
