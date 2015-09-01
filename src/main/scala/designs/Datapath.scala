package mini

import Chisel._

object Const {
  val PC_START = UInt(0x2000)
}

class DatapathIO extends Bundle {
  val stall = Bool(INPUT)
  val host = new HostIO
  val icache = new CacheIO
  val dcache = new CacheIO
  val ctrl = (new ControlSignals).flip
}

import Control._

class Datapath extends Module with CoreParams {
  val io = new DatapathIO
  val alu = Module(new ALU)
  val brCond  = Module(new BrCond)  
  val regFile = Module(new RegFile) 
  val immGen  = Module(new ImmGenWire) 

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(UInt(0, instLen))
  val fe_pc   = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = RegInit(UInt(0, instLen))
  val ew_pc  = Reg(UInt())
  val ew_alu = Reg(UInt())
 
  /****** Fetch *****/
  val pc = RegInit(Const.PC_START-UInt(4)) 
  val iaddr = Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum, pc + UInt(4))
  val inst  = Mux(io.ctrl.inst_type === I_KILL || brCond.io.taken, Instructions.NOP, io.icache.dout)
 
  io.icache.addr := iaddr 
  io.icache.re   := io.ctrl.inst_re
  pc             := Mux(io.icache.re && !io.dcache.we.orR, iaddr, pc)
 
  // Pipelining
  when (!io.stall) {
    fe_pc   := pc
    fe_inst := inst
  }

  /****** Execute *****/
  // Decode
  io.ctrl.inst  := fe_inst
  io.ctrl.stall := io.stall

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
  val rs1NotZero = rs1_addr.orR
  val rs2NotZero = rs2_addr.orR 
  val alutype = io.ctrl.wb_en && io.ctrl.wb_sel === WB_ALU
  val ex_rd_addr = ew_inst(11, 7)
  val rs1 = Mux(alutype && rs1NotZero && (rs1_addr === ex_rd_addr), ew_alu, regFile.io.rdata1)
  val rs2 = Mux(alutype && rs2NotZero && (rs2_addr === ex_rd_addr), ew_alu, regFile.io.rdata2) 
  
  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1 
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  val woffset = alu.io.sum(1) << UInt(4) | alu.io.sum(0) << UInt(3)
  io.dcache.re   := io.ctrl.data_re 
  io.dcache.addr := Mux(io.stall, ew_alu, alu.io.sum)
  io.dcache.we   := Mux(io.stall, UInt("b0000"), MuxLookup(io.ctrl.st_type, UInt("b0000"), Seq(
    ST_SW -> UInt("b1111"),
    ST_SH -> (UInt("b11") << alu.io.sum(1,0)),
    ST_SB -> (UInt("b1")  << alu.io.sum(1,0)) )))
  io.dcache.din  := rs2 << woffset 
  
  // Pipelining
  when(!io.stall) {
    ew_pc   := fe_pc
    ew_inst := fe_inst
    ew_alu  := alu.io.out
  }

  // Load
  val loffset = ew_alu(1) << UInt(4) | ew_alu(0) << UInt(3)
  val lshift = io.dcache.dout >> loffset
  val load =  MuxLookup(io.ctrl.ld_type, io.dcache.dout.zext, Seq(
    LD_LH  -> lshift(15, 0).toSInt,
    LD_LB  -> lshift(7, 0).toSInt,
    LD_LHU -> lshift(15, 0).zext,
    LD_LBU -> lshift(7, 0).zext) )
    
  val csr = Module(new CSR)
  csr.io.host <> io.host
  csr.io.src := ew_alu 
  csr.io.csr := ew_inst(31, 20) 
  csr.io.cmd := io.ctrl.csr_cmd

  // Regfile Write
  val regWrite = MuxLookup(io.ctrl.wb_sel, ew_alu.zext, Seq(
    WB_MEM  -> load,
    WB_PC_4 -> (ew_pc + UInt(4)).zext,
    WB_CSR  -> csr.io.out.zext) )

  regFile.io.wen   := io.ctrl.wb_en
  regFile.io.waddr := ex_rd_addr
  regFile.io.wdata := regWrite
}
