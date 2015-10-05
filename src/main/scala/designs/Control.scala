package mini

import Chisel._

object Control {
  val Y = Bool(true)
  val N = Bool(false)

  // pc_sel
  val PC_4   = UInt(0, 2)
  val PC_ALU = UInt(1, 2)
  val PC_0   = UInt(2, 2)
  val PC_EPC = UInt(3, 2)

  // A_sel
  val A_XXX  = UInt(0, 1)
  val A_PC   = UInt(0, 1)
  val A_RS1  = UInt(1, 1)

  // B_sel
  val B_XXX  = UInt(0, 1)
  val B_IMM  = UInt(0, 1)
  val B_RS2  = UInt(1, 1)

  // imm_sel
  val IMM_X  = UInt(0, 3)
  val IMM_I  = UInt(1, 3)
  val IMM_S  = UInt(2, 3)
  val IMM_U  = UInt(3, 3)
  val IMM_J  = UInt(4, 3)
  val IMM_B  = UInt(5, 3)
  val IMM_Z  = UInt(6, 3)

  // br_type
  val BR_XXX = UInt(0, 3)
  val BR_LTU = UInt(1, 3)
  val BR_LT  = UInt(2, 3)
  val BR_EQ  = UInt(3, 3)
  val BR_GEU = UInt(4, 3)
  val BR_GE  = UInt(5, 3)
  val BR_NE  = UInt(6, 3)

  // st_type
  val ST_XXX = UInt(0, 2)
  val ST_SW  = UInt(1, 2)
  val ST_SH  = UInt(2, 2)
  val ST_SB  = UInt(3, 2)

  // ld_type
  val LD_XXX = UInt(0, 3)
  val LD_LW  = UInt(1, 3)
  val LD_LH  = UInt(2, 3)
  val LD_LB  = UInt(3, 3)
  val LD_LHU = UInt(4, 3)
  val LD_LBU = UInt(5, 3)

  // wb_sel
  val WB_ALU = UInt(0, 2)
  val WB_MEM = UInt(1, 2)
  val WB_PC4 = UInt(2, 2)
  val WB_CSR = UInt(3, 2)

  import Instructions._
  import ALU._

  val default =
    //                                                            kill                        wb_en  illegal?
    //            pc_sel  A_sel   B_sel  imm_sel   alu_op   br_type |  st_type ld_type wb_sel  | csr_cmd |
    //              |       |       |     |          |          |   |     |       |       |    |  |      |
             List(PC_4,   A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, Y)
  val map = Array(
    LUI   -> List(PC_4  , A_PC,   B_IMM, IMM_U, ALU_COPY_B, BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    AUIPC -> List(PC_4  , A_PC,   B_IMM, IMM_U, ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    JAL   -> List(PC_ALU, A_PC,   B_IMM, IMM_J, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_XXX, WB_PC4, Y, CSR.N, N),
    JALR  -> List(PC_ALU, A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_XXX, WB_PC4, Y, CSR.N, N),
    BEQ   -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_EQ , N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    BNE   -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_NE , N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    BLT   -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_LT , N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    BGE   -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_GE , N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    BLTU  -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_LTU, N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    BGEU  -> List(PC_4  , A_PC,   B_IMM, IMM_B, ALU_ADD   , BR_GEU, N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    LB    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LB , WB_MEM, Y, CSR.N, N),
    LH    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LH , WB_MEM, Y, CSR.N, N),
    LW    -> List(PC_0  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LW , WB_MEM, Y, CSR.N, N),
    LBU   -> List(PC_0  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LBU, WB_MEM, Y, CSR.N, N),
    LHU   -> List(PC_0  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, Y, ST_XXX, LD_LHU, WB_MEM, Y, CSR.N, N),
    SB    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, ALU_ADD   , BR_XXX, N, ST_SB , LD_XXX, WB_ALU, N, CSR.N, N),
    SH    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, ALU_ADD   , BR_XXX, N, ST_SH , LD_XXX, WB_ALU, N, CSR.N, N),
    SW    -> List(PC_4  , A_RS1,  B_IMM, IMM_S, ALU_ADD   , BR_XXX, N, ST_SW , LD_XXX, WB_ALU, N, CSR.N, N),
    ADDI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLTI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_SLT   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLTIU -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_SLTU  , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    XORI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_XOR   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    ORI   -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_OR    , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    ANDI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_AND   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLLI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_SLL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SRLI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_SRL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SRAI  -> List(PC_4  , A_RS1,  B_IMM, IMM_I, ALU_SRA   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    ADD   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_ADD   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SUB   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SUB   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLL   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SLL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLT   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SLT   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SLTU  -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SLTU  , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    XOR   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_XOR   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SRL   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SRL   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    SRA   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_SRA   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    OR    -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_OR    , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    AND   -> List(PC_4  , A_RS1,  B_RS2, IMM_X, ALU_AND   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, Y, CSR.N, N),
    FENCE -> List(PC_4  , A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    FENCEI-> List(PC_0  , A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N),
    CSRRW -> List(PC_0  , A_RS1,  B_XXX, IMM_X, ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.W, N),
    CSRRS -> List(PC_0  , A_RS1,  B_XXX, IMM_X, ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.S, N),
    CSRRC -> List(PC_0  , A_RS1,  B_XXX, IMM_X, ALU_COPY_A, BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.C, N),
    CSRRWI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.W, N),
    CSRRSI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.S, N),
    CSRRCI-> List(PC_0  , A_XXX,  B_XXX, IMM_Z, ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, Y, CSR.C, N),
    ECALL -> List(PC_4  , A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_CSR, N, CSR.P, N),
    EBREAK-> List(PC_4  , A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_CSR, N, CSR.P, N),
    ERET  -> List(PC_EPC, A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, Y, ST_XXX, LD_XXX, WB_CSR, N, CSR.P, N),
    WFI   -> List(PC_4  , A_XXX,  B_XXX, IMM_X, ALU_XXX   , BR_XXX, N, ST_XXX, LD_XXX, WB_ALU, N, CSR.N, N))
}

class ControlSignals extends CoreBundle {
  val pc_sel    = UInt(OUTPUT, 2) 
  val inst_en   = Bool(OUTPUT)
  val inst_kill = Bool(OUTPUT)
  val A_sel     = UInt(OUTPUT, 1)
  val B_sel     = UInt(OUTPUT, 1)
  val imm_sel   = UInt(OUTPUT, 3)
  val alu_op    = UInt(OUTPUT, 4)
  val br_type   = UInt(OUTPUT, 3)
  val data_en   = Bool(OUTPUT)
  val st_type   = UInt(OUTPUT, 2)
  val st_type_r = UInt(OUTPUT, 2)
  val ld_type   = UInt(OUTPUT, 3)
  val wb_sel    = UInt(OUTPUT, 2) 
  val wb_en     = Bool(OUTPUT)
  val csr_cmd   = UInt(OUTPUT, 3)
  val illegal   = Bool(OUTPUT)
  val pc_check  = Bool(OUTPUT)
 
  val inst      = UInt(INPUT, xlen)
  val stall     = Bool(INPUT)
  val flush     = Bool(INPUT)
}

class ControlIO extends Bundle {
  val ctrl = new ControlSignals
}

class Control extends Module {
  val io = new ControlIO
  val ctrlSignals = ListLookup(io.ctrl.inst, Control.default, Control.map)
  val st_type  = Reg(io.ctrl.st_type)
  val ld_type  = Reg(ctrlSignals(8))
  val wb_sel   = Reg(ctrlSignals(9))
  val wb_en    = Reg(Bool())
  val csr_cmd  = Reg(ctrlSignals(11))
  val illegal  = Reg(Bool())
  val pc_check = Reg(Bool())

  // Control signals for Fetch
  io.ctrl.pc_sel    := ctrlSignals(0)
  io.ctrl.inst_kill := ctrlSignals(6).toBool 

  // Control signals for Execute
  io.ctrl.A_sel   := ctrlSignals(1)
  io.ctrl.B_sel   := ctrlSignals(2)
  io.ctrl.imm_sel := ctrlSignals(3)
  io.ctrl.alu_op  := ctrlSignals(4)
  io.ctrl.br_type := ctrlSignals(5)

  when(!io.ctrl.stall && !io.ctrl.flush) {
    st_type  := io.ctrl.st_type
    ld_type  := ctrlSignals(8)
    wb_sel   := ctrlSignals(9)
    wb_en    := ctrlSignals(10).toBool 
    csr_cmd  := ctrlSignals(11)
    illegal  := ctrlSignals(12).toBool 
    pc_check := io.ctrl.pc_sel === Control.PC_ALU
  }.elsewhen(reset || !io.ctrl.stall && io.ctrl.flush) {
    st_type  := UInt(0)
    ld_type  := UInt(0)
    wb_sel   := UInt(0)
    wb_en    := Bool(false)
    csr_cmd  := UInt(0)
    illegal  := Bool(false)
    pc_check := Bool(false)
  }

  // D$ signals
  io.ctrl.st_type := Mux(io.ctrl.stall, st_type, ctrlSignals(7))
  io.ctrl.data_en := !io.ctrl.stall && (ctrlSignals(7).orR || ctrlSignals(8).orR)
                                 
  // Control signals for Write Back
  io.ctrl.ld_type := ld_type
  io.ctrl.wb_en   := wb_en 
  io.ctrl.wb_sel  := wb_sel

  // Control signals for CSR
  io.ctrl.st_type_r := st_type
  io.ctrl.csr_cmd   := csr_cmd
  io.ctrl.illegal   := illegal
  io.ctrl.pc_check  := pc_check
}
