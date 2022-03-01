// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._

object CSR {
  val N = 0.U(3.W) // Not a CSR instruction
  val W = 1.U(3.W) // rd <= CSR, CSR <= rs1 (write)
  val S = 2.U(3.W) // rd <= CSR, CSR[i] <= 1 if rs1[i] (set)
  val C = 3.U(3.W) // rd <= CSR, CSR[i] <= 0 if !rs1[i] (clear)
  val P = 4.U(3.W) // privileged instruction

  // Supports machine & user modes
  val PRV_U = 0x0.U(2.W)
  val PRV_M = 0x3.U(2.W)

  // User-level CSR (unprivileged counters) addrs
  val cycle = 0xC00.U(12.W)
  val time = 0xC01.U(12.W)
  val instret = 0xC02.U(12.W)
  val cycleh = 0xC80.U(12.W)
  val timeh = 0xC81.U(12.W)
  val instreth = 0xC82.U(12.W)

  //// Machine-level CSR addrs
  // Machine Information Registers
  val mvendorid = 0xF11.U(12.W) // vendor ID (formerly part of mcpuid)
  val marchid = 0xF12.U(12.W) // arch ID (formerly part of mcpuid)
  val mimpid = 0xF13.U(12.W) // implementation ID
  val mhartid = 0xF14.U(12.W) // hardware thread ID

  // Machine Trap Setup
  val mstatus = 0x300.U(12.W) // machine status
  val misa = 0x301.U(12.W) // isa and extensions
  // medeleg and mideleg shouldn't exist if S-mode isn't supported
  // val medeleg = 0x302.U(12.W) // machine exception delegation
  // val mideleg = 0x303.U(12.W) // machine interrupt delegation
  val mie = 0x304.U(12.W) // machine interrupt-enable
  val mtvec = 0x305.U(12.W) // trap handler base address
  val mstatush = 0x310.U(12.W) // upper word of mstatus (rv32)

  // Machine Trap Handling
  val mscratch = 0x340.U(12.W) // scratch register for trap handlers
  val mepc = 0x341.U(12.W) // exception PC
  val mcause = 0x342.U(12.W) // trap cause
  val mtval = 0x343.U(12.W) // bad address / instruction
  val mip = 0x344.U(12.W) // interrupt pending

  // Machine Memory Protection
  val pmpcfg0 = 0x3A0.U(12.W) // physical memory protection conf (TODO: there are 16 such registers)
  val pmpaddr0 = 0x3B0.U(12.W) // physical memory protection addr (TODO: there are 64 such registers)

  // Machine Timers and Counters
  val mcycle = 0xB00.U(12.W)
  val minstret = 0xB02.U(12.W)
  val mcycleh = 0xB80.U(12.W)
  val minstreth = 0xB82.U(12.W)

  val regs = List(
    cycle,
    time,
    instret,
    cycleh,
    timeh,
    instreth,
    mvendorid,
    marchid,
    mimpid,
    mhartid,
    mstatus,
    misa,
    mie,
    mtvec,
    mscratch,
    mepc,
    mcause,
    mtval,
    mip,
    pmpcfg0,
    pmpaddr0,
    mcycle,
    minstret,
    mcycleh,
    minstreth
  )
}

object Cause {
  val InstAddrMisaligned = 0x0.U
  val IllegalInst = 0x2.U
  val Breakpoint = 0x3.U
  val LoadAddrMisaligned = 0x4.U
  val StoreAddrMisaligned = 0x6.U
  val Ecall = 0x8.U
}

class CSRIO(xlen: Int) extends Bundle {
  val stall = Input(Bool())
  val cmd = Input(UInt(3.W))
  val in = Input(UInt(xlen.W))
  val out = Output(UInt(xlen.W))
  // Exception
  val pc = Input(UInt(xlen.W))
  val addr = Input(UInt(xlen.W))
  val inst = Input(UInt(xlen.W))
  val illegal = Input(Bool())
  val st_type = Input(UInt(2.W))
  val ld_type = Input(UInt(3.W))
  val pc_check = Input(Bool())
  val expt = Output(Bool())
  val evec = Output(UInt(xlen.W))
  val epc = Output(UInt(xlen.W))
}

class CSR(val xlen: Int) extends Module {
  val io = IO(new CSRIO(xlen))

  val csr_addr = io.inst(31, 20)
  val rs1_addr = io.inst(19, 15)

  // user counters, TODO: same counters used for mcycle, minstret (they should be distinct)
  val cycle = RegInit(0.U(64.W))
  val time = RegInit(0.U(64.W))
  val instret = RegInit(0.U(64.W))
  // TODO: mtime, mtimeh, mtimecmp, mtimecmph are all memory mapped now (not in the CSR file)

  // CPU info
  object MXL extends ChiselEnum {
    val rv32 = Value(1.U(2.W))
    val rv64 = Value(2.U(2.W))
    val rv128 = Value(3.U(2.W))
  }
  def xlen2mxl(xlen: Int): UInt = {
    xlen match {
      case 32 => MXL.rv32.asUInt
      case 64 => MXL.rv64.asUInt
      case 128 => MXL.rv128.asUInt
    }
  }
  val mxlen = xlen // machine CSR width
  val mxl = xlen2mxl(xlen) // machine XLEN (usually the same as xlen)
  val extensions =
    (1 << 8) | // rv32i/64i base isa
    (1 << 20) // user mode implemented
  val misa = Cat(mxl, 0.U((mxlen-28).W), extensions.U(26.W))
  val mvendorid = 0.U(mxlen.W) // non-commercial implementation
  val marchid = 0.U(mxlen.W) // microarch of the hart: unimplemented
  val mimpid = 0.U(mxlen.W) // processor impl encoding: unimplemented
  val mhartid = 0.U(mxlen.W) // only one hart

  // Trap Setup
  val current_prv_level = RegInit(CSR.PRV_M)
  // mstatus
  // trap handling info
  val SIE = 0.B // supervisor interrupt enable
  val MIE = RegInit(0.B) // machine interrupt enable
  val SPIE = 0.B // supervisor IE prior to trap
  val MPIE = RegInit(0.B) // machine IE prior to trap
  val SPP = 0.B // supervisor privilege mode prior to trap
  val MPP = RegInit(CSR.PRV_U) // machine privilege mode prior to trap  // TODO: should the reset value of MPP be user mode?
  // ISA extensions and extension state tracking
  val VS = 0.U(2.W) // state of vector extension
  val FS = 0.U(2.W) // state of F extension (FP support)
  val XS = 0.U(2.W) // state of custom ISA extension
  val SD = 0.B // any state dirty? (0 = no support for VS, FS, XS)
  // endianness
  val UBE = 0.B // user mode endianness (LE)
  val SBE = 0.B // supervisor mode endianness (LE)
  val MBE = 0.B // machine mode endianness (LE)
  // supervisor mode stuff
  val MPRV = 0.B // memory privilege (0 = Ld/St use translation/privilege of current mode)
  val SUM = 0.B // permit supervisor user memory access (unused if we're never in supervisor mode)
  val MXR = 0.B // make executable readable (0 = loads from executable pages will trap, S-mode not supported)
  val TVM = 0.B // trap virtual memory (0 = S-mode not supported)
  val TW = 0.B // timeout wait (0 = WFI may execute in lower priv mode indefinetely)
  val TSR = 0.B // trap SRET (0 = S-mode not suported)
  val mstatus = if (xlen == 64) { // rv64 version
    val UXL = 0.U(2.W)
    val SXL = 0.U(2.W)
    Cat(SD, 0.U(25.W), MBE, SBE, SXL, UXL, 0.U(9.W), TSR, TW, TVM, MXR, SUM, MPRV, XS, FS, MPP, VS, SPP, MPIE, UBE, SPIE, 0.B, MIE, 0.B, SIE, 0.B)
  } else { // rv32 version
    require(xlen == 32)
    Cat(0.U(26.W), MBE, SBE, 0.U(4.W), SD, 0.U(8.W), TSR, TW, TVM, MXR, SUM, MPRV, XS, FS, MPP, VS, SPP, MPIE, UBE, SPIE, 0.B, MIE, 0.B, SIE, 0.B)
  }
  Predef.assert(mstatus.getWidth == 64)

  val MEIE = false.B // machine level external interrupt enable
  val MTIE = RegInit(false.B) // machine level timer interrupt enable
  val MSIE = RegInit(false.B) // machine level SW interrupt enable
  val SEIE = false.B // supervisor level external IE
  val STIE = false.B // supervisor level timer IE
  val SSIE = false.B // supervisor level software IE
  val mie = Cat(0.U((mxlen - 16).W), 0.U(4.W), MEIE, 0.B, SEIE, 0.B, MTIE, 0.B, STIE, 0.B, MSIE, 0.B, SSIE, 0.B)
  Predef.assert(mie.getWidth == mxlen)

  val mtvec = Reg(UInt(mxlen.W))
  assert(mtvec(1,0) === 0.U, "Only direct trap mode is supported")

  // Machine Trap Handling
  val mscratch = Reg(UInt(mxlen.W))
  val mepc = Reg(UInt(mxlen.W))
  val mcause = Reg(UInt(mxlen.W))
  val mtval = Reg(UInt(mxlen.W))

  val MEIP = false.B // machine external interrupt pending
  val MTIP = RegInit(false.B) // machine timer interrupt pending
  val MSIP = RegInit(false.B) // machine SW interrupt pending
  val SEIP = false.B // supervisor external IP
  val STIP = false.B // supervisor time IP
  val SSIP = false.B // supervisor SW IP
  val mip = Cat(0.U((mxlen - 16).W), 0.U(4.W), MEIP, 0.B, SEIP, 0.B, MTIP, 0.B, STIP, 0.B, MSIP, 0.B, SSIP, 0.B)
  Predef.assert(mip.getWidth == mxlen)

  // Machine Memory Protection
  val pmpcfg0 = 0.U(xlen.W)
  val pmpaddr0 = 0.U(xlen.W)

  val csrFileGeneral: Seq[(BitPat, UInt)] = Seq(
    BitPat(CSR.cycle) -> (if (xlen == 32) cycle(31, 0) else cycle),
    BitPat(CSR.time) -> (if (xlen == 32) time(31, 0) else time),
    BitPat(CSR.instret) -> (if (xlen == 32) instret(31, 0) else instret),
    BitPat(CSR.mvendorid) -> mvendorid,
    BitPat(CSR.marchid) -> marchid,
    BitPat(CSR.mimpid) -> mimpid,
    BitPat(CSR.mhartid) -> mhartid,
    BitPat(CSR.mstatus) -> (if (xlen == 32) mstatus(31, 0) else mstatus),
    BitPat(CSR.misa) -> misa,
    BitPat(CSR.mie) -> mie,
    BitPat(CSR.mtvec) -> mtvec,
    BitPat(CSR.mscratch) -> mscratch,
    BitPat(CSR.mepc) -> mepc,
    BitPat(CSR.mcause) -> mcause,
    BitPat(CSR.mtval) -> mtval,
    BitPat(CSR.mip) -> mip,
    BitPat(CSR.pmpcfg0) -> pmpcfg0,
    BitPat(CSR.pmpaddr0) -> pmpaddr0,
    BitPat(CSR.mcycle) -> (if (xlen == 32) cycle(31, 0) else cycle),
    BitPat(CSR.minstret) -> (if (xlen == 32) instret(31, 0) else instret),
  )
  // csrFileGeneral.foreach { x => println(x._1.value); println(x._2.getWidth) }
  Predef.assert(csrFileGeneral.map(_._2).forall(_.getWidth == mxlen))
  val csrFileRv32 = Seq(
    BitPat(CSR.cycleh) -> cycle(63, 32),
    BitPat(CSR.timeh) -> time(63, 32),
    BitPat(CSR.instreth) -> instret(63, 32),
    BitPat(CSR.mstatush) -> mstatus(63, 32),
    BitPat(CSR.mcycleh) -> cycle(63, 32),
    BitPat(CSR.minstret) -> instret(63, 32)
  )
  val csrFile = if (xlen == 32) {csrFileGeneral ++ csrFileRv32} else csrFileGeneral

  io.out := Lookup(csr_addr, 0.U, csrFile).asUInt

  val privValid = csr_addr(9, 8) <= current_prv_level
  val privInst = io.cmd === CSR.P
  val isEcall = privInst && Instructions.ECALL === io.inst // TODO: optimize logic
  val isEbreak = privInst && Instructions.EBREAK === io.inst // TODO: ''
  val isMret = privInst && Instructions.MRET === io.inst // TODO: ''
  val csrValid = csrFile.map(_._1 === csr_addr).reduce(_ || _)
  val csrRO = csr_addr(11, 10).andR
  val wen = io.cmd === CSR.W || ((io.cmd === CSR.S || io.cmd === CSR.C) && rs1_addr.orR)
  val wdata = MuxLookup(
    io.cmd,
    0.U,
    Seq(
      CSR.W -> io.in,
      CSR.S -> (io.out | io.in),
      CSR.C -> (io.out & (~io.in).asUInt)
    )
  )
  val iaddrInvalid = io.pc_check && io.addr(1)
  val laddrInvalid = MuxLookup(
    io.ld_type,
    false.B,
    Seq(Control.LD_LW -> io.addr(1, 0).orR, Control.LD_LH -> io.addr(0), Control.LD_LHU -> io.addr(0))
  )
  val saddrInvalid =
    MuxLookup(io.st_type, false.B, Seq(Control.ST_SW -> io.addr(1, 0).orR, Control.ST_SH -> io.addr(0)))
  io.expt := io.illegal || iaddrInvalid || laddrInvalid || saddrInvalid ||
    ((io.cmd === CSR.W || io.cmd === CSR.S || io.cmd === CSR.C) && (!csrValid || !privValid)) || wen && csrRO ||
    (privInst && !privValid) || isEcall || isEbreak
  io.evec := mtvec
  io.epc := mepc

  // Counters
  time := time + 1.U
  cycle := cycle + 1.U
  val isInstRet = io.inst =/= Instructions.NOP && (!io.expt || isEcall || isEbreak) && !io.stall
  when(isInstRet) { instret := instret + 1.U }

  when(!io.stall) {
    when(io.expt) {
      mepc := io.pc >> 2 << 2
      when(iaddrInvalid) {
        mcause := Cause.InstAddrMisaligned
      }.elsewhen(laddrInvalid) {
        mcause := Cause.LoadAddrMisaligned
      }.elsewhen(saddrInvalid) {
        mcause := Cause.StoreAddrMisaligned
      }.elsewhen(isEcall) {
        mcause := Cause.Ecall + current_prv_level // 8 = ecall from U, 11 = ecall from M
      }.elsewhen(isEbreak) {
        mcause := Cause.Breakpoint
      }.otherwise {
        mcause := Cause.IllegalInst
      }
      current_prv_level := CSR.PRV_M
      MPIE := MIE
      MPP := current_prv_level
      MIE := 0.B
      when(iaddrInvalid || laddrInvalid || saddrInvalid) { mepc := io.addr }
    }.elsewhen(isMret) {
      current_prv_level := MPP
      MIE := MPIE
      MPP := CSR.PRV_U
      MPIE := 1.B
    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) {
        MIE := wdata(3)
      }.elsewhen(csr_addr === CSR.mip) {
        MTIP := wdata(7)
        MSIP := wdata(3)
      }.elsewhen(csr_addr === CSR.mie) {
        MTIE := wdata(7)
        MSIE := wdata(3)
      }
        .elsewhen(csr_addr === CSR.mscratch) { mscratch := wdata }
        .elsewhen(csr_addr === CSR.mepc) { mepc := wdata >> 2.U << 2.U }
        .elsewhen(csr_addr === CSR.mcause) { mcause := wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
        .elsewhen(csr_addr === CSR.cycle) { cycle := wdata } // TODO: should only write to lower half of cycle
        .elsewhen(csr_addr === CSR.cycleh) { cycle := wdata } // TODO: should only write to upper half of cycle
        .elsewhen(csr_addr === CSR.mcycle) { cycle := wdata } // TODO
        .elsewhen(csr_addr === CSR.mcycleh) { cycle:= wdata } // TODO
        .elsewhen(csr_addr === CSR.instret) { instret := wdata } // TODO
        .elsewhen(csr_addr === CSR.instreth) { instret := wdata } // TODO
        .elsewhen(csr_addr === CSR.minstret) { instret := wdata } // TODO
        .elsewhen(csr_addr === CSR.minstreth) { instret := wdata } // TODO
        .elsewhen(csr_addr === CSR.time) { time := wdata } // TODO
        .elsewhen(csr_addr === CSR.timeh) { time := wdata } // TODO
        .elsewhen(csr_addr === CSR.mtvec) { mtvec := wdata }
    }
  }
}
