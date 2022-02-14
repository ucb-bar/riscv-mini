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
  val medeleg = 0x302.U(12.W) // machine exception delegation
  val mideleg = 0x303.U(12.W) // machine interrupt delegation
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
  // TODO: mtime, mtimeh, mtimecmp are all memory mapped now, not CSRs
  // val mtimecmp = 0x321.U(12.W) // TODO: no longer a CSR, now memory mapped
  // time csr = read only shadow of mtime
  // same for cycle, instret CSRs
  // val mtime = 0x701.U(12.W)
  // val mtimeh = 0x741.U(12.W)
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
    medeleg,
    mideleg,
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
  // Excpetion
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

  // user counters
  val cycle = RegInit(0.U(64.W)) // same counter is used for time
  val instret = RegInit(0.U(64.W))

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
  val misa = Cat(mxl, 0.U(mxlen-28), extensions.U(26.W))
  val mvendorid = 0.U(mxlen.W) // non-commercial implementation
  val marchid = 0.U(mxlen.W) // microarch of the hart: unimplemented
  val mimpid = 0.U(mxlen.W) // processor impl encoding: unimplemented
  val mhartid = 0.U(mxlen.W) // only one hart

  //// mstatus
  // trap handling info
  val SIE = 0.B // supervisor interrupt enable
  val MIE = 0.B // machine interrupt enable
  val SPIE = 0.B // supervisor IE prior to trap
  val MPIE = 0.B // machine IE prior to trap
  val SPP = 0.B // supervisor privilege mode prior to trap
  val MPP = 0.U(2.W) // machine privilege mode prior to trap
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

  // trap handling
  val mtvec = Const.PC_EVEC.U(xlen.W)
  val mtdeleg = 0x0.U(xlen.W)

  val PRV = RegInit(CSR.PRV_M)
  val PRV1 = RegInit(CSR.PRV_M)
  val PRV2 = 0.U(2.W)
  val PRV3 = 0.U(2.W)
  val IE = RegInit(false.B)
  val IE1 = RegInit(false.B)
  val IE2 = false.B
  val IE3 = false.B
  // virtualization management field
  val VM = 0.U(5.W)
  // memory privilege
  val MPRV = false.B

  // interrupt handling
  val MTIP = RegInit(false.B)
  val HTIP = false.B
  val STIP = false.B
  val MTIE = RegInit(false.B)
  val HTIE = false.B
  val STIE = false.B
  val MSIP = RegInit(false.B)
  val HSIP = false.B
  val SSIP = false.B
  val MSIE = RegInit(false.B)
  val HSIE = false.B
  val SSIE = false.B

  val mip = Cat(0.U((xlen - 8).W), MTIP, HTIP, STIP, false.B, MSIP, HSIP, SSIP, false.B)
  val mie = Cat(0.U((xlen - 8).W), MTIE, HTIE, STIE, false.B, MSIE, HSIE, SSIE, false.B)

  val mscratch = Reg(UInt(xlen.W))

  val mepc = Reg(UInt(xlen.W))
  val mcause = Reg(UInt(xlen.W))
  val mbadaddr = Reg(UInt(xlen.W))

  val csrFileGeneral: Seq[(BitPat, UInt)] = Seq(
    BitPat(CSR.cycle) -> (if (xlen == 32) cycle(31, 0) else cycle),
    BitPat(CSR.time) -> (if (xlen == 32) cycle(31, 0) else cycle),
    BitPat(CSR.instret) -> (if (xlen == 32) instret(31, 0) else instret),
    BitPat(CSR.mvendorid) -> mvendorid,
    BitPat(CSR.marchid) -> marchid,
    BitPat(CSR.mimpid) -> mimpid,
    BitPat(CSR.mhartid) -> mhartid,
    BitPat(CSR.mstatus) -> (if (xlen == 32) mstatus(31, 0) else mstatus),
    BitPat(CSR.misa) -> misa,
    BitPat(CSR.medeleg),
    BitPat(CSR.mideleg),
    BitPat(CSR.mie),
    BitPat(CSR.mtvec),
    BitPat(CSR.mscratch),
    BitPat(CSR.mepc),
    BitPat(CSR.mcause),
    BitPat(CSR.mtval),
    BitPat(CSR.mip),
    BitPat(CSR.pmpcfg0),
    BitPat(CSR.pmpaddr0),
    BitPat(CSR.mcycle) -> (if (xlen == 32) cycle(31, 0) else cycle),
    BitPat(CSR.minstret) -> (if (xlen == 32) instret(31, 0) else instret),
  )
  Predef.assert(csrFileGeneral.map(_._2).forall(_.getWidth == xlen))
  val csrFileRv32 = Seq(
    BitPat(CSR.cycleh) -> cycle(63, 32),
    BitPat(CSR.timeh) -> cycle(63, 32),
    BitPat(CSR.instreth) -> instret(63, 32),
    BitPat(CSR.mstatush) -> mstatus(63, 32),
    BitPat(CSR.mcycleh) -> cycle(63, 32),
    BitPat(CSR.minstret) -> instred(63, 32)
  )
  val csrFile = if (xlen == 32) {csrFileGeneral ++ csrFileRv32} else csrFileGeneral

  io.out := Lookup(csr_addr, 0.U, csrFile).asUInt

  val privValid = csr_addr(9, 8) <= PRV
  val privInst = io.cmd === CSR.P
  val isEcall = privInst && !csr_addr(0) && !csr_addr(8)
  val isEbreak = privInst && csr_addr(0) && !csr_addr(8)
  val isEret = privInst && !csr_addr(0) && csr_addr(8)
  val csrValid = csrFile.map(_._1 === csr_addr).reduce(_ || _)
  val csrRO = csr_addr(11, 10).andR || csr_addr === CSR.mtvec || csr_addr === CSR.mtdeleg
  val wen = io.cmd === CSR.W || io.cmd(1) && rs1_addr.orR
  val wdata = MuxLookup(
    io.cmd,
    0.U,
    Seq(
      CSR.W -> io.in,
      CSR.S -> (io.out | io.in),
      CSR.C -> (io.out & ~io.in)
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
    io.cmd(1, 0).orR && (!csrValid || !privValid) || wen && csrRO ||
    (privInst && !privValid) || isEcall || isEbreak
  io.evec := mtvec + (PRV << 6)
  io.epc := mepc

  // Counters
  time := time + 1.U
  when(time.andR) { timeh := timeh + 1.U }
  cycle := cycle + 1.U
  when(cycle.andR) { cycleh := cycleh + 1.U }
  val isInstRet = io.inst =/= Instructions.NOP && (!io.expt || isEcall || isEbreak) && !io.stall
  when(isInstRet) { instret := instret + 1.U }
  when(isInstRet && instret.andR) { instreth := instreth + 1.U }

  when(!io.stall) {
    when(io.expt) {
      mepc := io.pc >> 2 << 2
      mcause := Mux(
        iaddrInvalid,
        Cause.InstAddrMisaligned,
        Mux(
          laddrInvalid,
          Cause.LoadAddrMisaligned,
          Mux(
            saddrInvalid,
            Cause.StoreAddrMisaligned,
            Mux(isEcall, Cause.Ecall + PRV, Mux(isEbreak, Cause.Breakpoint, Cause.IllegalInst))
          )
        )
      )
      PRV := CSR.PRV_M
      IE := false.B
      PRV1 := PRV
      IE1 := IE
      when(iaddrInvalid || laddrInvalid || saddrInvalid) { mbadaddr := io.addr }
    }.elsewhen(isEret) {
      PRV := PRV1
      IE := IE1
      PRV1 := CSR.PRV_U
      IE1 := true.B
    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) {
        PRV1 := wdata(5, 4)
        IE1 := wdata(3)
        PRV := wdata(2, 1)
        IE := wdata(0)
      }
        .elsewhen(csr_addr === CSR.mip) {
          MTIP := wdata(7)
          MSIP := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mie) {
          MTIE := wdata(7)
          MSIE := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mtime) { time := wdata }
        .elsewhen(csr_addr === CSR.mtimeh) { timeh := wdata }
        .elsewhen(csr_addr === CSR.mtimecmp) { mtimecmp := wdata }
        .elsewhen(csr_addr === CSR.mscratch) { mscratch := wdata }
        .elsewhen(csr_addr === CSR.mepc) { mepc := wdata >> 2.U << 2.U }
        .elsewhen(csr_addr === CSR.mcause) { mcause := wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
        .elsewhen(csr_addr === CSR.mbadaddr) { mbadaddr := wdata }
        .elsewhen(csr_addr === CSR.mtohost) { mtohost := wdata }
        .elsewhen(csr_addr === CSR.mfromhost) { mfromhost := wdata }
        .elsewhen(csr_addr === CSR.cyclew) { cycle := wdata }
        .elsewhen(csr_addr === CSR.timew) { time := wdata }
        .elsewhen(csr_addr === CSR.instretw) { instret := wdata }
        .elsewhen(csr_addr === CSR.cyclehw) { cycleh := wdata }
        .elsewhen(csr_addr === CSR.timehw) { timeh := wdata }
        .elsewhen(csr_addr === CSR.instrethw) { instreth := wdata }
    }
  }
}
