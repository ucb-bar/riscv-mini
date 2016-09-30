package mini

import Chisel._
import cde.Parameters
import scala.collection.immutable.ListMap

object CSR {
  val N = UInt(0, 3)
  val W = UInt(1, 3)
  val S = UInt(2, 3)
  val C = UInt(3, 3)
  val P = UInt(4, 3)

  // Supports machine & user modes
  val PRV_U = UInt(0x0, 2)
  val PRV_M = UInt(0x3, 2)

  // User-level CSR addrs
  val cycle    = UInt(0xc00, 12)
  val time     = UInt(0xc01, 12)
  val instret  = UInt(0xc02, 12)
  val cycleh   = UInt(0xc80, 12)
  val timeh    = UInt(0xc81, 12)
  val instreth = UInt(0xc82, 12)

  // Supervisor-level CSR addrs
  val cyclew    = UInt(0x900, 12)
  val timew     = UInt(0x901, 12)
  val instretw  = UInt(0x902, 12)
  val cyclehw   = UInt(0x980, 12)
  val timehw    = UInt(0x981, 12)
  val instrethw = UInt(0x982, 12)

  // Machine-level CSR addrs
  // Machine Information Registers
  val mcpuid   = UInt(0xf00, 12)
  val mimpid   = UInt(0xf01, 12)
  val mhartid  = UInt(0xf10, 12)
  // Machine Trap Setup
  val mstatus  = UInt(0x300, 12)
  val mtvec    = UInt(0x301, 12)
  val mtdeleg  = UInt(0x302, 12) 
  val mie      = UInt(0x304, 12)
  val mtimecmp = UInt(0x321, 12)
  // Machine Timers and Counters
  val mtime    = UInt(0x701, 12)
  val mtimeh   = UInt(0x741, 12)
  // Machine Trap Handling
  val mscratch = UInt(0x340, 12)
  val mepc     = UInt(0x341, 12)
  val mcause   = UInt(0x342, 12)
  val mbadaddr = UInt(0x343, 12)
  val mip      = UInt(0x344, 12)
  // Hachine HITF
  val mtohost   = UInt(0x780, 12)
  val mfromhost = UInt(0x781, 12)

  val regs = List(
    cycle, time, instret, cycleh, timeh, instreth,
    cyclew, timew, instretw, cyclehw, timehw, instrethw,
    mcpuid, mimpid, mhartid, mtvec, mtdeleg, mie,
    mtimecmp, mtime, mtimeh, mscratch, mepc, mcause, mbadaddr, mip,
    mtohost, mfromhost, mstatus) map (BitPat(_))
}

object Cause {
  val InstAddrMisaligned  = UInt(0x0)
  val IllegalInst         = UInt(0x2)
  val Breakpoint          = UInt(0x3)
  val LoadAddrMisaligned  = UInt(0x4)
  val StoreAddrMisaligned = UInt(0x6)
  val Ecall               = UInt(0x8)
}

class CSRIO(implicit p: Parameters)  extends CoreBundle()(p) {
  val stall = Bool(INPUT)
  val cmd   = UInt(INPUT, 3)
  val in    = UInt(INPUT, xlen)
  val out   = UInt(OUTPUT, xlen)
  // Excpetion
  val pc       = UInt(INPUT, xlen)
  val addr     = UInt(INPUT, xlen)
  val inst     = UInt(INPUT, xlen)
  val illegal  = Bool(INPUT)
  val st_type  = UInt(INPUT, 2)
  val ld_type  = UInt(INPUT, 3)
  val pc_check = Bool(INPUT)
  val expt     = Bool(OUTPUT)
  val evec     = UInt(OUTPUT, xlen)
  val epc      = UInt(OUTPUT, xlen)
  // HTIF
  val host = new HostIO
}

class CSR(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new CSRIO)

  val csr_addr = io.inst(31, 20)
  val rs1_addr = io.inst(19, 15)

  // user counters
  val time     = RegInit(UInt(0, xlen))
  val timeh    = RegInit(UInt(0, xlen))
  val cycle    = RegInit(UInt(0, xlen))
  val cycleh   = RegInit(UInt(0, xlen))
  val instret  = RegInit(UInt(0, xlen))
  val instreth = RegInit(UInt(0, xlen))

  val mcpuid  = Cat(UInt(0, 2) /* RV32I */, UInt(0, xlen-28), 
                    UInt(1 << ('I' - 'A') /* Base ISA */| 
                         1 << ('U' - 'A') /* User Mode */, 26))
  val mimpid  = UInt(0, xlen) // not implemented
  val mhartid = UInt(0, xlen) // only one hart

  // interrupt enable stack
  val PRV  = RegInit(CSR.PRV_M)
  val PRV1 = RegInit(CSR.PRV_M)
  val PRV2 = UInt(0, 2)
  val PRV3 = UInt(0, 2)
  val IE  = RegInit(Bool(false))
  val IE1 = RegInit(Bool(false))
  val IE2 = Bool(false)
  val IE3 = Bool(false)
  // virtualization management field
  val VM = UInt(0, 5)
  // memory privilege
  val MPRV = Bool(false)
  // extention context status
  val XS = UInt(0, 2)
  val FS = UInt(0, 2)
  val SD = UInt(0, 1)
  val mstatus = Cat(SD, UInt(0, xlen-23), VM, MPRV, XS, FS, PRV3, IE3, PRV2, IE2, PRV1, IE1, PRV, IE)
  val mtvec   = Const.PC_EVEC
  val mtdeleg = UInt(0x0, xlen)
  
  // interrupt registers
  val MTIP = RegInit(Bool(false))
  val HTIP = Bool(false)
  val STIP = Bool(false)
  val MTIE = RegInit(Bool(false))
  val HTIE = Bool(false)
  val STIE = Bool(false)
  val MSIP = RegInit(Bool(false))
  val HSIP = Bool(false)
  val SSIP = Bool(false)
  val MSIE = RegInit(Bool(false))
  val HSIE = Bool(false)
  val SSIE = Bool(false)
  val mip = Cat(UInt(0, xlen-8), MTIP, HTIP, STIP, Bool(false), MSIP, HSIP, SSIP, Bool(false))
  val mie = Cat(UInt(0, xlen-8), MTIE, HTIE, STIE, Bool(false), MSIE, HSIE, SSIE, Bool(false))

  val mtimecmp = Reg(UInt(width=xlen)) 

  val mscratch = Reg(UInt(width=xlen))

  val mepc = Reg(UInt(width=xlen))
  val mcause = Reg(UInt(width=xlen))
  val mbadaddr = Reg(UInt(width=xlen))

  val mtohost = RegInit(UInt(0, xlen))
  val mfromhost = Reg(UInt(width=xlen))
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }

  val csrFile = CSR.regs zip List(
    cycle, time, instret, cycleh, timeh, instreth,
    cycle, time, instret, cycleh, timeh, instreth,
    mcpuid, mimpid, mhartid, mtvec, mtdeleg, mie,
    mtimecmp, time, timeh, mscratch, mepc, mcause, mbadaddr, mip,
    mtohost, mfromhost, mstatus)

  io.out := Lookup(csr_addr, UInt(0), csrFile).asUInt

  val privValid = csr_addr(9, 8) <= PRV
  val privInst  = io.cmd === CSR.P
  val isEcall   = privInst && !csr_addr(0) && !csr_addr(8)
  val isEbreak  = privInst &&  csr_addr(0) && !csr_addr(8)
  val isEret    = privInst && !csr_addr(0) &&  csr_addr(8)
  val csrValid  = csrFile map (_._1 === csr_addr) reduce (_ || _)
  val csrRO     = csr_addr(11, 10).andR || csr_addr === CSR.mtvec || csr_addr === CSR.mtdeleg
  val wen       = io.cmd === CSR.W || io.cmd(1) && rs1_addr.orR 
  val wdata     = MuxLookup(io.cmd, UInt(0), Seq(
    CSR.W -> io.in,
    CSR.S -> (io.out | io.in),
    CSR.C -> (io.out & ~io.in)
  ))
  val iaddrInvalid = io.pc_check && io.addr(1)
  val laddrInvalid = MuxLookup(io.ld_type, Bool(false), Seq(
    Control.LD_LW -> io.addr(1, 0).orR, Control.LD_LH -> io.addr(0), Control.LD_LHU -> io.addr(0)))
  val saddrInvalid = MuxLookup(io.st_type, Bool(false), Seq(
    Control.ST_SW -> io.addr(1, 0).orR, Control.ST_SH -> io.addr(0)))
  io.expt := io.illegal || iaddrInvalid || laddrInvalid || saddrInvalid ||
             io.cmd(1, 0).orR && (!csrValid || !privValid) || wen && csrRO || 
             (privInst && !privValid) || isEcall || isEbreak
  io.evec := mtvec + (PRV << UInt(6))
  io.epc  := mepc

  // Counters
  time := time + UInt(1)
  when(time.andR) { timeh := timeh + UInt(1) }
  cycle := cycle + UInt(1)
  when(cycle.andR) { cycleh := cycleh + UInt(1) }
  val isInstRet = io.inst =/= Instructions.NOP && (!io.expt || isEcall || isEbreak) && !io.stall
  when(isInstRet) { instret := instret + UInt(1) }
  when(isInstRet && instret.andR) { instreth := instreth + UInt(1) }

  when(!io.stall) {
    when(io.expt) {
      mepc   := io.pc >> 2 << 2
      mcause := Mux(iaddrInvalid, Cause.InstAddrMisaligned,
                Mux(laddrInvalid, Cause.LoadAddrMisaligned,
                Mux(saddrInvalid, Cause.StoreAddrMisaligned,
                Mux(isEcall,      Cause.Ecall + PRV,
                Mux(isEbreak,     Cause.Breakpoint, Cause.IllegalInst)))))
      PRV  := CSR.PRV_M
      IE   := Bool(false)
      PRV1 := PRV
      IE1  := IE
      when(iaddrInvalid || laddrInvalid || saddrInvalid) { mbadaddr := io.addr }
    }.elsewhen(isEret) {
      PRV  := PRV1
      IE   := IE1
      PRV1 := CSR.PRV_U
      IE1  := Bool(true)
    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) { 
        PRV1 := wdata(5, 4)
        IE1  := wdata(3)
        PRV  := wdata(2, 1)
        IE   := wdata(0)
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
      .elsewhen(csr_addr === CSR.mepc) { mepc := wdata >> 2 << 2 }
      .elsewhen(csr_addr === CSR.mcause) { mcause := wdata & UInt(BigInt(1) << (xlen-1) | 0xf) }
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
