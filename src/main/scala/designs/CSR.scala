package mini

import Chisel._
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
}

object Cause {
  val InstAddrMisaligned = UInt(0x0)
  val IllegalInst        = UInt(0x2)
  val Breakpoint         = UInt(0x3)
  val LoadAddrMisaligned = UInt(0x4)
  val Ecall              = UInt(0x8)
}

class CSRIO extends CoreBundle {
  val cmd  = UInt(INPUT, 3)
  val csr  = UInt(INPUT, 12)
  val src  = UInt(INPUT, 5)
  val in   = UInt(INPUT, instLen)
  val out  = UInt(OUTPUT, instLen)
  // Excpetion
  val pc   = UInt(INPUT, instLen)
  val expt = Bool(OUTPUT)
  val eret = Bool(OUTPUT)
  val evec = UInt(OUTPUT, instLen)
  val illegal_inst = Bool(INPUT)
  // HTIF
  val host = new HostIO
}

class CSR extends Module with CoreParams {
  val io = new CSRIO

  val mcpuid  = Cat(UInt(0, 2) /* RV32I */, UInt(0, instLen-28), 
                    UInt(1 << ('I' - 'A') /* Base ISA */| 
                         1 << ('U' - 'A') /* User Mode */, 26))
  val mimpid  = UInt(0, instLen) // not implemented
  val mhartid = UInt(0, instLen) // only one hart

  // interrupt enable stack
  val PRV  = RegInit(CSR.PRV_U)
  val PRV1 = UInt(0, 2)
  val PRV2 = UInt(0, 2)
  val PRV3 = UInt(0, 2)
  val IE  = RegInit(Bool(true))
  val IE1 = Bool(false)
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
  val mstatus = Cat(SD, UInt(0, instLen-23), VM, MPRV, XS, FS, PRV3, IE3, PRV2, IE2, PRV1, IE1, PRV, IE)
  val mtvec   = Const.PC_EVEC
  val mtdeleg = UInt(0x0, instLen)
  
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
  val mip = Cat(UInt(0, instLen-8), MTIP, HTIP, STIP, Bool(false), MSIP, HSIP, SSIP, Bool(false))
  val mie = Cat(UInt(0, instLen-8), MTIE, HTIE, STIE, Bool(false), MSIE, HSIE, SSIE, Bool(false))

  val mtime = RegInit(UInt(0, instLen))
  val mtimeh = RegInit(UInt(0, instLen))
  val mtimecmp = Reg(UInt(width=instLen)) 

  val mscratch = Reg(UInt(width=instLen))

  val mepc = Reg(UInt(width=instLen))
  val mcause = RegInit(UInt(0, instLen))
  val mbadaddr = Reg(UInt(width=instLen))

  val mtohost = RegInit(UInt(0, instLen))
  val mfromhost = Reg(UInt(width=instLen))
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }
 
  val csrFile = ListMap(
    CSR.mcpuid    -> mcpuid,
    CSR.mimpid    -> mimpid,
    CSR.mhartid   -> mhartid,
    CSR.mstatus   -> mstatus,
    CSR.mtvec     -> mtvec,
    CSR.mtdeleg   -> mtdeleg,
    CSR.mie       -> mie,
    CSR.mtimecmp  -> mtimecmp,
    CSR.mtime     -> mtime,
    CSR.mtimeh    -> mtimeh,
    CSR.mscratch  -> mscratch,
    CSR.mepc      -> mepc,
    CSR.mcause    -> mcause,
    CSR.mbadaddr  -> mbadaddr,
    CSR.mip       -> mip,
    CSR.mtohost   -> mtohost,
    CSR.mfromhost -> mfromhost)
  val csr = Lookup(io.csr, UInt(0), csrFile.toSeq)
  io.out := csr.zext

  val privValid = io.csr(9, 8) <= PRV
  val privInst  = io.cmd === CSR.P
  val isEcall   = privInst && !io.csr(0) && !io.csr(8)
  val isEbreak  = privInst && io.csr(0)  && !io.csr(8)
  val isEret    = privInst && !io.csr(0) && io.csr(8)
  val csrValid  = csrFile map (_._1 === io.csr) reduce (_ || _)
  val csrRO     = io.csr(11, 10).andR || io.csr === CSR.mtvec || io.csr === CSR.mtdeleg
  val wen       = io.cmd(1, 0).orR && io.src.orR
  val wdata     = MuxLookup(io.cmd, UInt(0), Seq(
    CSR.W -> io.in,
    CSR.S -> (io.out | io.in),
    CSR.C -> (io.out & ~io.in)
  ))
  io.expt := io.illegal_inst || io.cmd(1, 0).orR && (!csrValid || !privValid || (io.src.orR && csrRO)) ||
             (privInst && !privValid) || isEcall || isEbreak
  io.eret := isEret
  io.evec := Mux(io.eret, mepc, mtvec + (PRV << UInt(6)))

  // Timer
  mtime := mtime + UInt(1)
  when(mtime.andR) { mtimeh := mtimeh + UInt(1) }

  when(io.expt) {
    mepc   := io.pc & SInt(-4)
    mcause := Mux(isEcall,  Cause.Ecall + PRV,
              Mux(isEbreak, Cause.Breakpoint, Cause.IllegalInst))
    PRV := CSR.PRV_M
    IE  := Bool(false)
  }.elsewhen(io.eret) {
    PRV := CSR.PRV_U
    IE  := Bool(true)
  }.elsewhen(wen) {
    when(io.csr === CSR.mstatus) { 
      PRV := wdata(2, 1)
      IE  := wdata(0)
    }
    .elsewhen(io.csr === CSR.mip) {
      MTIP := wdata(7)
      MSIP := wdata(3)
    }
    .elsewhen(io.csr === CSR.mie) {
      MTIE := wdata(7)
      MSIE := wdata(3)
    }
    .elsewhen(io.csr === CSR.mtime) { mtime := wdata }
    .elsewhen(io.csr === CSR.mtimeh) { mtimeh := wdata }
    .elsewhen(io.csr === CSR.mtimecmp) { mtimecmp := wdata }
    .elsewhen(io.csr === CSR.mscratch) { mscratch := wdata }
    .elsewhen(io.csr === CSR.mepc) { mepc := wdata & SInt(-4) }
    .elsewhen(io.csr === CSR.mcause) { mcause := wdata & UInt(BigInt(1) << (instLen-1) | 0xf) }
    .elsewhen(io.csr === CSR.mbadaddr) { mbadaddr := wdata }
    .elsewhen(io.csr === CSR.mtohost) { mtohost := wdata }
    .elsewhen(io.csr === CSR.mfromhost) { mfromhost := wdata }
  }
}
