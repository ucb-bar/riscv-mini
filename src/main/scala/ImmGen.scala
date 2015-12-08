package mini

import Chisel._
import cde.Parameters
import Control._

class ImmGenIO(implicit p: Parameters) extends CoreBundle()(p) {
  val inst = UInt(INPUT, xlen)
  val sel  = UInt(INPUT, 3)
  val out  = UInt(OUTPUT, xlen)
}

abstract class ImmGen(implicit p: Parameters) extends Module {
  val io = new ImmGenIO
}

class ImmGenWire(implicit p: Parameters) extends ImmGen()(p) {
  val Iimm = io.inst(31, 20).toSInt
  val Simm = Cat(io.inst(31, 25), io.inst(11,7)).toSInt
  val Bimm = Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11, 8), UInt(0,1)).toSInt
  val Uimm = Cat(io.inst(31, 12), UInt(0, 12)).toSInt
  val Jimm = Cat(io.inst(31), io.inst(19, 12), io.inst(20), io.inst(30, 25), io.inst(24, 21), UInt(0, 1)).toSInt
  val Zimm = io.inst(19, 15).zext

  io.out := MuxLookup(io.sel, Iimm & SInt(-2), 
    Seq(IMM_I -> Iimm, IMM_S -> Simm, IMM_B -> Bimm, IMM_U -> Uimm, IMM_J -> Jimm, IMM_Z -> Zimm))
}

class ImmGenMux(implicit p: Parameters) extends ImmGen()(p) {
  val sign = Mux(io.sel === IMM_Z, SInt(0), io.inst(31).toSInt)
  val b30_20 = Mux(io.sel === IMM_U, io.inst(30,20).toSInt, sign)
  val b19_12 = Mux(io.sel =/= IMM_U && io.sel =/= IMM_J, sign, io.inst(19,12).toSInt)
  val b11 = Mux(io.sel === IMM_U || io.sel === IMM_Z, SInt(0),
            Mux(io.sel === IMM_J, io.inst(20).toSInt,
            Mux(io.sel === IMM_B, io.inst(7).toSInt, sign)))
  val b10_5 = Mux(io.sel === IMM_U || io.sel === IMM_Z, UInt(0), io.inst(30,25))
  val b4_1 = Mux(io.sel === IMM_U, UInt(0),
             Mux(io.sel === IMM_S || io.sel === IMM_B, io.inst(11,8),
             Mux(io.sel === IMM_Z, io.inst(19,16), io.inst(24,21))))
  val b0 = Mux(io.sel === IMM_S, io.inst(7),
           Mux(io.sel === IMM_I, io.inst(20),
           Mux(io.sel === IMM_Z, io.inst(15), UInt(0))))

  io.out := Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).toSInt
}
