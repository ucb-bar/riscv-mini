// See LICENSE for license details.

package mini

import chisel3._
import config.Parameters
import Control._

class BrCondIO(implicit p: Parameters) extends CoreBundle()(p) {
  val rs1 = Input(UInt(xlen.W))
  val rs2 = Input(UInt(xlen.W))
  val br_type = Input(UInt(3.W))
  val taken = Output(Bool())
}

abstract class BrCond(implicit p: Parameters) extends Module {
  val io = IO(new BrCondIO)
}

class BrCondSimple(implicit p: Parameters) extends BrCond()(p) {
  val eq   = io.rs1 === io.rs2
  val neq  = !eq
  val lt   = io.rs1.asSInt < io.rs2.asSInt
  val ge   = !lt
  val ltu  = io.rs1 < io.rs2
  val geu  = !ltu
  io.taken :=     
    ((io.br_type === BR_EQ) && eq) ||
    ((io.br_type === BR_NE) && neq) ||
    ((io.br_type === BR_LT) && lt) ||
    ((io.br_type === BR_GE) && ge) ||
    ((io.br_type === BR_LTU) && ltu) ||
    ((io.br_type === BR_GEU) && geu)
}

class BrCondArea(implicit val p: Parameters) extends BrCond()(p) with CoreParams {
  val diff = io.rs1 - io.rs2
  val neq  = diff.orR
  val eq   = !neq
  val isSameSign = io.rs1(xlen-1) === io.rs2(xlen-1)
  val lt   = Mux(isSameSign, diff(xlen-1), io.rs1(xlen-1))
  val ltu  = Mux(isSameSign, diff(xlen-1), io.rs2(xlen-1))
  val ge   = !lt
  val geu  = !ltu
  io.taken :=     
    ((io.br_type === BR_EQ) && eq) ||
    ((io.br_type === BR_NE) && neq) ||
    ((io.br_type === BR_LT) && lt) ||
    ((io.br_type === BR_GE) && ge) ||
    ((io.br_type === BR_LTU) && ltu) ||
    ((io.br_type === BR_GEU) && geu)
}
