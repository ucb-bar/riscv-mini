// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util.Valid
import config.{Parameters, Field}

case object XLEN extends Field[Int]
case object Trace extends Field[Boolean]
case object BuildALU extends Field[Parameters => ALU]
case object BuildImmGen extends Field[Parameters => ImmGen]
case object BuildBrCond extends Field[Parameters => BrCond]

abstract trait CoreParams {
  implicit val p: Parameters
  val xlen = p(XLEN)
}

abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p) with CoreParams

class HostIO(implicit p: Parameters) extends CoreBundle()(p) {
  val fromhost = Flipped(Valid(UInt(xlen.W)))
  val tohost   = Output(UInt(xlen.W))
}

class CoreIO(implicit p: Parameters) extends Bundle {
  val host = new HostIO
  val icache = Flipped((new CacheIO))
  val dcache = Flipped((new CacheIO))
}

class Core(implicit val p: Parameters) extends Module with CoreParams {
  val io = IO(new CoreIO)
  val dpath = Module(new Datapath) 
  val ctrl  = Module(new Control)

  io.host <> dpath.io.host
  dpath.io.icache <> io.icache
  dpath.io.dcache <> io.dcache
  dpath.io.ctrl <> ctrl.io
}
