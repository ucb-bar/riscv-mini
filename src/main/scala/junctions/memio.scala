// See LICENSE for license details.

package junctions
import Chisel._
import scala.math._
import cde.{Parameters, Field}

case object MIFAddrBits extends Field[Int]
case object MIFDataBits extends Field[Int]
case object MIFTagBits extends Field[Int]
case object MIFDataBeats extends Field[Int]

trait HasMIFParameters {
  implicit val p: Parameters
  val mifTagBits = p(MIFTagBits)
  val mifAddrBits = p(MIFAddrBits)
  val mifDataBits = p(MIFDataBits)
  val mifDataBeats = p(MIFDataBeats)
}
 
abstract class MIFModule(implicit val p: Parameters) extends Module with HasMIFParameters
abstract class MIFBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasMIFParameters

trait HasMemData extends HasMIFParameters {
  val data = Bits(width = mifDataBits)
}

trait HasMemAddr extends HasMIFParameters {
  val addr = UInt(width = mifAddrBits)
}

trait HasMemTag extends HasMIFParameters {
  val tag = UInt(width = mifTagBits)
}

class MemReqCmd(implicit p: Parameters) extends MIFBundle()(p) with HasMemAddr with HasMemTag {
  val rw = Bool()
}

class MemTag(implicit p: Parameters) extends MIFBundle()(p) with HasMemTag
class MemData(implicit p: Parameters) extends MIFBundle()(p) with HasMemData
class MemResp(implicit p: Parameters) extends MIFBundle()(p) with HasMemData with HasMemTag

class MemIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val req_cmd  = Decoupled(new MemReqCmd)
  val req_data = Decoupled(new MemData)
  val resp     = Decoupled(new MemResp).flip
}
