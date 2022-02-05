// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package junctions
import chisel3._
import scala.math.max
import scala.collection.mutable.ArraySeq
import config._
import util._

case object NastiKey extends Field[NastiParameters]

case class NastiParameters(dataBits: Int, addrBits: Int, idBits: Int)

trait HasNastiParameters {
  implicit val p: Parameters
  val nastiExternal = p(NastiKey)
  val nastiXDataBits = nastiExternal.dataBits
  val nastiWStrobeBits = nastiXDataBits / 8
  val nastiXAddrBits = nastiExternal.addrBits
  val nastiWIdBits = nastiExternal.idBits
  val nastiRIdBits = nastiExternal.idBits
  val nastiXIdBits = max(nastiWIdBits, nastiRIdBits)
  val nastiXUserBits = 1
  val nastiAWUserBits = nastiXUserBits
  val nastiWUserBits = nastiXUserBits
  val nastiBUserBits = nastiXUserBits
  val nastiARUserBits = nastiXUserBits
  val nastiRUserBits = nastiXUserBits
  val nastiXLenBits = 8
  val nastiXSizeBits = 3
  val nastiXBurstBits = 2
  val nastiXCacheBits = 4
  val nastiXProtBits = 3
  val nastiXQosBits = 4
  val nastiXRegionBits = 4
  val nastiXRespBits = 2

  def bytesToXSize(bytes: UInt) = MuxLookup(
    bytes,
    "b111".U,
    Seq(
      1.U -> 0.U,
      2.U -> 1.U,
      4.U -> 2.U,
      8.U -> 3.U,
      16.U -> 4.U,
      32.U -> 5.U,
      64.U -> 6.U,
      128.U -> 7.U
    )
  )
}

abstract class NastiModule(implicit val p: Parameters) extends Module with HasNastiParameters
abstract class NastiBundle(implicit val p: Parameters) extends Bundle with HasNastiParameters

abstract class NastiChannel(implicit p: Parameters) extends NastiBundle()(p)
abstract class NastiMasterToSlaveChannel(implicit p: Parameters) extends NastiChannel()(p)
abstract class NastiSlaveToMasterChannel(implicit p: Parameters) extends NastiChannel()(p)

trait HasNastiMetadata extends HasNastiParameters {
  val addr = UInt(nastiXAddrBits.W)
  val len = UInt(nastiXLenBits.W)
  val size = UInt(nastiXSizeBits.W)
  val burst = UInt(nastiXBurstBits.W)
  val lock = Bool()
  val cache = UInt(nastiXCacheBits.W)
  val prot = UInt(nastiXProtBits.W)
  val qos = UInt(nastiXQosBits.W)
  val region = UInt(nastiXRegionBits.W)
}

trait HasNastiData extends HasNastiParameters {
  val data = UInt(nastiXDataBits.W)
  val last = Bool()
}

class NastiReadIO(implicit val p: Parameters) extends Bundle {
  val ar = Decoupled(new NastiReadAddressChannel)
  val r = Flipped(Decoupled(new NastiReadDataChannel))
}

class NastiWriteIO(implicit val p: Parameters) extends Bundle {
  val aw = Decoupled(new NastiWriteAddressChannel)
  val w = Decoupled(new NastiWriteDataChannel)
  val b = Flipped(Decoupled(new NastiWriteResponseChannel))
}

class NastiIO(implicit val p: Parameters) extends Bundle {
  val aw = Decoupled(new NastiWriteAddressChannel)
  val w = Decoupled(new NastiWriteDataChannel)
  val b = Flipped(Decoupled(new NastiWriteResponseChannel))
  val ar = Decoupled(new NastiReadAddressChannel)
  val r = Flipped(Decoupled(new NastiReadDataChannel))
}

class NastiAddressChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p) with HasNastiMetadata

class NastiResponseChannel(implicit p: Parameters) extends NastiSlaveToMasterChannel()(p) {
  val resp = UInt(nastiXRespBits.W)
}

class NastiWriteAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id = UInt(nastiWIdBits.W)
  val user = UInt(nastiAWUserBits.W)
}

class NastiWriteDataChannel(implicit p: Parameters) extends NastiMasterToSlaveChannel()(p) with HasNastiData {
  val id = UInt(nastiWIdBits.W)
  val strb = UInt(nastiWStrobeBits.W)
  val user = UInt(nastiWUserBits.W)
}

class NastiWriteResponseChannel(implicit p: Parameters) extends NastiResponseChannel()(p) {
  val id = UInt(nastiWIdBits.W)
  val user = UInt(nastiBUserBits.W)
}

class NastiReadAddressChannel(implicit p: Parameters) extends NastiAddressChannel()(p) {
  val id = UInt(nastiRIdBits.W)
  val user = UInt(nastiARUserBits.W)
}

class NastiReadDataChannel(implicit p: Parameters) extends NastiResponseChannel()(p) with HasNastiData {
  val id = UInt(nastiRIdBits.W)
  val user = UInt(nastiRUserBits.W)
}

object NastiConstants {
  def BURST_FIXED = "b00".U
  def BURST_INCR = "b01".U
  def BURST_WRAP = "b10".U

  def RESP_OKAY = "b00".U
  def RESP_EXOKAY = "b01".U
  def RESP_SLVERR = "b10".U
  def RESP_DECERR = "b11".U

  def CACHE_DEVICE_NOBUF = "b0000".U
  def CACHE_DEVICE_BUF = "b0001".U
  def CACHE_NORMAL_NOCACHE_NOBUF = "b0010".U
  def CACHE_NORMAL_NOCACHE_BUF = "b0011".U

  def AXPROT(instruction: Bool, nonsecure: Bool, privileged: Bool): UInt =
    Cat(instruction, nonsecure, privileged)

  def AXPROT(instruction: Boolean, nonsecure: Boolean, privileged: Boolean): UInt =
    AXPROT(instruction.B, nonsecure.B, privileged.B)
}

import NastiConstants._

object NastiWriteAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt, len: UInt = 0.U, burst: UInt = BURST_INCR)(implicit p: Parameters) = {
    val aw = Wire(new NastiWriteAddressChannel)
    aw.id := id
    aw.addr := addr
    aw.len := len
    aw.size := size
    aw.burst := burst
    aw.lock := false.B
    aw.cache := CACHE_DEVICE_NOBUF
    aw.prot := AXPROT(false, false, false)
    aw.qos := "b0000".U
    aw.region := "b0000".U
    aw.user := 0.U
    aw
  }
}

object NastiReadAddressChannel {
  def apply(id: UInt, addr: UInt, size: UInt, len: UInt = 0.U, burst: UInt = BURST_INCR)(implicit p: Parameters) = {
    val ar = Wire(new NastiReadAddressChannel)
    ar.id := id
    ar.addr := addr
    ar.len := len
    ar.size := size
    ar.burst := burst
    ar.lock := false.B
    ar.cache := CACHE_DEVICE_NOBUF
    ar.prot := AXPROT(false, false, false)
    ar.qos := 0.U
    ar.region := 0.U
    ar.user := 0.U
    ar
  }
}

object NastiWriteDataChannel {
  def apply(
    data: UInt,
    strb: Option[UInt] = None,
    last: Bool = true.B,
    id:   UInt = 0.U
  )(
    implicit p: Parameters
  ): NastiWriteDataChannel = {
    val w = Wire(new NastiWriteDataChannel)
    w.strb := strb.getOrElse(Fill(w.nastiWStrobeBits, 1.U))
    w.data := data
    w.last := last
    w.id := id
    w.user := 0.U
    w
  }
}

object NastiReadDataChannel {
  def apply(
    id:   UInt,
    data: UInt,
    last: Bool = true.B,
    resp: UInt = 0.U
  )(
    implicit p: Parameters
  ) = {
    val r = Wire(new NastiReadDataChannel)
    r.id := id
    r.data := data
    r.last := last
    r.resp := resp
    r.user := 0.U
    r
  }
}

object NastiWriteResponseChannel {
  def apply(id: UInt, resp: UInt = 0.U)(implicit p: Parameters) = {
    val b = Wire(new NastiWriteResponseChannel)
    b.id := id
    b.resp := resp
    b.user := 0.U
    b
  }
}

class NastiArbiterIO(arbN: Int)(implicit p: Parameters) extends Bundle {
  val master = Flipped(Vec(arbN, new NastiIO))
  val slave = new NastiIO
}

/** Arbitrate among arbN masters requesting to a single slave */
class NastiArbiter(val arbN: Int)(implicit p: Parameters) extends NastiModule {
  val io = new NastiArbiterIO(arbN)

  if (arbN > 1) {
    val arbIdBits = log2Up(arbN)

    val ar_arb = Module(new RRArbiter(new NastiReadAddressChannel, arbN))
    val aw_arb = Module(new RRArbiter(new NastiWriteAddressChannel, arbN))

    val slave_r_arb_id = io.slave.r.bits.id(arbIdBits - 1, 0)
    val slave_b_arb_id = io.slave.b.bits.id(arbIdBits - 1, 0)

    val w_chosen = Reg(UInt(arbIdBits.W))
    val w_done = RegInit(true.B)

    when(aw_arb.io.out.fire) {
      w_chosen := aw_arb.io.chosen
      w_done := false.B
    }

    when(io.slave.w.fire && io.slave.w.bits.last) {
      w_done := true.B
    }

    for (i <- 0 until arbN) {
      val m_ar = io.master(i).ar
      val m_aw = io.master(i).aw
      val m_r = io.master(i).r
      val m_b = io.master(i).b
      val a_ar = ar_arb.io.in(i)
      val a_aw = aw_arb.io.in(i)
      val m_w = io.master(i).w

      a_ar <> m_ar
      a_ar.bits.id := Cat(m_ar.bits.id, i.U(arbIdBits.W))

      a_aw <> m_aw
      a_aw.bits.id := Cat(m_aw.bits.id, i.U(arbIdBits.W))

      m_r.valid := io.slave.r.valid && slave_r_arb_id === i.U
      m_r.bits := io.slave.r.bits
      m_r.bits.id := io.slave.r.bits.id >> arbIdBits.U

      m_b.valid := io.slave.b.valid && slave_b_arb_id === i.U
      m_b.bits := io.slave.b.bits
      m_b.bits.id := io.slave.b.bits.id >> arbIdBits.U

      m_w.ready := io.slave.w.ready && w_chosen === i.U && !w_done
    }

    io.slave.r.ready := io.master(slave_r_arb_id).r.ready
    io.slave.b.ready := io.master(slave_b_arb_id).b.ready

    io.slave.w.bits := io.master(w_chosen).w.bits
    io.slave.w.valid := io.master(w_chosen).w.valid && !w_done

    io.slave.ar <> ar_arb.io.out

    io.slave.aw.bits <> aw_arb.io.out.bits
    io.slave.aw.valid := aw_arb.io.out.valid && w_done
    aw_arb.io.out.ready := io.slave.aw.ready && w_done

  } else { io.slave <> io.master.head }
}
