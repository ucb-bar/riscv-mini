package mini

import Chisel._

case object XLEN extends Field[Int]
case object MemLen extends Field[Int]
case object TagLen extends Field[Int]
case object BuildImmGen extends Field[() => ImmGen]
case object BuildBrCond extends Field[() => BrCond]

object Config {
  val params = Parameters.empty alter (
    (key, site, here, up) => key match {
      case XLEN => 32
      case BuildImmGen => () => Module(new ImmGenWire)
      case BuildBrCond => () => Module(new BrCondArea)
      case MemLen => 32
      case TagLen => 5
    }
  )
}

abstract trait CoreParams extends UsesParameters {
  val xlen = params(XLEN)
}

abstract trait CoreBundle extends Bundle with CoreParams

abstract trait MemParams extends UsesParameters {
  val xlen = params(XLEN)
  val memLen = params(MemLen)
  val tagLen = params(TagLen)
}

abstract trait MemBundle extends Bundle with MemParams
