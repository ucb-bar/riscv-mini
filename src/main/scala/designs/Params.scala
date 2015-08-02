package mini

import Chisel._

case object InstLen extends Field[Int]
case object AddrLen extends Field[Int]
case object MemLen extends Field[Int]
case object TagLen extends Field[Int]

object Config {
  val params = Parameters.empty alter (
    (key, site, here, up) => key match {
      case InstLen => 32
      case AddrLen => here(InstLen)
      case MemLen => 32
      case TagLen => 5
    }
  )
}

abstract trait CoreParams extends UsesParameters {
  val instLen = params(InstLen)
  val addrLen = params(AddrLen)
}

abstract trait CoreBundle extends Bundle with CoreParams

abstract trait MemParams extends UsesParameters {
  val addrLen = params(AddrLen)
  val memLen = params(MemLen)
  val tagLen = params(TagLen)
}

abstract trait MemBundle extends Bundle with MemParams
