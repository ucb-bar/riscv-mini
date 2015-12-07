package mini

import Chisel._
import cde.Parameters
import junctions._

object Config extends cde.Config {
  val params = Parameters.empty alter (
    (key, site, here, up) => key match {
      // Core
      case XLEN => 32
      case BuildALU    => (p: Parameters) => Module(new ALUArea()(p))
      case BuildImmGen => (p: Parameters) => Module(new ImmGenWire()(p))
      case BuildBrCond => (p: Parameters) => Module(new BrCondArea()(p))
      // Cache
      case NWays => 1 // TODO: set-associative
      case NSets => 256 
      case CacheBlockBytes => 4 * (here(XLEN) >> 3) // 4 x 32 bits = 16B
      // MemIO(memserdes.scala in junctions)
      case MIFAddrBits  => here(XLEN)
      case MIFDataBits  => here(CacheBlockBytes) << 3
      case MIFTagBits   => 5
      case MIFDataBeats => 0
    }
  )
}
