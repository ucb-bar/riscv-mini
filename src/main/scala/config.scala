package mini

import Chisel._
import junctions._

object Config {
  val params = Parameters.empty alter (
    (key, site, here, up) => key match {
      // Core
      case XLEN => 32
      case BuildALU    => () => Module(new ALUArea)
      case BuildImmGen => () => Module(new ImmGenWire)
      case BuildBrCond => () => Module(new BrCondArea)
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
