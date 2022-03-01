// See LICENSE for license details.

package mini

import junctions.NastiBundleParameters
import chisel3._

case class Config(core: CoreConfig, cache: CacheConfig, nasti: NastiBundleParameters)

object MiniConfig {
  def apply(): Config = {
    val xlen = 32
    Config(
      core = CoreConfig(
        xlen = xlen,
        makeAlu = new AluArea(_),
        makeBrCond = new BrCondArea(_),
        makeImmGen = new ImmGenWire(_)
      ),
      cache = CacheConfig(
        nWays = 1,
        nSets = 256,
        bytesPerBlock = 4 * (xlen / 8), // 4 * 32 bits = 16B
        cacheable = (addr: UInt) => addr >= 0x80000000.U(xlen.W)
      ),
      nasti = NastiBundleParameters(
        addrBits = 32,
        dataBits = 64,
        idBits = 5
      )
    )
  }
}
