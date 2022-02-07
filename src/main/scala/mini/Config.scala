// See LICENSE for license details.

package mini

import junctions.NastiBundleParameters

case class Config(core: CoreConfig, cache: CacheConfig, nasti: NastiBundleParameters)

object MiniConfig {
  def apply(): Config = Config(
    core = CoreConfig(
      xlen = 32,
      makeAlu = new AluArea(_),
      makeBrCond = new BrCondArea(_),
      makeImmGen = new ImmGenWire(_)
    ),
    cache = CacheConfig(
      nWays = 1,
      nSets = 256,
      blockBytes = 4 * 32
    ),
    nasti = NastiBundleParameters(
      addrBits = 32,
      dataBits = 64,
      idBits = 5
    )
  )
}
