package mini

import Chisel._

class CacheReq extends CoreBundle {
  val addr = UInt(width=xlen)
  val data = UInt(width=xlen)
  val mask = UInt(width=4)
}

class CacheResp extends CoreBundle {
  val data = UInt(width=xlen)
}

class CacheIO extends Bundle {
  val req  = Valid(new CacheReq).flip
  val resp = Valid(new CacheResp)
}
