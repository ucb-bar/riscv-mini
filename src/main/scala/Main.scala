package mini

import Chisel._
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    // Compile for synthesis
    val params = cde.Parameters.root((new MiniConfig).toInstance)
    val circuit = Driver.elaborate(() => new Tile(params))
    val firrtl = new java.io.File(s"${args(0)}/${circuit.name}.fir")
    val dir = new java.io.File(args(0))
    if (!dir.exists) dir.mkdirs
    Driver.dumpFirrtl(circuit, Some(firrtl))
    Driver.firrtlToVerilog(circuit.name, dir).!
  }
}
