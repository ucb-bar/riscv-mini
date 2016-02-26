package mini

import Chisel._
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    // Compile for synthesis
    val compileArgs = Array(
      "--backend", "v", "--targetDir", args(0),
      "--minimumCompatibility", "3.0", "--noInlineMem",
      "--compile", "--compileInitializationUnoptimized") 
    implicit val p = cde.Parameters.root((new MiniConfig).toInstance)
    chiselMain(compileArgs, () => Module(new Tile))
  }
}
