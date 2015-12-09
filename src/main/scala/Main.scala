package mini

import Chisel._
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) {
    val mod = args(1)
    val dir = args(2)
    val compileArgs = Array(
      "--backend", args(3), "--targetDir", dir,
      "--minimumCompatibility", "3.0", 
      "--genHarness", "--compile", "--compileInitializationUnoptimized", 
      "--vcd", "--vcdMem", "--debug")
    val runArgs = Array("--backend", "null", "--test", "--vcd", "--vcdMem", "--debug",
      "--testCommand", s"${dir}/${mod}", s"+vpdfile=${dir}/${args(3)}.vpd", "+vpdmem")
    val testerArgs = args drop 4
    implicit val p = cde.Parameters.root((new MiniConfig).toInstance)
    args(0) match {
      case "compile" => mod match {
        case "Core" =>
          chiselMain(compileArgs, () => Module(new Core))
        case "Tile" =>
          chiselMain(compileArgs, () => Module(new Tile))
        case _ =>
      }
      case "test" => mod match {
        case "Core" =>
          chiselMainTest(runArgs, () => Module(new Core))(c => new CoreTester(c, testerArgs))
        case "Tile" =>
          chiselMainTest(runArgs, () => Module(new Tile))(c => new TileTester(c, testerArgs))
        case _ =>
      }
    }
  }
}
