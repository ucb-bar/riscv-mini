package mini

import Chisel._
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) = {
    val (chiselArgs, testArgs) = args.tail partition (_.head != '+')
    val res = args(0) match {
      case "ALUTop" => chiselMainTest(chiselArgs, () => Module(new ALUTop)(Config.params)){
        c => new ALUTests(c) }
      case "Control" => chiselMainTest(chiselArgs, () => Module(new Control)(Config.params)) {
        c => new ControlTests(c) }
      case "Datapath" => chiselMainTest(chiselArgs, () => Module(new Datapath)(Config.params)) {
        c => new DatapathTests(c) }
      case "Core" => chiselMainTest(chiselArgs, () => Module(new Core)(Config.params)) {
        c => new CoreSimpleTests(c) }
      case "CoreHex" => chiselMainTest(chiselArgs, () => Module(new Core)(Config.params)) { 
        c => new CoreTests(c, testArgs) }
      case "Tile" => chiselMainTest(chiselArgs, () => Module(new Tile)(Config.params)) {
        c => new TileTests(c, testArgs) }
      case _ =>
    }
  }
}
