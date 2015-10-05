package mini

import Chisel._
import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]) = {
    val (chiselArgs, testArgs) = args.tail partition (_.head != '+')
    val res = args(0) match {
      case "ALUSimple" => chiselMainTest(chiselArgs, () => Module(new ALUSimple)(Config.params)){
        c => new ALUTests(c) }
      case "ALUArea" => chiselMainTest(chiselArgs, () => Module(new ALUArea)(Config.params)){
        c => new ALUTests(c) }
      case "BrCondSimple" => chiselMainTest(chiselArgs, () => Module(new BrCondSimple)(Config.params)){
        c => new BrCondTests(c) }
      case "BrCondArea" => chiselMainTest(chiselArgs, () => Module(new BrCondArea)(Config.params)){
        c => new BrCondTests(c) }
      case "ImmGenWire" => chiselMainTest(chiselArgs, () => Module(new ImmGenWire)(Config.params)){
        c => new ImmGenTests(c) }
      case "ImmGenMux" => chiselMainTest(chiselArgs, () => Module(new ImmGenMux)(Config.params)){
        c => new ImmGenTests(c) }
      case "CSR" => chiselMainTest(chiselArgs, () => Module(new CSR)(Config.params)){
        c => new CSRTests(c) }
      case "Control" => chiselMainTest(chiselArgs, () => Module(new Control)(Config.params)) {
        c => new ControlTests(c) }
      case "Datapath" => chiselMainTest(chiselArgs, () => Module(new Datapath)(Config.params)) {
        c => new DatapathTests(c) }
      case "Cache" => chiselMainTest(chiselArgs, () => Module(new Cache)(Config.params)) {
        c => new CacheTests(c) }
      case "Core" => chiselMainTest(chiselArgs, () => Module(new Core)(Config.params)) {
        c => new CoreTester(c, testArgs) }
      case "Tile" => chiselMainTest(chiselArgs, () => Module(new Tile)(Config.params)) {
        c => new TileTester(c, testArgs) }
      case _ =>
    }
  }
}
