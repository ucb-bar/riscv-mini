package mini

import Chisel._
import org.scalatest.FunSuite
import scala.reflect.ClassTag

class MiniTestSuite extends FunSuite {
  private val dir = new java.io.File("test-outputs")
  private val baseArgs = Array(
    "--targetDir", dir.getPath.toString,
    "--minimumCompatibility", "3.0",
    "--genHarness", "--compile", "--test")
  private val debugArgs = Array("--debug", "--vcd", "--vcdMem")

  def launchTester[M <: Module : ClassTag, T <: Tester[M]](b: String, t: M => T, debug: Boolean=true) {
    val args = Array("--backend", b) ++ baseArgs ++ (if (debug) debugArgs else Array[String]())
    val ctor = implicitly[ClassTag[M]].runtimeClass.getConstructors.head
    chiselMainTest(args, () => Module(ctor.newInstance().asInstanceOf[M])(Config.params))(t)
  }

  def launchCppTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T, debug: Boolean=true) {
    launchTester("c", t, debug)
  }

  def launchVeriTester[M <: Module : ClassTag, T <: Tester[M]](t: M => T, debug: Boolean=true) {
    launchTester("v", t, debug)
  }

  test("ALUSimple Cpp Test") {
    launchCppTester((c: ALUSimple) => new ALUTests(c))
  }
  test("ALUArea Cpp Test") {
    launchCppTester((c: ALUArea) => new ALUTests(c))
  }
  test("BrCondSimple Cpp Test") {
    launchCppTester((c: BrCondSimple) => new BrCondTests(c))
  }
  test("BrCondArea Cpp Test") {
    launchCppTester((c: BrCondArea) => new BrCondTests(c))
  }
  test("ImmGenWire Cpp Test") {
    launchCppTester((c: ImmGenWire) => new ImmGenTests(c))
  }
  test("ImmGenMux Cpp Test") {
    launchCppTester((c: ImmGenMux) => new ImmGenTests(c))
  }
  test("Control Cpp Test") {
    launchCppTester((c: Control) => new ControlTests(c))
  }
  test("CSR Cpp Test") {
    launchCppTester((c: CSR) => new CSRTests(c))
  }
  test("Datapath Cpp Test") {
    launchCppTester((c: Datapath) => new DatapathTests(c))
  }
  test("Cache Cpp Test") {
    launchCppTester((c: Cache) => new CacheTests(c))
  }
  test("Core Simple Cpp Tests") {
    launchCppTester((c: Core) => new CoreSimpleTests(c))
  }


  test("ALUSimple Verilog Test") {
    launchVeriTester((c: ALUSimple) => new ALUTests(c))
  }
  test("ALUArea Verilog Test") {
    launchVeriTester((c: ALUArea) => new ALUTests(c))
  }
  test("BrCondSimple Verilog Test") {
    launchVeriTester((c: BrCondSimple) => new BrCondTests(c))
  }
  test("BrCondArea Verilog Test") {
    launchVeriTester((c: BrCondArea) => new BrCondTests(c))
  }
  test("ImmGenWire Verilog Test") {
    launchVeriTester((c: ImmGenWire) => new ImmGenTests(c))
  }
  test("ImmGenMux Verilog Test") {
    launchVeriTester((c: ImmGenMux) => new ImmGenTests(c))
  }
  test("Control Verilog Test") {
    launchVeriTester((c: Control) => new ControlTests(c))
  }
  test("CSR Verilog Test") {
    launchVeriTester((c: CSR) => new CSRTests(c))
  }
  test("Datapath Verilog Test") {
    launchVeriTester((c: Datapath) => new DatapathTests(c))
  }
  test("Cache Verilog Test") {
    launchVeriTester((c: Cache) => new CacheTests(c))
  } 
  test("Core Simple Verilog Tests") {
    launchVeriTester((c: Core) => new CoreSimpleTests(c))
  }

  test("Core ISA Cpp Tests") {
    launchCppTester((c: Core) => new CoreTester(c, Array("+isa=riscv-tests/isa", "+max-cycles=3000")), false)
  }
  test("Core Benchmark Cpp Tests") {
    launchCppTester((c: Core) => new CoreTester(c, Array("+bmarks=riscv-bmarks/", "+max-cycles=500000")), false)
  }
 
  test("Core ISA Verilog Tests") {
    launchVeriTester((c: Core) => new CoreTester(c, Array("+isa=riscv-tests/isa", "+max-cycles=3000")), false)
  }
  test("Core Benchmark Verilog Tests") {
    launchVeriTester((c: Core) => new CoreTester(c, Array("+bmarks=riscv-bmarks/", "+max-cycles=500000")), false)
  }

  test("Tile ISA Cpp Tests") {
    launchCppTester((c: Tile) => new TileTester(c, Array("+isa=riscv-tests/isa", "+max-cycles=15000")), false)
  }
  test("Tile Benchmark Cpp Tests") {
    launchCppTester((c: Tile) => new TileTester(c, Array("+bmarks=riscv-bmarks/", "+max-cycles=1500000")), false)
  }

  test("Tile ISA Verilog Tests") {
    launchVeriTester((c: Tile) => new TileTester(c, Array("+isa=riscv-tests/isa", "+max-cycles=15000")), false)
  }
  test("Tile Benchmark Verilog Tests") {
    launchVeriTester((c: Tile) => new TileTester(c, Array("+bmarks=riscv-bmarks/", "+max-cycles=1500000")), false)
  }
} 
