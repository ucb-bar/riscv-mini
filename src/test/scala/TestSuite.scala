package mini

import Chisel._
import java.io.{File, PrintStream}
import sys.process.stringSeqToProcess

trait RiscVTests {
  trait TestType
  case object ISATests extends TestType
  case object BmarkTests extends TestType

  val isaTests = (List("simple", "add", "addi", "auipc", "and", "andi", // TODO: "fence_i",
    "sb", "sh", "sw", "lb", "lbu", "lh", "lhu", "lui", "lw",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "j", "jal", "jalr",
    "or", "ori", "sll", "slli", "slt", "slti", "sra", "srai", "sub", "xor", "xori"
  ) map (t => s"rv32ui-p-${t}")) ++ (List(
    "sbreak", "scall", "illegal", "ma_fetch", "ma_addr", "csr" //, TODO: "timer"
  ) map (t => s"rv32mi-p-${t}"))

  val bmarkTests = List(
    "median.riscv", "multiply.riscv", "qsort.riscv", "towers.riscv", "vvadd.riscv"
  )
}

abstract class MiniTestSuite extends org.scalatest.FlatSpec with RiscVTests {
  private val outDir = new File("test-outs")
  private val logDir = new File("test-logs")
  private val dumpDir = new File("test-dumps")
  private val baseArgs = Array(
    "--targetDir", outDir.getPath.toString,
    "--minimumCompatibility", "3.0", "--genHarness", 
    "--compile", "--compileInitializationUnoptimized")
  private def testArgs(b: String) = baseArgs ++ Array("--backend", b)
  private def debugArgs(b: String) = testArgs(b) ++ Array("--debug", "--vcd", "--vcdMem")
  implicit val p = cde.Parameters.root((new MiniConfig).toInstance)

  def elaborate[T <: Module](c: => T, b: String, debug: Boolean=false): T = {
    val args = if (debug) debugArgs(b) else testArgs(b)
    chiselMain(args, () => Module(c))
  }

  def elaborateCpp[T <: Module](c: => T, debug: Boolean=false) = elaborate(c, "c", debug)

  def elaborateVerilog[T <: Module](c: => T, debug: Boolean=false) = elaborate(c, "v", debug)

  private def checkUnitTests[T <: Module](c: => T, log: Option[PrintStream] = None) {
    assert(c match {
      case m: ALU      => (new ALUTests(m, log)).finish
      case m: BrCond   => (new BrCondTests(m, log)).finish
      case m: ImmGen   => (new ImmGenTests(m, log)).finish
      case m: Control  => (new ControlTests(m, log)).finish
      case m: CSR      => (new CSRTests(m, log)).finish
      case m: Datapath => (new DatapathTests(m, log)).finish
      case m: Cache    => (new CacheTests(m, log)).finish
      case m: Core     => (new CoreSimpleTests(m, log)).finish
      case _ => false
    })
  }

  def runUnitTests[M <: Module](c: => M) {
    val modName = c.getClass.getSimpleName
    behavior of (s"${c.getClass.getName}")
    it should "pass Chisel Emulator" in {
      val log = new PrintStream(s"${logDir.getPath}/${modName}-cpp.log")
      checkUnitTests(elaborateCpp(c, true), Some(log))
    }
    it should "pass Verilog Simulator" in {
      val log = new PrintStream(s"${logDir.getPath}/${modName}-v.log")
      checkUnitTests(elaborateVerilog(c, true), Some(log))
    }
  }

  def runTester[T <: Module](c: T, t: TestType) {
    val (dir, tests, maxcycles) = t match {
      case ISATests   => (new java.io.File("riscv-tests/isa"), isaTests, 15000L)
      case BmarkTests => (new java.io.File("riscv-bmarks"), bmarkTests, 500000L)
    }
    val simulator = Driver.backend match {
      case _: VerilogBackend => "Verilog Simulation"
      case _: CppBackend     => "Chisel Emulator"
      case _                 => "???"
    }
    val modName = c.getClass.getSimpleName
    behavior of s"${modName} on ${simulator}"
    assert(dir.exists)
    val dirPath = dir.getPath.toString
    tests map { test =>
      val loadmem = s"${dir.getPath}/${test}.hex"
      val (log, dump) = Driver.backend match {
        case _: CppBackend     => (
          Some(s"${logDir.getPath}/${modName}-${test}-cpp.log"),
          Some(s"${dumpDir.getPath}/${modName}-${test}-cpp.vcd"))
        case _: VerilogBackend => (
          Some(s"${logDir.getPath}/${modName}-${test}-v.log"),
          Some(s"${dumpDir.getPath}/${modName}-${test}-v.vcd"))
      }
      val args = new MiniTestArgs(loadmem, maxcycles, dump, log)
      if (!(new File(loadmem).exists)) {
        assert(Seq("make", "-C", dir.getPath.toString, s"${test}.hex", 
                   """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""").! == 0)
      }
      println(s"runs ${test} on ${simulator} of ${modName}")
      test -> (c match {
        case core: Core => (new CoreTester(core, args)).finish
        case tile: Tile => (new TileTester(tile, args)).finish
      })
    } foreach {case (test, pass) =>
      it should s"pass ${test}" in { assert(pass) }
    }
  }

  if (!logDir.exists) logDir.mkdir
  if (!dumpDir.exists) dumpDir.mkdir
}

class UnitTestSuite extends MiniTestSuite {
  runUnitTests(new ALUSimple)
  runUnitTests(new ALUArea)
  runUnitTests(new BrCondSimple)
  runUnitTests(new BrCondArea)
  runUnitTests(new ImmGenWire)
  runUnitTests(new ImmGenMux)
  runUnitTests(new Control)
  runUnitTests(new CSR)
  runUnitTests(new Cache)
  runUnitTests(new Core)
}

class ISATestSuite extends MiniTestSuite {
  val t = ISATests
  runTester(elaborateCpp(new Core), t)
  runTester(elaborateCpp(new Tile), t)
  runTester(elaborateVerilog(new Core), t)
  runTester(elaborateVerilog(new Tile), t)
}

class ISADebugTestSuite extends MiniTestSuite {
  val t = ISATests
  // runTester(elaborateCpp(new Core, true), t)
  runTester(elaborateCpp(new Tile, true), t)
  // runTester(elaborateVerilog(new Core, true), t)
  runTester(elaborateVerilog(new Tile, true), t)
}

class BmarkTestSuite extends MiniTestSuite {
  val t = BmarkTests
  runTester(elaborateCpp(new Core), t)
  runTester(elaborateCpp(new Tile), t)
  runTester(elaborateVerilog(new Core), t)
  runTester(elaborateVerilog(new Tile), t)
}

class BmarkDebugTestSuite extends MiniTestSuite {
  val t = BmarkTests
  // runTester(elaborateCpp(new Core, true), t)
  runTester(elaborateCpp(new Tile, true), t)
  // runTester(elaborateVerilog(new Core, true), t)
  runTester(elaborateVerilog(new Tile, true), t)
}
