package mini

import Chisel._
import java.io.{File, PrintStream}
import sys.process.stringSeqToProcess

abstract class MiniTestSuite extends org.scalatest.FlatSpec {
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
    behavior of (s"${c.getClass.getName}")
    it should "pass Chisel Emulator" in {
      val log = new PrintStream(s"${logDir.getPath}/${c.getClass.getName}-cpp.log")
      checkUnitTests(elaborateCpp(c, true), Some(log))
    }
    it should "pass Verilog Simulator" in {
      val log = new PrintStream(s"${logDir.getPath}/${c.getClass.getName}-v.log")
      checkUnitTests(elaborateVerilog(c, true), Some(log))
    }
  }

  def runTester[T <: Module](c: T, dir: File, tests: List[String], maxcycles: Long = 500000) {
    val simulator = Driver.backend match {
      case _: VerilogBackend => "Verilog Simulation"
      case _: CppBackend     => "Chisel Emulator"
      case _                 => "???"
    }
    val postfix = if (Driver.isVCD) "for Debugging" else ""
    behavior of s"${c.getClass.getName} with ${simulator} ${postfix}"
    assert(dir.exists)
    val dirPath = dir.getPath.toString
    tests map { test =>
      val loadmem = s"${dir.getPath}/${test}.hex"
      val postfix = Driver.backend match {case _: VerilogBackend => "v" case _: CppBackend => "cpp"}
      val log = new PrintStream(s"${logDir.getPath}/${c.getClass.getName}-${test}-${postfix}.log")
      val vcd = s"${dumpDir}/${c.getClass.getName}-${test}-${postfix}.vcd"
      val vpd = s"${dumpDir}/${c.getClass.getName}-${test}-${postfix}.vpd"
      val dump = Driver.backend match {
        case _: VerilogBackend => Some(vpd)
        case _: CppBackend     => Some(vcd)
        case _                 => None
      }
      val args = new MiniTestArgs(loadmem, maxcycles, log=Some(log), dumpFile=dump) 
      if (!(new File(loadmem).exists)) {
        assert(Seq("make", "-C", dir.getPath.toString, s"{test}.hex", 
                   """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""").! == 0)
      }
      println(s"runs ${test} on ${simulator} of ${c.getClass.getName}")
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
  val dir = new File("riscv-tests/isa")
  runTester(elaborateCpp(new Core), dir, isaTests, 15000)
  runTester(elaborateCpp(new Tile), dir, isaTests, 15000)
  runTester(elaborateVerilog(new Core), dir, isaTests, 15000)
  runTester(elaborateVerilog(new Tile), dir, isaTests, 15000)
}

class ISADebugTestSuite extends MiniTestSuite {
  val dir = new File("riscv-tests/isa")
  runTester(elaborateCpp(new Core, true), dir, isaTests, 15000)
  runTester(elaborateCpp(new Tile, true), dir, isaTests, 15000)
  runTester(elaborateVerilog(new Core, true), dir, isaTests, 15000)
  runTester(elaborateVerilog(new Tile, true), dir, isaTests, 15000)
}

class BmarkTestSuite extends MiniTestSuite {
  val dir = new File("riscv-bmarks")
  runTester(elaborateCpp(new Core), dir, bmarkTests)
  runTester(elaborateCpp(new Tile), dir, bmarkTests)
  runTester(elaborateVerilog(new Core), dir, bmarkTests)
  runTester(elaborateVerilog(new Tile), dir, bmarkTests)
}

class BmarkDebugTestSuite extends MiniTestSuite {
  val dir = new File("riscv-bmarks")
  runTester(elaborateCpp(new Core, true), dir, bmarkTests)
  runTester(elaborateCpp(new Tile, true), dir, bmarkTests)
  runTester(elaborateVerilog(new Core, true), dir, bmarkTests)
  runTester(elaborateVerilog(new Tile, true), dir, bmarkTests)
}
