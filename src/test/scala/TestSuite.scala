package mini

import Chisel._
import Chisel.swtesters.{chiselMain, chiselMainTest, ClassicTester}
import java.io.{File, PrintStream}
import sys.process.stringSeqToProcess
import scala.reflect.ClassTag

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
  protected val outDir = new File("test-outs")
  protected val logDir = new File("test-logs")
  protected val dumpDir = new File("test-dumps")
  protected val baseArgs = Array(
    "--targetDir", outDir.getCanonicalPath.toString, "--genHarness", "--compile")
  implicit val p = cde.Parameters.root((new MiniConfig).toInstance)

  def elaborate[T <: Module : ClassTag](c: => T, debug: Boolean=false): T = {
    val modName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    chiselMain(baseArgs ++ Array("--testCommand", s"${outDir}/V${modName}"), () => c)
  }

  def runUnitTests[M <: Module : ClassTag](c: => M)(tester: M => ClassicTester[M]) {
    val modName = implicitly[ClassTag[M]].runtimeClass.getSimpleName
    behavior of modName
    it should "pass verilator" in {
      chiselMainTest(baseArgs, () => c)(tester)
    }
  }

  def runTester[M <: Module : ClassTag](c: M, t: TestType) {
    val (dir, tests, maxcycles) = t match {
      case ISATests   => (new java.io.File("riscv-tests/isa"), isaTests, 15000L)
      case BmarkTests => (new java.io.File("riscv-bmarks"), bmarkTests, 500000L)
    }
    val modName = implicitly[ClassTag[M]].runtimeClass.getSimpleName
    behavior of modName
    assert(dir.exists)
    tests map { test =>
      val loadmem = s"${dir}/${test}.hex"
      val args = new MiniTestArgs(loadmem, maxcycles)
      if (!(new File(loadmem).exists)) {
        assert(Seq("make", "-C", dir.getPath.toString, s"${test}.hex", 
                   """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""").! == 0)
      }
      println(s"runs ${test} of ${modName}")
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
  runUnitTests(new ALUSimple)(m => new ALUTests(m))
  runUnitTests(new ALUArea)(m => new ALUTests(m))
  runUnitTests(new BrCondSimple)(m => new BrCondTests(m))
  runUnitTests(new ImmGenWire)(m => new ImmGenTests(m))
  runUnitTests(new ImmGenMux)(m => new ImmGenTests(m))
  runUnitTests(new Control)(m => new ControlTests(m))
  // runUnitTests(new CSR)(m => new CSRTests(m))
  // runUnitTests(new Datapath)(m => new DatapathTests(m))
  runUnitTests(new Cache)(m => new CacheTests(m))
  // runUnitTests(new Core)(m => new CoreSimpleTests(m))
}

class ISATestSuite extends MiniTestSuite {
  val t = ISATests
  runTester(elaborate(new Core), t)
  runTester(elaborate(new Tile), t)
}

class BmarkTestSuite extends MiniTestSuite {
  val t = BmarkTests
  runTester(elaborate(new Core), t)
  runTester(elaborate(new Tile), t)
}
