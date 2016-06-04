package mini

import Chisel._
import Chisel.iotesters.{chiselMain, chiselMainTest, PeekPokeTester}
import java.io.{File, PrintStream}
import sys.process.stringSeqToProcess
import scala.reflect.ClassTag
import org.scalatest.{FlatSpec, BeforeAndAfterAll, ParallelTestExecution}

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

  val outDir = new File("test-outs") ; outDir.mkdirs
  def baseArgs(dir: File) = Array(
    "--targetDir", dir.getCanonicalPath.toString,
    "--v", "--genHarness", "--compile", "--test")
  implicit val p = cde.Parameters.root((new MiniConfig).toInstance)
}

class UnitTestSuite extends org.scalatest.FlatSpec with RiscVTests {
  def runUnitTests[M <: Module : ClassTag](c: => M)(tester: M => PeekPokeTester[M]) {
    val modName = implicitly[ClassTag[M]].runtimeClass.getSimpleName
    val dir = new File(s"$outDir/$modName")
    behavior of modName
    it should "pass verilator" in {
      val args = baseArgs(dir) ++ Array(
        "--logFile", s"$dir/$modName-verilator.log")
      chiselMainTest(args, () => c)(tester)
    }
  }
  runUnitTests(new ALUSimple)(m => new ALUTests(m))
  runUnitTests(new ALUArea)(m => new ALUTests(m))
  runUnitTests(new BrCondSimple)(m => new BrCondTests(m))
  runUnitTests(new ImmGenWire)(m => new ImmGenTests(m))
  runUnitTests(new ImmGenMux)(m => new ImmGenTests(m))
  runUnitTests(new Control)(m => new ControlTests(m))
  runUnitTests(new CSR)(m => new CSRTests(m))
  runUnitTests(new Datapath)(m => new DatapathTests(m))
  runUnitTests(new Cache)(m => new CacheTests(m))
  runUnitTests(new Core)(m => new CoreSimpleTests(m))
}

abstract class MiniTestSuite extends org.scalatest.FlatSpec with RiscVTests { 
  def runTester[T <: Module : ClassTag](mod: => T, t: TestType, latency: Int = 5) {
    val dutName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    val args = baseArgs(new File(s"$outDir/$dutName")) ++ Array("--noUpdate")
    val dut = chiselMain(args, () => mod)
    val (dir, tests, maxcycles) = t match {
      case ISATests   => (new File("riscv-tests/isa"), isaTests, 15000L)
      case BmarkTests => (new File("riscv-bmarks"), bmarkTests, 1500000L)
    }
    assert(dir.exists)
    behavior of dutName
    tests foreach { t => it should s"pass $t" in {
      val loadmem = s"$dir/$t.hex"
      val logFile = Some(s"$outDir/$dutName/$t-verilator.log")
      val waveform = Some(s"$outDir/$dutName/$t.vcd")
      val args = new MiniTestArgs(loadmem, maxcycles, logFile=logFile, waveform=waveform, memlatency=latency)
      if (!(new File(loadmem).exists)) {
        assert(Seq("make", "-C", dir.getPath.toString, s"$t.hex", 
                   """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""").! == 0)
      }
      assert(dut match {
        case core: Core => (new CoreTester(core, args)).finish
        case tile: Tile => (new TileTester(tile, args)).finish
      })
    }}
  }
}

class CoreISATestSuite extends MiniTestSuite {
  runTester(new Core, ISATests)
}

class TileISATestSuite extends MiniTestSuite {
  runTester(new Tile(p), ISATests)
}

class CoreBmarkTestSuite extends MiniTestSuite {
  runTester(new Core, BmarkTests)
}

class TileBmarkTestSuite extends MiniTestSuite {
  runTester(new Tile(p), BmarkTests)
}
