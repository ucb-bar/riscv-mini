package mini

import chisel3.Module
import chisel3.iotesters.{chiselMain, chiselMainTest, PeekPokeTester, Driver}
import java.io.{File, PrintStream}
import sys.process.stringSeqToProcess
import scala.reflect.ClassTag
import scala.concurrent.{Future, Await, ExecutionContext}

object TestParams {
  implicit def p = cde.Parameters.root((new MiniConfig).toInstance)
}
import TestParams.p

abstract class UnitTest[+M <: Module : ClassTag](c: => M)(tester: M => PeekPokeTester[M])
    extends org.scalatest.FlatSpec {
  val outDir = new File("test-outs") ; outDir.mkdirs
  def baseArgs(dir: File) = Array(
    "--targetDir", dir.getCanonicalPath.toString,
    "--v", "--genHarness", "--compile", "--test")

  val modName = implicitly[ClassTag[M]].runtimeClass.getSimpleName
  val dir = new File(s"$outDir/$modName")
  behavior of modName
  it should "pass verilator" in {
    val args = baseArgs(dir) ++ Array("--logFile", s"$dir/$modName-verilator.log")
    chiselMainTest(args, () => c)(tester)
  }
  it should "pass vcs" in {
    val args = baseArgs(dir) ++ Array("--vcs", "--logFile", s"$dir/$modName-vcs.log")
    chiselMainTest(args, () => c)(tester)
  }
}

class ALUSimpleUnitTest extends UnitTest(new ALUSimple()(p))(m => new ALUTests(m))
class ALUAreaUnitTest extends UnitTest(new ALUArea()(p))(m => new ALUTests(m))
class BrCondSimpleUnitTest extends UnitTest(new BrCondSimple()(p))(m => new BrCondTests(m))
class ImmGenWireUnitTest extends UnitTest(new ImmGenWire()(p))(m => new ImmGenTests(m)) 
class ImmGenMuxUnitTest extends UnitTest(new ImmGenMux()(p))(m => new ImmGenTests(m))
class ControlUnitTest extends UnitTest(new Control()(p))(m => new ControlTests(m))
class CSRUnitTest extends UnitTest(new CSR()(p))(m => new CSRTests(m))
class DatapathUnitTest extends UnitTest(new Datapath()(p))(m => new DatapathTests(m))
class CacheUnitTest extends UnitTest(new Cache()(p))(m => new CacheTests(m))
class CoreUnitTest extends UnitTest(new Core()(p))(m => new CoreSimpleTests(m))

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
  def baseArgs(dir: File, vcs: Boolean = false) = Array(
    "--targetDir", dir.getCanonicalPath.toString,
    "--v", "--genHarness", "--compile", "--test", "--noUpdate") ++
    (if (vcs) Array("--vcs") else Nil)
}

abstract class MiniTestSuite[+T <: Module : ClassTag](
    dutGen: => T, vcs: Boolean, N: Int = 10) extends org.scalatest.FlatSpec with RiscVTests {
  val backend = if (vcs) "vcs" else "verilator"
  val dutName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  val args = baseArgs(new File(s"$outDir/$dutName"), vcs)
  val dut = chiselMain(args, () => dutGen)
  behavior of s"$dutName in $backend"

  def runTests(testType: TestType) = {
    val (dir, tests, maxcycles) = testType match {
      case ISATests   => (new File("riscv-tests/isa"), isaTests, 15000L)
      case BmarkTests => (new File("riscv-bmarks"), bmarkTests, 1500000L)
    }
    assert(dir.exists)
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val results = tests.zipWithIndex sliding (N, N) map { subtests => Future {
      val subresults = subtests map {case (t, i) =>
        val loadmem = s"$dir/$t.hex"
        val logFile = Some(s"$outDir/$dutName/$t-$backend.log")
        val waveform = Some(s"$outDir/$dutName/$t.%s".format(if (vcs) "vpd" else "vcd"))
        val testCmd = List(s"$outDir/$dutName/%s$dutName".format(if (vcs) "" else "V"))
        val args = new MiniTestArgs(loadmem, maxcycles, logFile, waveform, testCmd, false)// latency)
        if (!(new File(loadmem).exists)) {
          assert(Seq("make", "-C", dir.getPath.toString, s"$t.hex", 
                     """'RISCV_GCC=$(RISCV_PREFIX)gcc -m32'""").! == 0)
        }
        Future { t -> (dut match {
          case _: Core => Driver.run(() => dutGen.asInstanceOf[Core])(m => new CoreTester(m, args))
          case _: Tile => Driver.run(() => dutGen.asInstanceOf[Tile])(m => new TileTester(m, args))
        })}
      } 
      Await.result(Future.sequence(subresults), Duration.Inf)
    } }
    Await.result(Future.sequence(results), Duration.Inf).flatten foreach { case (name, pass) =>
      it should s"pass $name" in { assert(pass) }
    }
  }
  // runTests(ISATests)
  runTests(BmarkTests)
}

class CoreVeriTests extends MiniTestSuite(new Core()(p), false)
class CoreVCSTests extends MiniTestSuite(new Core()(p), true)

class TileVeriTests extends MiniTestSuite(new Tile(p), false)
class TileVCSTests extends MiniTestSuite(new Tile(p), true)
