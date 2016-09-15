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
  Seq("verilator", "vcs") foreach {backend =>
    it should s"pass $backend" in {
      val args = baseArgs(dir) ++ Array(
        "--backend", backend,
        "--logFile", (new File(dir, s"$modName-$backend.log")).toString)
      chiselMainTest(args, () => c)(tester)
    }
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

trait TestType {
  def tests: List[String]
  def maxcycles: Long
}
case object SimpleTests extends TestType {
  val tests = List("rv32ui-p-simple")
  val maxcycles = 15000L
}
case object ISATests extends TestType {
  val tests = (List("simple", "add", "addi", "auipc", "and", "andi", // TODO: "fence_i",
    "sb", "sh", "sw", "lb", "lbu", "lh", "lhu", "lui", "lw",
    "beq", "bge", "bgeu", "blt", "bltu", "bne", "j", "jal", "jalr",
    "or", "ori", "sll", "slli", "slt", "slti", "sra", "srai", "sub", "xor", "xori"
  ) map (t => s"rv32ui-p-${t}")) ++ (List(
    "sbreak", "scall", "illegal", "ma_fetch", "ma_addr", "csr" //, TODO: "timer"
  ) map (t => s"rv32mi-p-${t}"))
  val maxcycles = 15000L
}
case object BmarkTests extends TestType {
  val tests = List(
    "median.riscv", "multiply.riscv", "qsort.riscv", "towers.riscv", "vvadd.riscv"
  )
  val maxcycles = 1500000L
}

abstract class MiniTestSuite[+T <: Module : ClassTag](
    dutGen: => T,
    backend: String,
    testType: TestType,
    N: Int = 5) extends org.scalatest.FlatSpec {
  val dutName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  val baseDir = new File(s"test-outs/$dutName") ; baseDir.mkdirs
  val dir = new File(baseDir, testType.toString) ; dir.mkdirs
  val args = Array(
    "--targetDir", dir.toString, "--backend", backend,
    "--v", "--genHarness", "--compile", "--test")
  val dut = chiselMain(args, () => dutGen)
  val vcs = backend == "vcs"

  behavior of s"$dutName in $backend"
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  val results = testType.tests.zipWithIndex sliding (N, N) map { subtests =>
    val subresults = subtests map {case (t, i) =>
      val loadmem = getClass.getResourceAsStream(s"/$t.hex")
      val logFile = Some(new File(dir, s"$t-$backend.log"))
      val waveform = Some(new File(dir, s"$t.%s".format(if (vcs) "vpd" else "vcd")))
      val testCmd = new File(dir, s"%s$dutName".format(if (vcs) "" else "V"))
      val args = new MiniTestArgs(loadmem, logFile, false, testType.maxcycles, 16) // latency)
      Future(t -> (dut match {
        case _: Core => Driver.run(
          () => dutGen.asInstanceOf[Core], testCmd, waveform)(m => new CoreTester(m, args))
        case _: Tile => Driver.run(
          () => dutGen.asInstanceOf[Tile], testCmd, waveform)(m => new TileTester(m, args))
      }))
    }
    Await.result(Future.sequence(subresults), Duration.Inf)
  }
  results.flatten foreach { case (name, pass) => it should s"pass $name" in { assert(pass) } }
}

class CoreCppISATests extends MiniTestSuite(new Core()(p), "verilator", ISATests)
class CoreCppBmarkTests extends MiniTestSuite(new Core()(p), "verilator", BmarkTests)
class CoreVCSISATests extends MiniTestSuite(new Core()(p), "vcs", ISATests)
class CoreVCSBmarkTests extends MiniTestSuite(new Core()(p), "vcs", BmarkTests)

class TileCppISATests extends MiniTestSuite(new Tile(p), "verilator", ISATests)
class TileCppBmarkTests extends MiniTestSuite(new Tile(p), "verilator", BmarkTests)
class TileVCSISATests extends MiniTestSuite(new Tile(p), "vcs", ISATests)
class TileVCSBmarkTests extends MiniTestSuite(new Tile(p), "vcs", BmarkTests)
