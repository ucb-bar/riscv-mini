package mini

import chisel3.Module
import chisel3.iotesters.{chiselMain, chiselMainTest, PeekPokeTester, Driver}
import java.io.{File, PrintStream}
import scala.sys.process.{BasicIO, stringSeqToProcess}
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.reflect.ClassTag

object TestParams {
  implicit def p = cde.Parameters.root((new MiniConfig).toInstance)
}
import TestParams.p

object checkBackend {
  def apply(b: String) = {
    val cmd = Seq("bash", "-c", "which %s".format(b))
    b == "firrtl" || (cmd run BasicIO(false, new StringBuffer, None)).exitValue == 0
  }
}

abstract class UnitTest[+M <: Module : ClassTag](c: => M)(tester: M => PeekPokeTester[M])
    extends org.scalatest.FlatSpec {
  val outDir = new File("test-outs") ; outDir.mkdirs
  def baseArgs(dir: File) = Array(
    "--targetDir", dir.getCanonicalPath.toString,
    "--v", "--genHarness", "--compile", "--test")

  val modName = implicitly[ClassTag[M]].runtimeClass.getSimpleName
  val dir = new File(s"$outDir/$modName")
  behavior of modName
  Seq("verilator", "vcs") foreach { backend =>
    if (checkBackend(backend)) {
      it should s"pass $backend" in {
        val args = baseArgs(dir) ++ Array(
          "--backend", backend,
          "--logFile", (new File(dir, s"$modName-$backend.log")).toString)
        chiselMainTest(args, () => c)(tester)
      }
    } else {
      ignore should s"pass $backend" in { assert(false) }
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

abstract class MiniTestSuite[+T <: Module : ClassTag](
    dutGen: => T,
    backend: String,
    testType: TestType,
    N: Int = 5,
    latency: Int = 8) extends org.scalatest.FlatSpec {
  val dutName = implicitly[ClassTag[T]].runtimeClass.getSimpleName
  val baseDir = new File(s"test-outs/$dutName") ; baseDir.mkdirs
  val dir = new File(baseDir, testType.toString) ; dir.mkdirs
  val args = Array(
    "--targetDir", dir.toString, "--backend", backend,
    "--v", "--genHarness", "--compile", "--test")

  behavior of s"$dutName in $backend"
  if (checkBackend(backend)) {
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    val dut = chiselMain(args, () => dutGen)
    val vcs = backend == "vcs"
    val results = testType.tests.zipWithIndex sliding (N, N) map { subtests =>
      val subresults = subtests map {case (t, i) =>
        val loadmem = getClass.getResourceAsStream(s"/$t.hex")
        val logFile = Some(new File(dir, s"$t-$backend.log"))
        val waveform = Some(new File(dir, s"$t.%s".format(if (vcs) "vpd" else "vcd")))
        val testCmd = new File(dir, s"%s$dutName".format(if (vcs) "" else "V"))
        val args = new MiniTestArgs(loadmem, logFile, false, testType.maxcycles, latency)
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
  } else {
    testType.tests foreach { name => ignore should s"pass $name" in { assert(false) } }
  }
}

class CoreCppISATests extends MiniTestSuite(new Core()(p), "verilator", ISATests)
class CoreCppBmarkTests extends MiniTestSuite(new Core()(p), "verilator", BmarkTests)
class CoreVCSISATests extends MiniTestSuite(new Core()(p), "vcs", ISATests)
class CoreVCSBmarkTests extends MiniTestSuite(new Core()(p), "vcs", BmarkTests)

class TileCppISATests extends MiniTestSuite(new Tile(p), "verilator", ISATests)
class TileCppBmarkTests extends MiniTestSuite(new Tile(p), "verilator", BmarkTests)
class TileVCSISATests extends MiniTestSuite(new Tile(p), "vcs", ISATests)
class TileVCSBmarkTests extends MiniTestSuite(new Tile(p), "vcs", BmarkTests)
