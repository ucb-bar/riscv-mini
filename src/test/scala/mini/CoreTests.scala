// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.experimental.{annotate, ChiselAnnotation}
import firrtl.annotations.MemoryArrayInitAnnotation
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CoreTester(core: => Core, benchmark: String, trace: Boolean = false) extends BasicTester {
  val memWords = 1 << 20
  val hexfile = os.pwd / "tests" / f"$benchmark.hex"
  val words: Seq[BigInt] = os.read.lines(hexfile)
    .filter(_.nonEmpty)
    .map(_.grouped(8).map(BigInt(_, 16))).flatMap(_.toSeq.reverse)
    .padTo(memWords, 0)

  val dut = Module(core)
  val xlen = dut.conf.xlen
  dut.io.host.fromhost.bits := DontCare
  dut.io.host.fromhost.valid := false.B

  val imem = Mem(memWords, UInt(xlen.W))
  annotate(new ChiselAnnotation {
    override def toFirrtl = MemoryArrayInitAnnotation(imem.toTarget, words)
  })
  val dmem = Mem(memWords, UInt(xlen.W))
  annotate(new ChiselAnnotation {
    override def toFirrtl = MemoryArrayInitAnnotation(dmem.toTarget, words)
  })

  val cycle = RegInit(0.U(32.W))
  val iaddr = dut.io.icache.req.bits.addr / (xlen / 8).U
  val daddr = dut.io.dcache.req.bits.addr / (xlen / 8).U
  val write = (0 until (xlen / 8)).foldLeft(0.U(xlen.W)) { (write, i) =>
    write |
      (Mux(
        (dut.io.dcache.req.valid && dut.io.dcache.req.bits.mask(i)).asBool,
        dut.io.dcache.req.bits.data,
        dmem(daddr)
      )(8 * (i + 1) - 1, 8 * i) << (8 * i).U).asUInt
  }
  dut.io.icache.resp.valid := !reset.asBool
  dut.io.dcache.resp.valid := !reset.asBool
  dut.io.icache.resp.bits.data := RegNext(imem(iaddr))
  dut.io.dcache.resp.bits.data := RegNext(dmem(daddr))

  when(dut.io.icache.req.valid) {
    if (trace) printf("INST[%x] => %x\n", iaddr * (xlen / 8).U, imem(iaddr))
  }
  when(dut.io.dcache.req.valid) {
    when(dut.io.dcache.req.bits.mask.orR) {
      dmem(daddr) := write
      if (trace) printf("MEM[%x] <= %x\n", daddr * (xlen / 8).U, write)
    }.otherwise {
      if (trace) printf("MEM[%x] => %x\n", daddr * (xlen / 8).U, dmem(daddr))
    }
  }
  cycle := cycle + 1.U
  when(dut.io.host.tohost =/= 0.U) {
    printf("cycles: %d\n", cycle)
    assert((dut.io.host.tohost >> 1.U).asUInt === 0.U, "* tohost: %d *\n", dut.io.host.tohost)
    stop()
  }
}

object DefaultCoreConfig {
  def apply() = MiniConfig().core
}

class CoreSimpleTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Core")
  it should "execute a simple test" in {
    test(new CoreTester(new Core(DefaultCoreConfig()), "rv32ui-p-simple")).runUntilStop(15000)
  }
}

abstract class CoreTests(cfg: TestConfig, useVerilator: Boolean = false)
    extends AnyFlatSpec
    with ChiselScalatestTester {
  behavior.of("Core")
  val opts = if (useVerilator) Seq(VerilatorBackendAnnotation) else Seq()
  cfg.tests.foreach { name =>
    it should s"execute $name" taggedAs IntegrationTest in {
      test(new CoreTester(new Core(DefaultCoreConfig()), name)).withAnnotations(opts).runUntilStop(cfg.maxcycles)
    }
  }
}

class CoreISATests extends CoreTests(ISATests)
class CoreBmarkTests extends CoreTests(BmarkTests, true)
