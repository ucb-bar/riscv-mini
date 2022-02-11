// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class CoreTester(core: => Core, benchmark: String, trace: Boolean = false) extends BasicTester {
  val filename = "tests/32/" + benchmark + ".hex" // we have 32 bits per memory entry

  val dut = Module(core)
  val xlen = dut.conf.xlen
  dut.io.host.fromhost.bits := DontCare
  dut.io.host.fromhost.valid := false.B

  val imem = Mem(1 << 20, UInt(xlen.W))
  loadMemoryFromFileInline(imem, filename)
  val dmem = Mem(1 << 20, UInt(xlen.W))
  loadMemoryFromFileInline(dmem, filename)

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
