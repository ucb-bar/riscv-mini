// See LICENSE for license details.

package mini

import chisel3._
import chisel3.testers._
import chisel3.util.experimental.loadMemoryFromFileInline
import chiseltest._
import mini.TestParams.p
import org.scalatest.flatspec.AnyFlatSpec

class CoreTester(core: => Core, benchmark: String)
                (implicit p: freechips.rocketchip.config.Parameters) extends BasicTester {
  val filename = "tests/32/" + benchmark // we have 32 bits per memory entry

  val xlen = p(XLEN)
  val dut = Module(core)
  dut.io.host.fromhost.bits := DontCare
  dut.io.host.fromhost.valid := false.B

  val imem = Mem(1 << 20, UInt(xlen.W))
  loadMemoryFromFileInline(imem, filename)
  val dmem = Mem(1 << 20, UInt(xlen.W))
  loadMemoryFromFileInline(dmem, filename)

  val cycle = RegInit(0.U(32.W))
  val iaddr = dut.io.icache.req.bits.addr / (xlen / 8).U
  val daddr = dut.io.dcache.req.bits.addr / (xlen / 8).U
  val write = ((0 until (xlen / 8)) foldLeft 0.U(xlen.W)) { (write, i) =>
    write |
      ((Mux((dut.io.dcache.req.valid && dut.io.dcache.req.bits.mask(i)).asBool,
        dut.io.dcache.req.bits.data, dmem(daddr))(8 * (i + 1) - 1, 8 * i)) << (8 * i).U).asUInt
  }
  dut.io.icache.resp.valid := !reset.asBool
  dut.io.dcache.resp.valid := !reset.asBool
  dut.io.icache.resp.bits.data := RegNext(imem(iaddr))
  dut.io.dcache.resp.bits.data := RegNext(dmem(daddr))

  when(dut.io.icache.req.valid) {
    if (p(Trace)) printf("INST[%x] => %x\n", iaddr * (xlen / 8).U, imem(iaddr))
  }
  when(dut.io.dcache.req.valid) {
    when(dut.io.dcache.req.bits.mask.orR) {
      dmem(daddr) := write
      if (p(Trace)) printf("MEM[%x] <= %x\n", daddr * (xlen / 8).U, write)
    }.otherwise {
      if (p(Trace)) printf("MEM[%x] => %x\n", daddr * (xlen / 8).U, dmem(daddr))
    }
  }
  cycle := cycle + 1.U
  when(dut.io.host.tohost =/= 0.U) {
    printf("cycles: %d\n", cycle)
    assert((dut.io.host.tohost >> 1.U).asUInt === 0.U,
      "* tohost: %d *\n", dut.io.host.tohost)
    stop()
  }
}

class CoreSimpleTests extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Core"

  it should "execute a simple test" in {
    test(new CoreTester(new Core, "rv32ui-p-simple.hex")).runUntilStop(15000)
  }
}

//abstract class CoreTests(testType: TestType) extends IntegrationTests(
//  (loadmem, maxcycles) => new CoreTester(new Core, loadmem, maxcycles), testType)
//class CoreSimpleTests extends CoreTests(SimpleTests)
//class CoreISATests extends CoreTests(ISATests)
//TODO:  These are uncommented because they take a long time with Treadle.
// class CoreBmarkTests extends CoreTests(BmarkTests)
