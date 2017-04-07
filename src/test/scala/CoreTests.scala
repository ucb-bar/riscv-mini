// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._
import chisel3.testers._
import TestParams._

class CoreTester(core: => Core,
                 loadmem: Iterator[String],
                 maxcycles: Long)
                (implicit p: config.Parameters) extends BasicTester with HexUtils {
  val xlen = p(XLEN)
  val dut = Module(core)
  dut.io.host.fromhost.bits := 0.U
  dut.io.host.fromhost.valid := false.B

  val _hex = Vec(loadMem(loadmem, xlen) map (x => Cat(x.reverse))) 
  val imem = Mem(1 << 20, UInt(xlen.W))
  val dmem = Mem(1 << 20, UInt(xlen.W))
  val sInit :: sRun :: Nil = Enum(UInt(), 2)
  val state = RegInit(sInit)
  val cycle = RegInit(0.U(32.W))
  val (cntr, done) = Counter(state === sInit, _hex.size * (1 << 8))
  val iaddr = dut.io.icache.req.bits.addr / (xlen / 8).U
  val daddr = dut.io.dcache.req.bits.addr / (xlen / 8).U
  val write = ((0 until (xlen / 8)) foldLeft 0.U(xlen.W)){ (write, i) => write |
    (Mux(dut.io.dcache.req.valid && dut.io.dcache.req.bits.mask(i),
         dut.io.dcache.req.bits.data, dmem(daddr))(8*(i+1)-1, 8*i)) << (8*i).U
  }
  dut.reset := state === sInit
  dut.io.icache.resp.valid := state === sRun
  dut.io.dcache.resp.valid := state === sRun
  dut.io.icache.resp.bits.data := RegNext(imem(iaddr))
  dut.io.dcache.resp.bits.data := RegNext(dmem(daddr))
 
  val chunk = Wire(UInt(xlen.W))
  chunk := _hex(cntr >> 8.U) >> (cntr(7, 0) * xlen.U)

  switch(state) {
    is(sInit) {
      imem(cntr) := chunk
      dmem(cntr) := chunk
      when(done) { state := sRun }
      if (p(Trace)) printf("LOAMEM[%x] <= %x\n", cntr * (xlen / 8).U, chunk)
    }
    is(sRun) {
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
      assert(cycle < maxcycles.U)
      when(dut.io.host.tohost =/= 0.U) {
        printf("cycles: %d\n", cycle)
        assert((dut.io.host.tohost >> 1.U) === 0.U,
          "* tohost: %d *\n", dut.io.host.tohost)
        stop(); stop()
      }
    }
  }
}

abstract class CoreTests(testType: TestType) extends IntegrationTests(
  (loadmem, maxcycles) => new CoreTester(new Core, loadmem, maxcycles), testType)
class CoreSimpleTests extends CoreTests(SimpleTests)
class CoreISATests extends CoreTests(ISATests)
class CoreBmarkTests extends CoreTests(BmarkTests)
