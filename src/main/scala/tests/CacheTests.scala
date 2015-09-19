package mini

import Chisel._

class CacheTests(c: Cache) extends Tester(c) {
  abstract class CacheTask
  object ReadOnHit extends CacheTask
  object WriteOnHit extends CacheTask
  object ReadOnMiss extends CacheTask
  object WriteOnMiss extends CacheTask
  object ReadOnWriteBack extends CacheTask
  object WriteOnWriteBack extends CacheTask

  implicit def bigIntToInt(x: BigInt) = x.toInt
  implicit def bigIntToBoolean(x: BigInt) = x != 0
  implicit def booleanToBigInt(x: Boolean) = if (x) BigInt(1) else BigInt(0)

  def rand_tag = rnd.nextInt(1 << c.tlen)
  def rand_idx = rnd.nextInt(1 << c.slen)
  def rand_off = rnd.nextInt(1 << c.blen)
  def rand_word = rnd.nextInt() & 0xffffffff
  def rand_data = (0 until (1 << c.blen) foldLeft BigInt(0))((r, x) => r | (BigInt(rnd.nextInt() & 0xff) << 8*x))
  def rand_mask = (1 << (c.xlen / 8)) - 1
  def addr(tag: Int, idx: Int, off: Int) = int(((tag << c.slen | idx) << c.blen) | off)

  val mem = new MagicMem(c.bBytes)

  def doWrite(tag: Int, idx: Int, off: Int, data: Int, mask: Int) {
    val bmask = BigInt(mask) << ((off >> c.byteOffsetBits) * (c.xlen / 8))
    val bdata = ((0 until c.nWords) foldLeft BigInt(0))((r, i) => r | (int(data) << (i * c.xlen)))
    mem.write(addr(tag, idx, 0), bdata, bmask)
  }

  def doWriteBack {
    expect(c.io.mem.req_cmd.valid, 1)
    expect(c.io.mem.req_cmd.bits.rw, 1)
    expect(c.io.mem.req_data.valid, 1)
    val wb_addr = peek(c.io.mem.req_cmd.bits.addr) << c.blen
    val wb_data = peek(c.io.mem.req_data.bits.data)
    expect(wb_data == mem.read(wb_addr), "CHECK WRITE BACK(%s)".format(mem.read(wb_addr).toString(16)))
    poke(c.io.mem.req_cmd.ready, 1)
    poke(c.io.mem.req_data.ready, 1)
    step(1)
    poke(c.io.mem.req_cmd.ready, 0)
    poke(c.io.mem.req_data.ready, 0)
  }

  def doRefill(tag: Int, idx: Int, data: BigInt) {
    expect(c.io.mem.req_cmd.valid, 1)
    expect(c.io.mem.req_cmd.bits.rw, 0)
    expect(c.io.mem.req_cmd.bits.addr, tag << c.slen | idx)
    expect(c.io.mem.req_cmd.bits.tag, 0)
    expect(c.io.mem.req_data.valid, 0)
    poke(c.io.mem.req_cmd.ready, 1)
    step(1)
    poke(c.io.mem.req_cmd.ready, 0)
    expect(c.io.mem.resp.ready, 1)
    step(5)
    mem.write(addr(tag, idx, 0), data)
    poke(c.io.mem.resp.bits.data, data)
    poke(c.io.mem.resp.bits.tag, 0)
    poke(c.io.mem.resp.valid, 1)
    expect(c.is_alloc, 1)
  }

  def doReadOnHit(tag: Int, idx: Int, off: Int) {
    println("[READ ON HIT] addr = %s".format(addr(tag, idx, off).toString(16)))
    expect(c.hit, 1)
    poke(c.io.cpu.req.valid, 1)
    poke(c.io.cpu.req.bits.addr, rand_word)
    poke(c.io.cpu.req.bits.data, rand_word)
    poke(c.io.cpu.req.bits.mask, 0)
    expect(c.io.cpu.resp.valid, 1)
    expect(c.io.cpu.resp.bits.data, mem.read(addr(tag, idx, off), c.xlen / 8))
    expect(c.io.mem.req_cmd.valid, 0)
    expect(c.io.mem.req_data.valid, 0)
    expect(c.io.mem.resp.ready, 0)
  }

  def doReadOnMiss(tag: Int, idx: Int, off: Int, wb: Boolean = false) {
    println("[READ ON MISS] addr = %s".format(addr(tag, idx, off).toString(16)))
    val bdata = rand_data
    expect(c.hit, 0)
    expect(c.io.cpu.resp.valid, 0)
    poke(c.io.cpu.req.valid, 0)
    if (wb) doWriteBack
    doRefill(tag, idx, bdata)
    expect(c.io.cpu.resp.valid, 1)
    expect(c.io.cpu.resp.bits.data, mem.read(addr(tag, idx, off), c.xlen / 8))
  }

  def doWriteOnHit(tag: Int, idx: Int, off: Int, data: Int, mask: Int) {
    println("[WRITE ON HIT] addr = %s".format(addr(tag, idx, off).toString(16)))
    expect(c.hit, 1)
    expect(c.io.cpu.resp.valid, 0)
    expect(c.io.mem.req_cmd.valid, 0)
    expect(c.io.mem.req_data.valid, 0)
    poke(c.io.cpu.req.valid, 0)
    step(1)
    expect(c.io.cpu.resp.valid, 1)
    doWrite(tag, idx, off, data, mask)
  }

  def doWriteOnMiss(tag: Int, idx: Int, off: Int, data: Int, mask: Int, wb: Boolean = false) {
    println("[WRITE ON MISS] addr = %s".format(addr(tag, idx, off).toString(16)))
    val bdata = rand_data
    expect(c.hit, 0)
    expect(c.io.cpu.resp.valid, 0)
    poke(c.io.cpu.req.valid, 0)
    if (wb) doWriteBack
    doRefill(tag, idx, bdata)
    expect(c.io.cpu.resp.valid, 0)
    step(1) // need a cycle to refill
    step(1) // neea a cycle to write
    expect(c.io.cpu.resp.valid, 1)
    doWrite(tag, idx, off, data, mask)
  }

  var taskCount = 0
  def doTask(tag: Int, idx: Int, off: Int, task: CacheTask, data: Int = rand_word, mask: Int = 0) {
    println("\n*** NEW TASK %d ***".format(taskCount))
    taskCount += 1
    poke(c.io.cpu.req.valid, 1)
    poke(c.io.cpu.req.bits.addr, addr(tag, idx, off))
    poke(c.io.cpu.req.bits.data, data)
    poke(c.io.cpu.req.bits.mask, mask)
    step(1)
    poke(c.io.mem.resp.valid, 0)
    task match {
      case ReadOnHit => doReadOnHit(tag, idx, off)
      case ReadOnMiss => doReadOnMiss(tag, idx, off)
      case WriteOnHit => doWriteOnHit(tag, idx, off, data, mask) 
      case WriteOnMiss => doWriteOnMiss(tag, idx, off, data, mask)
      case ReadOnWriteBack => doReadOnMiss(tag, idx, off, true)
      case WriteOnWriteBack => doWriteOnMiss(tag, idx, off, data, mask, true)
    }
  }

  println("TAG: %d bits, IDX: %d bits, BLOCK OFFSET: %d bits".format(c.tlen, c.slen, c.blen))
  expect(c.io.cpu.resp.valid, 1)
  poke(c.io.cpu.abort, 0)
  poke(c.io.mem.req_cmd.ready, 0)
  poke(c.io.mem.req_data.ready, 0)
  poke(c.io.mem.resp.valid, 0)

  val tags = Vector(rand_tag, rand_tag, rand_tag)
  val idxs = Vector(rand_idx, rand_idx)
  val offs = Vector(rand_off, rand_off, rand_off, rand_off, rand_off, rand_off)
  doTask(tags(0), idxs(0), offs(0), ReadOnMiss)
  doTask(tags(0), idxs(0), offs(1), ReadOnHit)
  doTask(tags(1), idxs(0), offs(0), if (tags(1) != tags(0)) ReadOnMiss else ReadOnHit)
  doTask(tags(1), idxs(0), offs(2), ReadOnHit)
  doTask(tags(1), idxs(0), offs(3), ReadOnHit)
  doTask(tags(1), idxs(0), offs(4), WriteOnHit, mask=rand_mask)
  doTask(tags(1), idxs(0), offs(4), ReadOnHit)
  doTask(tags(2), idxs(0), offs(5), if (tags(2) != tags(1)) ReadOnWriteBack else ReadOnHit)
  doTask(tags(0), idxs(1), offs(0), WriteOnMiss, mask=rand_mask)
  doTask(tags(0), idxs(1), offs(0), ReadOnHit)
  doTask(tags(0), idxs(1), offs(1), ReadOnHit)
  doTask(tags(1), idxs(1), offs(2), if (tags(1) != tags(0)) WriteOnWriteBack else WriteOnHit, mask=rand_mask)
  doTask(tags(1), idxs(1), offs(3), ReadOnHit)
  doTask(tags(2), idxs(1), offs(4), if (tags(2) != tags(1)) ReadOnWriteBack else ReadOnHit)
  doTask(tags(2), idxs(1), offs(5), ReadOnHit) 
}
