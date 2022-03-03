package mini

import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import pprint.pprintln

import scala.collection.mutable

object ModelingUtils {
  def bytesToWord(b: Array[Byte]): BigInt = {
    // assume that the byte array is in little endian
    assert(b.length == 4)
    ((b(3) & 0xFFL) << 24) | ((b(2) & 0xFFL) << 16) | ((b(1) & 0xFFL) << 8) | (b(0) & 0xFFL)
  }

  def wordToBytes(word: BigInt, numBytes: Int): Array[Byte] = {
    val bytes: Array[Byte] = word.toByteArray.padTo(numBytes, 0)
    val bytesTrim = if (bytes.head == 0 && bytes.length == numBytes + 1) bytes.slice(1, bytes.length) else bytes
    assert(bytesTrim.length == numBytes)
    bytesTrim.reverse // return results in little endian
  }

  def hexStringToBytes(data: String): Array[Byte] = {
    assert(data.length % 2 == 0)
    val expectedBytes = data.length / 2
    val bytes: Array[Byte] = BigInt(data, radix=16).toByteArray.padTo(expectedBytes, 0)
    val bytesTrim = if (bytes.head == 0 && bytes.length == expectedBytes + 1) bytes.slice(1, bytes.length) else bytes
    bytesTrim.reverse // return results in little endian
  }
}

sealed trait Activity
case class Read(addr: Long, data: Byte) extends Activity {
  override def toString: String = f"Read($addr%x = $data%x)"
}
case class Write(addr: Long, data: Byte) extends Activity {
  override def toString: String = f"Write($addr%x = $data%x)"
}

// TODO: avoid internal mutable state in both models and make then functional
class DRAMModel() {
  private val mem = mutable.Map.empty[Long, Byte].withDefaultValue(0)
  val activityLog = mutable.ListBuffer.empty[Activity]

  def clearLog(): Unit = activityLog.clear()

  def read(addr: Long): Byte = {
    activityLog += Read(addr, mem(addr))
    mem(addr)
  }

  def write(addr: Long, data: Byte): Unit = {
    activityLog += Write(addr, data)
    mem(addr) = data
  }

  def write(addr: Long, data: String): Unit = {
    val bytes = ModelingUtils.hexStringToBytes(data)
    bytes.zipWithIndex.foreach { case (byte, idx) =>
      write(addr + idx, byte)
    }
  }
}

class CacheModel(p: CacheConfig, cpuDataWidth: Int) {
  // TODO: hardcoded parameters
  val wordsPerBlock = p.blockBytes / 4
  val bytesPerWord = 4
  val dataMem: Array[Array[Array[Byte]]] = Array.fill(p.nSets)(Array.fill(wordsPerBlock)(Array.fill(bytesPerWord)(0)))
  val tagMem: Array[Int] = Array.fill(p.nSets)(0)
  val valid: Array[Boolean] = Array.fill(p.nSets)(false)
  val dirty: Array[Boolean] = Array.fill(p.nSets)(false)

  case class Address(tag: Int, set: Int, wordIdx: Int, byteOff: Int)
  object Address {
    def fromCacheReq(req: CacheReq): Address = { // TODO: use derived cache parameters
      val addr = req.addr.litValue.toInt
      Address(addr >> 12 & 0xfffff, addr >> 4 & 0xff, addr >> 2 & 0x3, addr & 0x3)
    }
  }

  def getBaseAddr(tag: Int, set: Int): Long = {
    ((tag & 0xFFFFFL) << 12) | (set & 0xFF) << 4
  }

  def readCacheBlock(setIdx: Int, wordIdx: Int): CacheResp = {
    new CacheResp(cpuDataWidth).Lit(_.data -> ModelingUtils.bytesToWord(dataMem(setIdx)(wordIdx)).U)
  }

  def writeCacheBlock(setIdx: Int, wordIdx: Int, data: BigInt, mask: Int): Unit = {
    val bytes = ModelingUtils.wordToBytes(data, 4)
    bytes.zipWithIndex.foreach { case (byte, byteIdx) =>
      if (((mask >> byteIdx) & 0x1) == 1) {
        dataMem(setIdx)(wordIdx)(byteIdx) = byte
      }
    }
    dirty(setIdx) = true
  }

  def maybeEvictAndRefill(newAddr: Address, dramModel: DRAMModel): Unit = {
    /*
    val eviction: Map[Int, Byte] = if (dirty(addr.set)) { // Evict existing entry if dirty
      dataMem(addr.set).flatten.zipWithIndex.map { case (byte, byteIdx) =>
        ((tagMem(addr.set) << 12) | (addr.set << 4)) + byteIdx -> byte
      }.toMap
    } else {
      Map.empty
    }
     */
    // Evict existing cache block if its dirty and valid
    if (dirty(newAddr.set) && valid(newAddr.set)) {
      dataMem(newAddr.set).flatten.zipWithIndex.foreach { case (byte, byteIdx) =>
        dramModel.write(getBaseAddr(tagMem(newAddr.set), newAddr.set) + byteIdx, byte)
      }
    }
    // Fetch new cache block
    for (wordIdx <- 0 until 4) {
      for (byteIdx <- 0 until 4) {
        dataMem(newAddr.set)(wordIdx)(byteIdx) = dramModel.read(getBaseAddr(newAddr.tag, newAddr.set) + (wordIdx * 4) + byteIdx)
      }
    }
    // Set valid/dirty bit and tag
    valid(newAddr.set) = true
    dirty(newAddr.set) = false
    tagMem(newAddr.set) = newAddr.tag
  }

  def tick(req: CacheReq, abort: Boolean, dramModel: DRAMModel): CacheResp = {
    if (abort) {
      new CacheResp(cpuDataWidth).Lit(_.data -> DontCare)
    } else {
      val addr = Address.fromCacheReq(req)
      //println(addr, req)
      val read = req.mask.litValue == 0
      val hit = (tagMem(addr.set) == addr.tag) && valid(addr.set)

      if (hit && read) {
        readCacheBlock(addr.set, addr.wordIdx)
      } else if (!hit && read) {
        maybeEvictAndRefill(addr, dramModel)
        readCacheBlock(addr.set, addr.wordIdx)
      } else if (hit && !read) { // writing to block already in cache
        writeCacheBlock(addr.set, addr.wordIdx, req.data.litValue, req.mask.litValue.toInt)
        new CacheResp(cpuDataWidth).Lit(_.data -> 0.U)
      } else { // writing
        assert (!hit && !read)
        maybeEvictAndRefill(addr, dramModel)
        writeCacheBlock(addr.set, addr.wordIdx, req.data.litValue, req.mask.litValue.toInt)
        new CacheResp(cpuDataWidth).Lit(_.data -> 0.U)
      }
    }
  }
}

class CacheModelTests extends AnyFlatSpec {
  "bytesToWord" should "work" in {
    assert(ModelingUtils.bytesToWord(Array(0xef.toByte, 0xbe.toByte, 0xad.toByte, 0xde.toByte)) == 0xdeadbeefL)
    assert(ModelingUtils.bytesToWord(Array(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte)) == 0xffffffffL)
    assert(ModelingUtils.bytesToWord(Array(0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)) == 0x00000000L)
    assert(ModelingUtils.bytesToWord(Array(0xff.toByte, 0x00.toByte, 0xff.toByte, 0x00.toByte)) == 0x00ff00ffL)
    assert(ModelingUtils.bytesToWord(Array(0x00.toByte, 0xff.toByte, 0x00.toByte, 0xff.toByte)) == 0xff00ff00L)
  }

  "hexStringToBytes" should "work" in {
    assert(ModelingUtils.hexStringToBytes("deadbeef") sameElements Array(0xef.toByte, 0xbe.toByte, 0xad.toByte, 0xde.toByte))
    assert(ModelingUtils.hexStringToBytes("ffffffff") sameElements Array(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    assert(ModelingUtils.hexStringToBytes("00000000") sameElements Array(0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte))
    assert(ModelingUtils.hexStringToBytes("00ff00ff") sameElements Array(0xff.toByte, 0x00.toByte, 0xff.toByte, 0x00.toByte))
    assert(ModelingUtils.hexStringToBytes("ff00ff00") sameElements Array(0x00.toByte, 0xff.toByte, 0x00.toByte, 0xff.toByte))
    assert(ModelingUtils.hexStringToBytes("123456789abc") sameElements Array(0xbc.toByte, 0x9a.toByte, 0x78.toByte, 0x56.toByte, 0x34.toByte, 0x12.toByte))
  }

  "DRAMModel" should "behave correctly" in {
    val dram = new DRAMModel()
    dram.write(0x8000_0000L, "deadbeef")
    assert(dram.read(0x8000_0000L) == 0xef.toByte)
    assert(dram.read(0x8000_0003L) == 0xde.toByte)
    dram.write(0x8000_0000L, "12345678abcdef00")
    assert(dram.read(0x8000_0000L) == 0x00.toByte)
    assert(dram.read(0x8000_0003L) == 0xab.toByte)
    assert(dram.read(0x8000_0004L) == 0x78.toByte)
    assert(dram.read(0x8000_0007L) == 0x12.toByte)
  }

  val p = MiniConfig()
  "CacheModel" should "give correct results with manual stimulus" in {
    val m = new CacheModel(p.cache, 32)
    val d = new DRAMModel()
    d.write(0x8000_0000L, "deadbeef")
    d.write(0x8000_0004L, "00ff00ff")
    d.write(0x8000_0008L, "ff00ff00")
    d.write(0x8000_000CL, "55555555")
    d.write(0x8000_1008L, "aaaaaaaa")
    d.clearLog()
    val requestProto = new CacheReq(32, 32)
    val respProto = new CacheResp(32)

    // Stimulus
    val requests = Seq(
      // (1) reads from DRAM
      requestProto.Lit(_.addr -> 0x8000_0000L.U, _.mask -> 0.U, _.data -> 0.U),
      requestProto.Lit(_.addr -> 0x8000_0002L.U, _.mask -> 0.U, _.data -> 0.U), // unaligned read access
      requestProto.Lit(_.addr -> 0x8000_0004L.U, _.mask -> 0.U, _.data -> 0.U),
      requestProto.Lit(_.addr -> 0x8000_0008L.U, _.mask -> 0.U, _.data -> 0.U),
      // (2) evict cache entry
      requestProto.Lit(_.addr -> 0x8000_1008L.U, _.mask -> 0.U, _.data -> 0.U), // belongs to same set (force eviction)
      // (3) write to the cache block
      requestProto.Lit(_.addr -> 0x8000_1000L.U, _.mask -> 0xf.U, _.data -> 0xffffffffL.U),
      requestProto.Lit(_.addr -> 0x8000_1004L.U, _.mask -> 0xc.U, _.data -> 0xababababL.U),
      requestProto.Lit(_.addr -> 0x8000_1008L.U, _.mask -> 0x3.U, _.data -> 0xcdcdcdcdL.U),
      // (4) read the cache block
      requestProto.Lit(_.addr -> 0x8000_1000L.U, _.mask -> 0.U, _.data -> 0.U),
      requestProto.Lit(_.addr -> 0x8000_1004L.U, _.mask -> 0.U, _.data -> 0.U),
      requestProto.Lit(_.addr -> 0x8000_1008L.U, _.mask -> 0.U, _.data -> 0.U),
      // (5) evict cache entry (force writeback)
      requestProto.Lit(_.addr -> 0x8000_0000L.U, _.mask -> 0.U, _.data -> 0.U),
      // (6) write to and read from uncached address
      requestProto.Lit(_.addr -> 0x8000_0400L.U, _.mask -> 0xf.U, _.data -> 0xabcdabcdL.U),
      requestProto.Lit(_.addr -> 0x8000_0400L.U, _.mask -> 0.U, _.data -> 0.U),
    )
    // Expected Cache Responses to CPU
    val expectedResps = Seq(
      // (1) reads from DRAM
      respProto.Lit(_.data -> 0xdeadbeefL.U),
      respProto.Lit(_.data -> 0xdeadbeefL.U),
      respProto.Lit(_.data -> 0x00ff00ffL.U),
      respProto.Lit(_.data -> 0xff00ff00L.U),
      // (2) post-eviction read
      respProto.Lit(_.data -> 0xaaaaaaaaL.U),
      // (3) writes
      respProto.Lit(_.data -> 0.U),
      respProto.Lit(_.data -> 0.U),
      respProto.Lit(_.data -> 0.U),
      // (4) read from cache block
      respProto.Lit(_.data -> 0xffffffffL.U),
      respProto.Lit(_.data -> 0xabab0000L.U),
      respProto.Lit(_.data -> 0xaaaacdcdL.U),
      // (5) post-eviction read
      respProto.Lit(_.data -> 0xdeadbeefL.U),
      // (6) write to uncached address
      respProto.Lit(_.data -> 0.U),
      respProto.Lit(_.data -> 0xabcdabcdL.U)
    )
    // Expected Cache Activity with DRAM
    val expectedDRAMActivity = Seq(
      Seq.tabulate(16)(i => Read(0x8000_0000L + i, 0)), // (1) read one cache block
      Seq.tabulate(16)(i => Read(0x8000_1000L + i, 0)), // (2), (3), (4) eviction shouldn't cause writeback, fetch new cache block
      Seq.tabulate(16)(i => Write(0x8000_1000L + i, 0)), // (5) eviction with writeback
      Seq.tabulate(16)(i => Read(0x8000_0000L + i, 0)), // (5) refill
      Seq.tabulate(16)(i => Read(0x8000_0400L + i, 0)), // (6) fetch new cache block
    ).flatten

    // Simulate model
    val resps = requests.map { req =>
      m.tick(req, false, d)
    }
    val dramActivity = d.activityLog
    // Debug
    // println(d.activityLog)

    // Check cache responses
    assert(resps.length == expectedResps.length)
    resps.zip(expectedResps).foreach { case (resp, expected) =>
      assert(resp.data.litValue == expected.data.litValue)
    }
    // Check DRAM activity
    assert(dramActivity.length == expectedDRAMActivity.length)
    dramActivity.zip(expectedDRAMActivity).foreach { case (act, expectedAct) =>
      act match {
        // TODO: ugly, this isn't necessary, there's a better way
        case Read(addr, data) => assert(addr == expectedAct.asInstanceOf[Read].addr)
        case Write(addr, data) => assert(addr == expectedAct.asInstanceOf[Write].addr)
      }
    }
  }
}
