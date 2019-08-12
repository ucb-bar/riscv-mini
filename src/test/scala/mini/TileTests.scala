// See LICENSE for license details.

package mini

import chisel3._
import chisel3.aop.Aspect
import chisel3.experimental.ExtModule
import chisel3.testers._
import chisel3.util._
import firrtl.AnnotationSeq
import freechips.rocketchip.config.Parameters
import junctions._
import mini._
import mini.TestParams._

abstract class TileTests(testType: TestType, annotations: AnnotationSeq = Nil, params: Option[Parameters] = None) extends IntegrationTests(
  (loadmem, maxcycles) => new TileTester(new Tile(if(params.isDefined) params.get else p), loadmem, maxcycles), testType, 6, annotations)
class TileSimpleTests extends TileTests(SimpleTests)
class TileISATests extends TileTests(ISATests)
class TileBmarkTests extends TileTests(BmarkTests)
class TileLargeBmarkTests extends TileTests(LargeBmarkTests)
