// See LICENSE for license details.

package mini

import java.io.{File, FileWriter}

import chisel3.stage.ChiselGeneratorAnnotation
import firrtl.options.TargetDirAnnotation

object Main extends App {
  val params = (new MiniConfig).toInstance
  new chisel3.stage.ChiselStage().execute(args, Seq(
    ChiselGeneratorAnnotation(() => new Tile(params)),
    TargetDirAnnotation("test_run_dir")
  ))
}
