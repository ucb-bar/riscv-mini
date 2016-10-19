package mini

import java.io.{File, FileWriter}

object Main extends App {
  val dir = new File(args(0)) ; dir.mkdirs
  val params = cde.Parameters.root((new MiniConfig).toInstance)
  val chirrtl = firrtl.Parser.parse(chisel3.Driver.emit(() => new Tile(params)))
  val writer = new FileWriter(new File(dir, s"${chirrtl.main}.fir"))
  firrtl.FIRRTLEmitter run (chirrtl, writer)
  writer.close

  val annotations = new firrtl.Annotations.AnnotationMap(Nil)
  val verilog = new FileWriter(new File(dir, s"${chirrtl.main}.v"))
  new firrtl.VerilogCompiler compile (chirrtl, annotations, verilog)
  verilog.close
}
