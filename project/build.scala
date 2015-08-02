import sbt._
import Keys._

object DebugBuild extends Build {
  lazy val chisel   = Project("chisel", base=file("chisel"))
  lazy val root     = Project("riscv-mini", base=file(".")).dependsOn(chisel)
}
