import sbt._
import Keys._

object DebugBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq("-deprecation","-unchecked")
  )
  lazy val chisel    = project
  lazy val junctions = project.dependsOn(chisel)
  lazy val root      = (project in file(".")).settings(settings:_*).dependsOn(junctions)
}
