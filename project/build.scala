import sbt._
import Keys._

object MiniBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-deprecation","-unchecked")
  )
  lazy val chisel    = project
  lazy val firrtl    = project
  lazy val testers   = project dependsOn (chisel, firrtl)
  lazy val cde       = project dependsOn chisel
  lazy val junctions = project dependsOn cde
  lazy val root      = (project in file(".")).settings(settings:_*).dependsOn(junctions, testers)
}
