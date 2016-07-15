import sbt._
import Keys._

object MiniBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-deprecation","-unchecked"),
    libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.3.15"),
    parallelExecution in Test := false
  )
  lazy val chisel    = project
  lazy val firrtl    = project
  lazy val cde       = project dependsOn chisel
  lazy val junctions = project dependsOn cde
  lazy val interp    = project dependsOn firrtl
  lazy val testers   = project dependsOn (chisel, interp)
  lazy val root      = (project in file(".")).settings(settings:_*).dependsOn(junctions, testers)
}
