import sbt._
import Keys._

object MiniBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "riscv-mini",
    version := "2.0-SNAPSHOT",
    organization := "edu.berkeley.cs",
    scalaVersion := "2.11.7",
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")
    )
  )

  lazy val firrtl    = project
  lazy val chisel    = project dependsOn firrtl
  lazy val cde       = project
  lazy val junctions = project dependsOn (chisel, cde)
  lazy val mini      = project in file(".") dependsOn junctions
}
