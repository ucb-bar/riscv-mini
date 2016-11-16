import sbt._
import Keys._

object MiniBuild extends Build {
  val defaultVersions = Map(
    "chisel3" -> "3.0-BETA-SNAPSHOT",
    "firrtl" -> "0.2-BETA-SNAPSHOT",
    "chisel-iotesters" -> "1.1-BETA-SNAPSHOT"
  )
  lazy val commonSettings = Seq(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    libraryDependencies ++= (Seq("chisel3", "firrtl", "chisel-iotesters") map { dep: String =>
      "edu.berkeley.cs" %% dep % sys.props.getOrElse(s"${dep}Version", defaultVersions(dep))
    }),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.mavenLocal
    )
  )
  lazy val miniSettings = commonSettings ++ Seq(
    name := "riscv-mini",
    version := "2.0-SNAPSHOT",
    organization := "edu.berkeley.cs",
    scalaVersion := "2.11.7",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
  )

  lazy val cde       = project settings commonSettings
  lazy val junctions = project settings commonSettings dependsOn cde
  lazy val mini      = project in file(".") settings miniSettings dependsOn (cde, junctions)
}
