import sbt._
import Keys._

object MiniBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases")),
    libraryDependencies += "edu.berkeley.cs" %% "chisel3" % sys.props("chisel3Version")
  )
  lazy val cde       = project
  lazy val junctions = project settings (settings:_*) dependsOn cde
  lazy val interp    = project
  lazy val testers   = project dependsOn interp
  lazy val root      = project in file(".") dependsOn (junctions, testers)
}
