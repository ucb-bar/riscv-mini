organization := "edu.berkeley.cs"

version := "2.0"

name := "riscv-mini"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

val defaultVersions = Map(
  "chisel3" -> "3.0-BETA-SNAPSHOT",
  "firrtl" -> "0.2-BETA-SNAPSHOT"
)

libraryDependencies ++= (Seq("chisel3", "firrtl") map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
})

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:donggyukim/riscv-mini.git"
