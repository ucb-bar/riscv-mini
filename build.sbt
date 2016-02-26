organization := "edu.berkeley.cs"

version := "2.0"

name := "riscv-mini"

scalaVersion := "2.11.7"

// Provide a managed dependency on chisel if -DchiselVersion="" is supplied on the command line.
libraryDependencies ++= (Seq("chisel").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.11" % "2.2.4" % "test")
libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:donggyukim/riscv-mini.git"
