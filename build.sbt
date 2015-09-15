organization := "edu.berkeley.cs"

version := "2.0"

name := "riscv-mini"

scalaVersion := "2.11.6"

// Provide a managed dependency on chisel if -DchiselVersion="" is supplied on the command line.
libraryDependencies ++= (Seq("chisel").map {
  dep: String => sys.props.get(dep + "Version") map { "edu.berkeley.cs" %% dep % _ }}).flatten

site.settings

site.includeScaladoc()

ghpages.settings

git.remoteRepo := "git@github.com:donggyukim/riscv-mini.git"
