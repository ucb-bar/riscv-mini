// Deal with potential problems with anonymous class structural definitions in Scala > 2.11
def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
// The following are the default development versions, not the "release" versions.
val defaultVersions = Map("chisel3" -> "3.2-SNAPSHOT")

val commonSettings = Seq(
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.11.12", "2.12.4"),
  scalacOptions := scalacOptionsVersion(scalaVersion.value),
  libraryDependencies ++= Seq("chisel3").map { dep: String =>
    "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  },
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5"
  ),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )
)

val miniSettings = commonSettings ++ Seq(
  name := "riscv-mini",
  version := "2.0-SNAPSHOT",
  organization := "edu.berkeley.cs")

lazy val lib  = project settings commonSettings
lazy val mini = project in file(".") settings miniSettings dependsOn lib
