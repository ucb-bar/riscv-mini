// See LICENSE for license details.

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Seq(
  "chisel3" -> "3.5.0",
  "treadle" -> "1.5.0"
)

val commonSettings = Seq(
  scalaVersion := "2.12.13",
  crossScalaVersions := Seq("2.12.13", "2.13.5"),
  libraryDependencies ++= defaultVersions.map { case (dep, ver) =>
    "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", ver) },
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  ),
  scalacOptions ++= Seq(
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
  ),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions.toMap.get("chisel3").get cross CrossVersion.full),
)

val miniSettings = commonSettings ++ Seq(
  name := "riscv-mini",
  version := "2.1-SNAPSHOT",
  organization := "edu.berkeley.cs"
)

lazy val lib  = project settings commonSettings
lazy val mini = project in file(".") settings miniSettings dependsOn lib

publishMavenStyle := true

Test / publishArtifact := false
pomIncludeRepository := { x => false }

pomExtra := (
<url>http://chisel.eecs.berkeley.edu/</url>
<licenses>
  <license>
    <name>BSD-style</name>
    <url>http://www.opensource.org/licenses/bsd-license.php</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<scm>
  <url>https://github.com/ucb-bar/riscv-mini.git</url>
  <connection>scm:git:github.com/ucb-bar/riscv-mini.git</connection>
</scm>
<developers>
  <developer>
    <id>donggyukim</id>
    <name>Donggyu Kim</name>
    <url>http://donggyukim.github.io/</url>
  </developer>
  <developer>
    <id>chick</id>
    <name>Chick</name>
    <url>https://aspire.eecs.berkeley.edu/author/chick/</url>
  </developer>
</developers>
)

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  }
  else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}
