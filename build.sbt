// See LICENSE for license details.

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

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Seq(
  "chisel3" -> "3.3-SNAPSHOT"
)

val commonSettings = Seq(
  scalaVersion := "2.12.6",
  crossScalaVersions := Seq("2.12.6", "2.11.12"),
  libraryDependencies ++= defaultVersions.map { case (dep, ver) =>
    "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", ver) },
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5"
  ),
  scalacOptions ++= scalacOptionsVersion(scalaVersion.value),
  scalacOptions ++= Seq("-deprecation", "-feature", "-language:reflectiveCalls"),
  javacOptions ++= javacOptionsVersion(scalaVersion.value),
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  )
)

val miniSettings = commonSettings ++ Seq(
  name := "riscv-mini",
  version := "2.1-SNAPSHOT",
  organization := "edu.berkeley.cs")

lazy val lib  = project settings commonSettings
lazy val mini = project in file(".") settings miniSettings dependsOn lib

publishMavenStyle := true

publishArtifact in Test := false
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
    <name>Charles Markley</name>
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
