ThisBuild / scalaVersion     := "2.13.7"
ThisBuild / version          := "2.5.0"
ThisBuild / organization     := "edu.berkeley.cs"

val chiselVersion = "6.2.0"
val chiseltestVersion = "6.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "riscv-mini",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "edu.berkeley.cs" %% "chiseltest" % chiseltestVersion % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full),
  )
