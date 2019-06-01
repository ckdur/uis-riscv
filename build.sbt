// Provide a managed dependency on X if -DXVersion="" is supplied on the command line ?

import sbt.complete._
import sbt.complete.DefaultParsers._
import xerial.sbt.pack._
import sys.process._

enablePlugins(PackPlugin)

lazy val commonSettings = Seq(
  organization := "uis",
  version      := "0.1",
  scalaVersion := "2.12.6",
  parallelExecution in Global := false,
  traceLevel   := 15,
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value),
  libraryDependencies ++= Seq("org.json4s" %% "json4s-jackson" % "3.5.0"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

val uisriscvSettings = Seq(
  organization := "uis",
  version := "0.1",
  name := "uis-riscv",
  scalaVersion := "2.12.6",
  scalacOptions ++= Seq("-deprecation","-unchecked","-Xsource:2.11"),
  scalaSource in Compile := baseDirectory.value / "src",
  mainClass in (Compile, run) := Some("generator.Main"),
)

lazy val chisel = (project in file("chisel3")).settings(commonSettings)
lazy val firrtl = (project in file("firrtl")).settings(commonSettings)
lazy val root = (project in file(".")).settings(commonSettings, uisriscvSettings).dependsOn(chisel,firrtl)
