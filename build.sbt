ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.6.4"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

val SourceCode = "com.lihaoyi" %% "sourcecode" % "0.2.7"
val ScalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1"

lazy val corundum = (project in file("."))
  .settings(
    name := "SpinalCorundum",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin, SourceCode, ScalaTest)
  )

// for simulation
fork := true
