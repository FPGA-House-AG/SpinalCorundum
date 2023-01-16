ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "online.blackwire"

val spinalVersion = "1.7.3a"

val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

val sourceCode = "com.lihaoyi" %% "sourcecode" % "0.2.7"
val scalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1"
// for {Dense,Sparse}{Vector,Matrix) used by LowCost.scala_
//val breeze = "org.scalanlp" %% "breeze" % "0.11.2"

lazy val spinalCorundum = (project in file("."))
  .settings(
    name := "SpinalCorundum",
    libraryDependencies ++= Seq(sourceCode, scalaTest),
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)
  )

fork := true
