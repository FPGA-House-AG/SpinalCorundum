ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

// build against locally installed ../SpinalHDL
val spinalVersion = "1.6.5"
//val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
//val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
//val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

val SourceCode = "com.lihaoyi" %% "sourcecode" % "0.2.7"
val ScalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1"

lazy val corundum = (project in file("."))
  .settings(
    name := "SpinalCorundum",
    scalacOptions += s"-Xplugin:${new File(baseDirectory.value + s"/../SpinalHDL/idslplugin/target/scala-2.11/spinalhdl-idsl-plugin_2.11-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    libraryDependencies ++= Seq(/*spinalCore, spinalLib, spinalIdslPlugin, */SourceCode, ScalaTest)
  )
.dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore,spinalHdlLib)
lazy val spinalHdlIdslPlugin = ProjectRef(file("../SpinalHDL"), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file("../SpinalHDL"), "sim")
lazy val spinalHdlCore = ProjectRef(file("../SpinalHDL"), "core")
lazy val spinalHdlLib = ProjectRef(file("../SpinalHDL"), "lib")

// for simulation
fork := true
