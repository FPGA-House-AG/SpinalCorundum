ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.7.3" // "dev"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

val SourceCode = "com.lihaoyi" %% "sourcecode" % "0.2.7"
val ScalaTest = "org.scalatest" % "scalatest_2.11" % "2.2.1"

val spinalDir = "../SpinalHDL.upstream"

lazy val corundum = (project in file("."))
  .settings(
    name := "SpinalCorundum",
    scalacOptions += s"-Xplugin:${new File(baseDirectory.value /*+ spinalDir*/ + s"/../SpinalHDL.upstream/idslplugin/target/scala-2.11/spinalhdl-idsl-plugin_2.11-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin",
    //spinalCore, spinalLib, spinalIdslPlugin,
    libraryDependencies ++= Seq(SourceCode, ScalaTest)
  )
.dependsOn(spinalHdlIdslPlugin, spinalHdlSim,spinalHdlCore, spinalHdlLib)
lazy val spinalHdlIdslPlugin = ProjectRef(file(spinalDir), "idslplugin")
lazy val spinalHdlSim = ProjectRef(file(spinalDir), "sim")
lazy val spinalHdlCore = ProjectRef(file(spinalDir), "core")
lazy val spinalHdlLib = ProjectRef(file(spinalDir), "lib")

// for simulation
fork := true
