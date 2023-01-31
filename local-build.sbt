ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "online.blackwire"

// sometimes we want to work against a local SpinalHDL checkout

val spinalVersion = "dev"
val spinalDir = "../SpinalHDL.upstream"
//val spinalVersion = "dev"
//val spinalDir = "../SpinalHDL.dev"

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
    //libraryDependencies ++= Seq(breeze),
    //libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin)

    // the following 3 lines take SpinalHDL from a local checkout, above line must be commented out
    scalacOptions += s"-Xplugin:${new File(baseDirectory.value + s"/" + spinalDir + s"/idslplugin/target/scala-2.11/spinalhdl-idsl-plugin_2.11-$spinalVersion.jar")}",
    scalacOptions += s"-Xplugin-require:idsl-plugin"
  ).dependsOn(localHdlIdslPlugin, localHdlSim, localHdlCore, localHdlLib)
lazy val localHdlIdslPlugin = ProjectRef(file(spinalDir), "idslplugin")
lazy val localHdlSim = ProjectRef(file(spinalDir), "sim")
lazy val localHdlCore = ProjectRef(file(spinalDir), "core")
lazy val localHdlLib = ProjectRef(file(spinalDir), "lib")

// for simulation
fork := true
