package corundum

import spinal.core._
import spinal.lib._

import sourcecode._
import java.io._

object SourceCodeGitHash {
  import sys.process._
  //def thisSourceFileGitHash(pathname: String) = (try {
  def apply_old(pathname: String) = (try {
    val path : java.io.File = new java.io.File(pathname)
    val dir = if (path.isFile()) path.getParentFile() else path
    s"git -C ${dir.toString} rev-parse HEAD".!!
  } catch{
    case e : java.io.IOException => "???"
  }).linesIterator.next()

  def apply(/*pathname: String*/)(implicit file: sourcecode.File) = (try {
    val pathname = file.value.toString()
    val path : java.io.File = new java.io.File(pathname)
    val dir = if (path.isFile()) path.getParentFile() else path
    s"git -C ${dir.toString} rev-parse HEAD".!!
  } catch{
    case e : java.io.IOException => "???"
  }).linesIterator.next()

}