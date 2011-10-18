package cc.acs.commons.util

object FileOps {
  import java.io.File
  def file(s:String) = new File(s)  
  def file(d:File, s:String) = new File(d, s)
  def fistream(f:java.io.File) = new java.io.FileInputStream(f)
  def fistream(s:String) = new java.io.FileInputStream(file(s))
}
