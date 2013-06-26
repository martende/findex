package org.fmindex
object Util {
  val MB = 1024*1024
  def printMemUsage {
    val runtime = Runtime.getRuntime();     
    println("##### Heap utilization statistics [MB] #####")
    println("Used Memory:"+ (runtime.totalMemory() - runtime.freeMemory()) / MB)
    println("Free Memory:"+ runtime.freeMemory() / MB)
    println("Total Memory:" + runtime.totalMemory() / MB)
    println("Max Memory:" + runtime.maxMemory() / MB)
  }

  def isBinary(f:java.io.File):Option[Boolean] = {
    var in:java.io.FileInputStream = null
    try {
      in =  new java.io.FileInputStream(f)
      val b = new Array[Byte](1024)
      val n = in.read(b)
      if ( n < 0 ) {
        None
      } else if ( b.view.slice(0,n).exists(_==0) )
        Some(true)
      else 
        Some(false)
        
    } catch {
      case _:java.io.FileNotFoundException  => {
        //debug(2,"DirBWTReader: read file '%s' File Not Found".format(f))
        None
      }
      case _ : Throwable => 
        //debug(1,"DirBWTReader: read file '%s' Exception".format(f))
        None
    } finally {
      if ( in != null ) in.close()
    }
  }

  implicit def Array2ByteArrayWrapper(_s:Array[Byte]) = new ByteArrayWrapper(_s)

}