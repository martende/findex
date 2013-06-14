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

  implicit def Array2ByteArrayWrapper(_s:Array[Byte]) = new ByteArrayWrapper(_s)

}