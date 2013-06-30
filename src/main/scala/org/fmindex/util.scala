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

  object ConsoleProgress{
  
    def apply(label:String,width:Int,labelColor:Option[String] = None, progressColor:Option[String] = None, percentColor:Option[String] = None):Double => Unit = {
      var previousLineWidth = 0
      var complete = false
      
      {progress:Double =>
        if(!complete){
          //clear the old line
          print("\b" * previousLineWidth)
          
          //print the new line
          val barWidth = (width * progress).toInt
          val barRemainder = width - barWidth
          
          val labelOutput = label+": "
          val progressBar = "["+("=" * barWidth)+(" " * barRemainder)+"] "
          val percent = (math.round(progress * 1000.0)/10.0)+"%"
          
          labelColor foreach print
          print(labelOutput)
          if(labelColor.isDefined) print(Console.RESET)
          
          progressColor foreach print
          print(progressBar)
          if(progressColor.isDefined) print(Console.RESET)
          
          percentColor foreach print
          print(percent)
          if(percentColor.isDefined) print(Console.RESET)
                  
          previousLineWidth = labelOutput.size + progressBar.size + percent.size
          
          //deal with 100% progress
          if(progress >= 1.0){
            complete = true
            println()
          }
        }
      }
    }
  }
  object bwtstring {
    val ALPHA_SIZE = 256
    // string to counts array
    def str2c(t:Array[Byte]) = {
      val l = t.length
      var i = 0 
      val occ = new Array[Int](ALPHA_SIZE)
      while (i < l) {
        occ(t(i)&0xff)+=1
        i+=1
      }
      occ
    }
    // string to bucket starts array
    def str2bs(t:Array[Byte]) = {
      val c = str2c(t)
      c2bs(c)
    }
    // c to bucket starts array
    def c2bs(c:Array[Int]) = {
      val bs = new Array[Int](ALPHA_SIZE)
      var tot = 0
      var i = 0      
      while (i < ALPHA_SIZE) {
        bs(i)+=tot
        tot+=c(i)
        i+=1
      }
      bs 
    }
    // string (bwt) to bucket starts array
    def bwt2occ(bwt:Array[Byte]) = {
      val n = bwt.length
      val occ = new Array[Int](n)
      val bkt=str2bs(bwt)
      var i = 0
      while ( i < n) {
        val c = bwt(i)
        val j = bkt(c)
        occ(j)=i
        bkt(c)=(j+1)
        i+=1
      }
      occ
    }

  }
  

}