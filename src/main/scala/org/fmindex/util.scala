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
  def timer[R](block: => R,evaled:Long => Unit): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    evaled(t1-t0)
    result
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
    // bwt,fm to string
    def bwtFm2t(bwt:Array[Byte],fm:Array[Int],idx:Int) = {
      val n = bwt.length      
      val t = new Array[Byte](n)
      var i = 0
      
      var j = fm(idx)
      while ( i < n -1 ) {
        t(i)=bwt(j)
        j = fm(j)
        i=i+1
      }
      t(n-1)=0
      //t(j-1)=0
      t
    }

    // bwt,fm,bs to lcp ( longest common prefix)
    def bwtFm2LCP(bwt:Array[Byte],fm:Array[Int],bs:Array[Long],idx:Int) = {
      val n = bwt.length      
      val LCP = new Array[Int](n)
      var i = 0
      var nextk = 0
      var k = idx
      var h = 0

      def ibs2c(i:Int):Int = {
        assert(i < n)
        val j = bs.indexWhere(i < _,0)
        j-1
      }
      def iterChar(_j:Int,_h:Int,_temp:Int) = {
        var h = _h
        var j = _j
        var temp = _temp        
        if (h != 0 && temp == -1) {
          var t = 0
          while ( h > 0 ) {
            j = fm(j)
            h -= 1
          }
          temp = j
        } else {
          if ( temp != -1 ) {
            j = fm(temp)
            temp = j
          }
        }
        (temp,ibs2c(j))
      }

      while ( i < n) {
        if (k==0) {
          LCP(k) = 0
        } else {
          var temp1 = -1
          var temp2 = -1
          var j = k-1
          var stop = false
          while(  i+h < n && ! stop) {
            val ic1 = iterChar(k,h,temp1)
            val ic2 = iterChar(j,h,temp2)
            if ( ic1._2 == ic2._2) {
              temp1 = ic1._1
              temp2 = ic2._1
              h += 1
            } else {
              stop=true
            }
          }
          LCP(k-1)=h
        }
        if( h > 0 ) h -= 1
        k=fm(k)
        i+=1
      }
      LCP
    }
    def bwtFm2sa(bwt:Array[Byte],fm:Array[Int],eof:Int) = {
      val n = bwt.length      
      val sa = new Array[Int](n)
      var i = eof
      var j = 0
      while ( j < n) {
        sa(i)=j
        i = fm(i)
        j+=1
      }
      sa
    }
    def printSA(bwt:Array[Byte],fm:Array[Int],eof:Int,lcp:Array[Int]=null) = {
      val sa = bwtFm2sa(bwt,fm,eof)
      val s = bwtFm2t(bwt,fm,eof)
      val n = bwt.length
      println("*** printSA")
      def stringLike(i:Int):String = {
        val idx = sa(i)
        if ( idx == -1 )
          List.fill(n)(".").mkString
        else
          (s.view.slice(idx,n) ++ s.view.slice(0,idx)).map{_.toChar}mkString("")
      }
      for ( i <- 0 until n) {
        var strelem = stringLike(i)
        if (strelem.length > 40 ) {
          strelem = strelem.substring(0,10) + "..." + strelem.substring(strelem.length-10,strelem.length) 
        }
        if ( lcp != null) {
          printf("%2d -> %2d.\t%s\t%d\n",i,sa(i),strelem.map(c => if ( c < 0x20) '_' else c ),lcp(i))   
        } else {
          printf("%2d -> %2d.\t%s\n",i,sa(i),strelem.map(c => if ( c < 0x20) '_' else c ))   
        }
        
      }
    }
  }
  
  def safeChr(c:Int) = if (c >= 0x20 && c < 0x7f) "%c".format(c) else "%d".format(c)

}