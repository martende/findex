package org.findex


class SuffixArray(T:Array[Byte]) {
  val n = T.size
  val SA: Array[Int] = Array.fill[Int](n)(-1)
  val t: Array[Boolean] = new Array[Boolean](n)
  val k = 256


  def getBuckets() = {
    val C = new Array[Int](k)
    T foreach { idx:Byte => C(idx & 0xff)+=1 }
    val B = new Array[Int](k)
    var sum = 0
    for (i <- 0 until k) {
      sum+=C(i)
      B(i) = sum
    }
    B
  }
  def getBucketStarts() = {
    val C = new Array[Int](k)
    T foreach { idx:Byte => C(idx & 0xff)+=1 }
    val B = new Array[Int](k)
    var sum = 0
    for (i <- 0 until k) {
      B(i) = sum
      sum+=C(i)
    }
    B
  }

  def chr(i:Int) =  T(i) & 0xff
  def isLMS(i:Int) = i>0 && t(i) && !t(i-1) 
  def buildSL() = {
    t(n-2) = false  // 'L'
    t(n-1) = true   // 'S'
    for ( i <- n-3 to 0 by -1) {
      t(i) = if ( chr(i) < chr(i+1) || chr(i) == chr(i+1) && t(i+1) == true ) true else false
    }
  }
  def markLMS(bkt:Array[Int]) = {
    for ( i <- 1 until n) {
      if (isLMS(i)) {
        val t = bkt(chr(i))-1
        bkt(chr(i))=t
        SA(t) = i
      }
    }
  }
  def printSL(sl:Array[Boolean] = t) = {
    println("*** printSL")
    
    for ( i <- 0 until n-1) {
      printf("%c",chr(i))
    }
    printf("$\n")

    println(SL2String(sl))

  }
  def printSA(lmsOnly:Boolean=false) = {
    println("*** printSA")
    def stringLike(i:Int):String = {
      val idx = SA(i)
      if ( idx == -1 ) 
        List.fill(n)(".").mkString
      else
        new String(T.slice(idx,n)) + "$" + new String(T.slice(0,idx))
    }
    for ( i <- 0 until n) {
      if ( ! lmsOnly || isLMS(SA(i))) {
        printf("%2d -> %2d.\t%s\n",i,SA(i),stringLike(i)) 
      }
    }
  }

  def SL2String(sl:Array[Boolean] = t) = sl.map( x => if (x) "S" else "L" ).mkString("")
  
  def printBuckets(b:Array[Int]) {
    var skip = false
    var prev = b(0)
    println("*** printBuckets")
    printf("0(\\0)\t%d\n",b(0))
    for ( i <- 1 until b.size) {
      if ( b(i) != prev ) {
        if (skip) {
          printf("...\n",b(0))
          skip = false
        }
        printf("%d(%c)\t%d\n" , i,i , b(i))
      } else {
        skip = true
      }
      prev = b(i)      
    }
  }
  def initOrderedSA = {
    (0 until n).foreach(i => SA(i)=i )
  }
  def naiveBuild() = {
    import scala.util.Sorting.quickSort
    initOrderedSA
    class SASorter extends Ordering[Int] { 
      def compare(x: Int,y: Int) = { 
        val xi = SA(x)
        val yi = SA(y)
        val xs = new String(T.slice(xi,n) ++ Array[Byte](0) ++  T.slice(0,xi))
        val ys = new String(T.slice(yi,n) ++ Array[Byte](0) ++ T.slice(0,yi))
        xs compare ys
      } 
    }
    SA.sorted(new SASorter).copyToArray(SA)
  }
  
  def naiveIsSASorted(firstCount:Int=n):Boolean = {
    var xi = SA(0)
    if ( xi >= 0 ) {
      var prev =  new String(T.slice(xi,n) ++ Array[Byte](0) ++  T.slice(0,xi))
      for (i <- 1 until firstCount) {
        xi = SA(i)
        val current =  new String(T.slice(xi,n) ++ Array[Byte](0) ++  T.slice(0,xi))
        if (current < prev) return false
        prev = current
      } 
      true
    } else {
      false
    }
  }

  def induceSAl() = {
    val bkt = getBucketStarts()
    for (i <- 0 until n) {
      val j = SA(i)-1
      if ( j>=0 && t(j) == false ) { // L put it on start bucket
        val l = chr(j)
        val k = bkt(l)
        bkt(l)=k+1
        SA(k) = j
      }
    }
  }

  def induceSAs() = {
    val bkt = getBuckets()
    for ( i <- n-1 to 0 by -1 ) {
      val j = SA(i)-1
      if ( j>=0 && t(j) == true ) { // S put it on end of bucket
        val l = chr(j)
        val k = bkt(l)-1
        bkt(l)=k
        SA(k) = j
      }
    }
  }

  def buildStep1() = {
    val bkt = getBuckets()
    
    buildSL()
    markLMS(bkt)
    induceSAl()
    induceSAs()
  }
  
  // compact all the sorted substrings into the first n1 items of SA
  // on this step all LMS are already sorted
  def fillSAWithLMS():Int = {
    var n1 = 0
    for (i <- 0 until n) {
      if(isLMS(SA(i))) {
        SA(n1)=SA(i)
        n1+=1
      }
    }
    for ( i<- n1 until n) {
      SA(i) = -1
    }
    return n1
  }
  
  // find the lexicographic names of all substrings

  def calcLexNames(n1:Int) {
    var prev = -1
    var name = 0
    for (i <- 0 until n1) {
      var pos = SA(i)
      var diff = false
      var d = 0
      
      def reachedLMS(d:Int) = d > 0 && (isLMS(pos+d) || isLMS(prev+d))
      def cmpSuff(cur:Int,prev:Int,len:Int) = chr(cur+len)!=chr(prev+len) || t(cur+len)!=t(prev+len)

      if (  prev != -1  ) 
        while (! diff && d < n && ! reachedLMS(d) ) {
          if (cmpSuff(pos,prev,d) ) diff=true else d+=1
        }
      else
        diff = true
      if ( diff/* || cmpSuff(pos,prev,d)*/) {
        name+=1
        prev = pos
      }
      

      pos= if (pos % 2==0) pos/2 else (pos-1)/2
      SA(n1+pos)=name-1;
    }
    var j = n-1
    for (i<-n-1 to n1 by -1 ) {
      if (SA(i)>=0) {
        SA(j)=SA(i)
        j = j - 1
      }
    }
    println(n-n1,n)
    println(SA(13),SA(14),SA(15),SA(16))
  }

  def build = {
    buildStep1()
    

    
    val lms_count = fillSAWithLMS()
    calcLexNames(lms_count)

    // getCounts

    // getBuckets - end of symbol buckets



    // b = -1; i = n - 1; j = n; m = 0; c0 = T.get(n - 1);
    // do { c1 = c0; } while((0 <= --i) && ((c0 = T.get(i)) >= c1));

  }

}

object IndexMaker {
  object totoshka {
    val k = 1
  }
    import java.io.File
    import org.apache.commons.io.IOUtils
    import scalax.io._

    val EOF = 0xff
    val POWER = 256

    def recursiveListFiles(f: File): Stream[File] = {
        if (! f.isDirectory ) {
            Stream(f)
        } else {
            val these = f.listFiles
            if ( these == null )
                Stream.empty
            else
                these.filter(! _.isDirectory).toStream append these.filter(_.isDirectory).flatMap(recursiveListFiles)
        }
    }
    def packTo(buf:java.io.ByteArrayOutputStream,f:File) = {
      try {
        val fs = new java.io.FileInputStream(f)
        IOUtils.copy(fs,buf)
        buf.write(EOF)
      } catch  {
        case e:java.io.FileNotFoundException =>
      }
    }

    def buildSA(data: Array[Byte]) {
      val xs = new Array[Byte](data.size)

    }

  /*
template<typename string_type, typename sarray_type, typename index_type>
int
saisxx(string_type T, sarray_type SA, index_type n, index_type k = 256) {
int err;
if((n < 0) || (k <= 0)) { return -1; }
if(n <= 1) { if(n == 1) { SA[0] = 0; } return 0; }
try { err = saisxx_private::suffixsort(T, SA, 0, n, k, false); }
catch(...) { err = -2; }
return err;
}

if (saisxx(s.begin(), sa.begin(), (int)s.size(), 0x100) == -1) {
  */
    def make(dir:String,recursive:Boolean=true) = {
        val files = recursiveListFiles(new File(dir))
        val buf = new java.io.ByteArrayOutputStream()
        
        println(files)

        files foreach { f:File  => packTo(buf,f) } 
        
        // TODO: nado poluchit v pamiati vse faili kakto naibolee deshevim metodom
        val data = buf.toByteArray()

        println("Readed " + data.size  + " bytes")

        buildSA(data)

        println(data(0))
        /*
        for(i <- 0 to buf.size()) {
            println("i  " + i + "  " + buf[i] );
        }
        */
        
        //for((x,i) <- xs.view.zipWithIndex) println("String #" + i + " is " + x)
    }
}