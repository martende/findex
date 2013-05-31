package org.fmindex


abstract class AbstractSuffixArray[T<:AnyVal](_s:Array[T]) {
  val S:Array[T] = _s
  val n = S.size
  val SA: Array[Int] = Array.fill[Int](n)(-1)
  val t: Array[Boolean] = new Array[Boolean](n)
  val k = 256


  var lmsCount:Int = 0
  var S1:SuffixIntArray = null

  def chr(i:Int):Int
  def toInt(i:T):Int
  def arrayToString(s : Array[T]) : String
  def arrayToString(s : scala.collection.mutable.ArraySeq[T]):String
  val ZERO:Array[T]

  def getBuckets() = {
    val C = new Array[Int](k)
    S foreach { idx:T => C(toInt(idx))+=1 }
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
    S foreach { idx:T => C(toInt(idx))+=1 }
    val B = new Array[Int](k)
    var sum = 0
    for (i <- 0 until k) {
      B(i) = sum
      sum+=C(i)
    }
    B
  }

  def isLMS(i:Int) = i>0 && t(i) && !t(i-1)

  def buildSL() = {
    t(n-2) = false  // 'L'
    t(n-1) = true   // 'S'
    for ( i <- n-3 to 0 by -1) {
      t(i) = if ( chr(i) < chr(i+1) || chr(i) == chr(i+1) && t(i+1) == true ) true else false
    }
  }

  def markLMS(bkt:Array[Int]) = {
    var j = 0
    for ( i <- 1 until n) {
      if (isLMS(i)) {
        val t = bkt(chr(i))-1
        bkt(chr(i))=t
        SA(t) = i
        j+=1
      }
    }
    lmsCount = j
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
        arrayToString(S.slice(idx,n)) + "$" + arrayToString(S.slice(0,idx))
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
        val xs = arrayToString(S.slice(xi,n) ++ ZERO ++  S.slice(0,xi))
        val ys = arrayToString(S.slice(yi,n) ++ ZERO ++ S.slice(0,yi))
        xs compare ys
      } 
    }
    SA.sorted(new SASorter).copyToArray(SA)
  }
  
  def naiveIsSASorted(firstCount:Int=n):Boolean = {
    var xi = SA(0)
    if ( xi >= 0 ) {
      var prev =  arrayToString(S.slice(xi,n) ++ ZERO ++  S.slice(0,xi))
      for (i <- 1 until firstCount) {
        xi = SA(i)
        val current =  arrayToString(S.slice(xi,n) ++ ZERO ++  S.slice(0,xi))
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
  // n1 - start

  def calcLexNames(n1:Int):Pair[Int,Array[Int]] = {
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
    /* In SA from n1 to end are packed LMS indexes mit links to their names in string represented order
     pack it all to end of SA - just avoid sorting on O(n) from O(LMS_N*lg(LMS_N)) - its not obvios best solution
     in practice lms_count is really smaller as SA .
     */

    val sa1 = new Array[Int](n1)

    var j = n1-1
    for (i<-n-1 to n1 by -1 ) {
      if (SA(i)>=0) {
        sa1(j)=SA(i)
        j = j - 1
      }
    }

    return (name,sa1)
  }

  def sortReducedSA(reduced_string:Array[Int],names_count:Int):Array[Int] = {
    var SA1:Array[Int]=null

    if(reduced_string.size > names_count) {
      val suffix_builder = new SuffixIntArray(reduced_string,names_count)
      suffix_builder.build
      SA1 = suffix_builder.SA
    } else {
      // name of suffix is its order
      SA1 = new Array[Int](reduced_string.size)
      for ( i <- 0 until reduced_string.size) {
        SA1(reduced_string(i))=i
      }
    }
    return SA1
  }
  def buildStep2():Array[Int] =  {
    fillSAWithLMS()
    val (names_count,reduced_string) = calcLexNames(lmsCount)

    sortReducedSA(reduced_string,names_count)

  }

  def buildStep3(SA1:Array[Int]) =  {
    val bkt = getBuckets()
    var j = 0
    val s1 = new Array[Int](lmsCount)
    // WE Have all LMS sorted - now just set LMS sufixes in empty SA1 acording its order and repeat step1

    for (i<- 1 until n)
      if (isLMS(i)) {
        s1(j)=i
        j+=1
      }

    for (i <- 0 until lmsCount)
      SA(i) = s1(SA1(i))
    for (i <- lmsCount until n)
      SA(i) = -1

    // pack LMS them to buckets
    for (i <- lmsCount -1 to 0 by -1) {
      j = SA(i)
      SA(i) = -1
      val idx = bkt(chr(j))-1
      bkt(chr(j))=idx
      SA(idx)=j
    }

    induceSAl()
    induceSAs()

  }

  def build = {
    buildStep1()

    val SA1 = buildStep2()

    buildStep3(SA1)

  }

}

class SuffixArray(_s:Array[Byte]) extends AbstractSuffixArray(_s) {
  def chr(i:Int):Int =  S(i) & 0xff
  def toInt(i:Byte):Int =  i  & 0xff
  def arrayToString(s : Array[Byte]) : String = new String(s)
  val ZERO:Array[Byte] = Array[Byte](0)
  def arrayToString(s : scala.collection.mutable.ArraySeq[Byte]):String = {
    val ts = new Array[Byte](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
  }
}

class SuffixIntArray(_s:Array[Int],_power:Int) extends AbstractSuffixArray(_s) {
  override val k = _power
  def chr(i:Int):Int =  S(i)
  def toInt(i:Int):Int =  i
  def arrayToString(s : Array[Int]) : String = s.mkString(",")
  val ZERO:Array[Int] = Array[Int](0)
  def arrayToString(s : scala.collection.mutable.ArraySeq[Int]):String = {
    val ts = new Array[Int](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
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
