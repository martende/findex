package org.fmindex

import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._

import Util._

trait SuffixAlgo {
  val n:Int

  def cf(c:Byte):Int
  def occ(c:Byte,i:Int):Int
  
  def nextSubstr(i:Int,len:Int):String

  def search(in:Array[Byte]):Option[(Int,Int)] = {
    var sp = 0
    var ep = n
    var i = in.length-1

    while ((sp<ep) && (i >= 0)) {
      val c  = in(i)
      //printf("sp=%d ,ep=%d , i= %d c='%c'\n",sp,ep,i,c)
      i-=1
      //printf("new sp=%d cf(%c)=%d occ(%c,%d-1)=%d\n",cf(c) + occ(c,sp - 1),c,cf(c),c,sp,occ(c,sp - 1))
      //printf("new ep=%d cf(%c)=%d occ(%c,%d)=%d\n",cf(c) + occ(c,ep),c,cf(c),c,ep,occ(c,ep))
      sp = cf(c) + occ(c,sp - 1)
      ep = cf(c) + occ(c,ep-1)
    }
    //printf("sp=%d ,ep=%d , i= %d\n",sp,ep,i)
    if ( sp < ep ) Some((sp,ep)) else None
  }
  def getPrevRange(sp:Int,ep:Int,c:Byte):Option[(Int,Int)] = {
    var sp1 = cf(c) + occ(c,sp - 1)
    var ep1 = cf(c) + occ(c,ep-1)
    if ( sp1 < ep1 ) Some((sp1,ep1)) else None
  }
  
}
trait BWTDebugging[T] {
  self:BWTBuilder[T] => 

  def arrayToString(s : Array[T]) : String
  def arrayToString(s : scala.collection.mutable.ArraySeq[T]):String

  def printSA(filter:Int => Boolean = (_ => true) )  {
    println("*** printSA")
    def stringLike(i:Int):String = {
      val idx = SA(i)
      if ( idx == -1 )
        List.fill(n)(".").mkString
      else
        arrayToString(S.slice(idx,n)) + arrayToString(S.slice(0,idx))
    }
    for ( i <- 0 until n) {
      //if ( ! lmsOnly || isLMS(SA(i))) {
      if ( filter(SA(i)))
        printf("%2d -> %2d.\t%s\n",i,SA(i),stringLike(i))
    }
  }

  def printBuckets(b:Array[Int]) {
    var skip = false
    var prev = b(0)
    println("*** printBuckets")
    printf("0(\\0)\t%d\n",b(0))
    for ( i <- 1 until b.size) {
      if ( b(i) != prev ) {
        if (skip) {
          printf("...\n%d",b(0))
          skip = false
        }
        printf("%d(%c)\t%d\n" , i,i , b(i))
      } else {
        skip = true
      }
      prev = b(i)
    }
  }
}

trait BWTBuilder[T] {
  val S:ArrayWrapper[T]
  val n:Int
  val SA: Array[Int]
  def BWT(i:Int):T
  def build(debug:Boolean=false): Array[Int]
}

class BwtIndex(filename:String=null,data:Array[Byte]=null,partial_len:Int = -1) {
  import org.apache.commons.io.IOUtils
  assert(!(filename == null && data.isEmpty))
  
  val plainTextFile = filename+".plain"
  val bwtIndexFile  = filename+".bwt"
  val gttnIndexFile  = filename+".gtn"
  val headerFile  = filename+".conf"

  var _bwtRank:Option[Int] = None
  var _gt_tn:Option[Array[Boolean]] = None
  var _len = if ( partial_len > 0 ) partial_len 
    else if (data != null ) data.length
    else -1
  def length = _len

  // Plain data - kommen aus file or direct byte array 
  lazy val S:ArrayWrapper[Byte] = new ByteArrayWrapper(
    Option(data).getOrElse({
      if ( filename != null) {
        val input = new java.io.FileInputStream(plainTextFile)
        if ( _len < 0) {
          val buf = new java.io.ByteArrayOutputStream()
          IOUtils.copy(input,buf)
          input.close()
          buf.toByteArray()
        } else {
          val buf:Array[Byte] = new Array[Byte](_len)
          input.read(buf)
          buf
        }
      } else {
        throw new Exception("S can't be loaded")
      }
    })
  )
  // If S[i..] suffix bigger than t[0..*]
  lazy val GT_TN:Array[Boolean] = 
    loadGTN(_len).getOrElse({
      buildBWT()
      loadGTN(_len).getOrElse({
        throw new Exception("_gt_tn cant be calculated")
      })
    })
  

  def loadGTN(len:Int):Option[Array[Boolean]] = _gt_tn.orElse {
    if (filename != null ){
      var in:java.io.FileInputStream = null
      var dis:java.io.DataInputStream = null
      try {
        in = new java.io.FileInputStream(gttnIndexFile)
        dis = new java.io.DataInputStream(in);

        val buf = new Array[Boolean](len)
        var b = 0
        var i = 0
        while (i < len) {
          buf(i) = dis.readBoolean()
          i+=1
        }
        Option(buf)
      } catch {
        case _:java.io.FileNotFoundException  => None
      } finally {
        if ( in != null) in.close()
        if ( dis != null) dis.close()      
      }
    } else None
  }

  lazy val bwtRank = {
    _bwtRank.getOrElse({
      if ( data != null) {
        buildBWT()
        _bwtRank.getOrElse({
          throw new Exception("_bwtRank cant be calculated")
        })
      } else {
        // load
        throw new Exception("_bwtRank should be loaded")
      }      
    })
    // calculate or load
  }

  def savePlain() = {
    val fs = new java.io.FileOutputStream(plainTextFile)
    fs.write(S.data)
    fs.close()
  }

  def buildBWT() = {
    val sa = new SAIS0FreeBuilder(S.data)
    sa.build()
    if ( filename != null ) {
      sa.writeBWT(bwtIndexFile)
      sa.writeGTTN(gttnIndexFile)
      sa.writeHeader(headerFile)
    } else {
      // load all in memory  ?
      _gt_tn = Some(sa.calcGTTN)
    }
    _bwtRank = Some(sa.bwtRank)
  }
}


object BWTMerger /*extends BWTBuilder[Byte] with BWTDebugging[Byte]*/ {
  
  def kmp_preifx(t:ArrayWrapper[Byte]):Array[Int] = {
    var q = 1
    var n = t.length
    var k = 0
    val prefix = new Array[Int](n)
    while (q < n) {
      while (t(q) != t(k) && k!=0) k = prefix(k-1)
      if (t(q) == t(k)) k+=1
      prefix(q) = k
      q+=1
    }
    prefix
  }
  // Create new bwt1..bwt2
  def compute_gt_eof(bwt1:BwtIndex,bwt2:BwtIndex) = {
    val n = bwt1.length
    val gt_eof = new Array[Boolean](bwt1.length);
    val kmp_shift = kmp_preifx(bwt1.S)
    var i = 0
    var startj = 0
    while ( i < n) {
      var j = startj 
      while ( i + j  != n  && bwt1.S(i+j) == bwt2.S(j) ) j+=1
      if ( i + j  == n ) {
        gt_eof(i) = ! bwt2.GT_TN(j)
      } else {
        gt_eof(i) = ( bwt1.S(i+j) > bwt2.S(j)  )
      }
      if ( j==0 ) {
        startj = 0 
        i+=1        
      } else {
        val k = kmp_shift(j-1)
        printf("compute_gt_eof i=%d substr of len=%d k=%d\n",i,j,k)
        startj=k
        var h=1
        val m=j-k
        while (h < m) {
          gt_eof(i+h) = bwt2.GT_TN(h)
          h+=1
        }
        i+=m
      }
    }
    gt_eof
  }

  def build(bwt1:BwtIndex,bwt2:BwtIndex,debug:Boolean=false)  {
    val rank0 = bwt2.bwtRank

    assert(bwt1.length <= bwt2.length,"bwt1.length(%d) > bwt2.length(%d)".format(bwt1.length,bwt2.length))

    compute_gt_eof(bwt1,bwt2)

  }
}

trait SAISAlgo[T] extends BWTBuilder[T] with BWTDebugging[T] {
  val K:Int
  // Must be lazy
  lazy val t: Array[Boolean] = new Array[Boolean](n)
  val C:Array[Int]

  var lmsCount = -1

  lazy val bucketEnds:Array[Int] = {
    val cnt = C
    val b = new Array[Int](K)
    var sum = 0
    for (i <- 0 until K) {
      sum+=cnt(i)
      b(i) = sum
    }
    b
  }
  lazy val bucketStarts:Array[Int] = {
    val b = new Array[Int](K)
    b(0) = 0
    for (i <- 1 until K) {
      b(i)=bucketEnds(i-1)
    }
    b
  }

  def printSL(sl:Array[Boolean] = t)  {
    println("*** printSL")
    for ( i <- 0 until n-1) {
      printf("%c",S(i))
    }
    printf("$\n")
    println(SL2String(sl))
  }

  def SL2String(sl:Array[Boolean] = t) = sl.map( x => if (x) "S" else "L" ).mkString("")

  def isLMS(i:Int) = i>0 && t(i) && !t(i-1)
  
  def markLMS() {
    val bkt = bucketEnds.clone()
    var j = 0
    for ( i <- 1 until n) {
      if (isLMS(i)) {
        val t = bkt(S(i))-1
        bkt(S(i))=t
        SA(t) = i
        j+=1
      }
    }
    lmsCount = j
  }

  def buildSL()
  def induceSAl()
  def induceSAs()




  def sortReducedSA(reduced_string:Array[Int],names_count:Int):Array[Int] = {
    var SA1:Array[Int]=null

    if(reduced_string.size > names_count) {
      val suffix_builder = new SAISIntBuilder(new IntArrayWrapper(reduced_string),names_count)
      SA1 = suffix_builder.build()
    } else {
      // name of suffix is its order
      SA1 = new Array[Int](reduced_string.size)
      for ( i <- 0 until reduced_string.size) {
        SA1(reduced_string(i))=i
      }
    }
    SA1
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

    var lmsCount=0
    for ( i <- 1 until n) {
      if (isLMS(i)) {
        lmsCount+=1
      }
    }

    n1
  }

  /*
   printToFile(new java.io.File("/tmp/example.txt"))(p => {SA.foreach(p.println)})
  */
  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
  

  def calcLexNames(n1:Int):Pair[Int,Array[Int]]

  def buildStep1() {
    buildSL()
    markLMS()
    induceSAl()
    induceSAs()
  }

  def buildStep2():Array[Int] =  {
    fillSAWithLMS()
    val (names_count,reduced_string) = calcLexNames(lmsCount)

    sortReducedSA(reduced_string,names_count)

  }

  def buildStep3(sa1:Array[Int])
  
  def build(debug:Boolean=false): Array[Int] = {
    if ( debug ) {
      System.gc()
      println("Start SAIS build on len" , n)
      printMemUsage
    }
    buildStep1()
    val SA1 = buildStep2()
    buildStep3(SA1)
    SA
  }
}

abstract class ArrayWrapper[T](_s:Array[T])  {
  val data = _s
  def apply(i:Int):Int
  def update(i:Int,v:Int)
  // Abit Iterator API
  val length = _s.length
  def slice(from: Int, until: Int) = _s.slice(from,until)
  def foreach[U](f: T => U) {
    _s.foreach(f)
  }
}

class IntArrayWrapper(_s:Array[Int]) extends ArrayWrapper[Int](_s) {
  def apply(i:Int):Int   = _s(i)
  def update(i:Int,v:Int) {
    _s(i) = v
  }
}


class ByteArrayWrapper(_s:Array[Byte]) extends ArrayWrapper[Byte](_s) {
  def apply(i:Int):Int   = _s(i) & 0xff
  def update(i:Int,v:Int) {
    _s(i) = v.toByte
  }
  def update(i:Int,v:Byte) {
    _s(i) = v
  }
}

class ByteArrayNulledWrapper(_s:Array[Byte]) extends ArrayWrapper[Byte](_s) {
  override val length = _s.length +1 
  val lastI = _s.length

  def apply(i:Int):Int   = if (i == lastI ) 0 else _s(i) & 0xff
  def update(i:Int,v:Int) {
    _s(i) = v.toByte
  }
  def update(i:Int,v:Byte) {
    _s(i) = v
  }
  override def foreach[U](f: Byte => U) {
    super.foreach(f)
    f(0)
  }
  //override def slice(from: Int, until: Int) = ???
}

trait NaiveSearcher extends SuffixAlgo {
  this: SAISAlgo[Byte] =>
  var OCC:Array[Int] = _
  def cf(c:Byte):Int = bucketStarts(c)
  def occ(c:Byte,key:Int):Int = {
    assert(SA(0)>=0,"Suffix Array not built")
    if ( OCC == null) buildOCC
    val istart = bucketStarts(c) 
    var imin = istart
    var imax = if ( c==K-1 ) n else bucketStarts(c+1)-1
    if (imin <= imax) {
      var found = false
      var imid:Int = 0
      var ival:Int = 0
      while (! found && imax >= imin) {
        imid = (imax+imin) / 2
        ival = OCC(imid)
        if (ival < key)
          imin = imid + 1
        else if (ival > key)
          imax = imid - 1
        else
          found = true
      }
      if (ival <= key) (imid - istart+1) else (imid - istart ) 
    } else 0
    
  }
  
  def buildOCC() {
    assert(OCC==null,"OCC already exists")
    assert(SA(0)>=0,"Suffix Array not built")
    OCC = new Array[Int](n)
    val bkt=bucketStarts.clone()
    for ( i<- 0 until n) {
      val c = BWT(i)
      val j = bkt(c)
      OCC(j)=i
      bkt(c)=(j+1)
    }
  }
}


class SAIS0FreeBuilder(_s:Array[Byte]) extends SAISBuilder(new ByteArrayNulledWrapper(_s))  {
  
  var bwtRank:Int = -1

  override def build(debug:Boolean=false) = {
    super.build(debug)
    for ( i <- 1 until n) {
      SA(i-1)=SA(i)
    }
    bwtRank = SA.indexWhere {_ == 0}
    SA
  }

  def writeGTTN(fname:String) {
    val output = new java.io.FileOutputStream(fname)
    // Double gtn calculation , should use calcGTTN instead or 
    // implement memory saving algo 
    val gttn = new Array[Byte](n)
    assert(bwtRank > 0)
    for ( i<- 0 until n) {
      gttn(i) = if ( SA(i)  > bwtRank ) 1 else 0
    }
    output.write(gttn)
    output.close()
  }
  def calcGTTN = {
    val gttn = new Array[Boolean](n)
    assert(bwtRank > 0)
    for ( i<- bwtRank+1 until n ) {
      gttn(SA(i)) = true
    }
    gttn
  }

  def writeHeader(fname:String) {
    val output = new java.io.FileOutputStream(fname)
    val dos = new java.io.DataOutputStream(output)
    assert(bwtRank > 0)
    dos.writeLong(bwtRank)
    output.close()
    dos.close()
  }

}

abstract class AllowedSuffixType[T]

object AllowedSuffixType {
  implicit object aByte extends AllowedSuffixType[Byte]
  implicit object aInt extends AllowedSuffixType[Int]
}

object IndexMaker {
    import java.io.File
    import org.apache.commons.io.IOUtils
    import scalax.io._
    import Util._
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
    def packTo(out:java.io.ByteArrayOutputStream,f:File) = {
      val m = 1024*1024
      
      printf("packTo buf=%d\ttotal=%d\tfree=%d\tused=%d\t%s\n",
        out.size/m,
        Runtime.getRuntime().totalMemory()/m,
        Runtime.getRuntime().freeMemory()/m,
        (Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory())/m,
        f
      )
      
      //printMemUsage
      try {
        val fs = new java.io.FileInputStream(f)
        val buf0 = new Array[Byte](4096)
        val buf1 = new Array[Byte](4096*2)
        var len = fs.read(buf0)
        while(len > 0) {
          var i = 0
          var j = 0
          while ( i < len ) {
            if ( buf0(i) == 0) {
              buf1(j) = '\''
              j=j+1
              buf1(j) = '0'
            } else {
              buf1(j) = buf0(i)
            }
            j=j+1
            i=i+1
          }
          out.write(buf1, 0, j)
          len = fs.read(buf0)
        }
        fs.close
        out.write(EOF)
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
  
    import java.io.ByteArrayOutputStream
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }
    def processBuffer(buf:ByteArrayOutputStream) {
      System.gc()
      printf("processBuffer: Process %d MB\n" , buf.size/MB)
      printMemUsage
      buf.write(0)
      
      val sa = new SAISBuilder(buf.toByteArray())
      System.gc()
      printMemUsage
      time { sa.build(debug=true) }
      printMemUsage
    }
    def make(dir:String,recursive:Boolean=true) = {
        val files = recursiveListFiles(new File(dir))
        val buf = new java.io.ByteArrayOutputStream()
        /*
        time  1024*1024*50 = 1m54.985s
        */
        val BUFLEN = 1024*1024*50
        var start = false
        printMemUsage
        
        files foreach { f:File  => 
          /*
          if ( f.toString == "/usr/include/wx-2.8/wx/wxPython/i_files/_mimetype.i") start=true
          if ( start)*/
          packTo(buf,f) 

          if ( buf.size >= BUFLEN ) {
            processBuffer(buf)
            buf.reset
          }
        }
        if ( buf.size > 0)
          processBuffer(buf)
        printMemUsage
        /*
        // TODO: nado poluchit v pamiati vse faili kakto naibolee deshevim metodom
        val data = buf.toByteArray()

        println("Readed " + data.size  + " bytes")

        buildSA(data)

        println(data(0))
        */
        /*
        for(i <- 0 to buf.size()) {
            println("i  " + i + "  " + buf[i] );
        }
        */
        
        //for((x,i) <- xs.view.zipWithIndex) println("String #" + i + " is " + x)
    }
}
