package org.fmindex

import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._

import Util._

trait SuffixAlgo {
  val n:Int

  def cf(c:Byte):Int
  def occ(c:Byte,i:Int):Int

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

trait SuffixWalkingAlgo extends SuffixAlgo {
  def nextSubstr(i:Int,len:Int):String
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
        arrayToString(S.slice(idx,n) ++ S.slice(0,idx))
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
  // Alfabet size
  val K = 256 

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

  lazy val bucketStarts:Array[Int] = {
    if ( data != null ) {
      val C = new Array[Int](K)
      val bs = new Array[Int](K)
      for (i <- 0 until length ) {
        C(S(i))+=1
      }
      var tot = 0
      for (i <- 1 until K ) {
        bs(i)=tot
        tot+=C(i)
      }
      /*
      for (i <- 1 until K ) {
        bs(i)=bs(i-1)+C(i)
      }
      */
      bs
    } else {
       throw new Exception("bucketStarts can't be loaded") 
    }
  }
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

  def sa2bwt(sa:Array[Int]):Array[Byte] = {
    val bwt = new Array[Byte](sa.length)
    val n = sa.length
    var i = 0

    while ( i < n ) {
      var j = sa(i) - 1
      if ( j < 0) j = n-1
      bwt(i)=S(j)
      i+=1
    }
    bwt
  }
  
  def sa2bwt2(sa:Array[Int]):Array[Byte] = {
    val n = sa.length-1
    val bwt = new Array[Byte](n)
    var i = 0
    var rank0 = -1
    while ( i < n ) {
      var j = sa(i+1) - 1
      if ( j < 0) {
        rank0 = i
        j = n -1 
      }
      bwt(i)=S(j)
      i+=1
    }

    // Why its not just smthng else 
    // take care of the position where the eof symbol should go
    // writing a nearby symbol to help (run-length) compression 

    assert(rank0>0)
    bwt(rank0) = bwt(rank0-1)

    bwt
  }

  def savePlain() = {
    val fs = new java.io.FileOutputStream(plainTextFile)
    fs.write(S.data)
    fs.close()
  }

  def occ(c:Byte,i:Int) = {
    if ( sa == null) buildBWT()
    sa.occ(c,i)
  }
  var sa:SAIS0FreeBuilder = null

  def buildBWT() = {
    sa = new SAIS0FreeBuilder(S.data)
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
    val kmp_shift = kmp_preifx(bwt2.S)
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
        startj=k
        var h=1
        val m=j-k
        assert(k < j)
        while (h < m) {
          gt_eof(i+h) = bwt2.GT_TN(h)
          h+=1
        }
        i+=m
      }
    }
    gt_eof
  }

  def remap_alphabet(bwt1:BwtIndex,gt_eof:Array[Boolean]) = {
    
    def create_occ() = {
      val t = bwt1.S
      val n = bwt1.length - 1
      val occ = new Array[Int](bwt1.K+2)
      var i = 0
      while ( i < n ) {
        if ( t(i) < t(n) || ( t(i) == t(n) && ! gt_eof(i+1)) ) {
          occ(t(i))+=1
        } else {
          occ(t(i)+2)+=1
        }
        i+=1
      }
      occ(t(n)+1)+=1
      occ
    }
    def create_map(occ:Array[Int]) = {
      val map = new Array[Int](bwt1.K+2)
      val oclen = occ.length
      var asize = 0
      var i = 0
      while (i < oclen) {
        if ( occ(i)>0) {
          map(i)=asize
          asize+=1
        } else {
          map(i)=oclen
        }
        i+=1
      }
      (map,asize)
    }
    def create_mapped_string(map:Array[Int]) = {
      var i = 0
      val n = bwt1.length
      val n_1 = n -1 
      val t = bwt1.S
      val newt = new Array[Int](bwt1.length)
      while ( i < n ) {
        val c = if ( i == n - 1 ) t(i)+1 
          else if ( t(i) < t(n_1) || ( t(i) == t(n_1) && ! gt_eof(i+1) ) ) t(i)
          else t(i)+2
        newt(i) = map(c)
        i+=1
      }
      newt
    }
    val occ = create_occ()
    val (map,asize) = create_map(occ)
    (create_mapped_string(map),asize)
  }

  def remap_alphabet2(bwt1:BwtIndex,gt_eof:Array[Boolean]) = {
    
    def create_occ() = {
      val t = bwt1.S
      val n = bwt1.length - 1
      val occ = new Array[Int](bwt1.K+2)
      var i = 0
      while ( i < n ) {
        if ( t(i) < t(n) || ( t(i) == t(n) && ! gt_eof(i+1)) ) {
          occ(t(i))+=1
        } else {
          occ(t(i)+2)+=1
        }
        i+=1
      }
      occ(t(n)+1)+=1
      occ
    }
    def create_map(occ:Array[Int]) = {
      val map = new Array[Int](bwt1.K+2)
      val oclen = occ.length
      var asize = 1
      var i = 0
      while (i < oclen) {
        if ( occ(i)>0) {
          map(i)=asize
          asize+=1
        } else {
          map(i)=oclen
        }
        i+=1
      }
      (map,asize)
    }
    def create_mapped_string(map:Array[Int]) = {
      var i = 0
      val n = bwt1.length
      val n_1 = n -1 
      val t = bwt1.S
      val newt = new Array[Int](n+1)
      while ( i < n ) {
        val c = if ( i == n - 1 ) t(i)+1 
          else if ( t(i) < t(n_1) || ( t(i) == t(n_1) && ! gt_eof(i+1) ) ) t(i)
          else t(i)+2
        newt(i) = map(c)
        i+=1
      }
      newt(n)=0
      newt
    }
    val occ = create_occ()
    val (map,asize) = create_map(occ)
    (create_mapped_string(map),asize)
  }

  def suf_insert_bwt(bwt:Array[Byte],bwt1:BwtIndex,bwt2:BwtIndex) {
    val gaps = new Array[Int](bwt.length)
    var pfxBuffer:List[Byte] = List()
    val n = bwt2.length
    var c = bwt2.S(n-1)
    var i = 1

    pfxBuffer = c.toByte :: pfxBuffer 
    gaps(0)=1
    
    val start_sa_range = bwt1.bucketStarts
    var cur_rank = start_sa_range(c)
    gaps(cur_rank)+=1
    
    while ( i < n) {
      c = bwt2.S(n-i)
      val cfirst = start_sa_range(c)
      cur_rank = if (cur_rank==0) cfirst
      else cfirst + bwt1.occ(c.toByte,i)
      gaps(cur_rank)+=1
      i+=1
    }
  }

  def build(bwt1:BwtIndex,bwt2:BwtIndex,debug:Boolean=false)  {
    val rank0 = bwt2.bwtRank

    assert(bwt1.length <= bwt2.length,"bwt1.length(%d) > bwt2.length(%d)".format(bwt1.length,bwt2.length))


    val gt_eof = compute_gt_eof(bwt1,bwt2)
    val (remapped,k) = remap_alphabet(bwt1,gt_eof)
    
    val sa = new SAISIntBuilder(new IntArrayWrapper(remapped),k)
    sa.build()
    val ranks = sa.convertSA2Rank(sa.SA)
    
    val bwt = bwt1.sa2bwt(sa.SA)

    val newRank0 = sa.SA(0)
    val newRankLast = sa.SA(bwt1.length-1)
    sa.printSA()
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

  def buildSL() {
    t(n-2) = false  // 'L'
    t(n-1) = true   // 'S'
    for ( i <- n-3 to 0 by -1) {
      t(i) = S(i) < S(i+1) || (S(i) == S(i+1) && t(i+1) )
    }
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

  def induceSAl()  {
    val bkt = bucketStarts.clone()
    for (i <- 0 until n) {
      val j = SA(i)-1
      if ( j>=0 && ! t(j) ) { // L put it on start bucket
        val l = S(j)
        val k:Int = bkt(l)
        bkt(l)=k+1
        SA(k) = j
      }
    }
  }
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
      def cmpSuff(len:Int) = S(pos+len)!=S(prev+len) || t(pos+len)!=t(prev+len)

      if (  prev != -1  ) {
        while (! diff && d < n && ! reachedLMS(d) ) {
          if (cmpSuff(d) ) diff=true else d+=1
        }
        if ( d < n ) diff = diff || cmpSuff(d)
      }
      else
        diff = true

      if ( diff/* || cmpSuff(pos,prev,d)*/) {
        name+=1
        prev = pos
      }
      pos= if (pos % 2==0) pos/2 else (pos-1)/2
      SA(n1+pos)=name-1
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

    (name,sa1)
  }

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

class ByteArrayNulledOffsetWrapper(_s:Array[Byte],offset:Int) extends ArrayWrapper[Byte](_s) {
  override val length = _s.length +1 - offset
  val lastI = _s.length - offset

  def apply(i:Int):Int   = if (i == lastI ) 0 else _s(i+offset) & 0xff
  def update(i:Int,v:Int) {
    _s(i+offset) = v.toByte
  }

  def update(i:Int,v:Byte) {
    _s(i+offset) = v
  }
  override def foreach[U](f: Byte => U) {
    super.foreach(f)
    f(0)
  }
  //override def slice(from: Int, until: Int) = ???
}


trait NaiveSearcher extends SuffixWalkingAlgo {
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

/*
class OnullBWTSearcher(bwt:Array[Byte],bucketStarts:Array[Long],rk0:Int,alphaSize:Int=256) extends SuffixAlgo {
  val n = bwt.length
  
  val TWO224 = 0x01000000
  val MASK24 = 0x00ffffff
  val MASK8  = 0x000000ff

  def buildTable = {
    var i = 0
    var sb = 0
    while ( i < n) {
      if ( i & MASK24 ) {
        var j = 0
        while ( j < alphaSize) {
          sbCounts(sb)(j) = count(j)
          j+=1
        }
        sb+=1
      }
      if ( i & MASK8 ) {
        var j = 0
        while ( j < alphaSize) {
          num = count(j) - sbCounts(sb-1)(j)
          j+=1
          bwt32(i+j) += num
        }
      }
      if ( i != rk0 ) count(bwt(i))+=1
    }
  }
}
*/
class NaiveBWTSearcher(bwt:Array[Byte],bucketStarts:Array[Long],rk0:Int) extends SuffixAlgo {
  val K = bucketStarts.length
  val n = bwt.length

  val occtable:Array[Int] =  {
    val oct = new Array[Int](n)
    val bkt=bucketStarts.clone()
    var i = 0
    while ( i < n ) {
      val c = bwt(i) & 0xff
      val j = bkt(c).toInt
      if ( i != rk0) {
        oct(j)=i
        bkt(c)=(j+1)
      }
      i+=1
    }
    oct
  }
  def cf(c:Byte):Int = bucketStarts(c).toInt
  def occ(c:Byte,key:Int):Int = {
    val ci = c & 0xff
    val istart = bucketStarts(ci).toInt
    var imin = istart
    val iend = if ( c==K-1 ) n else bucketStarts(ci+1).toInt-1
    var imax = iend
    if (imin <= imax) {
      var found = false
      var imid:Int = 0
      var ival:Int = 0
      while (! found && imax >= imin) {
        imid = (imax+imin) / 2
        ival = occtable(imid)
        if (ival < key)
          imin = imid + 1
        else if (ival > key)
          imax = imid - 1
        else
          found = true
      }
      //printf("c=%c,key=%d,ival=%d,imid=%d,imax=%d (imid-istart)=%d gmax=%d\n",c,key,ival,imid,imax,imid - istart,iend)
      // actually our buckets contain one hole - for EOF - lets ignore it
      if ( imid == iend && ival== 0 ) (iend - istart)
      else if (ival <= key ) (imid - istart+1) else (imid - istart ) 
    } else 0
    
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
    val gttn = new Array[Boolean](n-1)
    assert(bwtRank >= 0)
    for ( i<- bwtRank+1 until n-1 ) {
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
