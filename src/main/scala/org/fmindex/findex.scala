package org.fmindex

import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._

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

trait BWTBuilder[T] {
  val S:ArrayWrapper[T]
  val n:Int
  val SA: Array[Int]
  def BWT(i:Int):T
  def build(): Array[Int]

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

class NaiveIntBuilder(_s:Array[Int]) extends BWTBuilder[Int] {
  val S:IntArrayWrapper = new IntArrayWrapper(_s)
  val n:Int = _s.size
  val SA: Array[Int] = {
    val s = new Array[Int](n)
    (0 until n).foreach(i => s(i)=i )
    s
  }

  def arrayToString(s : Array[Int]) : String = s.mkString(",")
  def arrayToString(s : scala.collection.mutable.ArraySeq[Int]):String = {
    val ts = new Array[Int](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
  }
  
  def BWT(i:Int):Int = {
    val pIdx = SA(i)-1
    S((if (pIdx >= 0) pIdx else n-1) )
  }

  val ZERO = Array(0:Int)

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

  def build(): Array[Int] =  {
    class SASorter extends Ordering[Int] {
      def compare(x: Int,y: Int) = {
        val xi = SA(x)
        val yi = SA(y)
        val xs = arrayToString(S.slice(xi,n) ++ ZERO ++  S.slice(0,xi))
        val ys = arrayToString(S.slice(yi,n) ++ ZERO ++ S.slice(0,yi))
        xs compare ys
      }
    }
    // TODOno: Sorting copying - passed for naive sort
    SA.sorted(new SASorter).copyToArray(SA)
    SA
  }

}
class NaiveBuilder(_s:Array[Byte]) extends BWTBuilder[Byte] {
  val S:ByteArrayWrapper = new ByteArrayWrapper(_s)
  val n:Int = _s.size
  val SA: Array[Int] = {
    val s = new Array[Int](n)
    (0 until n).foreach(i => s(i)=i )
    s
  }

  def arrayToString(s : Array[Byte]) : String = new String(s).replace('\0','$')
  def arrayToString(s : scala.collection.mutable.ArraySeq[Byte]):String = {
    val ts = new Array[Byte](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
  }

  def BWT(i:Int):Byte = {
    val pIdx = SA(i)-1
    S(if (pIdx >= 0) pIdx else n-1 ).toByte
  }
  val ZERO = Array(0:Byte)

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

  def build(): Array[Int] =  {
    class SASorter extends Ordering[Int] {
      def compare(x: Int,y: Int) = {
        val xi = SA(x)
        val yi = SA(y)
        val xs = arrayToString(S.slice(xi,n) ++ ZERO ++  S.slice(0,xi))
        val ys = arrayToString(S.slice(yi,n) ++ ZERO ++ S.slice(0,yi))
        xs compare ys
      }
    }
    // TODOno: Sorting copying - passed for naive sort
    SA.sorted(new SASorter).copyToArray(SA)
    SA
  }

}

trait SAISAlgo[T] extends BWTBuilder[T] {
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
      val suffix_builder = new SAISIntBuilder(reduced_string,names_count)
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
  def build():Array[Int] = {
    buildStep1()
    val SA1 = buildStep2()
    buildStep3(SA1)
    SA
  }
}

abstract class ArrayWrapper[T](_s:Array[T])  {
  def apply(i:Int):Int
  def update(i:Int,v:Int)
  // Abit Iterator API
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

class SAISIntBuilder(_s:Array[Int],_k:Int) extends SAISAlgo[Int] {
  type cType = Int
  val S:IntArrayWrapper = new IntArrayWrapper(_s)
  val K = _k
  val n = _s.size
  val SA: Array[cType] = Array.fill[cType](n)(-1)

  lazy val C:Array[cType] = {
    val cnt = new Array[cType](K)
    S foreach { idx:cType => cnt(idx)+=1 }
    cnt
  }

  def BWT(i:Int):Int = {
    val pIdx = SA(i)-1
    S(if (pIdx >= 0) pIdx else n-1 )
  }

  def arrayToString(s : Array[cType]) : String = s.mkString(",")
  def arrayToString(s : scala.collection.mutable.ArraySeq[cType]):String = {
    val ts = new Array[cType](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
  }

  def buildSL() {
    t(n-2) = false  // 'L'
    t(n-1) = true   // 'S'
    for ( i <- n-3 to 0 by -1) {
      t(i) = S(i) < S(i+1) || (S(i) == S(i+1) && t(i+1) )
    }
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
  def induceSAs() {
    val bkt = bucketEnds.clone()
    for ( i <- n-1 to 0 by -1 ) {
      val j = SA(i)-1
      if ( j>=0 && t(j) ) { // S put it on end of bucket
      val l = S(j)
        val k = bkt(l)-1
        bkt(l)=k
        SA(k) = j
      }
    }
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

      if (  prev != -1  )
        while (! diff && d < n && ! reachedLMS(d) ) {
          if (cmpSuff(d) ) diff=true else d+=1
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
  def buildStep3(SA1:Array[Int])  {
    val bkt = bucketEnds.clone()
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
      val idx = bkt(S(j))-1
      bkt(S(j))=idx
      SA(idx)=j
    }

    induceSAl()
    induceSAs()

  }

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

class SAISBuilder(_s:Array[Byte]) extends SAISAlgo[Byte] with NaiveSearcher {
  type cType = Byte
  val S:ByteArrayWrapper = new ByteArrayWrapper(_s)
  val K = 256
  val n = _s.size
  val SA: Array[Int] = Array.fill[Int](n)(-1)


  def getPrevI(i:Int) = {
    val c = BWT(i)
    cf(c) + occ(c,i - 1)
  }
  def getNextI(i:Int) = {
    val c = BWT(i)
    OCC(i)
  }
  def prevSubstr(sp:Int,len:Int):String = {
    var cp = sp
    var ret = new StringBuilder()
    for ( i <- 0 until len) {
      ret.append(BWT(cp).toChar)
      cp = getPrevI(cp)
    }
    ret.reverse.result
  }
  /*
                       BWT
   0 -> 11. $abracadabra
   1 -> 10. a$abracadabr
   2 ->  7. abra$abracad
   3 ->  0. abracadabra$
   4 ->  3. acadabra$abr
   5 ->  5. adabra$abrac
   6 ->  8. bra$abracada
   7 ->  1. bracadabra$a
   8 ->  4. cadabra$abra
   9 ->  6. dabra$abraca
  10 ->  9. ra$abracadab
  11 ->  2. racadabra$ab

  nextSubstr(5,3) - gives 'ada'
    first sym is S sym
  prevSubstr(5,3) - rac 'rac'
    first sym is BWT sym

  */
  def nextSubstr(sp:Int,len:Int):String = {
    var cp = getNextI(sp)
    var ret = new StringBuilder()
    for ( i <- 0 until len) {
      ret.append(BWT(cp).toChar)
      cp = getNextI(cp)
    }
    ret.toString
  }

  lazy val C:Array[Int] = {
    val cnt = new Array[Int](K)
    S foreach { idx:cType => cnt(idx & 0xff)+=1 }
    cnt
  }

  def BWT(i:Int):Byte = {
    val pIdx = SA(i)-1
    S(if (pIdx >= 0) pIdx else n-1 ).toByte
  }

  def arrayToString(s : Array[cType]) : String = new String(s).replace('\0','$')
  def arrayToString(s : scala.collection.mutable.ArraySeq[cType]):String = {
    val ts = new Array[cType](s.size)
    s.copyToArray(ts)
    arrayToString(ts)
  }

  def buildSL() {
    t(n-2) = false  // 'L'
    t(n-1) = true   // 'S'
    for ( i <- n-3 to 0 by -1) {
      t(i) = S(i) < S(i+1) || (S(i) == S(i+1) && t(i+1) )
    }
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
  def induceSAs() {
    val bkt = bucketEnds.clone()
    for ( i <- n-1 to 0 by -1 ) {
      val j = SA(i)-1
      if ( j>=0 && t(j) ) { // S put it on end of bucket
        val l = S(j)
        val k = bkt(l)-1
        bkt(l)=k
        SA(k) = j
      }
    }
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

      if (  prev != -1  )
        while (! diff && d < n && ! reachedLMS(d) ) {
          if (cmpSuff(d) ) diff=true else d+=1
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
  def buildStep3(SA1:Array[Int])  {
    val bkt = bucketEnds.clone()
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
      val idx = bkt(S(j))-1
      bkt(S(j))=idx
      SA(idx)=j
    }

    induceSAl()
    induceSAs()

  }
  def writeFL(fname:String) {
    val outputc = Path.fromString(fname).outputStream(WriteTruncate:_*)
    val fl= new Array[Int](n)
    val bkt=bucketStarts.clone()
    val bkt0 = bucketStarts
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      val L_i = S(if (pIdx >= 0) pIdx else pIdx+n )
      val j = bkt(L_i)
      fl(j)=i
      bkt(L_i)=(j+1).toByte
    }
    outputc.write(bkt0)
    val output = Resource.fromFile(fname)
    output.append(fl)
  }
  def writeBWTNative(fname:String) {
    val output = new java.io.FileOutputStream(fname)
    val bwt= new Array[cType](n)
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      bwt(i) = S(if (pIdx >= 0) pIdx else pIdx+n ).toByte
    }
    output.write(bwt)
    output.close()
  }

  def writeBWT(fname:String) {
    val output = Path.fromString(fname).outputStream(WriteTruncate:_*)
    val bwt= new Array[cType](n)
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      bwt(i) = S(if (pIdx >= 0) pIdx else pIdx+n ).toByte
    }
    output.write(bwt)
  }

  def writeBWT3(fname:String) {
    val output:Output = Resource.fromFile(fname).outputStream
    val bwt= new Array[cType](n)
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      bwt(i) = S(if (pIdx >= 0) pIdx else pIdx+n ).toByte
    }
    output.write(bwt)
  }
  def writeBWTBulk(fname:String) {
    val output:Output = Resource.fromFile(fname)
    //val bwt= new Array[cType](n)
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      output.write(S(if (pIdx >= 0) pIdx else pIdx+n ))
    }
  }
  def writeBWTBulk2(fname:String) {
    val output = Path.fromString(fname).outputStream(WriteTruncate:_*)
    for ( i<- 0 until n) {
      val pIdx = SA(i)-1
      output.write(S(if (pIdx >= 0) pIdx else pIdx+n ))
    }
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
      /*
      printf("packTo buf=%d\theap=%d\tfree=%d\t%s\n",
        out.size/m,
        Runtime.getRuntime().totalMemory()/m,
        Runtime.getRuntime().freeMemory()/m,
        f
      )
      */
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
  val MB = 1024*1024
  def printMemUsage {
    
         
    //Getting the runtime reference from system
    val runtime = Runtime.getRuntime();
     
    println("##### Heap utilization statistics [MB] #####");
     
    //Print used memory
    println("Used Memory:"
        + (runtime.totalMemory() - runtime.freeMemory()) / MB);

    //Print free memory
    println("Free Memory:"
        + runtime.freeMemory() / MB);
     
    //Print total available memory
    println("Total Memory:" + runtime.totalMemory() / MB);

    //Print Maximum available memory
    println("Max Memory:" + runtime.maxMemory() / MB);
  }
    import java.io.ByteArrayOutputStream
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }
    def processBuffer(buf:ByteArrayOutputStream) {
      printf("processBuffer: Process %d MB\n" , buf.size/MB)
      printMemUsage
      buf.write(0)
      
      val sa = new SAISBuilder(buf.toByteArray())
      printMemUsage
      time { sa.build }
      printMemUsage
    }
    def make(dir:String,recursive:Boolean=true) = {
        val files = recursiveListFiles(new File(dir))
        val buf = new java.io.ByteArrayOutputStream()
        /*
        time  1024*1024*50 = 1m54.985s
        */
        val BUFLEN = 1024*1024*100
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
