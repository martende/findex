package org.fmindex

import scalax.io._
import scalax.file.Path
import scalax.io.StandardOpenOption._

import Util._

class SAISIntBuilder(_s:ArrayWrapper[Int],_k:Int) extends SAISAlgo[Int] {
  type cType = Int
  val S:ArrayWrapper[Int] = _s
  val K = _k
  val n = _s.length
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
  
  def convertSA2Rank(sa:Array[Int]) = {
    val r = new Array[Int](sa.length)
    var i = 0
    val n = sa.length
    while ( i < n) {
      r(sa(i))=i
      i+=1
    }
    r
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


class SAISBuilder(_s:ArrayWrapper[Byte]) extends SAISAlgo[Byte] with NaiveSearcher {
  type cType = Byte
  val S:ArrayWrapper[Byte] = _s
  val K = 256
  val n = _s.length
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