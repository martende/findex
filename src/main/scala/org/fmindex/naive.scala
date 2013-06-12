package org.fmindex
/**
  
  naive BWT build approaches direct strings sortings

*/
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

  def build(debug:Boolean=true): Array[Int] =  {
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

  def build(debug:Boolean=true): Array[Int] =  {
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
