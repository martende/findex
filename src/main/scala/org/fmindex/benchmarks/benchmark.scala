package org.fmindex.benchmarks



import SAIS._

import annotation.tailrec
import com.google.caliper.Param
// a caliper benchmark is a class that extends com.google.caliper.Benchmark
// the SimpleScalaBenchmark trait does it and also adds some convenience functionality

trait RandomGenerator {
  // Random generator
  val random = new scala.util.Random

  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])

}

class BenchmarkBWTWrite extends SimpleScalaBenchmark with RandomGenerator {

  var SA: org.fmindex.fmindex0.SuffixArray = _
  override def setUp() {
    // set up all your benchmark data here
    val is = fromString(randomAlphanumericString(9999))
    SA = new org.fmindex.fmindex0.SuffixArray(is)
    SA.build
  }

  def time_writeBWT(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    SA.writeBWT("/tmp/bwt.txt")

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }
  def time_writeBWTBulk(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    SA.writeBWTBulk("/tmp/bwt.txt")

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }
  def time_writeBWTBulk2(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    SA.writeBWTBulk2("/tmp/bwt.txt")

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }
  def time_writeBWTJava(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    SA.writeBWTNative("/tmp/bwt.txt")

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }
  def time_writeBWT3(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    SA.writeBWT3("/tmp/bwt.txt")

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }

}

class Benchmark extends SimpleScalaBenchmark with RandomGenerator {

  // to make your benchmark depend on one or more parameterized values, create fields with the name you want
  // the parameter to be known by, and add this annotation (see @Param javadocs for more details)
  // caliper will inject the respective value at runtime and make sure to run all combinations
  @Param(Array("10", "100", "1000", "5000","10000","20000","30000","40000","50000"))
  val length: Int = 0

  var is: Array[Byte] = _
  var SA: Array[Int] = _


  override def setUp() {
    // set up all your benchmark data here
    is = fromString(randomAlphanumericString(length))
    SA = new Array[Int](length+1)
  }

  def timeScalaOldSuffixArray(reps: Int) = repeat(reps) {
    var result = 0
    var sf = new org.fmindex.fmindex0.SuffixArray(is)
    sf.build
    result
  }
  def timeScalaSuffixArray(reps: Int) = repeat(reps) {
    var result = 0
    var sf = new org.fmindex.SAISBuilder(is)
    sf.build
    result
  }

  /*
  def timeJavaSuffixArray(reps:Int) = repeat(reps) {
    var result = 0


    sais.suffixsort(is, SA, is.size);

    result
  }
  */

  override def tearDown() {
    // clean up after yourself if required
  }

}
