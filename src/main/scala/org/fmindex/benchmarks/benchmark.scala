package org.fmindex.benchmarks

import org.fmindex.SuffixArray

import SAIS._

import annotation.tailrec
import com.google.caliper.Param
// a caliper benchmark is a class that extends com.google.caliper.Benchmark
// the SimpleScalaBenchmark trait does it and also adds some convenience functionality

class Benchmark extends SimpleScalaBenchmark {

  // to make your benchmark depend on one or more parameterized values, create fields with the name you want
  // the parameter to be known by, and add this annotation (see @Param javadocs for more details)
  // caliper will inject the respective value at runtime and make sure to run all combinations
  @Param(Array("10", "100", "1000", "5000","10000","20000"))
  val length: Int = 0

  var is: Array[Byte] = _
  var SA: Array[Int] = _

  // Random generator
  val random = new scala.util.Random

  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])

  override def setUp() {
    // set up all your benchmark data here
    is = fromString(randomAlphanumericString(length))
    SA = new Array[Int](length+1)
  }

  // the actual code you'd like to test needs to live in one or more methods
  // whose names begin with 'time' and which accept a single 'reps: Int' parameter
  // the body of the method simply executes the code we wish to measure, 'reps' times
  // you can use the 'repeat' method from the SimpleScalaBenchmark trait to repeat with relatively low overhead
  // however, if your code snippet is very fast you might want to implement the reps loop directly with 'while'
  def timeScalaSuffixArray(reps: Int) = repeat(reps) {
    //////////////////// CODE SNIPPET ONE ////////////////////

    var result = 0

    var sf = new SuffixArray(is)
    sf.build

    result // always have your snippet return a value that cannot easily be "optimized away"

    //////////////////////////////////////////////////////////
  }

  def timeJavaSuffixArray(reps:Int) = repeat(reps) {
    var result = 0


    sais.suffixsort(is, SA, is.size);

    result
  }
  override def tearDown() {
    // clean up after yourself if required
  }

}
