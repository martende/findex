package org.findex.test

import org.scalatest.FunSuite
import org.findex._

class GSuite extends FunSuite {
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])
  test("SuffixArray generics") {
    val bs = fromString("\276")
    val sa = new SuffixArray(bs)
    assert(sa.toInt(sa.S(0)) == 194)
    assert(sa.chr(0) == 194)
  }
}
class ExampleSuite extends FunSuite {
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])


  test("SuffixArray basics") {
    /*
                         01234567890 */
    val bs = fromString("missisippi")
    val sa = new SuffixArray(bs)
    assert(sa.n==11)
    val bkt = sa.getBuckets()
    assert(bkt(0)==1 && bkt(105)==5 && bkt(109)==6 && bkt(112)==8 && bkt(115) == 11  && bkt(115) == 11)
    //sa.printBuckets(sa.getBuckets())
    sa.buildSL()
    //sa.printSL(sa.t)
    assert(sa.SL2String() == "LSLLSLSLLLS")
    sa.markLMS(bkt)
    
    assert(sa.SA(0) == 10 )
    assert(sa.SA(1) == -1 )
    assert(sa.SA(2) == 6 )
    //sa.printSA()

    val bkt1 = sa.getBucketStarts()

    assert(bkt1(0)==0 && bkt(1)==1 && bkt(2)==1 && bkt(106)==5)

    // sa.printSA()

    sa.induceSAl()


    

    //sa.printSA()
  }
  test("article example") {
    val sa = new SuffixArray(fromString("mmiissiissiippii"))
    val naive = new SuffixArray(fromString("mmiissiissiippii"))
    val bkt = sa.getBuckets()
    
    sa.buildSL()
    sa.markLMS(bkt)

    assert(sa.SA(0) == 16 )
    assert(sa.SA(6) == 10 )
    assert(sa.SA(7) == 6 )
    assert(sa.SA(8) == 2 )

    sa.induceSAl()
    assert(Array(16,15,14,-1,-1,-1,10,6,2,1,0,13,12,9,5,8,4).sameElements(sa.SA))

    sa.induceSAs()

    assert(Array(16,15,14,10,6,2,11,7,3,1,0,13,12,9,5,8,4).sameElements(sa.SA))

    //sa.printSA()

    naive.naiveBuild()
    
    // actually everething is already sorted
    assert(naive.SA.sameElements(sa.SA))

    // sa.buildStep2()
    val lms_count = sa.fillSAWithLMS()
    var (names_count,sa1:Array[Int]) = sa.calcLexNames(lms_count)

    assert(names_count==3)
    assert(Array(2,2,1,0).sameElements(sa1))

    // Avoid recursve plays - direct go to step3
    var SA3  = new SuffixIntArray(sa1,names_count)
    SA3.naiveBuild()
    sa.buildStep3(sa1,SA3.SA)

    assert(Array(16,15,14,10,6,2,11,7,3,1,0,13,12,9,5,8,4).sameElements(sa.SA))


    // buildStep3()
    //val sorted_sa = sa.sortReducedSA(sa1,names_count)
    //println(sorted_sa.mkString(","))

    // S1: 2 2 1 0

    //sa.printSA(lmsOnly=true)
  }

  test("nonaive example - suffixes are not sorted after first induce step") {
    // Random generator
    val random = new scala.util.Random

    // Generate a random string of length n from the given alphabet
    def randomString(alphabet: String)(n: Int): String = 
      Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

    // Generate a random alphabnumeric string of length n
    def randomAlphanumericString(n: Int) = 
      randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

    val in = // randomAlphanumericString(200)
      "2b2w9vzrtqy3vzclgoofxgz9nal81y1fg8rozxkb5aaep1vpafp3cgsumc0z1rhpatcwo4d7nxc751h3a4woj3dbjf6ynfbkoom8sxoc9t3dqzkfs9akc6cmsy7cndi6bf116fju5rcsysixgkaih4zbkl8qo3ko2c42f34x6cqdew8x2jgz36r4bskabx02lxbfzokc"
    
    val sa = new SuffixArray(fromString(in))
    val naive = new SuffixArray(fromString(in))
    
    naive.naiveBuild()
    sa.buildStep1()
    // LMSess should be sorted
    val lms_count = sa.fillSAWithLMS()
    assert(lms_count==66)
    assert(sa.naiveIsSASorted(lms_count))
    // naive.printSA()
    // 148 -> 47.  pafp3cgs....
    // 149 -> 63.  patcwo4d....
    // sa.printSA()
    // 149 -> 63.  patcwo4d....
    // 148 -> 47.  pafp3cgs....

    assert(! naive.SA.sameElements(sa.SA))

  }
  test("naive sort") {
    val sa = new SuffixArray(fromString("missisippi"))
    sa.initOrderedSA
    assert(! sa.naiveIsSASorted())
    sa.naiveBuild()
    assert(sa.naiveIsSASorted())
    assert(Array(10,9,6,4,1,0,8,7,5,3,2).sameElements(sa.SA))
  }
                    /*
  test("pop is invoked on an empty stack") {

    val emptyStack = new Stack[Int]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
    assert(emptyStack.isEmpty)
  }*/
}