package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._
import scalax.io.StandardOpenOption._


class GSuite extends FunSuite {
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])
  test("SuffixArray generics bigInts conversion") {
    val bs = fromString("\276")
    val sa = new SAISBuilder(bs)
    assert(sa.S(0) == 194)
  }
}
trait RandomGenerator {
  // Random generator
  val random = new scala.util.Random

  // Generate a random string of length n from the given alphabet
  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(random.nextInt(alphabet.size)).map(alphabet).take(n).mkString

  // Generate a random alphabnumeric string of length n
  def randomAlphanumericString(n: Int) =
    randomString("abcdefghijklmnopqrstuvwxyz0123456789")(n)

  //def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])

}

class BasicTests extends FunSuite with RandomGenerator {
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])


  test("SAISBuilder basics") {
    /*                   01234567890 */
    val bs = fromString("missisippi")
    val sa = new SAISBuilder(bs)
    assert(sa.n==11)
    val bkt = sa.bucketEnds
    assert(bkt(0)==1 && bkt(105)==5 && bkt(109)==6 && bkt(112)==8 && bkt(115) == 11  && bkt(115) == 11)
    //sa.printBuckets(sa.getBuckets())
    sa.buildSL()
    //sa.printSL(sa.t)
    assert(sa.SL2String() == "LSLLSLSLLLS")
    sa.markLMS()
    assert(sa.SA(0) == 10 )
    assert(sa.SA(1) == -1 )
    assert(sa.SA(2) == 6 )
    //sa.printSA()
    val bkt1 = sa.bucketStarts
    assert(bkt1(0)==0 && bkt(1)==1 && bkt(2)==1 && bkt(106)==5)
    // sa.printSA()
    sa.induceSAl()
    //sa.printSA()
  }
  test("buckets test") {
    val sa = new SAISBuilder(fromString("aaaabbbccdd"))
    assert(sa.bucketStarts(0)==0 && sa.bucketStarts(1)==1 && sa.bucketStarts(98)==5 && sa.bucketStarts(99)==8)

  }
  test("article example") {
    val sa = new SAISBuilder(fromString("mmiissiissiippii"))
    val naive = new NaiveBuilder(fromString("mmiissiissiippii"))
    val bkt = sa.bucketEnds
    
    sa.buildSL()
    sa.markLMS()

    assert(sa.SA(0) == 16 )
    assert(sa.SA(6) == 10 )
    assert(sa.SA(7) == 6 )
    assert(sa.SA(8) == 2 )

    sa.induceSAl()
    assert(Array(16,15,14,-1,-1,-1,10,6,2,1,0,13,12,9,5,8,4).sameElements(sa.SA))

    sa.induceSAs()

    assert(Array(16,15,14,10,6,2,11,7,3,1,0,13,12,9,5,8,4).sameElements(sa.SA))

    //sa.printSA()

    naive.build()
    
    // actually everething is already sorted
    assert(naive.SA.sameElements(sa.SA))

    // sa.buildStep2()
    val lms_count = sa.fillSAWithLMS()
    var (names_count,sa1:Array[Int]) = sa.calcLexNames(lms_count)

    assert(names_count==3)
    assert(Array(2,2,1,0).sameElements(sa1))

    // Avoid recursve plays - direct go to step3
    var SA3  = new NaiveIntBuilder(sa1)
    SA3.build()
    sa.buildStep3(SA3.SA)

    assert(Array(16,15,14,10,6,2,11,7,3,1,0,13,12,9,5,8,4).sameElements(sa.SA))

  }
  test("1000 syms") {
    val in = randomAlphanumericString(1000)
    val sa = new SAISBuilder(fromString(in))
    sa.build()
  }
  test("nonaive example - suffixes are not sorted after first induce step") {

    val in = // randomAlphanumericString(200)
      "2b2w9vzrtqy3vzclgoofxgz9nal81y1fg8rozxkb5aaep1vpafp3cgsumc0z1rhpatcwo4d7nxc751h3a4woj3dbjf6ynfbkoom8sxoc9t3dqzkfs9akc6cmsy7cndi6bf116fju5rcsysixgkaih4zbkl8qo3ko2c42f34x6cqdew8x2jgz36r4bskabx02lxbfzokc"
    
    val sa = new SAISBuilder(fromString(in))
    val naive = new NaiveBuilder(fromString(in))
    
    naive.build()
    sa.buildStep1()
    // LMSess should be sorted
    val lms_count = sa.fillSAWithLMS()
    assert(lms_count==66)

    // naive.printSA()
    // 148 -> 47.  pafp3cgs....
    // 149 -> 63.  patcwo4d....
    // sa.printSA()
    // 149 -> 63.  patcwo4d....
    // 148 -> 47.  pafp3cgs....

    assert(! naive.SA.sameElements(sa.SA))

    // Test full cycle

    val sa2 = new SAISBuilder(fromString(in))
    sa2.build
    assert(naive.SA.sameElements(sa2.SA))
  }

  test("naive sort test") {
    val sa = new NaiveBuilder(fromString("missisippi"))
    assert(! sa.naiveIsSASorted())
    sa.build()
    assert(sa.naiveIsSASorted())
    assert(Array(10,9,6,4,1,0,8,7,5,3,2).sameElements(sa.SA))
  }
  /*
  test("occ test") {
    val sa = new SuffixArray(fromString("abracadabra"))
    sa.build
    println(sa.cf(1))
  }
  */
  test("bwt test") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    var f = Path.fromString("/tmp/bwttest.txt")
    sa.build
    sa.writeBWTNative(f.path)

    val ti:Input = Resource.fromFile(f.path)
    var content = new String(ti.byteArray).replace('\0','$')
    assert(content == "ard$rcaaaabb")
  }

  test("fl test") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    var f = Path.fromString("/tmp/bwttest.fl")
    sa.build
    sa.writeFL(f.path)
    val ti:Input = f.inputStream
    //println(java.nio.ByteBuffer.wrap(ti.byteArray))
    val bucket_size = 256 * 4

    val fl_len = f.size match {
      case Some(size) =>
        val s = size -  bucket_size
        (s - s  % 4).toInt
      case None => -1
    }

    assert(fl_len >0 )
    val fi = f.bytes.drop(bucket_size)
    val fl:Array[Int] = new Array[Int](fl_len/4)


    for ( i <- 0 until fl_len by 4 ) {
      fl(i/4)= (fi(i) << 24 )+ (fi(i+1) << 16) +( fi(i+2) << 8 ) + fi(i+3)
    }
    //
    assert(Array(3,0,6,7,8,9,10,11,5,2,1,4).sameElements(fl))
    
    f.delete()
    //var content = new String(ti.byteArray).replace('\0','$')

    //assert(content == "ard$rcaaaabb")
  }

  test("Naive SuffixAlgo test: occ/cf") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    assert(sa.cf(0) == 0)
    assert(sa.cf('a') == 1)
    assert(sa.cf('b') == 6)
    sa.build
    assert(sa.BWT(0) == 'a' , "")
    sa.buildOCC
    /*
    *** printSA
     0 -> 11. $abracadabra
     
     1 -> 10. a$abracadabr  0
     2 ->  7. abra$abracad  6
     3 ->  0. abracadabra$  7
     4 ->  3. acadabra$abr  8
     5 ->  5. adabra$abrac  9

     6 ->  8. bra$abracada
     7 ->  1. bracadabra$a
     8 ->  4. cadabra$abra
     9 ->  6. dabra$abraca
    10 ->  9. ra$abracadab
    11 ->  2. racadabra$ab
    */
    assert(Array(3,0,6,7,8,9,10,11,5,2,1,4).sameElements(sa.OCC))
    /*
      a r d $ r c a a a a b b
      0 1 2 3 4 5 6 7 8 9 0 1
    $ 0 0 0 1 1 1 1 1 1 1 1 1
    a 1 1 1 1 1 1 2 3 4 5 5 5
    b 0 0 0 0 0 0 0 0 0 0 1 2
    c 0 0 0 0 0 1 1 1 1 1 1 1
    d 0 0 1 1 1 1 1 1 1 1 1 1
    r 0 1 1 1 2 2 2 2 2 2 2 2
    */
   
    def row(c:Byte) = for (i<-0 until sa.n) yield sa.occ(c,i) 

    assert(Array(0,0,0,1,1,1,1,1,1,1,1,1).sameElements(row(0)))
    assert(Array(1,1,1,1,1,1,2,3,4,5,5,5).sameElements(row('a')))    
    assert(Array(0,0,0,0,0,0,0,0,0,0,1,2).sameElements(row('b')))
    assert(Array(0,0,0,0,0,1,1,1,1,1,1,1).sameElements(row('c')))
    assert(Array(0,0,1,1,1,1,1,1,1,1,1,1).sameElements(row('d')))
    assert(Array(0,1,1,1,2,2,2,2,2,2,2,2).sameElements(row('r')))
    assert(Array(0,0,0,0,0,0,0,0,0,0,0,0).sameElements(row('x')))

    //assert(sa.occ(0,3)==1,"sa.occ(0,3)="+sa.occ(0,3))
    //assert(sa.occ(0,4)==1,"sa.occ(0,3)="+sa.occ(0,4))
  }
}
