package org.fmindex.tests.old

import org.scalatest.FunSuite
import org.fmindex.fmindex0._
import scalax.file.Path
import scalax.io._

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
    val (names_count,sa1:Array[Int]) = sa.calcLexNames(lms_count)

    assert(names_count==3)
    assert(Array(2,2,1,0).sameElements(sa1))

    // Avoid recursve plays - direct go to step3
    val SA3  = new SuffixIntArray(sa1,names_count)
    SA3.naiveBuild()
    sa.buildStep3(SA3.SA)

    assert(Array(16,15,14,10,6,2,11,7,3,1,0,13,12,9,5,8,4).sameElements(sa.SA))

  }

  test("nonaive example - suffixes are not sorted after first induce step") {

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

    // Test full cycle

    val sa2 = new SuffixArray(fromString(in))
    sa2.build
    assert(naive.SA.sameElements(sa2.SA))
  }
  test("naive sort") {
    val sa = new SuffixArray(fromString("missisippi"))
    sa.initOrderedSA
    assert(! sa.naiveIsSASorted())
    sa.naiveBuild()
    assert(sa.naiveIsSASorted())
    assert(Array(10,9,6,4,1,0,8,7,5,3,2).sameElements(sa.SA))
  }

  test("bwt test") {
    val sa = new SuffixArray(fromString("abracadabra"))
    val f = Path.fromString("/tmp/bwttest.txt.0")
    sa.build
    sa.writeBWTNative(f.path)

    val ti:Input = Resource.fromFile(f.path)
    val content = new String(ti.byteArray).replace('\0','$')
    assert(content == "ard$rcaaaabb")
    f.delete()
  }

  test("fl test") {
    val sa = new SuffixArray(fromString("abracadabra"))
    val f = Path.fromString("/tmp/bwttest.fl.0")
    sa.build
    sa.writeFL(f.path)

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

}