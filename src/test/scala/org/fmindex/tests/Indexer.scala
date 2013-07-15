package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._
import org.fmindex.Util._
import scala.collection.mutable.BitSet
import java.io.File

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
  def randomDNAString(n: Int) =
    randomString("ATGC")(n)
  //def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])

  def testSA(s:Array[Int],sa:Array[Int],verbose:Boolean=false) {
    for ( i <- 0 until sa.length - 1 ) {
      val a = s.slice(sa(i),s.length)
      val b = s.slice(sa(i+1),s.length)
      val l = a.length min b.length
      var j = 0
      
      while ( j < l) {
        if ( a(j) < b(j)) {
          j=l+1
        } else if ( a(j)> b(j)) {
          printf("BAD a(%d)  > b(%d) on j = %d %d %d\n",i,i+1,j,a(j),b(j))
          if ( verbose ) {
            println("a="+a.mkString("."))
            println("b="+b.mkString("."))
          }
          j=l+1
        }
        j+=1
      }
    }
  }

}

class BasicTests extends FunSuite with RandomGenerator {
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
    sa2.build()
    assert(naive.SA.sameElements(sa2.SA))
  }
  /*
  test("saa") {

    val in = randomDNAString(100)
    
    val sa = new SAISBuilder(fromString(in))
    
    sa.build()
    sa.printSA()
    
  }
  */
  test("naive sort test") {
    val sa = new NaiveBuilder(fromString("missisippi"))
    assert(! sa.naiveIsSASorted())
    sa.build()
    assert(sa.naiveIsSASorted())
    assert(Array(10,9,6,4,1,0,8,7,5,3,2).sameElements(sa.SA))
  }

  test("sais builder") {
    val b = Array[Byte](97,115,100,10,97,115,100,10,-1,97,115,100,10,98,101,108,107,97,64,98,101,108,107,97,45,104,111,109,101,58,47,116,109,112,47,116,36,32,99,97,116,32,62,32,116,50,46,116,120,116,10,97,115,100,97,115,100,10,-1,0)
    val k = new SAISBuilder(b)
    k.build()
  }

  test("nulled array") {
    var nb = new  ByteArrayNulledWrapper("mmiissiissiippii".getBytes())
    var b0 = fromString("mmiissiissiippii")
    assert(nb.length == b0.length)
    for (i <- 0 until b0.length ) {
      assert(nb(i) == b0(i),"nb(i) == b0(i) i=%d nb(i)=%d b0(i)=%d".format(i,nb(i),b0(i)))
    }
    val sa = new SAISBuilder(nb)
    sa.build()
  }

  test("bwt test") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    var f = Path.fromString("/tmp/bwttest.txt")
    sa.build()
    sa.writeBWTNative(f.path)

    val ti:Input = Resource.fromFile(f.path)
    var content = new String(ti.byteArray).replace('\0','$')
    assert(content == "ard$rcaaaabb")
  }

  test("fl test") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    var f = Path.fromString("/tmp/bwttest.fl")
    sa.build()
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
    sa.build()
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

  }

  test("plain searching") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    sa.build()
    sa.buildOCC

    sa.search("bra".getBytes()) match {
      case None => assert(false,"bra not found")
      case Some((6,8)) => // OK
      case _ => assert(false ,"Bad answer")
    }
  }

  test("BWT walki") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    sa.build()
    sa.buildOCC
    //sa.printSA()
    // cadabra
    //    6   
    assert(sa.BWT(6).toChar == 'a')
    assert(sa.getPrevI(6)==2)
    assert(sa.BWT(sa.getPrevI(6)).toChar == 'd')
    assert(sa.BWT(sa.getPrevI(2)).toChar == 'a')
    
    assert(sa.getNextI(6)==10)
    assert(sa.getNextI(10)==1)

  }
  test("BWT substrings") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    sa.build()
    sa.buildOCC
    //sa.printSA()
    // cadabra
    assert(sa.nextSubstr(6,4)=="bra\0","sa.nextSubstr(6,4)="+sa.nextSubstr(6,4))
    assert(sa.prevSubstr(6,4)=="cada","sa.nextSubstr(6,4)="+sa.prevSubstr(6,4))

  }
  test("BWT substrings2") {
    val sa = new SAISBuilder(fromString("mmabcacadabbbca".reverse))
    sa.build()
    sa.buildOCC
    //sa.printSA()
    assert(sa.nextSubstr(11,3) == "cba",sa.nextSubstr(11,3))
    assert(sa.prevSubstr(11,3) == "aca",sa.prevSubstr(11,3))
  }
  test("getPrevRange") {
    val sa = new SAISBuilder(fromString("mmabcacadabbbca".reverse))
    sa.build()
    sa.buildOCC
    //sa.printSA()

    assert (sa.occ('b',6)==3)
    assert (sa.getPrevRange(0,16,'a')==Some((1,6)) )
    assert (sa.getPrevRange(1,6,'b')==Some((6,8)) )
  }

  test("reducing bug example") {
    val d = Array[Byte](18,6,17,11,3,22,27,20,15,27,2,6,2,14,18,6,17,10,11,0)

    val sa = new SAISBuilder(d)
    val bkt = sa.bucketEnds
    
    //testSA(d,javasa,verbose=false)
    
    sa.buildSL()
    sa.markLMS()
    // IN SL Should be LMSess

    // 18,6,17,11,3,22,27,20,15,27,2,6,2,14,18,6,17,10,11,0
    // L  S  L  L S  S  L  L  S  L S L S  S  L S  L  S  L S
    //    *       *           *    *   *       *     *    *
    // BUCKETS:
    // 0   2 2  3  6  6 6 10 11 11 14 15 17 17 18 18 20 22 27 27
    // 19,12,10,4,-1,15,1,17,-1,-1,-1, 8,-1,-1,-1,-1,-1,-1,-1,-1

    assert(sa.SA.sameElements(Array(
      19,12,10,4,-1,15,1,17,-1,-1,-1,8,-1,-1,-1,-1,-1,-1,-1,-1
    )))
    
    sa.induceSAl()

    assert(sa.SA.sameElements(Array(
      19,12,10,4,11,15,1,17,18,3,-1,8,16,2,14,0,7,-1,9,6
    )))
    
    sa.induceSAs()

    assert(sa.SA.sameElements(Array(
      19,10,12,4,11,15,1,17,18,3,13,8,16,2,14,0,7,5,9,6
    )))
    

    // buildStep2()
    sa.fillSAWithLMS()

    assert(sa.SA.sameElements(Array(
      19,10,12,4,15,1,17,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    )))

    // println( sa.SA.mkString(","))

    val (names_count,reduced_string) = sa.calcLexNames(sa.lmsCount)

    assert(reduced_string.sameElements(Array(5,3,7,1,2,4,6,0)))
  }

  test("IntArrayWrapper.test") {
    val d = Array(9,28,21,12,7,16,17,10,6,27,12,25,14,26,10,6,26,21,24,4,14,17,19,7,10,11,10,7,11,3,15,24,10,2,28,2,18,4,19,4,28,16,16,17,22,15,7,21,13,26,27,20,22,19,19,21,2,10,12,22,2,2,7,4,25,15,22,16,23,25,20,24,4,26,24,3,5,1,10,10,3,3,14,1,28,2,14,2,22,20,2,1,21,28,20,1,4,15,28,5,17,23,24,12,27,24,10,3,19,12,12,10,27,13,4,3,1,6,12,11,2,21,25,21,19,17,23,11,11,10,11,20,2,7,7,6,13,20,10,22,5,13,10,10,13,14,5,3,27,21,3,28,27,11,1,14,6,18,19,21,19,12,26,3,25,13,2,10,23,19,23,28,25,20,9,26,7,20,14,4,2,25,27,2,7,11,5,1,6,17,21,10,17,26,23,18,11,2,15,13,9,24,10,28,19,4,2,11,13,12,6,11,4,7,14,10,18,26,11,1,10,10,21,3,18,21,5,2,5,3,22,12,25,14,21,13,22,12,7,16,11,19,11,5,27,14,24,13,5,27,21,27,1,19,3,1,22,5,23,28,21,6,9,27,26,10,7,7,9,24,2,10,12,18,10,22,11,26,4,12,22,2,13,1,24,10,5,25,25,13,7,20,13,2,11,25,22,1,7,2,1,23,13,7,12,13,4,28,14,3,14,4,23,14,14,11,26,15,27,20,10,23,24,19,22,26,19,18,20,28,27,28,20,2,16,4,20,4,10,13,4,21,10,7,18,27,23,5,5,28,13,20,5,23,15,27,21,21,26,1,18,21,1,5,13,22,22,13,4,2,26,25,10,6,12,21,16,18,25,25,15,23,21,17,26,19,4,5,13,13,4,3,14,25,15,2,7,9,28,5,17,13,10,26,22,11,18,27,15,5,26,23,13,11,20,24,17,27,23,7,14,2,14,12,20,1,1,24,18,26,4,11,4,18,6,12,23,24,9,27,7,20,26,24,18,12,2,19,3,19,23,22,9,28,17,10,12,26,16,27,28,15,6,25,9,27,17,24,17,20,18,18,6,17,11,3,22,27,20,15,27,2,6,2,14,18,6,17,10,11,21,21,6,12,25,22,15,1,18,27,21,28,13,2,23,26,14,1,27,21,21,1,26,20,2,6,26,15,25,9,24,20,13,28,22,24,2,21,26,5,2,27,6,12,6,10,15,14,7,22,19,27,27,22,24,5,2,15,23,4,5,1,21,2,23,25,7,18,4,16,13,26,2,25,13,16,12,25,15,11,16,25,1,21,19,26,18,28,25,14,19,24,18,27,18,27,26,21,23,26,19,16,13,16,28,27,27,25,17,20,22,4,28,17,12,9,24,5,14,23,25,6,10,3,1,13,18,21,20,24,19,16,1,6,21,4,23,21,7,11,22,6,3,2,17,26,6,10,17,21,3,16,21,24,16,22,3,26,13,16,5,22,11,15,2,20,23,20,22,12,24,13,27,10,5,21,4,22,21,11,12,20,28,10,6,25,25,27,4,21,18,23,19,1,28,24,15,11,24,20,7,22,3,11,18,22,27,12,3,16,5,1,21,15,9,25,24,7,10,1,15,13,15,17,19,20,21,20,3,1,17,7,5,20,24,17,4,19,25,28,16,28,10,23,24,10,3,5,27,11,20,23,3,7,3,9,28,10,23,21,2,16,23,27,12,21,12,5,20,27,12,16,5,20,20,5,16,13,17,4,12,2,4,22,10,19,20,27,19,12,16,28,1,23,19,16,21,17,17,21,1,3,14,19,25,4,11,16,1,19,24,21,21,14,20,17,4,7,12,19,18,25,24,20,28,6,17,3,3,23,1,28,3,4,7,18,1,10,7,18,4,24,2,6,3,12,4,28,6,7,16,23,22,14,28,24,4,17,28,18,9,25,19,6,7,15,24,24,10,1,24,4,15,10,17,25,17,3,17,3,2,6,25,17,17,12,18,19,19,14,22,26,7,1,1,11,10,6,27,14,18,16,17,20,20,10,16,14,3,23,10,14,28,5,21,20,25,27,24,16,3,28,10,16,28,6,7,7,12,11,5,22,7,12,19,10,1,19,7,13,1,10,22,19,12,10,22,28,12,12,5,10,5,25,15,17,13,21,24,19,7,5,17,9,28,16,19,13,25,28,25,3,4,3,19,21,18,20,25,15,11,13,4,25,22,11,2,6,15,6,21,16,1,4,5,25,24,7,5,19,18,21,6,7,7,20,17,16,16,25,23,4,27,19,15,24,12,2,10,27,7,6,15,16,22,7,1,26,12,13,19,24,6,8,0)
    def checkOn(d:Array[Int]) {
      //val sa = new SAISIntBuilder(new IntArrayWrapper(d),d.max+1)
      val sa = new SAISBuilder(d map {_.toByte})
      sa.build()
      testSA(sa.S.data.map{_.toInt},sa.SA,verbose=false)

      import SAIS._
      var javasa  = new Array[Int](d.size)
      sais.suffixsort(d map {_.toByte}, javasa , d.size)
      testSA(d,javasa,verbose=false)

      for ( i <- 0 until sa.SA.length) {
        if (javasa(i) != sa.SA(i)) {
          assert(false,"Bad differ on %d %d != %d".format(i,javasa(i),sa.SA(i)))
        }
      }
    }
    val badArray = d.slice(471,490) :+ 0
    
    checkOn(badArray)
    checkOn(d)
    
  }
  
}




class ReTest extends FunSuite with RandomGenerator {
  /*
  def testRe(s:String,what:String) {
    var x = ReParser.parseItem(s)
    val t = x.nfa.dotDump
    assert(t == what,"Re " + s + " wait\n----\n" + what + "\n-----\nReceive:\n"+t)
  }
  test("nfa creation") {
    testRe("abc","""digraph graphname {
0 -> 2  [label="b"]
2 -> 4  [label="c"]
4 -> 6  [label="eps"]
5 -> 0  [label="a"]
6 -> F  [label="eps"]
S -> 5  [label="eps"]
}
""")
    testRe("ab(d)c","""digraph graphname {
0 -> 2  [label="b"]
2 -> 4  [label="d"]
4 -> 6  [label="c"]
6 -> 8  [label="eps"]
7 -> 0  [label="a"]
8 -> F  [label="eps"]
S -> 7  [label="eps"]
}
""")
    testRe("a(bc)(de)","""digraph graphname {
0 -> 5  [label="eps"]
10 -> 12  [label="eps"]
11 -> 8  [label="d"]
12 -> 14  [label="eps"]
13 -> 0  [label="a"]
14 -> F  [label="eps"]
2 -> 4  [label="c"]
4 -> 6  [label="eps"]
5 -> 2  [label="b"]
6 -> 11  [label="eps"]
8 -> 10  [label="e"]
S -> 13  [label="eps"]
}
""")
    testRe("a(b|ce|klm)d","""digraph graphname {
0 -> 13  [label="eps"]
1 -> 14  [label="eps"]
10 -> 12  [label="m"]
12 -> 14  [label="eps"]
13 -> 1  [label="b"]
13 -> 4  [label="c"]
13 -> 8  [label="k"]
14 -> 16  [label="d"]
16 -> 18  [label="eps"]
17 -> 0  [label="a"]
18 -> F  [label="eps"]
4 -> 5  [label="e"]
5 -> 14  [label="eps"]
8 -> 10  [label="l"]
S -> 17  [label="eps"]
}
""")
    testRe("a(bbbb|cc(c|d)c)d","""digraph graphname {
0 -> 21  [label="eps"]
10 -> 12  [label="c"]
12 -> 17  [label="eps"]
13 -> 18  [label="eps"]
16 -> 18  [label="eps"]
17 -> 13  [label="c"]
17 -> 16  [label="d"]
18 -> 20  [label="c"]
2 -> 4  [label="b"]
20 -> 22  [label="eps"]
21 -> 10  [label="c"]
21 -> 2  [label="b"]
22 -> 24  [label="d"]
24 -> 26  [label="eps"]
25 -> 0  [label="a"]
26 -> F  [label="eps"]
4 -> 6  [label="b"]
6 -> 7  [label="b"]
7 -> 22  [label="eps"]
S -> 25  [label="eps"]
}
""")
    // testRe("a(b|c)+d","")
    // testRe("a(b|c)*d","")
    // testRe("a(b|c)?d","")
  }
}
class NFATest extends FunSuite with RandomGenerator {
  test("transitions1") {
    val s = new NfaStartState()
    val l1 = new NfaState()
    val l2 = new NfaState()
    val l3 = new NfaState()
    val l4 = new NfaState()
    val l5 = new NfaState()
    val l6 = new NfaState()
    s.epsilon(l1)
    s.epsilon(l2)
    l2.epsilon(l3)
    l3.epsilon(s)
    l1.link(l4,'b')
    l1.link(l5,'a')
    l2.link(l6,'a')
    l3.link(l6,'b')
    assert(s.epsilons.sameElements(Set(s,l1,l2,l3)))    
    // Map(98 -> Set(NfaBaseState(5), NfaBaseState(7)), 97 -> Set(NfaBaseState(6), NfaBaseState(7)))
    val et = s.epsilonTransitions
    assert(et('a').sameElements(Set(l5,l6)))
    assert(et('b').sameElements(Set(l4,l6)))
  }
  
  test("transitions2") {
    val s = new NfaStartState()
    val l1 = new NfaState()
    val l2 = new NfaState()
    val l3 = new NfaState()
    val l4 = new NfaState()
    val l5 = new NfaState()
    val l6 = new NfaState()
    val l7 = new NfaState()
    s.epsilon(l1)
    s.epsilon(l2)
    l2.epsilon(l3)
    l3.epsilon(s)
    l1.link(l4,'b')
    l1.link(l5,'a')
    l2.link(l6,'a')
    l3.link(l6,'b')
    l6.link(l7,'c')
    assert(NFA.epsilons(Set(s,l2)).sameElements(s.epsilons))
    val et = NFA.epsilonTransitions(Set(s,l2,l6))
    assert(et('a').size==2)
    assert(et('b').size==2)
    assert(et('c').size==1)
    
  }
  */
}

class KMP extends FunSuite with RandomGenerator {
  test("kmp creation") 
  {
    KMPBuffer.init(randomAlphanumericString(1025).getBytes)
  }
}



class MergerTest extends FunSuite {
    import java.io.File

    class BWTLoader(f:File,bigEndian:Boolean=true) {
      val in = new java.io.FileInputStream(f)
      val inb = new java.io.DataInputStream(in)
      val size = if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
      val eof =  if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
      val b   = new Array[Byte](size.toInt)
      val rdn = inb.read(b)
      assert(rdn==size)
      inb.close
      in.close
      def == (that: BWTLoader): Boolean = size == that.size && eof == that.eof && b.sameElements(that.b)
    }

    class AUXLoader(f:File,bigEndian:Boolean=true) {
      val in = new java.io.FileInputStream(f)
      val inb = new java.io.DataInputStream(in)
      val occ   = new Array[Long](BWTMerger2.ALPHA_SIZE)
      
      for (i<-0 until occ.length) {
        occ(i) = if ( ! bigEndian ) java.lang.Long.reverseBytes(inb.readLong()) else inb.readLong()
      }

      inb.close
      in.close
      def == (that: AUXLoader): Boolean = occ.sameElements(that.occ)
    }

    test("FileBWTReader") {
      val r = new FileBWTReader("testdata/test.txt")
      var d = new Array[Byte](10)
      val n = r.copyReverse(d)
      assert(n == d.length,"n = %d != %d  ".format(n,d.length))
      val ts = d.reverse.map {_.toChar}  mkString("")
      assert(ts == "uexmskeany")
      
      val n2 = r.copyReverse(d)
      assert(n2 == d.length,"n = %d != %d  ".format(n2,d.length))
      val ts2 = d.reverse.map {_.toChar}  mkString("")
      
      assert(ts2 == "tsljkujjpz" , "ts2 = " + ts2)
    }

    test("FileBWTReader.small") {
      val r = new FileBWTReader("testdata/small.txt")
      var d = new Array[Byte](15)
      val n = r.copyReverse(d)
      assert(n == 10,"n = %d != %d  ".format(n,10))

      val ts = d.map { _.toChar }.slice(5,15).reverse mkString("")
      assert(ts == "missisippi" , "ts2 = " + ts)
      assert(r.isEmpty)
    }
    test("BWTMerger2 small file") {
      val r = new FileBWTReader("testdata/test1024.txt")
      val bm = new BWTMerger2(1024)
      val (of,af) = bm.merge(r)
      val bl = new BWTLoader(of)
      val tbl = new BWTLoader(new File("testdata/test1024.cmp.bwt"),false)
      assert(bl == tbl)
      val al = new AUXLoader(af)
      val tal = new AUXLoader(new File("testdata/test1024.cmp.aux"),false)
      assert(al == tal)
    }

    test("BWTMerger2.calcSA") {
      val bm = new BWTMerger2(1024)
      val d1 = "barbaarbadaacarb".reverse.getBytes
      val occ = bm.calcOcc(d1)
      assert(occ.view.slice(95,102).sameElements(Array(0,0,7,4,1,1,0)))
      val sa = bm.calcSA(d1)
      assert(sa.sameElements(Array(10,4,14,7,11,2,5,15,8,12,0,3,6,9,13,1)))
      val newRank0 = sa.indexOf(0)
      assert(newRank0==10)
    }

    test("BWTMerger2.remapAlphabet") {
      val bm = new BWTMerger2(1024)
      val d1 = "barbaarbadaacarb".getBytes
      val d2 = "abaacarbadacarba".getBytes
      val sa = bm.calcSA(d2)
      val gtTn = bm.calcGtTn(sa.indexOf(0),sa)
      assert(gtTn.mkString(",")=="1,3,4,5,6,7,8,9,10,11,12,13,14")
      val gtEof = bm.computeGtEof(d1,d2,gtTn)
      assert(gtEof.mkString(",") == "0,1,2,3,5,6,7,8,9,11,12,13,14,15")
      val (remapped,asize) = bm.remapAlphabet(d1,gtTn)
      assert(remapped.sameElements(Array(3,1,6,3,1,1,6,3,1,5,1,1,4,1,6,2,0)))
      assert(asize == 7)
    }

    test("BWTMerger2.remapAlphabet2") {
      val bm = new BWTMerger2(1024)
      val (remapped,asize) = bm.remapAlphabet(
        Array[Byte](64,112,104,101,110,111,117,118,108,98,111,107,107,119,117,104,112,110,111,97,102,101,97,99,101,110,106,98,101,103,103,97,106,100,105,115,98,100,100,120,106,110,106,117,118,98,111,122,101,120,119,114,107,99,98,118,116,107,110,113,107,101,105,118,122,110,107,115,101,106,122,100,102,117,106,98,109,115,101,106,103,102,109,122,101,99,114,119,102,-1,103,97,105,115,116,110,116,112,98,113,99,99,109,109,110,119,113,109,115,121,110,103,119,106,109,122,98,103,106,108,110,113,122,107,102,-1,120,119,102,113,98,109,99,115,102,-1),
        BitSet(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,20,21,23,24,25,26,28,29,30,32,33,34,35,37,38,39,40,41,42,43,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,92,93,94,95,96,97,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,117,118,119,120,121,122,123,124,125,126,127,128,129,131,132,133,134,135)
      )
      assert(asize==30)
      assert(remapped.mkString(",")=="1,17,9,6,15,16,22,23,13,3,16,12,12,24,22,9,17,15,16,2,7,6,2,4,6,15,11,3,6,8,8,2,11,5,10,20,3,5,5,25,11,15,11,22,23,3,16,27,6,25,24,19,12,4,3,23,21,12,15,18,12,6,10,23,27,15,12,20,6,11,27,5,7,22,11,3,14,20,6,11,8,7,14,27,6,4,19,24,7,29,8,2,10,20,21,15,21,17,3,18,4,4,14,14,15,24,18,14,20,26,15,8,24,11,14,27,3,8,11,13,15,18,27,12,7,29,25,24,7,18,3,14,4,20,7,28,0")
    }

    test("BWTMerger2 test2048 occ searcher") {
      val r = new FileBWTReader("testdata/test2048.txt")
      val bm = new BWTMerger2(1024)      
      val t1v = Array[Byte](110,121,122,112,114,122,111,105,97,98,113,103,97,110,106,104,107,104,112,97,119,120,104,121,111,111,117,107,105,101,109,117,101,107,117,106,107,105,116,119,97,112,109,100,116,103,103,107,107,106,97,99,116,115,114,102,113,98,102,106,114,109,118,102,103,122,102,97,120,114,98,113,110,116,120,104,121,113,100,122,102,111,105,118,102,104,101,108,100,118,97,110,118,117,122,112,111,102,103,98,113,116,121,114,113,98,118,97,111,117,110,121,117,110,113,110,104,119,114,118,118,108,122,113,100,103,116,114,112,101,119,118,98,97,113,116,115,109,102,119,122,105,105,122,101,98,111,103,109,109,101,111,105,108,102,115,117,117,112,103,105,115,98,110,113,106,114,118,122,101,97,107,103,101,105,104,97,102,104,110,105,107,115,120,111,119,111,114,116,106,117,108,109,119,106,120,120,115,103,101,118,116,104,101,116,98,107,98,118,104,114,104,98,113,121,104,116,115,117,110,104,120,107,111,106,118,118,98,109,115,98,108,105,105,121,111,113,118,115,119,97,100,115,110,109,113,120,112,103,98,108,118,102,116,115,103,99,116,113,98,122,99,109,122,97,102,116,118,106,116,111,114,108,119,111,101,99,103,118,110,109,103,107,104,112,114,105,115,106,102,104,106,113,103,104,107,103,99,98,103,118,107,109,111,120,104,109,109,97,121,109,102,122,115,121,97,118,108,102,116,115,106,107,100,104,113,100,113,112,117,112,104,98,107,116,106,104,111,108,121,115,117,113,120,113,98,117,119,110,118,112,122,99,98,120,111,114,117,100,110,119,116,101,118,99,98,106,100,109,111,117,116,98,100,114,117,97,109,114,99,114,107,109,121,99,108,99,119,103,120,105,109,107,100,102,111,110,114,103,98,115,114,110,106,110,100,116,122,111,110,112,113,112,115,97,105,111,106,120,99,107,114,106,121,109,111,109,109,112,117,120,111,100,118,109,97,107,111,120,116,103,107,119,106,99,120,120,104,115,115,98,109,121,113,116,118,110,100,107,103,105,98,110,117,116,111,104,99,108,114,110,113,97,105,113,121,112,102,102,109,113,100,110,111,102,117,119,118,103,116,116,121,107,99,103,102,98,121,104,115,105,111,101,121,109,112,109,116,104,116,121,110,109,121,115,122,107,121,97,108,105,102,117,115,116,121,99,114,113,110,121,122,111,103,110,107,115,114,104,109,105,118,103,100,118,102,104,99,110,102,106,104,109,112,118,107,122,114,100,110,105,106,109,105,112,107,114,116,122,122,122,116,100,99,103,108,117,101,103,103,108,98,106,119,113,121,105,112,110,104,120,99,100,118,121,119,112,117,112,112,122,116,99,103,113,119,105,108,105,122,121,112,116,116,104,110,117,111,105,108,114,119,109,112,118,115,108,110,98,104,119,112,110,108,118,98,110,105,105,99,107,104,106,118,111,100,118,102,98,122,106,113,107,103,115,98,105,97,119,105,98,117,117,105,97,107,119,109,115,98,100,119,102,113,103,114,101,100,116,101,118,117,117,112,114,116,120,111,120,119,117,111,98,109,108,116,107,100,119,98,113,103,98,97,101,112,110,114,121,104,104,106,118,107,113,111,99,103,122,122,114,109,116,120,108,101,111,115,119,105,115,122,114,107,115,99,102,110,106,105,109,121,122,98,121,101,115,118,120,116,115,114,110,98,116,105,110,121,119,104,120,101,106,119,113,111,113,100,112,100,98,103,104,120,97,99,116,117,111,103,115,99,117,113,97,108,104,108,116,114,110,114,100,98,109,116,102,101,110,104,104,117,99,111,99,107,97,99,105,101,102,108,110,117,117,121,114,104,109,101,118,101,97,99,118,109,114,109,111,108,121,103,113,113,122,116,100,107,107,113,104,103,98,107,122,114,98,105,105,115,113,119,100,98,98,114,115,111,110,112,116,102,118,114,116,102,115,104,101,114,105,105,106,101,113,101,112,100,105,97,121,102,122,100,107,100,98,114,112,115,100,101,109,111,115,112,99,102,114,119,120,119,120,111,101,99,102,103,97,109,103,102,104,106,99,118,102,116,107,118,115,121,111,107,113,112,101,101,113,107,122,112,111,106,100,109,105,121,112,112,113,117,99,100,105,118,98,113,102,109,112,114,102,120,119,101,122,104,110,120,116,98,121,110,101,103,103,114,122,100,110,106,107,117,119,100,105,97,111,99,118,118,122,103,117,107,119,104,117,105,105,109,100,101,103,103,99,99,112,113,118,112,104,116,98,104,113,108,109,120,114,99,115,103,103,117,113,119,108,114,114,98,109,100)
      val bs = Array[Long](0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30,78,114,152,186,223,269,315,360,390,432,458,505,548,592,631,677,723,762,810,848,892,926,955,990,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024,1024)
      
      val occ  = bm.calcOcc(t1v)
      val bs2 = bm.calcBs (occ)
      assert(bs2.sameElements(bs))
      val gtEof=BitSet(0,1,2,3,4,5,6,10,13,18,20,21,23,24,25,26,30,31,34,38,39,41,42,44,52,53,54,56,60,61,62,65,68,69,71,72,73,74,76,77,79,81,83,89,91,92,93,94,95,96,100,101,102,103,104,106,108,109,110,111,112,113,114,115,117,118,119,120,121,122,123,126,127,128,130,131,134,135,136,137,139,140,143,146,148,149,151,155,156,157,158,161,163,164,166,167,168,179,182,183,184,185,186,187,188,190,191,192,193,195,196,197,200,201,204,208,210,213,214,216,217,218,219,221,223,225,226,228,229,234,235,236,237,238,239,242,243,244,245,246,247,250,251,253,254,257,258,260,262,263,266,267,269,270,271,272,273,274,278,279,280,284,285,287,292,300,302,303,304,306,307,309,310,312,313,314,316,319,320,325,327,328,329,330,334,337,338,339,340,341,342,343,344,346,347,348,349,350,351,354,355,356,357,359,360,361,363,368,369,370,371,374,375,377,378,380,382,383,387,389,391,395,396,397,400,401,402,404,406,407,408,409,410,411,412,413,416,418,421,423,424,425,426,427,428,429,430,431,433,434,437,438,439,442,445,446,448,449,451,452,453,454,455,456,462,463,464,465,468,469,470,471,474,475,476,479,480,482,483,485,486,487,489,490,491,497,499,501,503,504,505,506,507,509,510,511,512,513,514,515,517,522,523,524,525,527,528,529,530,531,532,534,536,537,539,541,544,548,552,553,554,556,557,559,562,564,566,567,568,569,570,571,575,576,583,584,585,587,588,590,593,594,595,596,597,598,599,600,601,604,605,609,610,611,612,613,615,616,617,619,620,621,622,623,624,625,626,627,630,631,632,633,634,636,643,644,646,649,651,654,658,661,662,666,667,668,671,673,675,678,680,681,682,683,684,685,686,687,688,689,690,691,693,694,695,698,700,705,706,707,708,712,714,715,718,719,720,721,722,723,726,727,728,730,731,732,734,737,740,741,742,744,746,747,748,749,750,751,752,754,756,757,758,760,763,764,765,766,768,773,776,777,778,780,782,783,787,788,789,790,791,794,795,798,801,803,811,812,813,814,815,816,818,820,824,825,826,827,828,829,830,832,833,834,835,839,844,845,849,850,851,855,856,857,858,859,860,862,863,864,866,869,874,876,880,882,887,888,889,892,893,894,895,898,899,900,901,902,903,909,915,917,919,920,921,922,924,925,928,930,931,932,935,937,938,939,940,941,945,947,949,950,951,953,954,956,958,959,960,962,963,967,968,970,973,974,978,980,981,982,984,986,988,991,998,999,1000,1001,1003,1006,1007,1008,1009,1010,1012,1015,1016,1017,1018,1019,1020,1022)
      val (bwt,searcher,rankFirst,rankLast) = bm.calcSAStatistic(t1v,bs,gtEof)

      val s_d = searcher.cf('d')
      val e_d = searcher.cf('e')

      //println(bwt.toList.slice(0,1007).count(_.toChar=='j'))
      //println(searcher.occtable.slice(s_d,e_d).mkString(","))
      
      assert(searcher.occ('j',130) == 3 ) 
      assert(searcher.occ('j',131) == 4 ) 
      assert(searcher.occ('j',132) == 5 ) 
      assert(searcher.occ('j',133) == 5 ) 
      assert(searcher.occ('j',600) == 16 ) 

      assert(searcher.occ('j',954) == 29 ) 
      assert(searcher.occ('j',968) == 30 ) 
      assert(searcher.occ('j',1007) == 30 ) 
      
      assert(searcher.occ('d',30) == 0 ) 
      assert(searcher.occ('d',31) == 0 )
      assert(searcher.occ('d',32) == 1 )
      assert(searcher.occ('d',998) == 36 )
      assert(searcher.occ('d',999) == 37 )
      assert(searcher.occ('d',1000) == 37 ,"searcher.occ('d',1000) != 37 but %d".format(searcher.occ('d',1000)))

      assert(searcher.occ('d',1004) == 37,"searcher.occ('d',1004) != 37 but %d".format(searcher.occ('d',1004)))
      assert(searcher.occ('q',730) == 31,"searcher.occ('q',730) != 31 but %d".format(searcher.occ('q',730)))
      assert(searcher.occ('t',661) == 25)
      assert(searcher.occ('e',411) == 13)
      
      assert(searcher.occ(0xff.toByte,411) == 0)
    }
    test("BWTMerger2 test2048 occ 0xff searcher") {
      val r = new FileBWTReader("testdata/test2048.txt")
      val bm = new BWTMerger2(1024)      
      val t1v = Array[Byte](110,121,122,-1,114,122,111,105,97,98,113,103,97,110,106,104,107,104,112,97,119,120,104,121,111,111,117,107,105,101,109,117,101,107,117,106,107,105,116,119,97,112,109,100,116,103,103,107,107,106,97,99,116,115,114,102,113,98,102,106,114,109,118,102,103,122,102,97,120,114,98,113,110,116,120,104,121,113,100,122,102,111,105,118,102,104,101,108,100,118,97,110,118,117,122,112,111,102,103,98,113,116,121,114,113,98,118,97,111,117,110,121,117,110,113,110,104,119,114,118,118,108,122,113,100,103,116,114,112,101,119,118,98,97,113,116,115,109,102,119,122,105,105,122,101,98,111,103,109,109,101,111,105,108,102,115,117,117,112,103,105,115,98,110,113,106,114,118,122,101,97,107,103,101,105,104,97,102,104,110,105,107,115,120,111,119,111,114,116,106,117,108,109,119,106,120,120,115,103,101,118,116,104,101,116,98,107,98,118,104,114,104,98,113,121,104,116,115,117,110,104,120,107,111,106,118,118,98,109,115,98,108,105,105,121,111,113,118,115,119,97,100,115,110,109,113,120,112,103,98,108,118,102,116,115,103,99,116,113,98,122,99,109,122,97,102,116,118,106,116,111,114,108,119,111,101,99,103,118,110,109,103,107,104,112,114,105,115,106,102,104,106,113,103,104,107,103,99,98,103,118,107,109,111,120,104,109,109,97,121,109,102,122,115,121,97,118,108,102,116,115,106,107,100,104,113,100,113,112,117,112,104,98,107,116,106,104,111,108,121,115,117,113,120,113,98,117,119,110,118,112,122,99,98,120,111,114,117,100,110,119,116,101,118,99,98,106,100,109,111,117,116,98,100,114,117,97,109,114,99,114,107,109,121,99,108,99,119,103,120,105,109,107,100,102,111,110,114,103,98,115,114,110,106,110,100,116,122,111,110,112,113,112,115,97,105,111,106,120,99,107,114,106,121,109,111,109,109,112,117,120,111,100,118,109,97,107,111,120,116,103,107,119,106,99,120,120,104,115,115,98,109,121,113,116,118,110,100,107,103,105,98,110,117,116,111,104,99,108,114,110,113,97,105,113,121,112,102,102,109,113,100,110,111,102,117,119,118,103,116,116,121,107,99,103,102,98,121,104,115,105,111,101,121,109,112,109,116,104,116,121,110,109,121,115,122,107,121,97,108,105,102,117,115,116,121,99,114,113,110,121,122,111,103,110,107,115,114,104,109,105,118,103,100,118,102,104,99,110,102,106,104,109,112,118,107,122,114,100,110,105,106,109,105,112,107,114,116,122,122,122,116,100,99,103,108,117,101,103,103,108,98,106,119,113,121,105,112,110,104,120,99,100,118,121,119,112,117,112,112,122,116,99,103,113,119,105,108,105,122,121,112,116,116,104,110,117,111,105,108,114,119,109,112,118,115,108,110,98,104,119,112,110,108,118,98,110,105,105,99,107,104,106,118,111,100,118,102,98,122,106,113,107,103,115,98,105,97,119,105,98,117,117,105,97,107,119,109,115,98,100,119,102,113,103,114,101,100,116,101,118,117,117,112,114,116,120,111,120,119,117,111,98,109,108,116,107,100,119,98,113,103,98,97,101,112,110,114,121,104,104,106,118,107,113,111,99,103,122,122,114,109,116,120,108,101,111,115,119,105,115,122,114,107,115,99,102,110,106,105,109,121,122,98,121,101,115,118,120,116,115,114,110,98,116,105,110,121,119,104,120,101,106,119,113,111,113,100,112,100,98,103,104,120,97,99,116,117,111,103,115,99,117,113,97,108,104,108,116,114,110,114,100,98,109,116,102,101,110,104,104,117,99,111,99,107,97,99,105,101,102,108,110,117,117,121,114,104,109,101,118,101,97,99,118,109,114,109,111,108,121,103,113,113,122,116,100,107,107,113,104,103,98,107,122,114,98,105,105,115,113,119,100,98,98,114,115,111,110,112,116,102,118,114,116,102,115,104,101,114,105,105,106,101,113,101,112,100,105,97,121,102,122,100,107,100,98,114,112,115,100,101,109,111,115,112,99,102,114,119,120,119,120,111,101,99,102,103,97,109,103,102,104,106,99,118,102,116,107,118,115,121,111,107,113,112,101,101,113,107,122,112,111,106,100,109,105,121,112,112,113,117,99,100,105,118,98,113,102,109,112,114,102,120,119,101,122,104,110,120,116,98,121,110,101,103,103,114,122,100,110,106,107,117,119,100,105,97,111,99,118,118,122,103,117,107,119,104,117,105,105,109,100,101,103,103,99,99,112,113,118,112,104,116,98,104,113,108,109,120,114,99,115,103,103,117,113,119,108,114,114,98,109,100)
      val occ  = bm.calcOcc(t1v)
      val bs = bm.calcBs (occ)
      
      val gtEof=BitSet(0,1,2,3,4,5,6,10,13,18,20,21,23,24,25,26,30,31,34,38,39,41,42,44,52,53,54,56,60,61,62,65,68,69,71,72,73,74,76,77,79,81,83,89,91,92,93,94,95,96,100,101,102,103,104,106,108,109,110,111,112,113,114,115,117,118,119,120,121,122,123,126,127,128,130,131,134,135,136,137,139,140,143,146,148,149,151,155,156,157,158,161,163,164,166,167,168,179,182,183,184,185,186,187,188,190,191,192,193,195,196,197,200,201,204,208,210,213,214,216,217,218,219,221,223,225,226,228,229,234,235,236,237,238,239,242,243,244,245,246,247,250,251,253,254,257,258,260,262,263,266,267,269,270,271,272,273,274,278,279,280,284,285,287,292,300,302,303,304,306,307,309,310,312,313,314,316,319,320,325,327,328,329,330,334,337,338,339,340,341,342,343,344,346,347,348,349,350,351,354,355,356,357,359,360,361,363,368,369,370,371,374,375,377,378,380,382,383,387,389,391,395,396,397,400,401,402,404,406,407,408,409,410,411,412,413,416,418,421,423,424,425,426,427,428,429,430,431,433,434,437,438,439,442,445,446,448,449,451,452,453,454,455,456,462,463,464,465,468,469,470,471,474,475,476,479,480,482,483,485,486,487,489,490,491,497,499,501,503,504,505,506,507,509,510,511,512,513,514,515,517,522,523,524,525,527,528,529,530,531,532,534,536,537,539,541,544,548,552,553,554,556,557,559,562,564,566,567,568,569,570,571,575,576,583,584,585,587,588,590,593,594,595,596,597,598,599,600,601,604,605,609,610,611,612,613,615,616,617,619,620,621,622,623,624,625,626,627,630,631,632,633,634,636,643,644,646,649,651,654,658,661,662,666,667,668,671,673,675,678,680,681,682,683,684,685,686,687,688,689,690,691,693,694,695,698,700,705,706,707,708,712,714,715,718,719,720,721,722,723,726,727,728,730,731,732,734,737,740,741,742,744,746,747,748,749,750,751,752,754,756,757,758,760,763,764,765,766,768,773,776,777,778,780,782,783,787,788,789,790,791,794,795,798,801,803,811,812,813,814,815,816,818,820,824,825,826,827,828,829,830,832,833,834,835,839,844,845,849,850,851,855,856,857,858,859,860,862,863,864,866,869,874,876,880,882,887,888,889,892,893,894,895,898,899,900,901,902,903,909,915,917,919,920,921,922,924,925,928,930,931,932,935,937,938,939,940,941,945,947,949,950,951,953,954,956,958,959,960,962,963,967,968,970,973,974,978,980,981,982,984,986,988,991,998,999,1000,1001,1003,1006,1007,1008,1009,1010,1012,1015,1016,1017,1018,1019,1020,1022)
      val (bwt,searcher,rankFirst,rankLast) = bm.calcSAStatistic(t1v,bs,gtEof)

      val directIdx = bwt.indexOf(0xff.toByte)
      assert(directIdx==721)

      assert(searcher.occ(0xff.toByte,directIdx)==1 )
      assert(searcher.occ(0xff.toByte,directIdx-1)==0 )
      assert(searcher.occ(0xff.toByte,directIdx+1)==1 )
    }

    test("BWTMerger test2048.txt") {
        
        
        val r = new FileBWTReader("testdata/test2048.txt")
        val bm = new BWTMerger2(1024)
        val (of,af) = bm.merge(r)
        
        
        val bl = new BWTLoader(of)
        val tbl = new BWTLoader(new File("testdata/test2048.cmp.bwt"),false)
        assert(bl == tbl)
        
        
        val al = new AUXLoader(af)
        val tal = new AUXLoader(new File("testdata/test2048.cmp.aux"),false)
        assert(al == tal)
        
    }
    
    test("BWTMerger test3072.txt") {
        val r = new FileBWTReader("testdata/test3072.txt")
        val bm = new BWTMerger2(1024)
        val (of,af) = bm.merge(r)
        
        val bl = new BWTLoader(of)
        val tbl = new BWTLoader(new File("testdata/test3072.cmp.bwt"),false)
        assert(bl == tbl)
        
        val al = new AUXLoader(af)
        val tal = new AUXLoader(new File("testdata/test3072.cmp.aux"),false)
        assert(al == tal)
    }

    test("BWTMerger test.txt") {
      val r = new FileBWTReader("testdata/test.txt")
      val bm = new BWTMerger2(1024)
      val (of,af) = bm.merge(r)
      
      val bl = new BWTLoader(of)
      val tbl = new BWTLoader(new File("testdata/test.cmp.bwt"),false)
      assert(bl == tbl)
      
      val al = new AUXLoader(af)
      val tal = new AUXLoader(new File("testdata/test.cmp.aux"),false)
      assert(al == tal)      
    }

    test("BWTMerger test-part.txt") {
      val r = new FileBWTReader("testdata/test-part.txt")
      val bm = new BWTMerger2(1024)
      val (of,af) = bm.merge(r)
      
      val bl = new BWTLoader(of)
      val tbl = new BWTLoader(new File("testdata/test-part.cmp.bwt"),false)
      assert(bl == tbl)
      
      val al = new AUXLoader(af)
      val tal = new AUXLoader(new File("testdata/test-part.cmp.aux"),false)
      assert(al == tal)      
      
    }

    test("BWTMerger testdata/2048-0.txt second segment should have sa[0] = 0") {
      val r = new FileBWTReader("testdata/test2048-2.txt")
      val bm = new BWTMerger2(1024,debugLevel=0)
      val (of,af) = bm.merge(r)

      val bl = new BWTLoader(of)
      val tbl = new BWTLoader(new File("testdata/test2048-2.cmp.bwt"),false)
      assert(bl == tbl)
        
      val al = new AUXLoader(af)
      val tal = new AUXLoader(new File("testdata/test2048-2.cmp.aux"),false)
      assert(al == tal)      

    }

    test("KMPBuffer read long case") {
      /*
      emulate 
      [info] readBit pos=0 val=0
      [info] 06bc0000,29b34812,5090a849,90812111,a8080200,80800d41,12084888,0815124d,dc20f8dc,860820f8,0720b060,a8390595,c0001414,d6c66309,c219dace,7c000110,c330fbaf,e4fcdcdc,2f204585,121cc22c,
      stored_bits = 1787
      */
      val t = new Array[Byte](1024)
      val kmpIn = KMPBuffer.init(t)
      kmpIn.stored_bits = 1787
      val td =  Array[Int](0x06bc0000,0x29b34812,0x5090a849,0x90812111,0xa8080200,0x80800d41,0x12084888,0x0815124d,0xdc20f8dc,0x860820f8,0x0720b060,0xa8390595,0xc0001414,0xd6c66309,0xc219dace,0x7c000110,0xc330fbaf,0xe4fcdcdc,0x2f204585,0x121cc22c)
      for (i<- 0 until td.length ) {
        kmpIn.bit_buffer(i) = td(i)
      }
      val gt = kmpIn.readBit()
    }

}

class BWTCreatorTest extends FunSuite {
  
  test("fm for small.txt") {
      val r = new FileBWTReader("testdata/small.txt")
      val bm = new BWTMerger2(1024)
      val (bwtf,auxf) = bm.merge(r)

      val fc = new FMCreator(bwtf.getAbsolutePath,1024*1024)
      val fmf = fc.create()

      val bwt = new BWTLoader(bwtf)
      val a = bwt.readAll()
      
      val bwtd = (a.view.slice(0,bwt.eof.toInt)++Array[Byte](0)++a.view.slice(bwt.eof.toInt+1,a.length)).toArray
      val testd = bwtstring.bwt2occ(bwtd)
      
      val fm = new FMLoader(fmf)
      val d = fm.readAll()
  
      assert(d.sameElements(testd))
      
  }
  
  test("fm for test1024.txt") {
      val r = new FileBWTReader("testdata/test1024.txt")
      val bm = new BWTMerger2(1024)
      val (bwtf,auxf) = bm.merge(r)

      val fc = new FMCreator(bwtf.getAbsolutePath,1024)
      val fmf = fc.create()

      val bwt = new BWTLoader(bwtf)
      val a = bwt.readAll()
      val testd = bwtstring.bwt2occ((a.view.slice(0,bwt.eof.toInt)++Array[Byte](0)++a.view.slice(bwt.eof.toInt+1,a.length)).toArray)
      
      val fm = new FMLoader(fmf)
      val d = fm.readAll()

      assert(d.sameElements(testd))
  }

  test("fm for test2048.txt") {
      val r = new FileBWTReader("testdata/test2048.txt")
      val bm = new BWTMerger2(1024)
      val (bwtf,auxf) = bm.merge(r)

      val fc = new FMCreator(bwtf.getAbsolutePath,1024*1024)
      val fmf = fc.create()

      val bwt = new BWTLoader(bwtf)
      val a = bwt.readAll()
      val testd = bwtstring.bwt2occ((a.view.slice(0,bwt.eof.toInt)++Array[Byte](0)++a.view.slice(bwt.eof.toInt+1,a.length)).toArray)
      
      val fm = new FMLoader(fmf)
      val d = fm.readAll()

      assert(d.sameElements(testd))
  }

}

class DirBWTReaderTest extends FunSuite {
  /*
  test("DirBWTReader testdata/") {
    val r = new DirBWTReader("testdata/t1",debugLevel=0)
    var c = r.getByte
    assert(c==67,"c = " + c)
    c = r.getByte
    assert(c==67,"c = " + c)
    
    val t=new Array[Byte](1024*10)
    val n = r.copyReverse(t)
    
    assert(n==3073,"n != 3073 but %d".format(n))   
  }
  */
  /*
  test("DirBWTReader testdata/tbad") {
    val r = new DirBWTReader("testdata/tbad",debugLevel=0)
    var c = r.getByte
    
  }
  */
  /*
  test("DirBWTReader testdata/tbad") {
    val r = new DirBWTReader("testdata/tbad",debugLevel=0)
    val size = 1024
    var tot = 0
    for ( i <- 0 until 3) {
      printf("i=%d\n",i)
      val t1:Array[Byte] = new Array[Byte](size)
      val n = r.copyReverse(t1) 
      val t1v = t1.view.slice(size-n,t1.length).reverse
      val rr = r.reset
      for ( j <- 0 until tot) {
        rr.getByte
      }
      for ( j <- 0 until n) {
        assert(rr.getByte==t1v(j))
      }
      println("------------------")
      tot = tot + n
    }
  }
  */
}

class UtilTest extends FunSuite {
  test("isBinary") {
    assert(! Util.isBinary(new File("testdata/at.h")).getOrElse(true))
  }
}

class MergerTest2 extends FunSuite {

  class BWTLoader(f:File,bigEndian:Boolean=true) {
    val in = new java.io.FileInputStream(f)
    val inb = new java.io.DataInputStream(in)
    val size = if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
    val eof =  if (bigEndian) inb.readLong() else java.lang.Long.reverseBytes(inb.readLong())
    val b   = new Array[Byte](size.toInt)
    val rdn = inb.read(b)
    //assert(rdn==size)
    inb.close
    in.close
    def == (that: BWTLoader): Boolean = size == that.size && eof == that.eof && b.sameElements(that.b)
  }

  class AUXLoader(f:File,bigEndian:Boolean=true) {
    val in = new java.io.FileInputStream(f)
    val inb = new java.io.DataInputStream(in)
    val occ   = new Array[Long](BWTMerger2.ALPHA_SIZE)
    
    for (i<-0 until occ.length) {
      occ(i) = if ( ! bigEndian ) java.lang.Long.reverseBytes(inb.readLong()) else inb.readLong()
    }

    inb.close
    in.close
    def == (that: AUXLoader): Boolean = occ.sameElements(that.occ)
  }

  test("KMPBuffer2") {
    /*
    emulate 
    [info] readBit pos=0 val=0
    [info] 06bc0000,29b34812,5090a849,90812111,a8080200,80800d41,12084888,0815124d,dc20f8dc,860820f8,0720b060,a8390595,c0001414,d6c66309,c219dace,7c000110,c330fbaf,e4fcdcdc,2f204585,121cc22c,
    stored_bits = 1787
    */
    val t = new Array[Byte](1024)
    val kmpIn = KMPBuffer.init(t)
    kmpIn.stored_bits = 1787
    val td =  Array[Int](0x06bc0000,0x29b34812,0x5090a849,0x90812111,0xa8080200,0x80800d41,0x12084888,0x0815124d,0xdc20f8dc,0x860820f8,0x0720b060,0xa8390595,0xc0001414,0xd6c66309,0xc219dace,0x7c000110,0xc330fbaf,0xe4fcdcdc,0x2f204585,0x121cc22c)
    for (i<- 0 until td.length ) {
      kmpIn.bit_buffer(i) = td(i)
    }
    val gt = kmpIn.readBit()
  }
  


}

class StringPosReaderTest extends FunSuite {
  test("StringPosReader") {
    val rf = new java.io.RandomAccessFile("testdata/test1024.txt","r")
    val ts = new StringPosReader(rf,10,10)
    var s=""
    for (i<- 0 until 20) {
      s+=ts.next()
    }
    assert(s.length==20)
    assert("tsljkujjpzjerzfjyboh"==s)
  }
}

class LCPLoaderTest extends FunSuite {
  import Util.bwtstring._

  test("LCPLoader") {
    val r = new DirBWTReader("testdata/t2","testdata/t2",debugLevel=0)

    //val r = new StringBWTReader("mississippi",direct=true)
    val bm = new BWTMerger2(1024,debugLevel=0)
    val (bwtf,af) = bm.merge(r)
    val fmc = new FMCreator(bwtf.getAbsolutePath,1024*1024)
    val fmf = fmc.create()
    val fml = new FMLoader(fmf)
    val fm = fml.readAll()
    val bwtl = new BWTLoader(bwtf)
    val bwt = bwtl.readAll()
    val lcpc = new LCPCreator(bwtf.getAbsolutePath)
    val lcpf = lcpc.create()
    val lcpl = new LCPLoader(lcpf)

    val fcp = lcpl.readAll()
    val ccp = bwtFm2LCP(bwt,fm,fmc.bucketStarts,bwtl.eof.toInt)
    
    assert(fcp sameElements ccp.view.slice(0,fcp.length))
  }
}

class SALoaderTest extends FunSuite {
  import Util.bwtstring._

  test("SALoader") {
    val r = new DirBWTReader("testdata/t2","testdata/t2",debugLevel=0)

    //val r = new StringBWTReader("mississippi",direct=true)
    val bm = new BWTMerger2(1024,debugLevel=0)
    val (bwtf,af) = bm.merge(r)
    val fmc = new FMCreator(bwtf.getAbsolutePath,1024*1024)
    val fmf = fmc.create()
    val fml = new FMLoader(fmf)
    val fm = fml.readAll()
    val bwtl = new BWTLoader(bwtf)
    val bwt = bwtl.readAll()
    val sac = new SACreator(bwtf.getAbsolutePath)
    val saf = sac.create()
    val sal = new SALoader(saf)

    val fcp = sal.readAll()
    //println(fcp.mkString(","))
    val ccp = bwtFm2sa(bwt,fm,bwtl.eof.toInt)
    //println(ccp.mkString(","))
    assert(fcp sameElements ccp.view.slice(0,fcp.length))
  }
}


class BadTest extends FunSuite {
  import Util.bwtstring._
  
}

class CombinedIndexingTest extends FunSuite {
  import Util.bwtstring._

  test ("test1024.txt") {
    val dir = "testdata/test1024.txt"
    var selfTest:Boolean = true
    var createFM = true
    val i = 1
    import java.io.File

    val r = if ( (new File(dir)).isDirectory ) 
        new DirBWTReader(dir,"testdata/include",debugLevel=0,caching=true) else 
        new FileBWTReader(dir)
    
    val bm = new BWTMerger2(1024*i,debugLevel=0)
    val (of,af) = bm.merge(r)

    if ( createFM ) {
        val fm = new FMCreator(of.getAbsolutePath,1024*1024*i)
        fm.create()        
    }

    if ( selfTest ) {
        val bwtl = new BWTLoader(of)
        assert(bwtl.eof.toInt == 462,"BWT Eof=%d realvalue - 462\n".format(bwtl.eof.toInt))

        val sa = new NaiveFMSearcher(of.getAbsolutePath)
        assert(bwtl.read(0).toChar == 'u')
        assert(bwtl.read(1).toChar == 'b')
        assert(bwtl.read(2).toChar == 'x')
        assert(bwtl.read(bwtl.eof.toInt) == 0)
        assert(sa.getPrevI(bwtl.eof.toInt) == 0)
        assert(bwtl.read(sa.getPrevI(bwtl.eof.toInt)).toChar == 'u' ) // First in file
        assert(sa.getNextI(bwtl.eof.toInt)==517)

        assert(bwtl.read(sa.getNextI(bwtl.eof.toInt)).toChar=='l') // Last in file

        assert(sa.getPrevI(1)==48)
        assert(sa.getPrevI(48) == 649)
        assert(sa.nextSubstr(1,3)=="haa")
        assert(bwtl.read(1000).toChar=='b')
        assert(sa.nextSubstr(sa.getNextI(bwtl.eof.toInt),100)=="zajrtzbeqwbxdfpwjflmmsseewuudgfbtzqenjqafwzcnfanycigwsflfvxojxpqhhzekjdkhgsptqveavquuoqujbezdkarayom")
        assert(sa.nextSubstr(bwtl.eof.toInt,100)=="ajrtzbeqwbxdfpwjflmmsseewuudgfbtzqenjqafwzcnfanycigwsflfvxojxpqhhzekjdkhgsptqveavquuoqujbezdkarayoml")
        assert(sa.prevSubstr(1,5)=="bqxxa")
        assert(sa.prevSubstr(bwtl.eof.toInt,5)=="\0uexm") // StartFile + \0
        assert(sa.prevSubstr(sa.getPrevI(bwtl.eof.toInt),4)=="uexm")
    }

  }
}