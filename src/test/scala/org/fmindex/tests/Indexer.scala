package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._


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
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])

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
  test("sais builder") {
    val b = Array[Byte](97,115,100,10,97,115,100,10,-1,97,115,100,10,98,101,108,107,97,64,98,101,108,107,97,45,104,111,109,101,58,47,116,109,112,47,116,36,32,99,97,116,32,62,32,116,50,46,116,120,116,10,97,115,100,97,115,100,10,-1,0)
    val k = new SAISBuilder(b)
    k.build
  }
  
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
  test("plain searching") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    sa.build
    sa.buildOCC

    sa.search("bra".getBytes()) match {
      case None => assert(false,"bra not found")
      case Some((6,8)) => // OK
      case _ => assert(false ,"Bad answer")
    }
  }
  test("BWT walki") {
    val sa = new SAISBuilder(fromString("abracadabra"))
    sa.build
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
    sa.build
    sa.buildOCC
    //sa.printSA()
    // cadabra
    assert(sa.nextSubstr(6,4)=="bra\0","sa.nextSubstr(6,4)="+sa.nextSubstr(6,4))
    assert(sa.prevSubstr(6,4)=="cada","sa.nextSubstr(6,4)="+sa.prevSubstr(6,4))

  }
  test("BWT substrings2") {
    val sa = new SAISBuilder(fromString("mmabcacadabbbca".reverse))
    sa.build
    sa.buildOCC
    //sa.printSA()
    assert(sa.nextSubstr(11,3) == "cba",sa.nextSubstr(11,3))
    assert(sa.prevSubstr(11,3) == "aca",sa.prevSubstr(11,3))
  }
  test("getPrevRange") {
    val sa = new SAISBuilder(fromString("mmabcacadabbbca".reverse))
    sa.build
    sa.buildOCC
    //sa.printSA()

    assert (sa.occ('b',6)==3)
    assert (sa.getPrevRange(0,16,'a')==Some((1,6)) )
    assert (sa.getPrevRange(1,6,'b')==Some((6,8)) )
  }
}


class DFATests extends FunSuite with RandomGenerator {

  def `ab*c` = {
    val s = new StartState()
    val a = new State("a")
    val b = new State("b")
    val f = new FinishState()
    s.link(a,'a')
    a.link(b,'b')
    b.link(b,'b')
    b.link(f,'c')
    s
  }
  def `[cdfklm]b*c` = {
    val s = new StartState()
    val a = new State("a")
    val b = new State("b")
    val f = new FinishState()
    s.link(a,'c')
    s.link(a,'d')
    s.link(a,'f')
    s.link(a,'m')
    s.link(a,'k')
    s.link(a,'l')
    a.link(b,'b')
    b.link(b,'b')
    b.link(f,'c')
    s
  }
  def dfa1 ={
    val s = new StartState()
    val a = new State("a")
    val b = new State("b")
    val f = new FinishState()
    s.link(a,'c')
    s.link(a,'d')
    s.link(a,'f')
    s.link(a,'m')
    s.link(a,'l')
    s.link(a,0xfa)
    s.link(a,0xfb)
    s.link(a,0xfc)
    s.link(a,0xfd)
    s.link(a,0xfe)
    s.link(a,0xff)
    

    a.link(b,'b')
    b.link(b,'b')
    b.link(f,'c')
    s
  }
  test("basic dfa search") {
    // ab.*c
    val dfa = DFA.processLinkList(`ab*c`)
    assert(dfa.matchString("absbc")==false)
    assert(dfa.matchString("abbc"))
    assert(dfa.matchString("abc"))
  }
  test("dfa2") {
    /*
    [info] (a,97,b)
    [info] (b,100,f)
    [info] (a,100,f)
    [info] (s,97,a)
    [info] (b,97,b)
    */
    val s = new StartState()
    val a = new State("a")
    val b = new State("b")
    val f = new FinishState()
    s.link(a,'a')
    a.link(b,'a')
    a.link(f,'d')
    b.link(f,'d')
    b.link(b,'a')
    val dfa = DFA.processLinkList(s)
  }
  test("compileBuckets") {
    val dfa = DFA.processLinkList(`ab*c`)
    var m = dfa.buckets
    //dfa.dumpBuckets()
    assert(m.length == 4)
    assert(m(0).mkString(",")=="DFAChar('a'->1)")
    assert(m(1).mkString(",")=="DFAChar('b'->2)")
    assert(m(2).mkString(",")=="DFAChar('b'->2),DFAChar('c'->3)")
    assert(m(3).mkString(",")=="")
    val dfa2 = DFA.processLinkList(`[cdfklm]b*c`)
    var m2 = dfa2.buckets
    assert(m2.length == 4)
    assert(m2(0).mkString(",")=="DFABucket('c-d' ->1),DFAChar('f'->1),DFABucket('k-m' ->1)","movesBucket="+m(0).mkString(","))
    assert(m2(1).mkString(",")=="DFAChar('b'->2)")
    assert(m2(2).mkString(",")=="DFAChar('b'->2),DFAChar('c'->3)")
    assert(m2(3).mkString(",")=="")    
    val dfa3 = DFA.processLinkList(dfa1)
    assert(dfa3.buckets(0).mkString(",")=="DFABucket('c-d' ->1),DFAChar('f'->1),DFABucket('l-m' ->1),DFABucket('\\xfa-\\xff' ->1)")
  }
  test("match SA basics") {
    val sa = new SAISBuilder(fromString("mmabcacadabbbca".reverse))
    sa.build
    sa.buildOCC
    //sa.printSA()
    val dfa = DFA.processLinkList(`ab*c`)
    val results = dfa.matchSA(sa)
    assert(results.size == 2)
    assert(results(0).toString=="cbbba")
    assert(results(1).toString=="cba")
    assert(sa.nextSubstr(results(1).sp,results(1).len)=="cba","sa.nextSubstr(results(1).sp,results(1).len)="+sa.nextSubstr(results(1).sp,results(1).len))
    assert(sa.nextSubstr(results(0).sp,results(0).len)=="cbbba")
  }
  test("match SA basics - abit larger string") {
    val sa = new SAISBuilder(fromString("2b2w9vzrtqy3vzclgoofxgz9nal81y1fg8rozxkb5aaep1vpafp3cgsumc0z1rhpatcwo4d7nxc751h3a4woj3dbjf6ynfbkoom8sxoc9t3dqzkfs9akc6cmsy7cndi6bf116fju5rcsysixgkaih4zbkl8qo3ko2c42f34x6cqdew8x2jgz36r4bskabx02lxbfzokc".reverse))
    sa.build
    sa.buildOCC
    val dfa = DFA.processLinkList(`ab*c`)
    val results = dfa.matchSA(sa)    
    assert(results.size == 0)
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
class BadTest extends FunSuite with RandomGenerator {
  test("bwt merge") {
    val sa1 = new SAISBuilder(fromString("bracaabraba@"))
    val sa2 = new SAISBuilder(fromString("abracadabra@"))
    val sa3 = new SAISBuilder(fromString("bracaabraba@abracadabra@"))
    sa1.build()
    sa2.build()
    sa3.build()
    sa1.printSA()
    sa2.printSA()
    sa3.printSA()
  }
}
