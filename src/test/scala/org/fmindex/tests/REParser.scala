package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._
import org.fmindex.re2._

class RE2Parser  extends FunSuite with RandomGenerator {
  test("re2post") {
    val r = REParser.re2post("abc")
    assert(r=="ab.c.","r="+r)
  }
  test("re2post2") {
    val r = REParser.re2post("a(bb)+a")
    assert(r=="abb.+.a.","r="+r)
  }
  test("re2post3") {
    val r = REParser.re2post("(a|b)")
    assert(r=="ab|","r="+r)
  }
  test("re2post4") {
    val r = REParser.re2post("((a|b)*aba*)*(a|b)(a|b)")
    
    assert(r=="ab|*a.b.a*.*ab|.ab|.","r="+r)
  }
  test("createNFA") {
    var nfa = REParser.createNFA("ab.c.")
    nfa match {
      case REParser.ConstState('a',out) => 
        nfa = out.s
      case _ => assert(false)
    }
    nfa match {
      case REParser.ConstState('b',out) => 
        nfa = out.s
      case _ => assert(false)
    }
    nfa match {
      case REParser.ConstState('c',out) => 
        nfa = out.s
      case _ => assert(false)
    }
    nfa match {
      case REParser.MatchState => 
      case _ => assert(false)
    }
  }
  
  test("orNFA") {
    var nfa = REParser.createNFA("ab|c.")

    var (out1,out2) = nfa match {
      case REParser.SplitState(out1,out2) => 
        (out1.s,out2.s)
      case _ => assert(false)
    }
    
    out1 = out1 match {
      case REParser.ConstState('a',out) => 
        out.s
      case _ => assert(false)
    }
    out2 = out2 match {
      case REParser.ConstState('b',out) => 
        out.s
      case _ => assert(false)
    }

    out1 = out1 match {
      case REParser.ConstState('c',out) => 
        out.s
      case _ => assert(false)
    }

    out2 = out2 match {
      case REParser.ConstState('c',out) => 
        out.s
      case _ => assert(false)
    }

    out1 match {
      case REParser.MatchState => 
      case _ => assert(false)
    }
  }
  test("plusNFA") {
    var out1 = REParser.createNFA("a+")


    var out2 = out1 match {
      case REParser.ConstState('a',out) => 
        out.s
      case _ => assert(false)
    }

    var (out3,out4) = out2 match {
      case REParser.SplitState(out1,out2) => 
        (out1.s,out2.s)
      case _ => assert(false)
    }
    
    out1 = out3 match {
      case REParser.ConstState('a',out) => 
        out.s
      case _ => assert(false)
        ???
    }
    
    out4 match {
      case REParser.MatchState => 
      case _ => assert(false)
    }
  }
  
  test("starNFA") {
    var out1 = REParser.createNFA("a*")

    var (out2,out3) = out1 match {
      case REParser.SplitState(out1,out2) => 
        (out1.s,out2.s)
      case _ => assert(false)
    }

    out2 = out2 match {
      case REParser.ConstState('a',out) => 
        out.s
      case _ => assert(false)
        ???
    }

    out3 match {
      case REParser.MatchState => 
      case _ => assert(false)
    }
    
    out2 match {
      case REParser.SplitState(out1,out2) => 
        (out1.s,out2.s)
      case _ => assert(false)
    }

  }

  test("questionNFA") {
    var out1 = REParser.createNFA("a?")

    var (out2,out3) = out1 match {
      case REParser.SplitState(out1,out2) => 
        (out1.s,out2.s)
      case _ => assert(false)
    }

    out2 = out2 match {
      case REParser.ConstState('a',out) => 
        out.s
      case _ => assert(false)
        ???
    }

    out3 match {
      case REParser.MatchState => 
      case _ => assert(false)
    }
    
    out2 match {
      case REParser.MatchState => 
      case _ => assert(false)
    }

  }

  test("matchString1") {
    var re1 = REParser.createNFA("ab.c.")
    assert(REParser.matchNFA(re1,"abc")==true)
    assert(REParser.matchNFA(re1,"atc")==false)
  }

  test("matchString2") {
    var re1 = REParser.createNFA("am|c.")
    assert(REParser.matchNFA(re1,"ac")==true)
    assert(REParser.matchNFA(re1,"mc")==true)
    assert(REParser.matchNFA(re1,"Xc")==false)
    assert(REParser.matchNFA(re1,"c")==false)
  }

  test("matchString3") {
    val re = "(a|m)c"
    assert(REParser.matchString(re,"ac")==true)
    assert(REParser.matchString(re,"mc")==true)
    assert(REParser.matchString(re,"Xc")==false)
    assert(REParser.matchString(re,"c")==false)
  }

  test("matchString4") {
    val re = "a*b?c+"
    assert(REParser.matchString(re,"abc")==true)
    assert(REParser.matchString(re,"bc")==true)
    assert(REParser.matchString(re,"bcc")==true)
    assert(REParser.matchString(re,"aaabc")==true)
  }

  test("match SA basics") {
    val sa = new SAISBuilder(new ByteArrayNulledWrapper("mmabcacamabbbca".getBytes.reverse))
    sa.build()
    sa.buildOCC
    var re1 = REParser.createNFA("ma.b.")
    val results = REParser.matchSA(re1,sa,debugLevel=0)
    assert(results(0).toString == "[2 Results] bam")
  }
  test("match SA basics2") {
    val sa = new SAISBuilder(new ByteArrayNulledWrapper("mmabcacamabbbca".getBytes.reverse))
    sa.build()
    sa.buildOCC
    var re1 = REParser.createNFA("ba|c.")
    val results = REParser.matchSA(re1,sa,debugLevel=0)
    assert(results.toString == "List(ca, [2 Results] cb)")
  }

  test("NaiveFMSearcher") {
    val r = new FileBWTReader("testdata/small2.txt")
    val bm = new BWTMerger2(1024)
    val (bwtf,auxf) = bm.merge(r)

    val fc = new FMCreator(bwtf.getAbsolutePath,1024)
    val fmf = fc.create()

    val fms = new NaiveFMSearcher(fmf.getAbsolutePath)

    // iiiimppssss
    assert(fms.pos2char(0).toChar=='i')
    assert(fms.pos2char(1).toChar=='i')
    assert(fms.pos2char(2).toChar=='i')
    assert(fms.pos2char(3).toChar=='i')
    assert(fms.pos2char(4).toChar=='m')
    assert(fms.pos2char(5).toChar=='p')
    assert(fms.pos2char(6).toChar=='p')
    assert(fms.pos2char(7).toChar=='s')
    assert(fms.pos2char(8).toChar=='s')
    assert(fms.pos2char(9).toChar=='s')
    assert(fms.pos2char(10).toChar=='s')

    /*
     0 -> 10. $missisippi
     1 ->  9. i$missisipp
     2 ->  6. ippi$missis
     3 ->  4. isippi$miss
     4 ->  1. issisippi$m
     5 ->  0. missisippi$
     6 ->  8. pi$missisip
     7 ->  7. ppi$missisi
     8 ->  5. sippi$missi
     9 ->  3. sisippi$mis
    10 ->  2. ssisippi$mi
    */
    
    // i,p,s,s,m,m,p,i,i,s,i

    // println(fms.bwt.readAll().map{_.toChar}.mkString(","))
    // 0,6,7,9,4,1,5,2,3,8
    // println(fms.fm.readAll().mkString(","))
    assert(fms.getNextI(0)==5)
    assert(fms.getNextI(5)==4)
    assert(fms.getNextI(4)==10)
    assert(fms.getNextI(10)==9)
    assert(fms.getNextI(9)==3)

    assert(fms.getPrevI(3)==9)
    assert(fms.getPrevI(9)==10)
    assert(fms.getPrevI(10)==4)
    assert(fms.bwt.read(4).toChar=='m')
    assert(fms.getPrevI(4)==5)
    assert(fms.getPrevI(5)==0)
    assert(fms.getPrevI(0)==1)
  }
  test("match SA FMindex") {
    val r = new FileBWTReader("testdata/test1024.txt")
    val bm = new BWTMerger2(1024)
    val (bwtf,auxf) = bm.merge(r)

    val fc = new FMCreator(bwtf.getAbsolutePath,1024)
    val fmf = fc.create()

    val sa = new NaiveFMSearcher(fmf.getAbsolutePath)
    //println(sa.bucketStarts.mkString(","))
    //assert(sa.pos2char(10).toChar)
    var re1 = REParser.createNFA("ba|d|e|c.")
    val results = REParser.matchSA(re1,sa,debugLevel=2).map{_.toString}.toSet
    assert(results == Set("ec", "dc", "[2 Results] ac", "bc"))
    //assert(results.toString == "List(ca, [2 Results] cb)")
  }
}

class RE2Search  extends FunSuite with RandomGenerator {
  test("match SA FMindex") {
    val sa = new NaiveFMSearcher("testdata/include.fm")

    var re1 = REParser.createNFA("ba|d|e|c.")
    val results = REParser.matchSA(re1,sa,debugLevel=2).map{_.toString}.toSet
    assert(results == Set("ec", "dc", "[2 Results] ac", "bc"))
    //assert(results.toString == "List(ca, [2 Results] cb)")
  }
}
