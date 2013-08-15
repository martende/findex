package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._
import org.fmindex.re2._

class RE2Parser  extends FunSuite with RandomGenerator {
  test("re2post") {
    val r = REParser.re2poststr("abc")
    assert(r=="ab·c·","r="+r)
  }
  test("re2post2") {
    val r = REParser.re2poststr("a(bb)+a")
    assert(r=="abb·+·a·","r="+r)
  }
  test("re2post3") {
    val r = REParser.re2poststr("(a|b)")
    assert(r=="ab|","r="+r)
  }
  test("re2post4") {
    val r = REParser.re2poststr("((a|b)*aba*)*(a|b)(a|b)")
    
    assert(r=="ab|*a·b·a*·*ab|·ab|·","r="+r)
  }
  test("re2post5") {
    val r = REParser.re2poststr("a.*\\(b[a-z].*c")
    
    assert(r=="a.*·(·b·[abcdefghijklmnopqrstuvwxyz]·]·.*·c·","r="+r)
  }
  
  test("createNFA") {
    var nfa = REParser.createNFA(REParser.post2re("ab.c."))
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
    var nfa = REParser.createNFA(REParser.post2re("ab|c."))

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
    var out1 = REParser.createNFA(REParser.post2re("a+"))


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
    var out1 = REParser.createNFA(REParser.post2re("a*"))

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
    var out1 = REParser.createNFA(REParser.post2re("a?"))

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
    var re1 = REParser.createNFA(REParser.post2re("ab.c."))
    assert(REParser.matchNFA(re1,"abc")==true)
    assert(REParser.matchNFA(re1,"atc")==false)
  }

  test("matchString2") {
    var re1 = REParser.createNFA(REParser.post2re("am|c."))
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
  
  test("punktstring") {
    var re1 = REParser.createNFA(REParser.re2post(".c"))
    assert(REParser.matchNFA(re1,"ac")==true)
    assert(REParser.matchNFA(re1,"mc")==true)
    assert(REParser.matchNFA(re1,"Xc")==true)
    assert(REParser.matchNFA(re1,"c")==false)
  }

    

  test("match SA basics") {
    val sa = new SAISBuilder(new ByteArrayNulledWrapper("mmabcacamabbbca".getBytes.reverse))
    sa.build()
    sa.buildOCC
    var re1 = REParser.createNFA(REParser.post2re("ma.b."))
    val results = REParser.matchSA(re1,sa,debugLevel=0)
    assert(results(0).toString == "[2 Results] bam")
  }
  test("match SA basics2") {
    val sa = new SAISBuilder(new ByteArrayNulledWrapper("mmabcacamabbbca".getBytes.reverse))
    sa.build()
    sa.buildOCC
    var re1 = REParser.createNFA(REParser.post2re("ba|c."))
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
    var re1 = REParser.createNFA(REParser.post2re("ba|d|e|c."))
    val results = REParser.matchSA(re1,sa,debugLevel=0).map{_.toString}.toSet
    assert(results == Set("ec", "dc", "[2 Results] ac", "bc"))
    //assert(results.toString == "List(ca, [2 Results] cb)")
  }

  test("re .*c") {
    var re1 = REParser.createNFA(REParser.re2post(".*c"))
    assert(REParser.matchNFA(re1,"ac")==true)
    assert(REParser.matchNFA(re1,"mc")==true)
    assert(REParser.matchNFA(re1,"masdasdsda")==false)
    assert(REParser.matchNFA(re1,"Xcasdasdasdc")==true)
    assert(REParser.matchNFA(re1,"c")==true)
  }

}
class REAnalys extends FunSuite {
  test("anal1") {
    val re = REParser.re2post("abcd")
    val t = ReTree(re)
    //println(t.dotDump)
  }
  test("anal2") {
    val re = REParser.re2post("abcd*")
    val t = ReTree(re)
    //println(t.dotDump)
  }  

  test("anal3") {
    val re = REParser.re2post("abc*d")
    val t = ReTree(re)
    //println(t.dotDump)
  }  

  test("anal4") {
    val re = REParser.re2post("a*bcd")
    val t = ReTree(re)
    //println(t.dotDump)
  }

  test("anal5") {
    val re = REParser.re2post("a*b*c*d*")
    val t = ReTree(re)
    //println(t.dotDump)
  }  

  test("anal6") {
    val re = REParser.re2post("(ab)*")
    val t = ReTree(re)
    //println(t.dotDump)
  }

  test("anal7") {
    val re = REParser.re2post("(ab)*cd")
    val t = ReTree(re)
    //println(t.dotDump)
  }  
  test("anal8") {
    val re = REParser.re2post("(ab)*(cd*)*")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal9") {
    val re = REParser.re2post("(a|b)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }  
  test("anal10") {
    val re = REParser.re2post("(a|b|d|c)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal11") {
    val re = REParser.re2post("(a|b*|d|c)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal12") {
    val re = REParser.re2post("(a|b*|d|c)*|(abc)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal13") {
    val re = REParser.re2post("(a|b|c)|(c|d|e)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal14") {
    val re = REParser.re2post("[a-c]")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal15") {
    val re = REParser.re2post("a[b-d]e")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal16") {
    val re = REParser.re2post("a[b-d]*e")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal17") {
    val re = REParser.re2post("a[x.]e")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal18") {
    val re = REParser.re2post("a\\de")
    val t = ReTree(re,verbose=false)
    //t.showDot()
    //println(t.dotDump)
  }
  test("anal19") {
    val re = REParser.re2post("a+")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal20") {
    val re = REParser.re2post("a****")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal21") {
    val re = REParser.re2post("a+b")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal22") {
    val re = REParser.re2post("a+((b|c)+|d)")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal23") {
    val re = REParser.re2post("a*+")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal24") {
    val re = REParser.re2post("a+*")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal25") {
    val re = REParser.re2post("a+*+*++*")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal26") {
    val re = REParser.re2post("a?")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal27") {
    val re = REParser.re2post("(abc)?+|a?|bcd")
    val t = ReTree(re,verbose=false)
    //println(t.dotDump)
  }
  test("anal28") {
    val re = REParser.re2post("ab(cd|ef)+gh")
    val t = ReTree(re,verbose=false)
    //t.showDot()
  }
  test("anal29") {
    val re = REParser.re2post("(10\\.[0-9]|[1-9][0-9]|[1-2][0-5][0-5]\\.[0-9]|[1-9][0-9]|[1-2][0-5][0-5]\\.[0-9]|[1-9][0-9]|[1-2][0-5][0-5])")
    val t = ReTree(re,verbose=false)
    
    //t.showDot()
  }
  test("anal30") {
    val re = REParser.re2post("ab(cd)*ef")
    val t = ReTree(re)
    //t.showDot()
    //println(t.dotDump)
  }
}

class REAnalys2  extends FunSuite with RandomGenerator {
  test("anal1") {
    val re = REParser.re2post("a")
    val t = ReTree(re)
    assert(t.root.parent.toString=="<<<ROOT>>>")
    assert(t.root.childs.head.parent.toString=="F[a]")
  }
  test("anal2") {
    val re = REParser.re2post("ab*")
    val t = ReTree(re,removeNulls=false)
    //println(t.root.childs.tail.head.childs.head.parent)
    //println(t.root.childs.head.childs.head.parent)
    assert(t.root.childs.tail.head.childs.head.parent.toString=="*[b]")
  }
  test("anal2.1") {
    val re = REParser.re2post("a*(b|a)*bB*cd*e*")
    val t = ReTree(re,removeNulls=true)
    assert(t.root.childs.length==3)
    //println(t.root.childs.tail.head.childs.head.parent)
    //println(t.root.childs.head.childs.head.parent)
    //assert(t.root.childs.tail.head.childs.head.parent.toString=="*[[b]]")
  }
  test("anal2.2") {
    val re = REParser.re2post("a*(b|a)*b?B*c?d*e*")
    val t = ReTree(re,removeNulls=true)
    assert(t.root.childs.length==0)
    assert(t.root.isNull)
    //println(t.root.childs.tail.head.childs.head.parent)
    //println(t.root.childs.head.childs.head.parent)
    //assert(t.root.childs.tail.head.childs.head.parent.toString=="*[[b]]")
  }
  test("anal3") {
    val re = REParser.re2post("abcdef")
    val t = ReTree(re)
    assert(t.root.childs(3).asInstanceOf[ReTree.CharNode].num==4)  
  }

  test("anal4.follows") {
    val re = REParser.re2post("""abc(cde)*ef""")
    val t = ReTree(re)
    val F = t.root
    assert(F.follows == List())
    
    val a = F.childs(0)
    val b = F.childs(1)
    val c = F.childs(2)
    val cdeS = F.childs(3)
    val e = F.childs(4)
    val f = F.childs(5)
    assert(a.follows == List(b))
    assert(b.follows == List(c))
    assert(cdeS.follows == List(e))
    assert(e.follows == List(f))
    assert(f.follows == List())
    val cdeSF = cdeS.childs(0)
    val cdeSFc = cdeSF.childs(0)
    val cdeSFd = cdeSF.childs(1)
    val cdeSFe = cdeSF.childs(2)
    assert(cdeSF.follows == List(cdeSFc,e))
    assert(cdeSFc.follows == List(cdeSFd))
    assert(cdeSFd.follows == List(cdeSFe))
    assert(cdeSFe.follows == List(cdeSFc,e))  
  }

  test("anal4.follows.star") {
    val re = REParser.re2post("""ab*(cd)*(gh)*ij""")
    val t = ReTree(re)

    //t.showDot(showFirsts=true,showFollows=true)
  }

  test("anal4.follows.or.star") {
    val re = REParser.re2post("""a(cd|ef)*j""")
    val t = ReTree(re)

    //t.showDot(showFirsts=true,showFollows=true)
  }
  test("anal4.follows.or.?") {
    val re = REParser.re2post("""ab?j""")
    val t = ReTree(re)
    val F = t.root
    assert(F.childs(1).isNull)

    assert(F.childs(0).follows == List(F.childs(2),F.childs(1).childs(0)))
    // t.showDot(showFirsts=true,showFollows=true)
  }
  
  test("anal5") {
    val re = REParser.re2post(".*ab(cd)*(m(k|l)|tm*)(a|abc)(a*|(abc)*)ef(a*b*c*dg*)*gh")
    val t = ReTree(re)
    //println(t.root.firsts)
    //println(t.dotDump)
  }

  test("anal6") {
    val re = REParser.re2post("""(a|bX|cYZ)(a|b|c)""")
    val t = ReTree(re)
    t.showDot()
    assert(t.root.childs(1).childs(1).asInstanceOf[ReTree.CharNode].num==4)
    //t.showDot(showFirsts=true,showFollows=true)
  }

  test("anal7") {
    val re = REParser.re2post("""(a|b|c)(a|b|c)""")
    val t = ReTree(re)
    t.showDot()
    assert(t.root.childs(1).childs(1).asInstanceOf[ReTree.CharNode].num==2)
    //t.showDot(showFirsts=true,showFollows=true)
  }
}

class REAnalys3  extends FunSuite with RandomGenerator {
  import Util._
  
  test("anal1") {
    val re = REParser.re2post(".*(a|b)ca")
    val t = ReTree(re)
    
    val sa = new SAISBuilder(new ByteArrayNulledWrapper("mmabcacamabbbca".getBytes.reverse))
    sa.build()
    sa.buildOCC

    val ret = t.matchSA(sa,debugLevel=0)
    assert(ret.length==2,"RE returns %s".format(ret))
    
  }

  test("anal2") {
    val re = REParser.re2post("a.*(b|c)d.*f")
    val t = ReTree(re)
    
    val r = new FileBWTReader("testdata/test1024.txt")
    val bm = new BWTMerger2(1024)
    val (bwtf,auxf) = bm.merge(r)

    val fc = new FMCreator(bwtf.getAbsolutePath,1024)
    val fmf = fc.create()

    val sa = new NaiveFMSearcher(fmf.getAbsolutePath)


    val ret = t.matchSA(sa,debugLevel=0)
    //println(ret)
    
  }
}

class WordsDB extends FunSuite with RandomGenerator {
  test("test1") {
    val sa = new NaiveFMSearcher("testdata/words.fm")
    val re = REParser.re2post("a.*(b|c)da.*f",lineOnly=true)
    val t = ReTree(re)
    val ret = t.matchSA(sa,debugLevel=1)

  }
}

class ParalelSearch  extends FunSuite with RandomGenerator {
 test("ParalelSearch t2 dir") {
  import Util._
  val r = new DirBWTReader("testdata/t2","testdata/t2",debugLevel=0,caching=true)
  val bm = new BWTMerger2(1024*10,debugLevel=0)
  val (of,af) = bm.merge(r)
  val fm = new FMCreator(of.getAbsolutePath,1024*1024)
  val fmf = fm.create() 
  val lcp = new LCPCreator(of.getAbsolutePath,1024*1024)
  lcp.create() 
  val sa = new SACreator(of.getAbsolutePath,1024*1024)
  sa.create() 

  val lcps = new LCPSearcher(of.getAbsolutePath)
  val re1 = REParser.createNFA(REParser.re2post("1.*0"))
  REParser.paralelSearch(re1,lcps,debugLevel=10)
 }
}

class RE2Search  extends FunSuite with RandomGenerator {
  /*
  test("match SA FMindex") {
    
    val r = new FileBWTReader("testdata/test1024.2.txt")
    val bm = new BWTMerger2(1024,debugLevel=0)
    val (bwtf,auxf) = bm.merge(r)

    val fc = new FMCreator(bwtf.getAbsolutePath,1024)
    val fmf = fc.create()
    assert(fmf.getAbsolutePath.endsWith("testdata/test1024.2.fm"))
    
    val sa = new NaiveFMSearcher("testdata/test1024.2.fm")
    var re1 = REParser.createNFA(REParser.re2post("DPY.*zf"))
    val results = REParser.matchSA(re1,sa,debugLevel=1,maxIterations=0).map{_.toString}.toSet
    println(results)

    //assert(results.toString == "List(ca, [2 Results] cb)")
  }
 */
 test("match SA t2 dir") {
  import Util._
  val r = new DirBWTReader("testdata/t2","testdata/t2",debugLevel=0)
  val bm = new BWTMerger2(1024*10,debugLevel=0)
  val (of,af) = bm.merge(r)
  val fm = new FMCreator(of.getAbsolutePath,1024*1024)
  val fmf = fm.create() 
  val sa = new NaiveFMSearcher(fmf.getAbsolutePath)
  
  def re(s:String,maxIterations:Int=0,maxLength:Int=0,debugLevel:Int=0) = {
      val re1 = REParser.createNFA(REParser.re2post(s))
      var t:Double = 0.0
      val result = timer({
        REParser.matchSA(re1,sa,debugLevel=debugLevel-1,maxLength=maxLength,maxIterations=maxIterations).map{_.toString}.toSet
      },{
        x:Long => t = x/1e6
      })
      if (debugLevel>0)
        printf("RE '%s' - %.1f ms ret: %s\n",s,t,result.size.toString)
      result
  }
  re("99*0",maxIterations=100,maxLength=100,debugLevel=0)
  // re("9.*0",maxIterations=20,maxLength=100,debugLevel=2)
 }
  /*
 test("match SA FMindex") {
  import Util._

    val sa = try {
      new NaiveFMSearcher("testdata/linux.fm")
      } catch {
        case e:Exception => 
          new NaiveFMSearcher("testdata/include.fm")
      }
    
    def re(s:String,maxIterations:Int=0,maxLength:Int=0,debugLevel:Int=0) = {
      val re1 = REParser.createNFA(REParser.re2post(s))
      var t:Double = 0.0
      val result = timer({
        REParser.matchSA(re1,sa,debugLevel=debugLevel,maxLength=maxLength,maxIterations=maxIterations).map{_.toString}.toSet
      },{
        x:Long => t = x/1e6
      })
      printf("RE '%s' - %.1f ms ret: %s\n",s,t,result.size.toString)
      result
    }
    /*
    re("include")
    re("stdio.h")
    re("main")
    */
    /*
    println(sa.bucketStarts.zipWithIndex.map{
      x:Pair[Int,Int] => 
      "%s %d" format (safeChr(x._2),x._1)
    } mkString("\n"))
    */
    re("a.*br",maxIterations=5,maxLength=10,debugLevel=2)
    //println(results)
    //println(sa.nextSubstr(0,1000))
    //assert(results.toString == "List(ca, [2 Results] cb)")
  }
  */  
}
