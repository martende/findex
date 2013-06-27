package org.fmindex.tests

import org.scalatest.FunSuite
import org.fmindex._
import scalax.file.Path
import scalax.io._
import org.fmindex.Util._
import scala.collection.mutable.BitSet
import java.io.File

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
    sa.build()
    sa.buildOCC
    //sa.printSA()
    val dfa = DFA.processLinkList(`ab*c`,debugLevel=0)
    val results = dfa.matchSA(sa)
    assert(results.size == 2)
    assert(results(0).toString=="cbbba")
    assert(results(1).toString=="cba")
    assert(sa.nextSubstr(results(1).sp,results(1).len)=="cba","sa.nextSubstr(results(1).sp,results(1).len)="+sa.nextSubstr(results(1).sp,results(1).len))
    assert(sa.nextSubstr(results(0).sp,results(0).len)=="cbbba")
  }
  /*
  test("match SA basics - abit larger string") {
    val sa = new SAISBuilder(fromString("2b2w9vzrtqy3vzclgoofxgz9nal81y1fg8rozxkb5aaep1vpafp3cgsumc0z1rhpatcwo4d7nxc751h3a4woj3dbjf6ynfbkoom8sxoc9t3dqzkfs9akc6cmsy7cndi6bf116fju5rcsysixgkaih4zbkl8qo3ko2c42f34x6cqdew8x2jgz36r4bskabx02lxbfzokc".reverse))
    sa.build()
    sa.buildOCC
    val dfa = DFA.processLinkList(`ab*c`)
    val results = dfa.matchSA(sa)    
    assert(results.size == 0)
  }
*/
}