package org.fmindex.re2

import scala.collection.mutable.Stack
import org.fmindex.SuffixWalkingAlgo
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue

object REParser {
  val MIN_CHAR=2.toChar
  val MAX_CHAR=255.toChar
  class PostPoint
  class CharPoint(val c:Char) extends PostPoint {
    override def toString = c.toString
  }
  class IntervalPoint(val start:Char,val end:Char) extends PostPoint {
    override def toString = if (start == MIN_CHAR && end==MAX_CHAR) "." else "[%c-%c]".format(start,end)
  }
  class ConcatPoint extends PostPoint {
    override def toString = "."
  }
  class StarPoint extends PostPoint{
    override def toString = "*"
  }
  class QuestionPoint extends PostPoint{
    override def toString = "?"
  }
  class PlusPoint extends PostPoint{
    override def toString = "+"
  }
  class OrPoint extends PostPoint{
    override def toString = "|"
  }
  type PostfixRe = List[PostPoint]
  def re2post(str:String):PostfixRe = {
    var i = 0
    val l = str.length
    var natom=0
    var nalt = 0
    var dst =List[PostPoint]()
    case class Paren(nalt:Int,natom:Int)
    val s = Stack[Paren]()

    while ( i< l) {
      val c = str(i)
      c match {
        case '(' =>
          if(natom > 1){
            natom-=1
            dst = (new ConcatPoint()) :: dst
          }
          s.push(Paren(nalt,natom))
          nalt=0
          natom=0
        case '|' =>
          if ( natom == 0) throw new Exception("re2post syntax")
          natom-=1
          while(natom > 0) {
            dst = (new ConcatPoint())  :: dst
            natom-=1
          }
          nalt+=1
        case ')' =>
          if ( natom == 0) throw new Exception("re2post syntax")
          natom-=1
          while(natom > 0) {
            dst = (new ConcatPoint())  :: dst
            natom-=1
          }
          while(nalt > 0) {
            dst = (new OrPoint())  :: dst
            nalt-=1
          }
          val t = s.pop()
          nalt = t.nalt
          natom = t.natom+1

        case '*'|'+'|'?' =>
          if ( natom == 0) throw new Exception("re2post syntax")
          dst::= (c match {
            case '*' => (new StarPoint())
            case '+' => (new PlusPoint())
            case '?' => (new QuestionPoint())
          }       )

        case _   =>
          if(natom > 1){
            natom-=1
            dst = (new ConcatPoint())  :: dst
          }
          dst = (c match {
            case '.' => new IntervalPoint(MIN_CHAR,MAX_CHAR)
            case _ =>  new CharPoint(c)
          } ) :: dst
          natom+=1
      }
      i+=1
    }
    if (! s.isEmpty ) throw new Exception("re2post syntax")
    natom-=1
    while(natom > 0) {
      dst = (new ConcatPoint())  :: dst
      natom-=1
    }
    while(nalt > 0) {
      dst = (new OrPoint())  :: dst
      nalt-=1
    }
    dst.reverse
  }

  def re2poststr(str:String):String = re2post(str).map{_.toString}.mkString("")
  def post2re(str:String) = {
    var dst =List[PostPoint]()
    var i = 0
    val l = str.length
    while ( i< l) {
      val c = str(i)
      dst::=(c match {
        case '*' => new StarPoint()
        case '.' => new ConcatPoint()
        case '|' => new OrPoint()
        case '?' => new QuestionPoint()
        case '+' => new PlusPoint()
        case _ => new CharPoint(c)
      })
      i+=1
    }
    dst.reverse
  }
    
    case class State(c:Int,out:State=null,out1:State=null) {
        def link(s:State):State = State(c,s,null)
    }
    
    
    abstract class BaseState() {
        def links:List[LinkState] = List()
    }
    class LinkState(_s:BaseState=null) {
        var s:BaseState=_s
        override def toString = if (s == null) "<EMPTY>" else "<" + s.getClass + ">"
    }

    case class ConstState(c:Int,out:LinkState=new LinkState()) extends BaseState {
      def next = out.s
      override def links:List[LinkState] = List(out)
      override def toString = if (c >= 0x20 && c < 0x7f) "C[%c]".format(c) else "ConstState(%d)".format(c)
    }
    case class IntervalState(start:Int,end:Int,out:LinkState=new LinkState()) extends BaseState {
      def next = out.s
      override def links:List[LinkState] = List(out)
      override def toString = if (start.toChar==MIN_CHAR && end == MAX_CHAR) "I[.]" else
        "I[%s-%s]".format(
          if (start >= 0x20 && start < 0x7f) start.toChar.toString else start.toString,
          if (end >= 0x20 && end < 0x7f) end.toChar.toString else end.toString
        )
    }
    case class SplitState(out1:LinkState,out2:LinkState) extends BaseState {
        override def links:List[LinkState] = if ( out2 != null) List(out1,out2) else List(out1)
    }
    object MatchState extends BaseState {
        override def toString = "<MatchState>"
    }

    object State {
        val SPLIT = 256
        val MATCH = 257
    }
    def createNFA(postfix:PostfixRe):BaseState = {
        val l = postfix.length
        case class Frag(start:State,out:List[State]=List()) {
            def patch(s:State) = {
                Frag(start.link(s),List.fill(out.length)(s) )
            }
        }
        case class Frag0(start:BaseState,out:List[LinkState]) {
            def patch(s:BaseState) = {
                out.foreach(x => x.s = s)
            }
        }
        val s = Stack[Frag]()
        val s0 = Stack[Frag0]()
        def dumpStack {
            println("---------")
            for (i <- s0) {
                println(i)
            }
            println("---------")
        }
        var i = 0
        while ( i< l) {
            val c = postfix(i)
            //println(c)
            c match {
                case _:QuestionPoint =>
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    s0.push(Frag0(ns,open :: e.out))
                case _:StarPoint =>
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    e.patch(ns)
                    s0.push(Frag0(ns,List(open)))
                case _:PlusPoint =>
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    e.patch(ns)
                    s0.push(Frag0(e.start,List(open)))
                case _:ConcatPoint =>
                    val e2 = s0.pop
                    val e1 = s0.pop
                    e1.patch(e2.start)
                    s0.push(Frag0(e1.start,e2.out))
                case _:OrPoint =>
                    val e2 = s0.pop
                    val e1 = s0.pop
                    val ns = SplitState(new LinkState(e1.start),new LinkState(e2.start))
                    s0.push(Frag0(ns,e1.out ::: e2.out))
                case cp:CharPoint =>
                    val ns = ConstState(cp.c)
                    s0.push(Frag0(ns,ns.links))
                case cp:IntervalPoint =>
                  val ns = IntervalState(cp.start,cp.end)
                  s0.push(Frag0(ns,ns.links))
            }
            //dumpStack
            i+=1
        }
        
        val e0 = s0.pop()
        e0.patch(MatchState)
        s0.push(e0)
        //dumpStack
        e0.start

    }
    
    def matchNFA(nfa:BaseState,s:String,debug:Boolean=false):Boolean = {
        //var clist:List
        val l = s.length
        var i = 0
        
        def addstate(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
            if ( s == null || nlist(s)) nlist else
            s match {
                case SplitState(out1,out2) => 
                    val t = addstate(nlist,out1.s)
                    addstate(t,out2.s)
                case MatchState | ConstState(_,_) | IntervalState(_,_,_) => 
                    nlist + s
            }
        
        def step(clist:List[BaseState],c:Int):List[BaseState] = {
            var nlist:Set[BaseState]=Set()
            var i = clist
            while (! i.isEmpty) {
                i.head match {
                    case el @ ConstState(x,_) if (c==x ) =>
                      nlist = addstate(nlist,el.out.s)
                    case el @ IntervalState(s,e,_) if (c >= s && s <=e) =>
                      nlist = addstate(nlist,el.out.s)
                    case _ => 
                }

                i = i.tail
            }
            nlist.toList
        }

        var clist:List[BaseState] = addstate(Set(),nfa).toList
        
        while ( i< l) {
            val c = s(i)
            if ( debug ) printf("%2d.\t %c %s\n",i,c,clist)
            clist = step(clist, c)
            i+=1
        }
        if ( debug ) printf("RESULT\t %s\n",clist)
        clist.exists {_ == MatchState}
    }

    def matchString(re:String,s:String,debug:Boolean=false):Boolean = {
        var re1 = REParser.createNFA(re2post(re))
        matchNFA(re1,s,debug)
    }


      
    case class StatePoint(len:Int,state:BaseState,sp:Int,ep:Int) extends Ordered[StatePoint] {
      val cnt = ep - sp
      
      def compare(that:StatePoint) = if (that.cnt < this.cnt) 1 else if (that.cnt > this.cnt) -1 else 0

      override def toString = "(%s:%d,%d-%d,%d)" format (state,len,sp,ep,cnt)
      def overlaps(that: StatePoint) = {
        val i0 = (this.sp max that.sp) // lower bound of intersection interval
        val i1 = (this.ep min that.ep) // upper bound of intersection interval
        (i0 <= i1)
      }
      def liststates(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
          if ( s == null || nlist(s)) nlist else
          s match {
              case SplitState(out1,out2) => 
                  val t = liststates(nlist,out1.s)
                  liststates(t,out2.s)
              case MatchState | ConstState(_,_) | IntervalState(_,_,_) => 
                  nlist + s
          }
      def expand(sa:SuffixWalkingAlgo):List[StatePoint] = {
          var ret = List()
          state match {
              case el @ ConstState(chr1,_)  => sa.getPrevRange(sp,ep,chr1.toByte) match {
                  case Some((sp1,ep1))      => liststates(Set(),el.next).toList.map {StatePoint(len+1,_,sp1,ep1)}
                  case None                 => List()
              }
              case el @ IntervalState(start,end,_) => 
                val t = sa.getIntervalPrevRange(sp,ep,start,end)
                if (t.isEmpty) List() else {
                  val nextStates = liststates(Set(),el.next).toList
                  nextStates.map({ s:BaseState => t.map { p:Pair[Int,Int] => StatePoint(len+1,s,p._1,p._2)  } }).flatten
                }
          }
      }
    }
    
    case class SAResult(sa:SuffixWalkingAlgo,len:Int,sp:Int,ep:Int) {
        val cnt = ep - sp
        lazy val strResult:String = 
          if ( cnt ==1 ) 
            sa.nextSubstr(sp,len)
          else if ( cnt > 0) 
            "["+cnt + " Results] " + sa.nextSubstr(sp,len)
          else
            "[no results]" 
        override def toString = strResult
    }

    def matchSA(nfa:BaseState,sa:SuffixWalkingAlgo,debugLevel:Int=0,maxIterations:Int=0,maxLength:Int=0) = {
        def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("matchSA: " +s).format(xs: _*))
        def liststates(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
            if ( s == null || nlist(s)) nlist else
            s match {
                case SplitState(out1,out2) => 
                    val t = liststates(nlist,out1.s)
                    liststates(t,out2.s)
                case MatchState | ConstState(_,_) | IntervalState(_,_,_) => 
                    nlist + s
            }
        
        //var visited = Set[StatePoint]()
        var results = List[SAResult]()
        //println(moves(cur_state).mkString(","))
        var i = 0
        var pqFront = new scala.collection.mutable.PriorityQueue[StatePoint]()
        
        for ( s <- liststates( Set(),nfa ) ) {
            pqFront.enqueue(StatePoint(0,s,0,sa.n))
        }
        /*
        var statesFront = liststates( Set(),nfa ).map {
            StatePoint(0,_,0,sa.n)
        }.toList
        */

        val statesSeenIM = pqFront.toList.groupBy { _.state }.map { 
          s:(BaseState, List[StatePoint]) =>  s._1 -> s._2.map { c:StatePoint => (c.sp,c.ep) }  
        }
        val statesSeen:Map[BaseState,List[Pair[Int,Int]]] = Map(statesSeenIM.toSeq:_*)
        

        
        debug(2,"Start statesFrom %s",pqFront)
        //var statesFront = Set(StatePoint(addstate(Set(),nfa).toList,0,0,sa.n))

        def debugOverlapCheck(s:StatePoint,sf:Iterable[StatePoint]) = sf.find({
          ss:StatePoint => s.state == ss.state && (s overlaps ss)
        })
        
        def overlaps(sp:Int,ep:Int,pl:List[Pair[Int,Int]]) = pl.find({
          ss:Pair[Int,Int] => 
            val i0 = (sp max ss._1)
            val i1 = (ep min ss._2)
            (i0 <= i1)
        })
        def contains(sp:Int,ep:Int,pl:List[Pair[Int,Int]]) = pl.find({
          ss:Pair[Int,Int] => (sp >= ss._1) && (ep <= ss._2)
        })

        def debugStatesPower(sf:Iterable[StatePoint]) = sf.foldLeft(0)(_ + _.cnt)

        while( ! pqFront.isEmpty && (maxIterations == 0 || i < maxIterations ) ) {
            val dsCnt = debugStatesPower(pqFront)
            val dsSize = pqFront.length

            val state = pqFront.dequeue
            //statesFront = statesFront.tail
            val newStates = state.expand(sa)
            val _debugStatePower = debugStatesPower(newStates)
            val distinctStatesCnt = newStates.groupBy(_.state).size
            debug(2,"%2d.SC:%d.FS.%d Take State=%s and create newStates newStatesCnt=%d "+
              "newStatesPower=%d ratio=%f nratio=%f",i,dsCnt,dsSize,
              state,
              newStates.length,
              _debugStatePower,
              if ( _debugStatePower != 0  ) _debugStatePower/state.cnt.toFloat else -1.0,
              if ( _debugStatePower != 0  ) _debugStatePower/distinctStatesCnt/state.cnt.toFloat else -1.0
            )

            newStates.foreach {
                s:StatePoint=> s.state match {
                    case MatchState => 
                      results::=SAResult(sa,s.len,s.sp,s.ep)
                    case _ if maxLength == 0 || s.len < maxLength => 
                      /*if ( s.cnt < 10 ) {
                          printf("%s SMALLI!\n",s)
                      } else {*/
                          debugOverlapCheck(s,pqFront) match {
                            case Some(sp) => printf("%s OVERLAPPS %s !\n",s,sp)
                            case _        => 
                          }
                          val seenStates = statesSeen.getOrElse(s.state,List()) 
                          
                          overlaps(s.sp,s.ep,seenStates) match {
                            case Some((sp,ep)) => printf("SEEN: new state %s OVERLAPPS seen (%d,%d) !\n",s,sp,ep)
                            case _        => 
                          }
                          
                          contains(s.sp,s.ep,seenStates) match {
                            case Some((sp,ep)) => printf("SEEN: new state %s CONTAINS seen (%d,%d) !\n",s,sp,ep)
                            case _        => 
                              val updati = (s.sp,s.ep) :: seenStates
                              statesSeen(s.state) = updati
                              pqFront.enqueue(s)
                          }    
                      //}

                    case _ => 
                }
            }
            //debug(2,"statesFront %s",statesFront)
            /*
            if (/*newStates.isEmpty && */finishStates(state.state)) {
                results=SAResult(sa,state.len,state.sp,state.ep)::results
            }
            statesFront = statesFront + newStates
            */
            i+=1
            
        }
        
        debug(1,"Result = %s",results)

        //println(statesFront.head.expand(sa))
        results
    }
    
}
