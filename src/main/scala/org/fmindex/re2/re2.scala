package org.fmindex.re2

import scala.collection.mutable.Stack
import org.fmindex.SuffixWalkingAlgo

object REParser {

    def re2post(str:String):String = {
        var i = 0
        val l = str.length
        var natom=0
        var nalt = 0
        var dst =List[Char]()
        case class Paren(nalt:Int,natom:Int)
        val s = Stack[Paren]()

        while ( i< l) {
            val c = str(i)
            c match {
                case '(' => 
                    if(natom > 1){
                        natom-=1
                        dst = '.' :: dst
                    }
                    s.push(Paren(nalt,natom))
                    nalt=0
                    natom=0
                case '|' => 
                    if ( natom == 0) throw new Exception("re2post syntax")
                    natom-=1
                    while(natom > 0) {
                        dst = '.' :: dst
                        natom-=1
                    }
                    nalt+=1
                case ')' => 
                    if ( natom == 0) throw new Exception("re2post syntax")
                    natom-=1
                    while(natom > 0) {
                        dst = '.' :: dst
                        natom-=1
                    }
                    while(nalt > 0) {
                        dst = '|' :: dst
                        nalt-=1
                    }
                    val t = s.pop()
                    nalt = t.nalt
                    natom = t.natom+1

                case '*'|'+'|'?' => 
                    if ( natom == 0) throw new Exception("re2post syntax")
                    dst = c :: dst
                case _   => 
                    if(natom > 1){
                        natom-=1
                        dst = '.' :: dst
                    }
                    dst = c :: dst
                    natom+=1
            }
            i+=1
        }
        if (! s.isEmpty ) throw new Exception("re2post syntax")
        natom-=1
        while(natom > 0) {
            dst = '.' :: dst
            natom-=1
        }
        while(nalt > 0) {
            dst = '|' :: dst
            nalt-=1
        }
        dst.reverse.mkString("")
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
        override def toString = if (c >= 0x20 && c < 0x7f) "ConstState('%c')".format(c) else "ConstState(%d)".format(c) 
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
    def createNFA(postfix:String):BaseState = {
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
                case '?' => 
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    s0.push(Frag0(ns,open :: e.out))
                case '*' => 
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    e.patch(ns)
                    s0.push(Frag0(ns,List(open)))
                case '+' => 
                    val e = s0.pop
                    val open = new LinkState()
                    val ns = SplitState(new LinkState(e.start),open)
                    e.patch(ns)
                    s0.push(Frag0(e.start,List(open)))
                case '.' =>
                    val e2 = s0.pop
                    val e1 = s0.pop
                    e1.patch(e2.start)
                    s0.push(Frag0(e1.start,e2.out))
                case '|' =>
                    val e2 = s0.pop
                    val e1 = s0.pop
                    val ns = SplitState(new LinkState(e1.start),new LinkState(e2.start))
                    s0.push(Frag0(ns,e1.out ::: e2.out))
                case _ => 
                    val ns = ConstState(c)
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
                case MatchState | ConstState(_,_) => 
                    nlist + s
            }
        
        def step(clist:List[BaseState],c:Int):List[BaseState] = {
            var nlist:Set[BaseState]=Set()
            var i = clist
            while (! i.isEmpty) {
                i.head match {
                    case el @ ConstState(x,_) if (c==x ) => 
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


      
    case class StatePoint(len:Int,state:BaseState,sp:Int,ep:Int) {
        def liststates(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
            if ( s == null || nlist(s)) nlist else
            s match {
                case SplitState(out1,out2) => 
                    val t = liststates(nlist,out1.s)
                    liststates(t,out2.s)
                case MatchState | ConstState(_,_) => 
                    nlist + s
            }
        def expand(sa:SuffixWalkingAlgo):List[StatePoint] = {
            var ret = List()
            state match {
                case el @ ConstState(chr1,_)  => sa.getPrevRange(sp,ep,chr1.toByte) match {
                    case Some((sp1,ep1))      => liststates(Set(),el.next).toList.map {StatePoint(len+1,_,sp1,ep1)}
                    case None                 => List()
                }
                case _ => ???
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

    def matchSA(nfa:BaseState,sa:SuffixWalkingAlgo,debugLevel:Int=0) = {
        def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("matchSA: " +s).format(xs: _*))
        def liststates(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
            if ( s == null || nlist(s)) nlist else
            s match {
                case SplitState(out1,out2) => 
                    val t = liststates(nlist,out1.s)
                    liststates(t,out2.s)
                case MatchState | ConstState(_,_) => 
                    nlist + s
            }
        
        //var visited = Set[StatePoint]()
        var results = List[SAResult]()
        //println(moves(cur_state).mkString(","))
        var i = 0

        var statesFront = liststates( Set(),nfa ).map {
            StatePoint(0,_,0,sa.n)
        }.toList
        debug(2,"Start statesFrom %s",statesFront)
        //var statesFront = Set(StatePoint(addstate(Set(),nfa).toList,0,0,sa.n))

        
        while( ! statesFront.isEmpty && i < 10) {
            val state = statesFront.head
            statesFront = statesFront.tail
            debug(2,"%2d. Take State=%s",i,state)
            val newStates = state.expand(sa)
            debug(2,"newStates %s",newStates)
            newStates.foreach {
                s:StatePoint=> s.state match {
                    case MatchState => 
                    results::=SAResult(sa,s.len,s.sp,s.ep)
                    case _ => statesFront ::= s
                }
            }
            debug(2,"statesFront %s",statesFront)
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
