package org.fmindex.re2
import scala.collection.mutable.Stack
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
        override def links:List[LinkState] = List(out)
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
/*
      case class StatePoint(state:Int,len:Int,sp:Int,ep:Int) {
        def expand(sa:SuffixAlgo):List[StatePoint] = {
          var ret = List()
          println(this + " expand")
          val newStates = for (action <- buckets(state)) yield {
            println("Action " + action)
            val expands = action match {
              case DFAChar(state,chr1) => {
                println("getPrevRange",(sp,ep,chr1),sa.getPrevRange(sp,ep,chr1))
                sa.getPrevRange(sp,ep,chr1)
              }
              case _=> None
            }
            expands match {
              case Some((sp1,ep1)) => Some(StatePoint(action.state,len+1,sp1,ep1))
              case _=> None
            }
          }
          newStates.flatten
        }
      }
      */
    case class StatePoint(front:List[BaseState],len:Int,sp:Int,ep:Int) {
        def expand(sa:SuffixAlgo):List[StatePoint] = {
            var ret = List()
        }
    }
    case class SAResult(sa:SuffixAlgo,len:Int,sp:Int,ep:Int) {
        val cnt = ep - sp
        lazy val strResult:String = if ( cnt ==1 ) 
          sa.nextSubstr(sp,len)
          else if ( cnt > 0) 
            "["+cnt + " Results] " + sa.nextSubstr(sp,len)
          else
            "[no results]" 
        override def toString = strResult
    }

    def matchSA(nfa:BaseState,sa:SuffixAlgo) = {
        //var cur_state = 0
        
        //var visited = Set[StatePoint]()
        var results = List[SAResult]()
        //println(moves(cur_state).mkString(","))
        var i = 0

        var statesFront = Set(StatePoint(addstate(Set(),nfa).toList,0,0,sa.n))

        while( ! statesFront.isEmpty && i < 1) {
            val state = statesFront.head
            statesFront = statesFront.tail
            printf("%2d. Take State=%s\n",i,state)
            val newStates = state.expand(sa)
            if (/*newStates.isEmpty && */finishStates(state.state)) {
                results=SAResult(sa,state.len,state.sp,state.ep)::results
            }
            statesFront = statesFront + newStates
            i+=1
        }

        println("Result = " + results)
        //println(statesFront.head.expand(sa))
        results
    }
}
