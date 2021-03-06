package org.fmindex
import scala.collection.immutable.Queue

  /// NFA
  trait BaseNfaLink {
    val to:NfaBaseState
  }
  case class EpsilonLink(val to:NfaBaseState) extends BaseNfaLink
  case class NfaLink(val to:NfaBaseState,chr:Int) extends BaseNfaLink

  class NfaBaseState {
    NfaBaseState._idx+=1
    val idx:Int = NfaBaseState._idx
    def name:String = idx.toString
    override def toString = "NfaBaseState("+name+")"
    var links = List[BaseNfaLink]()
    def epsilons = {
      def _processLinkList(s:NfaBaseState,visited:Set[NfaBaseState]):Set[NfaBaseState] = {
        var v = visited + s
        for (l <- s.links if l.isInstanceOf[EpsilonLink] && ! visited(l.to) ) {
          v = v ++ _processLinkList(l.to,v)
        }
        v
      }
      _processLinkList(this,Set[NfaBaseState]())
    }
    def epsilonTransitions:Map[Int,Set[NfaBaseState]] = {
      val t0 = ( for { s <- epsilons ; l <- s.links if ! l.isInstanceOf[EpsilonLink] } yield l match {
          //case _:EpsilonLink => None
          case NfaLink(s,chr) => chr -> s.epsilons
      } ) 

      t0.groupBy(_._1).mapValues(x => x.map(_._2).reduceLeft( (a,b) => { a ++ b } ) )

    }
    def link(s:NfaBaseState,chr:Int):Unit = links = NfaLink(s,chr) :: links
    def epsilon(s:NfaBaseState) = links = links :+ EpsilonLink(s)
    def dotDump():String = {
      def _processLinkList(s:NfaBaseState,visited:Set[NfaBaseState]):Set[NfaBaseState] = {
        var v = visited + s
        for (l <- s.links if ! visited(l.to) ) {
          v = v ++ _processLinkList(l.to,v)
        }
        v
      }
      val visited = _processLinkList(this,Set[NfaBaseState]())
      
      val ret = new StringBuilder()
      var iret = List[String]()
      ret.append("digraph graphname {\n")
      def isAllDigits(x: String) = x forall Character.isDigit
      val min = visited.foldLeft(-1)((a,b) => {val x = b.name
        if (isAllDigits(x)) {
          val n = x.toInt
          if ( a == -1 || n < a ) n else a
        } else a
      })

      def parsed(x:String):String = if (isAllDigits(x)) {
        (x.toInt-min).toString
      } else {
        x
      }
      for ( v <- visited;l<-v.links) l match {
        case _:EpsilonLink => iret  = ("%s -> %s  [label=\"eps\"]\n" format ( parsed(v.name) , parsed(l.to.name) ) ) :: iret 
        case ll:NfaLink    =>  iret  = ("%s -> %s  [label=\"%c\"]\n" format ( parsed(v.name) , parsed(l.to.name) ,ll.chr ) ) :: iret 
      }
      for (s <- iret.sorted) ret append s
      ret append "}\n"
      
      ret.toString
    }
    def showGraph(dump:Boolean=true) {
      if (dump)
        println("nfaDump:\n"+dotDump+"-----\n")
      val output = new java.io.FileOutputStream("/tmp/nfa.dot")
      output.write(dotDump.getBytes)
      output.close()
      //Resource.fromFile("/tmp/file.dot").write(dotDump)
      Runtime.getRuntime.exec("dotty /tmp/nfa.dot")
    }
    
    
  }
  object NfaBaseState {
    var _idx:Int = 0
  }

  class NfaState extends NfaBaseState
  class NfaStartState extends NfaBaseState {
    override def name:String = "S"
  }
  class NfaFinishState extends NfaBaseState {
    override def name:String = "F"
  }

object NFA {
  def epsilons(s:Set[NfaBaseState]) = for {  e <- s ; s <- e.epsilons  } yield s
  def epsilonTransitions(ss:Set[NfaBaseState]):Map[Int,Set[NfaBaseState]] = {
      val t0 = ( for { s <- epsilons(ss) ; l <- s.links if ! l.isInstanceOf[EpsilonLink] } yield l match {
          //case _:EpsilonLink => None
          case NfaLink(s,chr) => chr -> s.epsilons
      } ) 

      t0.groupBy(_._1).mapValues(x => x.map(_._2).reduceLeft( (a,b) => { a ++ b } ) )

    }
}

class DFA(nstates:Int,nchars:Int,debugLevel:Int=0) {
  def debug(l:Int,s: =>String  ) = if (l<=debugLevel) println(s)

  val moves:Array[Array[Int]] = Array.fill(nstates)(Array.fill(nchars)(-1))
  // Same as moves but linked in buckets
  // DFABucket('c-d'' ->1),DFAChar('f'->1),DFABucket('k-m'' ->1) - moves to c,d,f,k,l,m
  var buckets:Array[List[DFAAction]] = _

  //for (i<-0 until nstates) moves(i) = new Array[Int](nchars)

  var curState = 0

  var finishStates = Set[Int]()

  protected var _startStateSetted = false
  protected var _stateIdx:Int = 1
  protected var _startState:StartState = _
  def addState(s:AnyState) = {
    assert(s.dfaIdx < 0, "State already used")
    s match {
      case ss:StartState =>
        s.dfaIdx = 0
        assert(_startState==null,"Start state already taken")
        _startState = ss
      case _:FinishState =>
        s.dfaIdx = _stateIdx
        finishStates = finishStates + _stateIdx
        _stateIdx+=1
      case _:AnyState =>
        s.dfaIdx = _stateIdx
        _stateIdx+=1
    }
  }
  def showGraph(dump:Boolean=true) {
    val dd =_startState.dotDump
    if ( dump) println("DFA Dump:\n"+dd+"-------")
    val output = new java.io.FileOutputStream("/tmp/file.dot")
    output.write(dd.getBytes)
    output.close()
    //Resource.fromFile("/tmp/file.dot").write(dotDump)
    Runtime.getRuntime.exec("dotty /tmp/file.dot")
  }
  def addLink(from:AnyState,to:AnyState,chr:Int) {
    assert(from.dfaIdx >= 0,"Start State not in DFA")
    assert(to.dfaIdx >= 0,"End State not in DFA")
    moves(from.dfaIdx)(chr)=to.dfaIdx
  }
  override def toString = _startState.toString

  def matchString(s:Array[Byte]):Boolean = {
    var i = 0
    val l = s.length
    var cur_state = 0
    while ( i!= l) {
      cur_state = moves(cur_state)(s(i))
      i+=1
      if (cur_state == -1) return false
    }
    return finishStates(cur_state)
  }

  def matchString(s:String):Boolean = matchString(s.getBytes)
  
  abstract class DFAAction(_state:Int) {
    val state = _state
    def prettyChr(c:Byte):String = {
      val d:Int = c & 0xff

      if ( d < 0x20 || d > 0x7e) "\\x" +d.toHexString else c.toChar.toString
      
    }
  }
  object DFAAction {
    def create(state:Int,char1:Int,char2:Int) = {
      if ( char1 == char2) {
        new DFAChar(state,char1.toByte)
      } else {
        new DFABucket(state,char1.toByte,char2.toByte)
      }
    }
  }
  case class DFAChar(override val state:Int,char:Byte) extends  DFAAction(state) {
    override def toString = "DFAChar('"+prettyChr(char)+"'->"+state+")"
  }
  class DFABucket(override val state:Int,char1:Byte,char2:Byte) extends  DFAAction(state) {
   override def toString = "DFABucket('"+prettyChr(char1)+"-"+prettyChr(char2)+"' ->"+state+")" 
  }


  def compileBuckets() = {
    var bkt:Array[List[DFAAction]] =  Array.fill(moves.length)(List())
    for (i <- 0 until moves.length) {
      var j =0 
      var last = -1
      var start_bucket = -1
      while ( j < nchars) {
        val v = moves(i)(j)
        if ( last != v) {
          if  (last != -1 ) 
            bkt(i) = DFAAction.create(last,start_bucket,j-1) :: bkt(i) 
          start_bucket = j
          last = v
        }
        j = j+1
      }
      if ( last != -1 ) {
        bkt(i) = DFAAction.create(last,start_bucket,nchars-1) ::  bkt(i) 
      }
      bkt(i) = bkt(i).reverse
    }
    buckets = bkt
  }
  def dumpBuckets() {
    val b = buckets
    for ( i <- 0 until b.length) {
      printf("%2d. ",i,b(i).length)
      print(b(i).mkString(","))
      printf("\n")
    }
  }
  
  case class DFAResult(sa:SuffixWalkingAlgo,len:Int,sp:Int,ep:Int) {
    val cnt = ep - sp
    lazy val strResult:String = if ( cnt ==1 ) 
      sa.nextSubstr(sp,len)
      else if ( cnt > 0) 
        "["+cnt + " Results] " + sa.nextSubstr(sp,len)
      else
        "[no results]" 
    override def toString = strResult
  }

  case class StatePoint(state:Int,len:Int,sp:Int,ep:Int) {
    def expand(sa:SuffixWalkingAlgo):List[StatePoint] = {
      var ret = List()
      val newStates = for (action <- buckets(state)) yield {
        val expands = action match {
          case DFAChar(state,chr1) => {
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
  
  def matchSA(sa:SuffixWalkingAlgo) = {
    var cur_state = 0
    var statesFront = Set(StatePoint(0,0,0,sa.n))
    var visited = Set[StatePoint]()
    var results = List[DFAResult]()
    //println(moves(cur_state).mkString(","))
    var i = 0
    
    while( ! statesFront.isEmpty && i < 500) {
      val state = statesFront.head;
      statesFront = statesFront.tail
      visited = visited + state
      debug(2,"%2d. Take State=%s".format(i,state))
      val newStates = state.expand(sa)
      if (/*newStates.isEmpty && */finishStates(state.state)) {
        results=DFAResult(sa,state.len,state.sp,state.ep)::results
      }
      debug(2,"found %d new states [%s]\n".format(newStates.length,newStates,newStates.mkString(",")))
      
      for (s <- newStates ) if (visited(s)) {
        debug(2,"state visited %s".format(newStates))
      } else {
        statesFront = statesFront + s
      }
      i+=1
    }
    
    results
  }
}

case class Link(to:AnyState,chr:Int) {
  override def toString = to.name+"["+chr.toChar+"]"
}

trait AnyState {
  val name:String
  var dfaIdx:Int = -1
  var links = List[Link]()
  def link(s:AnyState,chr:Int):Unit = links = Link(s,chr) :: links
  //override def toString = "["+name+"] Links=" + links.mkString(",")
  override def toString = "[[ ("+name+") Links=" + links.mkString(",")+"]]"

  def dotDump():String = {
      def _processLinkList(s:AnyState,visited:Set[AnyState]):Set[AnyState] = {
        var v = visited + s
        for (l <- s.links if ! visited(l.to) ) {
          v = v ++ _processLinkList(l.to,v)
        }
        v
      }
      val visited = _processLinkList(this,Set[AnyState]())
      
      val ret = new StringBuilder()
      var iret = List[String]()
      ret.append("digraph graphname {\n")
      for ( v <- visited;l<-v.links) {
         iret  = ("%s -> %s  [label=\"%c\"]\n" format (v.name,l.to.name,l.chr)) :: iret 
      }
      for (s <- iret.sorted) ret append s
      ret append "}\n"
      
      ret.toString
    }
}

class State(override val name:String="x") extends AnyState {

}

class StartState extends AnyState {
  val name = "START"
}

class FinishState extends AnyState {
  val name = "END"
  //override def link(s:AnyState,chr:Int) = assert(false,"Cant add link to Finish state")
  override def toString = "[*"+name+"]"
}

object DFA {
  type tStateSet = Set[AnyState]



  def fromNFA(initialState:NfaBaseState):DFA = {
    val init = Set(initialState)

    def psc(ts:Map[(Set[NfaBaseState],Int),Set[NfaBaseState]],q: Queue[Set[NfaBaseState]]):DFA =  q match {
      case Queue() => { // nothing more to do but to set up the DFA
        //val fs = ts.values.toSet filter { _ exists { finalStates contains } }
        val startState = new StartState()
        
        //DeterministicFiniteAutomaton(init, fs, ts)
        val initialStateSet = NFA.epsilons(Set(initialState))
        var idx = 0
        def createDfaState(x:Set[NfaBaseState]) = 
          if (x == initialStateSet ) startState
          else if (x exists { _.isInstanceOf[NfaFinishState]} ) new FinishState() 
          else {
            idx+=1
            new State(idx.toString)
          }

        val states = ((( ts.keySet map { _._1 } ) ++ ts.values.toSet ) map 
          { x => x -> createDfaState(x) }).toMap

        for ( (k,endState) <- ts ; (startState,transition) = k) {
          //println(startState,transition,endState)
          states(startState).link(states(endState),transition)
        }
        DFA.processLinkList(startState)
      }
      case _ => {
        val (elementset, rest) = q.dequeue
        //val nfaState
        val dfaStateSet = NFA.epsilons(elementset)
        val transitions:Map[Int,Set[NfaBaseState]] = NFA.epsilonTransitions(elementset)
        val newts = for { t <- transitions } yield (dfaStateSet,t._1) -> t._2
        //println("dfaStateSet",dfaStateSet)
        //println("transitions",transitions)
        //println("newts",newts)
        val sumts = ts ++ newts

        val unhandledStates = newts.values.toSet diff {
          sumts.keySet map { _._1 }
        }
        /*
        println("unhandledStates1",unhandledStates)
        println("unhandledStates2",summts.keySet map {_._1 })
        */
        psc(sumts, rest enqueue unhandledStates)
      }
    }
    psc(Map(), Queue(init))
  }

  def processLinkList(s:StartState,debugLevel:Int=0) = {
    def _processLinkList(s:AnyState,visited:tStateSet):tStateSet = {
      var v = visited + s
      for (l <- s.links if ! visited(l.to) ) {
        v = v ++ _processLinkList(l.to,v)
      }
      v
    }
    val visited = _processLinkList(s,Set[AnyState]())
    val dfa = new DFA(visited.size,256,debugLevel=debugLevel)
    for ( v <- visited) dfa.addState(v)
    for ( v <- visited;l<-v.links) dfa.addLink(v,l.to,l.chr)

    dfa.compileBuckets
    
    dfa
  }
}


object DFAPlay extends optional.Application {
/*  def main() {
    val s = new StartState()
    val a = new State("a")
    val b = new State("b")
    val f = new FinishState()
    s.link(a,'a')
    a.link(b,'b')
    b.link(b,'b')
    b.link(f,'c')
    println("processLinkList")
    val dfa = DFA.processLinkList(s)
    println("MAtch = " , dfa.matchString("absbc"))
  }
  */

  def main() {
    var m = "ab(cd?e)+k(aaa)k*\\fkk(ssk)*"
    //m ="m.*"
    m="((ab.*)|abbb)"
    val x = ReParser.parseItem(m)
    //x.nfa.showGraph()
    
    val dfa = DFA.fromNFA(x.nfa)
    //dfa.showGraph()
    var b = dfa.buckets
    dfa.dumpBuckets()    
    
    //println(dfa.matchString("abmb"))
  }

}
