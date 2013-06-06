package org.fmindex


class DFA(nstates:Int,nchars:Int) {
  var moves:Array[Array[Int]] = Array.fill(nstates)(Array.fill(nchars)(-1))
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

  def matchSA(s:SuffixAlgo):Boolean = {
    var cur_state = 0
    var statesFront = Set()
    println(moves(cur_state).mkString(","))
    true
  }
}

case class Link(to:AnyState,chr:Int) {
  override def toString = to.name+"["+chr.toChar+"]"
}

trait AnyState {
  val name:String
  var dfaIdx:Int = -1
  var links = List[Link]()
  def link(s:AnyState,chr:Int):Unit = links = links :+ Link(s,chr)
  override def toString = "["+name+"] Links=" + links.mkString(",")
}

case class State(name:String) extends AnyState {
}

class StartState extends AnyState {
  val name = "START"
}

class FinishState extends AnyState {
  val name = "END"
  override def link(s:AnyState,chr:Int) = assert(false,"Cant add link to Finish state")
  override def toString = "[*"+name+"]"
}

object DFA {
  type tStateSet = Set[AnyState]
  def processLinkList(s:StartState) = {
    def _processLinkList(s:AnyState,visited:tStateSet):tStateSet = {
      var v = visited + s
      for (l <- s.links if ! visited(l.to) ) {
        v = v ++ _processLinkList(l.to,v)
      }
      v
    }
    val visited = _processLinkList(s,Set[AnyState]())
    val dfa = new DFA(visited.size,256)
    for ( v <- visited) dfa.addState(v)
    for ( v <- visited;l<-v.links) dfa.addLink(v,l.to,l.chr)
    dfa
  }
}

object DFAPlay extends optional.Application {
  def main() {
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
}
