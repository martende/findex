package org.fmindex


class DFA(nstates:Int,nchars:Int) {
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
  
  case class DFAResult(sa:SuffixAlgo,len:Int,sp:Int,ep:Int) {
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
  
  def matchSA(sa:SuffixAlgo) = {
    var cur_state = 0
    var statesFront = Set(StatePoint(0,0,0,sa.n))
    var visited = Set[StatePoint]()
    var results = List[DFAResult]()
    //println(moves(cur_state).mkString(","))
    var i = 0
    dumpBuckets()
    while( ! statesFront.isEmpty && i < 500) {
      val state = statesFront.head;
      statesFront = statesFront.tail
      visited = visited + state
      printf("%2d. Take State=%s\n",i,state)
      val newStates = state.expand(sa)
      if (/*newStates.isEmpty && */finishStates(state.state)) {
        results=DFAResult(sa,state.len,state.sp,state.ep)::results
      }
      printf("found %d new states [%s]\n",newStates.length,newStates,newStates.mkString(","))
      for (s <- newStates ) if (visited(s)) {
        printf("state visited %s\n",newStates)
      } else {
        statesFront = statesFront + s
      }
      i+=1
    }
    println("Result = " + results)
    //println(statesFront.head.expand(sa))
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

    dfa.compileBuckets
    
    dfa
  }
}

/*
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
*/