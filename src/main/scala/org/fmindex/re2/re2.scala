package org.fmindex.re2

import scala.collection.mutable.Stack
import org.fmindex.SuffixWalkingAlgo
import org.fmindex.LCPSuffixWalkingAlgo
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
  class AltPoint(val alts:List[Char]) extends PostPoint {
    override def toString = "[" + alts.reverse.mkString("") + "]"
  }
  class ConcatPoint extends PostPoint {
    override def toString = "·"
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
    var quoted = false;
    
    def processChar(c:Char,quoted:Boolean) {
      if(natom > 1){
        natom-=1
        dst = (new ConcatPoint())  :: dst
      }
      dst::= (if (quoted) c match {
        case 'w' => new IntervalPoint('A','z')
        case 'd' => new IntervalPoint('0','9')
        case _ =>  new CharPoint(c)
      } else c match {
        case '.' => new IntervalPoint(MIN_CHAR,MAX_CHAR)
        case _ =>  new CharPoint(c)
      })
      
      natom+=1
    }
    def processAltChar(_i:Int) = {
      var i = _i
      var alts = List[Char]()
      var quoted = false
      var end = false
      var interval = false
      def processChar(c:Char) {
        if ( interval ) {
          if (alts.isEmpty) throw new Exception("re2post syntax")
          var cAlt = alts.head.toInt + 1 
          val eAlt = c.toInt
          if (cAlt > eAlt) throw new Exception("re2post syntax")
          while (cAlt <= eAlt ) {
            alts = cAlt.toChar :: alts
            cAlt += 1
          }
          interval = false
        }
        else alts = c :: alts
      }
      while ( i< l && ! end ) {
        val c = str(i)
        if ( quoted ) {
          processChar(c)
          quoted = false
        } else c match {
          case '\\' => quoted = true
          case '-'  => interval = true
          case ']'  => end  = true
          case _ => processChar(c)
        }
        i+=1
      }
      if ( ! end || interval ) throw new Exception("re2post syntax")
      
      if(natom > 1){
        natom-=1
        dst = (new ConcatPoint())  :: dst
      }
      dst = (new AltPoint(alts)) :: dst
      natom+=1

      i 
    }

    while ( i< l) {
      val c = str(i)
      if (! quoted) c match {
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
        case '['  => 
          i = processAltChar(i+1) - 1
        case '\\' => 
          quoted = true
        case '*'|'+'|'?' =>
          if ( natom == 0) throw new Exception("re2post syntax")
          dst::= (c match {
            case '*' => (new StarPoint())
            case '+' => (new PlusPoint())
            case '?' => (new QuestionPoint())
          }       )

        case _   =>
          processChar(c,false)
      } else {
        processChar(c,true)
        quoted = false
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
      def outStates = {
        def addState(nlist:Set[BaseState],s:BaseState):Set[BaseState] = 
          if ( s == null || nlist(s)) nlist else
          s match {
              case SplitState(out1,out2) => 
                  val t = addState(nlist,out1.s)
                  addState(t,out2.s)
              case MatchState | ConstState(_,_) | IntervalState(_,_,_) => 
                  nlist + s
          }
        addState(Set(),this).toList
      }
      
      def links:List[LinkState] = List()
    }
    class LinkState(_s:BaseState=null) {
      var s:BaseState=_s
      override def toString = if (s == null) "<EMPTY>" else "<" + s.getClass + ">"
    }

    abstract class TermState extends BaseState {
      def next:BaseState
    }

    case class ConstState(c:Int,out:LinkState=new LinkState()) extends TermState {
      override def next = out.s
      override def links:List[LinkState] = List(out)
      override def toString = if (c >= 0x20 && c < 0x7f) "C[%c]".format(c) else "ConstState(%d)".format(c)
    }
    case class IntervalState(start:Int,end:Int,out:LinkState=new LinkState()) extends TermState {
      override def next = out.s
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

    object ReTree {
      class Node {
        var childs = List[Node]()
      }
      class CharNode(var c:Char) extends Node {
        override def toString = "["+c+"]"
      }
      class CharList(var c:List[Char]) extends Node {
        override def toString = "["+c.mkString(",")+"]"
      }
      class UnarOpNode extends Node
      class StarNode extends UnarOpNode {
        def append(n:Node) = {
          assert(childs.isEmpty)
          childs::=n
        }
        override def toString = "*["+childs.mkString(",")+"]"
      }
      class QuestionNode extends UnarOpNode {
        def append(n:Node) = {
          assert(childs.isEmpty)
          childs::=n
        }
        override def toString = "?["+childs.mkString(",")+"]"
      }


      class PlusNode extends UnarOpNode {
        def append(n:Node) = {
          assert(childs.isEmpty)
          childs::=n
        }
        override def toString = "+["+childs.mkString(",")+"]"
      }
      
      class OrNode() extends Node {
        def append(n:Node) = {
          n match {
            case on:OrNode => childs:::=on.childs
            case _ => childs::=n
          }
          
        }
        override def toString = "O["+childs.mkString("|")+"]"
      }
      
      class FollowNode extends Node {
        override def toString = "F[" + childs.mkString(",") + "]"
        def append(n:Node) {
          n match {
            case ns:UnarOpNode => childs ::= ns
            case ns:OrNode => childs ::= ns            
            case _ => childs match {
              case Nil => childs::=n
              case x :: tail => x match {
                case c1:CharNode => n match {
                  case c2:CharNode => childs = new CharList(List(c2.c,c1.c)) :: tail
                }
                case c1:CharList => n match {
                  case c2:CharNode => c1.c ::= c2.c
                }
                case c1:UnarOpNode => childs ::= n
                case c1:OrNode   => childs ::= n
              }
            }
          }
        }
      }
      def apply(postfix:PostfixRe,verbose:Boolean=false) = {
        val l = postfix.length
        var i = 0
        var args = new Stack[Node]()

        println(postfix)
        while (i < l ) {
          val c = postfix(i)
          c match {
            case cp:IntervalPoint => 
              val el = new OrNode()
              var j = cp.start.toInt
              var end = cp.end.toInt
              while (j < end) {
                el.append(new CharNode(j.toChar))
                j+=1
              }
              args.push(el)
            case cp:AltPoint  => 
              val el = new OrNode()
              for (c <- cp.alts) {
                el.append(new CharNode(c))
              }
              args.push(el)
            case cp:CharPoint => args.push(new CharNode(cp.c))
            case cp:OrPoint   => 
              val a2 = args.pop
              val a1 = args.pop
              (a1,a2) match {
                case (x1 : CharNode,x2 : OrNode)   => 
                  x2.append(a1)
                  args.push(x2)
                case (x1 : UnarOpNode,x2 : OrNode)   => 
                  x2.append(a1)
                  args.push(x2)

                case (x1 : FollowNode,x2 : OrNode)   => 
                  x2.append(a1)
                  args.push(x2)

                case (x1 : FollowNode,x2 : FollowNode)   => 
                  val el = new OrNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)

                case (x1 : CharNode,x2 : CharNode)   => 
                  val el = new OrNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : UnarOpNode,x2 : FollowNode)   => 
                  val el = new OrNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : UnarOpNode,x2 : CharNode)   => 
                  val el = new OrNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : OrNode,x2 : OrNode)   => 
                  x2.append(a1)
                  args.push(x2)
                case _ => 
                  if (verbose)
                    println("Error!!! "+i+ "/"+l+". C="+c+" STACK="+args)
                  throw new MatchError("OrPoint have no match for a1=%s a2=%s".format(a1,a2))
              }
            case cp:ConcatPoint => 
              val a2 = args.pop
              val a1 = args.pop
              (a1,a2) match {
                case (x1 : OrNode,x2 : OrNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : CharNode,x2 : OrNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : CharNode,x2 : CharNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : UnarOpNode,x2 : CharNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : UnarOpNode,x2 : OrNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : CharNode,x2 : UnarOpNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)
                case (x1 : UnarOpNode,x2 : UnarOpNode)   => 
                  val el = new FollowNode()
                  el.append(a1)
                  el.append(a2)
                  args.push(el)

                //TODO:  Mix in one case
                case (x1 : FollowNode,x2 : CharNode) => 
                  x1.append(x2)
                  args.push(x1)
                case (x1 : FollowNode,x2 : OrNode) => 
                  x1.append(x2)
                  args.push(x1)

                case (x1 : FollowNode,x2 : UnarOpNode) => 
                  x1.append(x2)
                  args.push(x1)
                case _ => 
                  if (verbose)
                    println("Error!!! "+i+ "/"+l+". C="+c+" STACK="+args)
                  throw new MatchError("ConcatPoint have no match for a1=%s a2=%s".format(a1,a2))
              }
            case cp:PlusPoint => 
              val a1 = args.pop
              a1 match {
                case x:StarNode => args.push(a1)
                case x:QuestionNode => val el = new StarNode()
                  el.append(x.childs.head)
                  args.push(el)
                case x:PlusNode => val el = new StarNode()
                  el.append(x.childs.head)
                  args.push(el)
                case _ => val el = new PlusNode()
                  el.append(a1)
                  args.push(el)
              }
            case cp:StarPoint => 
              val a1 = args.pop
              a1 match {
                case x:StarNode => args.push(a1)
                case x:QuestionNode => val el = new StarNode()
                  el.append(x.childs.head)
                  args.push(el)
                case x:PlusNode => val el = new StarNode()
                  el.append(x.childs.head)
                  args.push(el)
                case _ => val el = new StarNode()
                  el.append(a1)
                  args.push(el)
              }
            case cp:QuestionPoint => 
              val a1 = args.pop
              a1 match {
                case x:QuestionNode => val el = new QuestionNode()
                  el.append(x.childs.head)
                  args.push(el)
                case x:StarNode => args.push(a1)
                case x:PlusNode => val el = new StarNode()
                  el.append(x.childs.head)
                  args.push(el)
                case _ => val el = new QuestionNode()
                  el.append(a1)
                  args.push(el)
              }
          }
          if (verbose)
            println(""+i+ "/"+l+". C="+c+" STACK="+args)
          i+=1
        }
        val a1 = postProcess(args.pop)

        val no = new ReTree(a1 match {
          case x:FollowNode => x
          case x:OrNode => 
            val el = new FollowNode()
            el.append(x)
            el
          case x:UnarOpNode  => 
            val el = new FollowNode()
            el.append(x)
            el
          case x => throw new Exception("Nonfollow Stack End"+x)
        })

        no
      }
      def postProcess(r:Node):Node = {
        def processChild(newL:List[Node],oldC:Node) = oldC match {
          case x:PlusNode => 
            var a1 = postProcess(x.childs.head)
            var a2 = new StarNode()
            a2.append(postProcess(x.childs.head))
            a1 :: a2 :: newL
          case _=> postProcess(oldC) :: newL
        }
        r match {
          case c:CharNode => new CharNode(c.c)
          case c:CharList => new CharList(c.c.reverse)
          case c:FollowNode => 
            val nc = new FollowNode()
            for (chld <- c.childs) {
              nc.childs = processChild(nc.childs,chld)
            }
            nc
          case c:QuestionNode =>
            val nc = new QuestionNode()
            for (chld <- c.childs) {
              nc.childs = processChild(nc.childs,chld)
            }
            nc
          case c:OrNode =>
            val nc = new OrNode()
            for (chld <- c.childs) {
              nc.childs = processChild(nc.childs,chld)
            }
            nc          
          case c:PlusNode =>
            val nc = new PlusNode()
            for (chld <- c.childs) {
              nc.childs = processChild(nc.childs,chld)
            }
            nc          
          case c:StarNode =>
            val nc = new StarNode()
            for (chld <- c.childs) {
              nc.childs = processChild(nc.childs,chld)
            }
            nc
      }
    }

    }
    class ReTree(var root:ReTree.FollowNode) {
      type Node = ReTree.Node
      def showDot() {
        val output = new java.io.FileOutputStream("/tmp/file.dot")
        output.write("digraph G {\ngraph [ordering=\"out\"];\n".getBytes)
        output.write(dotDump.getBytes)
        output.write("}".getBytes)
        output.close()
        Runtime.getRuntime.exec("dotty /tmp/file.dot")
      }
      def dotDump = {
        var nodes = Set[Node](root)
        var front = List[Node](root)

        while (! front.isEmpty) {
          val node = front.head
          front = front.tail
          for (c <- node.childs ) {
            if ( ! nodes.contains(c) ) {
              front::=c
              nodes +=c
            }
          }
        }
        val sb = new StringBuilder()
        
        var nodeNames = Map[Node,String]()
        var nameIdx = 0
        def getName(n:Node) = {
          nameIdx+=1
          (n match {
            case _:ReTree.FollowNode => "FollowNode"
            case _:ReTree.StarNode => "StarNode"
            case _:ReTree.OrNode => "OrNode"
            //case _ => "Node"
            case _ => "Node-"+n.toString
          }) + nameIdx.toString
        }
        for (n <- nodes) {
          val name = nodeNames.getOrElseUpdate(n,{ 
            getName(n)
          })
          sb++=("\"" + name + "\"" + "\n" )
          for (c <- n.childs) {
            val name2 = nodeNames.getOrElseUpdate(c,{ 
              getName(c)
            })
            sb++=("\"" + name +"\"" + " -> " +"\""+ name2 + "\""+"\n")
          }
        }
        
        sb.toString
      }
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
    



    def matchNFA2(startFront:List[BaseState],s:Iterator[Char],lcp:Int,debugLevel:Int=0):Boolean = {
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

        // var clist:List[BaseState] = addstate(Set(),nfa).toList
        var front = startFront
        var found = false

        while ( s.hasNext && ! found ) {
            val c = s.next()
            if ( debugLevel > 0 ) printf("%2d.\t %c %s\n",i,c,front)
            val nextfront = step(front, c)
            found = nextfront.exists {_ == MatchState}

            front = nextfront
            i+=1
        }
        if ( debugLevel > 0 ) printf("RESULT\t%s\n",front)

        found
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


    case class Interval(sp:Int,ep:Int) {
      val cnt = ep - sp
    }

    case class StatePoint(len:Int,state:BaseState,_intervals: List[Interval]) extends Ordered[StatePoint] {
      val intervals:List[Interval] = _intervals
      val icnt = intervals.size
      val cnt = intervals.foldLeft(0)(_ + _.cnt)
      
      def compare(that:StatePoint) = if (that.len < this.len) 1 else if (that.len > this.len) -1 else 0

      override def toString = "(%s:%d,icount=%d,len=%d)" format (state,len,icnt,cnt)

      lazy val nextStates = state match {
        case el:TermState =>  el.next.outStates
        case _ => ???
      }
      def expand(sa:SuffixWalkingAlgo):List[StatePoint] = {
          var ret = List()

          state match {
              case el @ ConstState(chr1,_)  => 
                var ret = List[Interval]()
                for (itvl <- intervals) {
                  sa.getPrevRange(itvl.sp,itvl.ep,chr1) match {
                    case Some((sp1,ep1))      => ret ::= Interval(sp1,ep1)
                    case None                 => 
                  }
                }
                if ( ! ret.isEmpty)
                  nextStates.map {StatePoint(len+1,_,ret)}
                else 
                  List()
              case el @ IntervalState(start,end,_) => 
                var ret = List[Interval]()
                for (itvl <- intervals; chr1 <- start until end) {
                  sa.getPrevRange(itvl.sp,itvl.ep,chr1) match {
                    case Some((sp1,ep1))      => ret ::= Interval(sp1,ep1)
                    case None                 => 
                  }
                }
                if ( ! ret.isEmpty)
                  nextStates.map {StatePoint(len+1,_,ret)}
                else 
                  List()
          }
      }
    }
    case class SATip(state:BaseState,len:Int,sp:Int,ep:Int)
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

    def paralelSearch(nfa:BaseState,lcpa:LCPSuffixWalkingAlgo, debugLevel:Int=0,maxIterations:Int=0,maxLength:Int=0) = {
      def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("paralelSearch: " +s).format(xs: _*))
      def dump(lcpa:LCPSuffixWalkingAlgo,i:Int,len:Int,l:Int=10) = {
        val it = lcpa.getStringOn(i)
        var s=""
        for (i <- 0 until l if it.hasNext) {
          //if ( i == len ) {
          //  s+="["+it.next().toString + "]"
          //} else {
          s+=it.next().toString
          //}
        }
        s
      }
      var results = List[SAResult]()
      
      val (tipResults,tips) = getSATip(nfa,lcpa,debugLevel=debugLevel-1,branchingFactor=10)
      println("tipResults",tipResults)
      //println("tips",tips)

      val tip = tips.head
      //for (tip <- tips ) {
        println("Take Tip",tip)
        
        var i = tip.sp
        while ( i < tip.ep ) {
          val lcp = if (i == tip.ep-1) 0 else lcpa.getLCP(i)
          println("Test string %d len=%d lcp=%d %s".format(i,tip.len,lcp,dump(lcpa,i,tip.len+1) ))
          matchNFA2(List(tip.state),lcpa.getStringOn(i),lcp,debugLevel-1)
          i+=1
        } 
        
      //}

    }

    def getSATip(nfa:BaseState,sa:SuffixWalkingAlgo,debugLevel:Int=0,branchingFactor:Int=100) = {
      def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("getSATip: " +s).format(xs: _*))
      var results = List[SAResult]()
      var i = 0
      var pqFront = new scala.collection.mutable.PriorityQueue[StatePoint]()
      for ( s <- nfa.outStates ) {
          pqFront.enqueue(StatePoint(0,s,List(Interval(0,sa.n))))
      }
      var frontSize = pqFront.size

      val statesSeenIM = pqFront.toList.groupBy { _.state }.map { 
        s:(BaseState, List[StatePoint]) =>  s._1 -> s._2.map { c:StatePoint => c.intervals }.flatten
      }
      val statesSeen:Map[BaseState,List[Interval]] = Map(statesSeenIM.toSeq:_*)

      

      while( ! pqFront.isEmpty && frontSize < branchingFactor ) {
          val state = pqFront.dequeue
          frontSize-=state.icnt
          val newStates = state.expand(sa)
          newStates.foreach {
              s:StatePoint=> s.state match {
                case MatchState => 
                  for (intl <- s.intervals) {
                    results::=SAResult(sa,s.len,intl.sp,intl.ep)
                  }
                case _ => 
                  frontSize+=s.icnt
                  pqFront.enqueue(s)
              }
          }
          i+=1          
      }
      var tips = pqFront.map{ sp:StatePoint => 
        sp.intervals.map{ in:Interval => 
          SATip(sp.state,sp.len,in.sp,in.ep)
        }
      }.flatten.toList
      println(pqFront)
      debug(1,"Result = %s, Tips=%s",results,tips.length)
      (results,tips)
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
            pqFront.enqueue(StatePoint(0,s,List(Interval(0,sa.n))))
        }
        /*
        var statesFront = liststates( Set(),nfa ).map {
            StatePoint(0,_,0,sa.n)
        }.toList
        */

        val statesSeenIM = pqFront.toList.groupBy { _.state }.map { 
          s:(BaseState, List[StatePoint]) =>  s._1 -> s._2.map { c:StatePoint => c.intervals }.flatten
        }
        val statesSeen:Map[BaseState,List[Interval]] = Map(statesSeenIM.toSeq:_*)
        

        
        debug(2,"Start statesFrom %s",pqFront)
        //var statesFront = Set(StatePoint(addstate(Set(),nfa).toList,0,0,sa.n))
        /*
        def debugOverlapCheck(s:StatePoint,sf:Iterable[StatePoint]) = sf.find({
          ss:StatePoint => s.state == ss.state && (s overlaps ss)
        })
        */
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
                      for (intl <- s.intervals) {
                        results::=SAResult(sa,s.len,intl.sp,intl.ep)
                      }
                    case _ if maxLength == 0 || s.len < maxLength => 
                      //val seenStates = statesSeen.getOrElse(s.state,List()) 
                      //val updati = (s.sp,s.ep) :: seenStates
                      //statesSeen(s.state) = updati
                      pqFront.enqueue(s) 
                      /*if ( s.cnt < 10 ) {
                          printf("%s SMALLI!\n",s)
                      } else {*/
                        /*
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
                          */
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
