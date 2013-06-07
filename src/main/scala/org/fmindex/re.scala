package org.fmindex
import scala.util.parsing.combinator._

object ReParser extends RegexParsers {
  
  
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

    var links = List[BaseNfaLink]()
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
      
      ret.append("digraph graphname {\n")
      
      for ( v <- visited;l<-v.links) l match {
        case _:EpsilonLink => ret.append("%s -> %s  [label=\"eps\"]\n" format (v.name,l.to.name))
        case ll:NfaLink    => ret.append("%s -> %s  [label=\"%c\"]\n" format (v.name,l.to.name,ll.chr))
      }
      ret append "}\n"
      
      ret.toString
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

  // Regexp parser NODES

  trait Node {
    val startState:NfaBaseState
    val endState:NfaBaseState
    val nfa = startState
    def connect(next:Node) = {
      (this,next) match {
        case (f,t:Atom) => f.endState.link(t.startState,t.byte)
        case (f,t) => println("NonConnect",f,t)
      }
    }
  }
  
  class LinkStartNode extends Node {
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = startState
  }

  class Operator
  case class StarOperator extends Operator
  case class PlusOperator extends Operator
  case class QuestionOperator extends Operator

  case class OppedNode(s:Node,op:Operator) extends Node {
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = new NfaState()
  }
  case class Atom(s:String) extends Node {
    def byte:Int = s.getBytes.apply(0) & 0xff
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = startState
    //def connect(v:Node) 
  }
  case class LinkedNodes(chain:List[Node]) extends Node {
    lazy val dummy = new LinkStartNode()

    override lazy val startState:NfaBaseState = dummy.startState
    override lazy val endState:NfaBaseState = chain.last.endState

    connectNodes()
    def connectNodes() {
      var cur = dummy :: chain
      while ( ! cur.tail.isEmpty ) {
        cur.head.connect(cur.tail.head)
        cur = cur.tail
      }
    }
  }

  case class StartEndNode(inner:Node) extends Node {
    override lazy val startState:NfaBaseState = new NfaStartState()
    override lazy val endState:NfaBaseState = new NfaFinishState()
    startState.epsilon(inner.startState)
    inner.endState.epsilon(endState)
  }

  def S:Parser[Node] = expr ^^ StartEndNode

  //def factor:Parser[Any] = item | "(" ~ expr ~ ")" 
  def expr:Parser[Node] = rep(item | expression) ^^ {
    // aj jaj jaj
    // case nodes:List[Node] => LinkedNodes(nodes)
    //case nodes:List => {
    //  LinkedNodes(nodes.asInstanceOf[List[Node]])
    //}
    //case v => println(v,v.getClass);v
    v => val l = v.length 
      if ( l > 1 ) LinkedNodes(v) 
      else if ( l == 1 ) v.head
      else LinkedNodes(v) 
  }
  def expression:Parser[Node] = "(" ~ expr ~ ")"  ~ opt(operator) ^^ {
    case "(" ~ expr ~ ")" ~ Some(op) => OppedNode(expr,op)
    case "(" ~ expr ~ ")" ~ None => expr
  }
//item | "(" ~ expr ~ ")" | item ~~ expr
  
  def operator:Parser[Operator] = "*" ^^^ StarOperator() | "+" ^^^ PlusOperator() | "?" ^^^ QuestionOperator() // | """\{\d,\d\}""".r ^^^ anyOp
  def item:Parser[Node] = item0 ~ operator ^^ { case v ~ op => OppedNode(v,op)  } | item0
  def item0:Parser[Atom] = atom ^^ Atom | quoted  ^^ Atom
  def atom:Parser[String] = "[a-z]".r 
  def quoted:Parser[String] = 
    "\\" ~> atom ^^ {
      case "n" => "\n"
      case v => v
    } | 
    "\\\\"  ^^^ "\\"

  def parseItem(str: String) = parseAll(S, str) match {
    case Success(result, _) => result
    // TODO: Correct error
    case failure : NoSuccess => scala.sys.error(failure.msg)
  }
}


object RePlay extends optional.Application {
  def main() {
    var m = "ab(cd?e)+k(aaa)k*\\fkk(ssk)*"
    m ="ab(d)c"
    println(m)
    val x = ReParser.parseItem(m)
    println(x)
    val dotDump = x.nfa.dotDump
    
    println(dotDump)
    
    val output = new java.io.FileOutputStream("/tmp/file.dot")
    output.write(dotDump.getBytes)
    output.close()

    //Resource.fromFile("/tmp/file.dot").write(dotDump)

    Runtime.getRuntime.exec("dotty /tmp/file.dot")
  }
}
