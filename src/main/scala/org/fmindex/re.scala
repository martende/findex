package org.fmindex
import scala.util.parsing.combinator._

object ReParser extends RegexParsers {  

  // Regexp parser NODES

  trait Node {
    val startState:NfaBaseState
    val endState:NfaBaseState
    val nfa = startState
    def connect(next:Node) = {
      (this,next) match {
        case (f,t:Atom) => f.endState.link(t.startState,t.byte)
        //case (f:Atom,t:LinkedNodes) => f.endState.epsilon(t.startState)
        //case (f:Atom,t:LinkedNodes) => f.endState.epsilon(t.startState)
        case (f,t) => f.endState.epsilon(t.startState)
      }
    }
  }
  
  class LinkStartNode extends Node {
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = startState
  }
  
  class LinkEndNode extends Node {
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = startState
  }

  class Operator
  case class StarOperator extends Operator
  case class PlusOperator extends Operator
  case class QuestionOperator extends Operator
  case class OrOperator extends Operator

  case class OppedNode(s:Node,op:Operator) extends Node {
    
    lazy val dummyStart = new LinkStartNode()
    lazy val dummyEnd   = new LinkEndNode()

    override lazy val startState:NfaBaseState = dummyStart.startState
    override lazy val endState:NfaBaseState = dummyEnd.endState

    connectNodes()
    def connectNodes() {
      println("connectNodes",startState.name,endState.name,s,s.startState.name,s.endState.name)
      op match {
        case _:StarOperator =>
          dummyStart.connect(s)
          dummyStart.connect(dummyEnd)
          s.connect(dummyEnd)
          dummyEnd.connect(dummyStart)
        case _:PlusOperator =>
          dummyStart.connect(s)
          s.connect(dummyEnd)
          dummyEnd.connect(dummyStart)
        case _:QuestionOperator =>
          dummyStart.connect(s)
          dummyStart.connect(dummyEnd)
          s.connect(dummyEnd)
        case _ => 
      }
    }
  }
  
  abstract class BaseAtom extends Node

  case class Atom(s:String) extends BaseAtom {
    def byte:Int = s.getBytes.apply(0) & 0xff
    override lazy val startState:NfaBaseState = new NfaState()
    override lazy val endState:NfaBaseState = startState
    //def connect(v:Node) 
  }
  
  val charsRecognizedAsPunkt = List('a','b','c','d')
  
  case class AtomOR(allowedChars:List[Char]) extends BaseAtom {
    lazy val dummyStart = new LinkStartNode()
    lazy val dummyEnd   = new LinkEndNode()    
    override lazy val startState:NfaBaseState = dummyStart.startState
    override lazy val endState:NfaBaseState = dummyEnd.endState
    for(c <- allowedChars) {
      val n = Atom(c.toString)
      dummyStart.connect(n)
      n.connect(dummyEnd)
    }
  }

  case class LinkedNodes(chain:List[Node]) extends Node {
    lazy val dummyStart = new LinkStartNode()
    lazy val dummyEnd   = new LinkEndNode()

    override lazy val startState:NfaBaseState = dummyStart.startState
    override lazy val endState:NfaBaseState = dummyEnd.endState

    connectNodes()
    def splitORParts(chain:List[Node]) = {
      var parts = List[List[Node]]()
      var cur = chain
      var orPart = List[Node]()
      while ( ! cur.isEmpty ) {
        cur.head match {
          case OppedNode(v,_:OrOperator) => 
            parts = (v :: orPart).reverse :: parts
            orPart = List[Node]()
          case _ => 
            orPart = cur.head :: orPart
        }
        cur = cur.tail
      }
      parts = orPart.reverse :: parts
      if (parts.exists {_.isEmpty}) throw new Exception("| as empty part")
      parts
    }
    def connectConcatNodes(chain:List[Node]) {
      var cur = chain
      while ( ! cur.tail.isEmpty ) {
        cur.head.connect(cur.tail.head)
        cur = cur.tail
      }
      cur.head.connect(dummyEnd)
    }
    def connectNodes() {
      println("connectNodes",chain)
      //var cur = dummy :: chain
      var parts = splitORParts(chain)
      println("parts=" + parts)
      parts.foreach { x:List[Node] => 
        connectConcatNodes(dummyStart :: x) 
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
  
  def operator:Parser[Operator] = "*" ^^^ StarOperator() | "+" ^^^ PlusOperator() | "?" ^^^ QuestionOperator() | "|" ^^^ OrOperator()// | """\{\d,\d\}""".r ^^^ anyOp
  def item:Parser[Node] = item0 ~ operator ^^ { case v ~ op => OppedNode(v,op)  } | item0
  def item0:Parser[BaseAtom] = "." ^^^ AtomOR(charsRecognizedAsPunkt) | atom ^^ Atom | quoted  ^^ Atom 
  
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
    m ="m.*"
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
