package org.fmindex.re2

import scala.collection.mutable.Stack
import org.fmindex.SuffixWalkingAlgo
import org.fmindex.LCPSuffixWalkingAlgo
import scala.collection.mutable.Map
import scala.collection.mutable.PriorityQueue

object ReTree {
  trait Node {
    def append(n:Node)
    var childs = List[Node]()
    var parent:Node = RootNode
    var follows = List[Node]()
    val isNull:Boolean
    val firsts:List[Node]
  }
  
  object RootNode extends Node {
    def append(n:Node) = ???
    lazy val isNull = ???
    lazy val firsts = ???
    override def toString = "<<<ROOT>>>"
  }

  class CharNode(var c:Char) extends Node {
    def append(n:Node) = ???
    lazy val isNull = false
    lazy val firsts = List(this)
    override def toString = "["+c+"]"
  }
  //class CharList(var c:List[Char]) extends Node {
  //  override def toString = "["+c.mkString(",")+"]"
  //}
  trait UnarOpNode extends Node {
    override def append(n:Node) = {
      assert(childs.isEmpty)
      n.parent = this
      childs::=n
    }
    lazy val firsts = childs.flatMap(_.firsts)
  }

  class StarNode extends UnarOpNode {
    lazy val isNull = true
    override def toString = "*["+childs.mkString(",")+"]"
  }
  class QuestionNode extends UnarOpNode {
    lazy val isNull = true
    override def toString = "?["+childs.mkString(",")+"]"
  }


  class PlusNode extends UnarOpNode {
    lazy val isNull = childs.forall{_.isNull}
    override def toString = "+["+childs.mkString(",")+"]"
  }
  
  class OrNode() extends Node {
    lazy val firsts = childs.flatMap(_.firsts)
    lazy val isNull = childs.exists{_.isNull}
    def append(n:Node) = {
      n match {
        case on:OrNode =>
          on.childs.foreach{_.parent=this}
          childs:::=on.childs

        case _ => 
          n.parent = this
          childs::=n
      }
      
    }
    override def toString = "O["+childs.mkString("|")+"]"
  }
  
  class FollowNode extends Node {


    lazy val firsts = {
      var p = childs
      var ret = List[Node]()
      while (! p.isEmpty && p.head.isNull) {
        ret :::=p.head.firsts
        p = p.tail
      }
      if ( ! p.isEmpty ) ret :::=p.head.firsts

      ret
    }
    lazy val isNull = childs.forall{_.isNull}
    override def toString = "F[" + childs.mkString(",") + "]"
    def append(n:Node) = {
      n.parent = this          
      childs::=n
    }
    /*
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
    */
  }
  def apply(postfix:REParser.PostfixRe,verbose:Boolean=false) = {
    val l = postfix.length
    var i = 0
    var args = new Stack[Node]()
    if ( verbose )
      println(postfix)
    while (i < l ) {
      val c = postfix(i)
      c match {
        case cp:REParser.IntervalPoint => 
          val el = new OrNode()
          var j = cp.start.toInt
          var end = cp.end.toInt
          while (j < end) {
            el.append(new CharNode(j.toChar))
            j+=1
          }
          args.push(el)
        case cp:REParser.AltPoint  => 
          val el = new OrNode()
          for (c <- cp.alts) {
            el.append(new CharNode(c))
          }
          args.push(el)
        case cp:REParser.CharPoint => args.push(new CharNode(cp.c))
        case cp:REParser.OrPoint   => 
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
            case (x1 : CharNode,x2 : FollowNode)   => 
              val el = new OrNode()
              el.append(a1)
              el.append(a2)
              args.push(el)
            case (x1 : UnarOpNode,x2 : CharNode)   => 
              val el = new OrNode()
              el.append(a1)
              el.append(a2)
              args.push(el)
            case (x1 : UnarOpNode,x2 : UnarOpNode)   => 
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
        case cp:REParser.ConcatPoint => 
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
        case cp:REParser.PlusPoint => 
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
        case cp:REParser.StarPoint => 
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
        case cp:REParser.QuestionPoint => 
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
    val a0 = args.pop

    val a2 = a0 match {
      case x:FollowNode => x
      case x:OrNode => 
        val el = new FollowNode()
        el.append(x)
        el
      case x:UnarOpNode  => 
        val el = new FollowNode()
        el.append(x)
        el
      case x:CharNode  => 
        val el = new FollowNode()
        el.append(x)
        el
      case x => throw new Exception("Nonfollow Stack End"+x)
    }

    val a1 =  postProcess(a2).asInstanceOf[FollowNode]

    setParents(a1,RootNode)
    

    val no = new ReTree(a1)
    no
  }
  def setParents(r:Node,parent:Node=RootNode) {
    r.parent=parent
    for (chld <- r.childs) {
      setParents(chld,r)
    }
  }

  def postProcess(r:Node,parent:Node=RootNode):Node = {
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
      //case c:CharList => new CharList(c.c.reverse)
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
      //case c:PlusNode =>
      //  val nc = new PlusNode()
      //  for (chld <- c.childs) {
      //    nc.childs = processChild(nc.childs,chld)
      //  }
      //  nc          
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
    val nb = new StringBuilder()
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
        val label = n match {
          case c:ReTree.CharNode => c.c
          case _ => name
        }
        nb++=("node[style=%s,label=%s] \"%s\";\n".format((if (n.isNull) "solid" else "filled" ),label,name))
        
      for (c <- n.childs) {
        val name2 = nodeNames.getOrElseUpdate(c,{ 
          getName(c)
        })
        sb++=("\"" + name +"\"" + " -> " +"\""+ name2 + "\""+"\n")
      }
    }
    
    nb.toString + sb.toString
  }
}