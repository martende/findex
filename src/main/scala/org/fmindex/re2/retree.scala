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
    lazy val follows:List[CharNode] = {
      parent match {
        case RootNode => List()
        case x:OrNode => x.follows
        case x:FollowNode => 
          var last = x.childs.dropWhile(_!=this).tail
          if ( ! last.isEmpty ) {
            var ret = last.head.firsts
            if ( last.head.isNull ) {
              last = last.tail
              while (! last.isEmpty && last.head.isNull) {
                ret :::= last.head.firsts
                last = last.tail
              }
              if (! last.isEmpty) {
                ret :::= last.head.firsts
              }
            }
            ret 
          } else x.follows
        case x:StarNode     => firsts ::: x.follows
        case x:QuestionNode => /*firsts :::*/ x.follows
        case _ => List()
      }
    }

    lazy val isLast:Boolean = {
      parent match {
        case RootNode => true
        case _:OrNode => parent.isLast
        case _:UnarOpNode => parent.isLast
        case x:FollowNode => 
          var last = x.childs.dropWhile(_!=this).tail
          if ( last.isEmpty || last.forall(_.isNull) ) parent.isLast else false
        case _ => true
      }
    }
    val isNull:Boolean
    val firsts:List[CharNode]
  }
  
  object RootNode extends Node {
    def append(n:Node) = ???
    lazy val isNull = ???
    lazy val firsts = ???
    override def toString = "<<<ROOT>>>"
  }

  class CharNode(var c:Char) extends Node {
    def append(n:Node) = ???
    var num = 0
    lazy val isNull = false
    lazy val firsts = List(this)
    override def toString = if (c >= 0x20 && c < 0x7f) "%c".format(c) else "%02x".format(c.toInt)
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
      var ret = List[CharNode]()
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
  def apply(postfix:REParser.PostfixRe,verbose:Boolean=false,removeNulls:Boolean=true) = {
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
            case (x1 : FollowNode,x2 : CharNode)   => 
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

    val a1 = postProcess(a2).asInstanceOf[FollowNode]
    val a3 = if ( removeNulls ) removeBorderNulls(a1) else a1

    setParents(a3,RootNode)
    setNums(a3)

    val no = new ReTree(a3.asInstanceOf[FollowNode])
    no
  }
  def removeBorderNulls(a1:FollowNode):FollowNode = {
    val n = new FollowNode()
    var p = a1.childs
    while (! p.isEmpty && p.head.isNull ) p = p.tail

    p = p.reverse

    while(! p.isEmpty && p.head.isNull ) p = p.tail

    while(! p.isEmpty ) {
      n.append(p.head)
      p = p.tail
    }
    n
  }
  def setParents(r:Node,parent:Node=RootNode) {
    r.parent=parent
    for (chld <- r.childs) {
      setParents(chld,r)
    }
  }

  def setNums(r:Node):Int = {
    def _setNums(r:Node,_idx:Int):Int = {
      var idx = _idx
      def __setNums(r:Node):Int = {
        r match {
          case x:OrNode => 
            var nidx = idx
            for (chld <- r.childs) {
              chld match {
                case x:CharNode => 
                  x.num = idx
                  nidx = nidx max idx + 1
                case _ => nidx = nidx max _setNums(chld,idx) 
              }
            }
            idx = nidx
          case _ => 
            for (chld <- r.childs) {
              chld match {
                case x:CharNode => x.num = idx;idx+=1
                case _ => __setNums(chld)
              }
            }
        }
        idx
      }
      __setNums(r)
    }

    _setNums(r,1)
  }

  /*
  def setNums(r:Node) {
    var idx = 1
    def _setNums(r:Node) {
      for (chld <- r.childs) {
        chld match {
          case x:CharNode => x.num = idx;idx+=1
          case _ => _setNums(chld)
        }
      }
    }
    _setNums(r)
  }
  */
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
  def showDot(showFirsts:Boolean=false,showNums:Boolean=true,showFollows:Boolean=false) {
    val output = new java.io.FileOutputStream("/tmp/file.dot")
    output.write("digraph G {\ngraph [ordering=\"out\" splines=true];\n".getBytes)
    output.write(dotDump(showFirsts,showNums,showFollows).getBytes)
    output.write("}".getBytes)
    output.close()
    Runtime.getRuntime.exec("dotty /tmp/file.dot")
  }
  def dotDump(showFirsts:Boolean=false,showNums:Boolean=true,showFollows:Boolean=false) = {
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
    val sb2 = new StringBuilder()
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
        val label = (n match {
          case c:ReTree.CharNode => c.c + (if (showNums) "\\n"+c.num else "")
          case _ => name + (if (showFirsts) "\\n"+n.firsts.mkString(",") else "")
        } ) 
        val shape = (n match {
          case c:ReTree.CharNode => "circle"
          case _ => "box"
        } )
        nb++=("node[shape=%s,style=%s,label=\"%s\"] \"%s\";\n".format(shape,(if (n.isNull) "solid" else "filled" ),label,name))
        
      for (c <- n.childs) {
        val name2 = nodeNames.getOrElseUpdate(c,{ 
          getName(c)
        })
        sb++=("\"" + name +"\"" + " -> " +"\""+ name2 + "\""+" [weight=100000]" +";\n")
      }
      if ( showFollows ) {
        n match {
          case c:ReTree.CharNode => 
            for (c <- c.follows) {
              val name2 = nodeNames.getOrElseUpdate(c,{ 
                getName(c)
              })
              sb2++=("\"" + name +"\"" + " -> " +"\""+ name2 + "\""+" [style=dotted weight=1]" +";\n")
            }
          case _ => 
        }
      }
      
    }
    
    nb.toString + sb.toString + sb2.toString
  }

  case class StatePoint(len:Int,sp:Int,ep:Int,state:ReTree.CharNode) extends Ordered[StatePoint] {
      val cnt = ep - sp
      def compare(that:StatePoint) = if (that.state.num < this.state.num) -1 else if (that.state.num > this.state.num) 1 else 0

      // override def toString = "(%s:%d,icount=%d,len=%d)" format (state,len,icnt,cnt)
  }


  def matchSA(sa:SuffixWalkingAlgo,debugLevel:Int=0,maxBranching:Int=1024,maxIterations:Int=1000,maxClasterLen:Int=100):List[SAResult] = {
    def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("matchSA: " +s).format(xs: _*))
    def retsCount(r:List[SAResult]) = r.view.map(_.cnt).sum 
    //def frontCount(r:List[StatePoint]) = r.view.map(_.cnt).sum 
    def front2Res(r:Iterable[StatePoint]) = r.view.map { x:StatePoint => SAResult(sa,x.len,x.sp,x.ep) }.toList

    val (ret,pqFront) = _matchSA(sa,root.firsts.map { StatePoint(0,0,sa.n,_) },debugLevel,maxBranching,maxIterations)
    
    if (pqFront.isEmpty) ret else {
      var fronts:List[List[SAResult]] = List( front2Res(pqFront)  )
      var curFront = pqFront
      while (! curFront.isEmpty ) {
        val farestStateNum = curFront.min.state.num
        val newStateFront  = curFront.filter(_.state.num == farestStateNum).groupBy(_.state).keys.toList
        val (ret2,pqFront2) = _matchSA(sa,newStateFront.map { StatePoint(0,0,sa.n,_) },debugLevel,maxBranching,maxIterations)
        curFront = pqFront2
        if ( pqFront2.isEmpty ) {
          fronts::=ret2
        } else {
          fronts::=front2Res(pqFront2)
        }
      }
      // 4 possible strategies 
      // 1. some really small - block < 1000 zb 
      // 2. medium size blocks that can be merged
      // 3. just minimum large block that can be bruteforced
      // 4. huge block - what should we do just bruteforce all
      val sizes = fronts.view.map {x => retsCount(x) ->  x}
      if ( debugLevel >= 1 ) {
        println("Alternatives found")
        sizes.view.foreach { x=> println(x._1) }
        println("------------------")
      }
      val minSize = sizes.view.map {_._1}.min

      if ( minSize < 10000 ) {
        debug(1,"minSize %d is rather small - take first alternative",minSize)
        val Some(vars) = sizes.view.find({_._1 == minSize})
        val oneRes = vars._2.head
        println(oneRes.sp,oneRes.ep,oneRes.len)
      }
      //for (c <- fronts) {
      //  println("Claster FrontsLen=%d".format(retsCount(c)))
      //}
      
      ret
    }
  }
  def _matchSA(sa:SuffixWalkingAlgo,inputStates:List[StatePoint],debugLevel:Int=0,maxBranching:Int=1024,maxIterations:Int=1000) = {
    def debug(l:Int,s: =>String  ,xs: Any*) = if (l<=debugLevel) println(("matchSA: " +s).format(xs: _*))

    var pqFront = new PriorityQueue[StatePoint]()
    var i = 1
    var ret = List[SAResult]()
    pqFront++=inputStates
    
    debug(1,"Start searching front %s",pqFront)

    while (! pqFront.isEmpty && pqFront.length < maxBranching && ( maxIterations==0 || i < maxIterations ) ) {
      val q = pqFront.dequeue

      debug(2,"%d Take State=%s#%d FrontSize=%d",i,q.state,q.state.num,pqFront.length)

      sa.getPrevRange(q.sp,q.ep,q.state.c) match {
        case Some((sp1,ep1))      => 
          debug(2,"%d Found for State=%s Interval = [ %d-%d ] ",i,q.state,sp1,ep1)
          if ( q.state.isLast ) {
            debug(2,"%d Found Matching for State=%s",i,q.state)
            ret ::= { SAResult(sa,q.len+1,sp1,ep1) }
          } else {
            debug(2,"%d Expands to %s ",i,q.state.follows)
            pqFront++=q.state.follows.map { StatePoint(q.len+1,sp1,ep1,_) }
          }
        
          // ret ::= Interval(sp1,ep1)
        case None                 => 
          debug(2,"%d Found for State=%s Nothing Found ",i,q.state)
      }

      i+=1
    }
    
    (ret,pqFront)
  }
}