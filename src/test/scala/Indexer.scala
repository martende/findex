package org.findex.test

import org.scalatest.FunSuite
import scala.collection.mutable.Stack
import org.findex._

class ExampleSuite extends FunSuite {
  def fromString(ts:String) = ts.getBytes ++ List(0.asInstanceOf[Byte])
  test("SuffixArray basics") {
    /*
                         01234567890 */
    val bs = fromString("missisippi")
    val sa = new SuffixArray(bs)
    assert(sa.n==11)
    println("fleft = " + sa.leftS(sa.n-1) )
    /*
    sa.build
    println(sa)
    println(new String(bs))

    IndexMaker.buildSA("ololshka".getBytes)
    println("helloWorld")
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    val oldSize = stack.size
    val result = stack.pop()
    assert(result === 2)
    assert(stack.size === oldSize - 1)
    */
  }
                    /*
  test("pop is invoked on an empty stack") {

    val emptyStack = new Stack[Int]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
    assert(emptyStack.isEmpty)
  }*/
}