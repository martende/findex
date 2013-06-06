package org.scala.fmindex
import scala.util.parsing.combinator._

object ReParser extends RegexParsers {
  
  case class Item(s:String)
  def S = expr

  //def factor:Parser[Any] = item | "(" ~ expr ~ ")" 
  def expr:Parser[Any] = rep(item | expression) 
  def expression = "(" ~ expr ~ ")"  ~ opt(operator) ^^ {
     case "(" ~ expr ~ ")" ~ op => expr
  }
//item | "(" ~ expr ~ ")" | item ~~ expr
  
  def operator = "*" | "+" | """\{\d,\d\}""".r
  def item:Parser[Any] = item0 ~ "*" | item0
  def item0:Parser[Any] = atom | quoted 
  def atom:Parser[String] = "[a-z]".r 
  def quoted:Parser[Any] = 
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
    val m = "ab(cde)kk*kk(ssk)+"
    println(m)
    println(ReParser.parseItem(m))
  }
}
