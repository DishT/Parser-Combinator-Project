package parsing

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

trait Location{
  def position: Int
  def isTheEnd : Boolean = input.length == position
  def input : String
  def matchRegex(r:Regex): Option[(Location, String)] =
    r.findPrefixOf(this.input.substring(position)) match{
      case Some(str) => {
        val newLoc = new StringInput(this.position+str.length, this.input)
        Some(newLoc, str)
      }
      case None => None
    }
}

case class StringInput(position:Int, input: String) extends Location //update the remaining parameters


object Location {
  implicit def fromString(s: String): Location = new StringInput(0, s)
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

private[parsing] sealed trait ParseState[+A]

case class ParseSuccess[A](loc: Location, result: A) extends ParseState[A]
case class ParseFailure(loc: Location, exMsg: String) extends ParseState[Nothing] //update the second parameter to error

trait Parser[A] {
  self =>
  protected def apply(loc: Location): ParseState[A]

  def parse(loc: Location): Try[A] =
    apply(loc) match{
      case ParseSuccess(newLoc, result) => {
        if (newLoc.isTheEnd) Success(result)
        else Failure(new Exception("Parse Failed")) // throw an error
      }
      case ParseFailure(newLoc, msg) => {
        Failure(new Exception(msg))
      }
    }

  
  def orElse(p: => Parser[A]): Parser[A] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc, result) => ParseSuccess(newLoc, result)
      case ParseFailure(_,_) => p(loc)
    }


  def andThen[B](p: => Parser[B]): Parser[(A, B)] = {
    for {
      a <- this
      b <- p
    } yield (a, b)
  }
  
  def map[B](f: A => B): Parser[B] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc, result) => ParseSuccess(newLoc, f(result))
      case ParseFailure(newLoc,msg) => ParseFailure(newLoc,msg)
    }
  
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc,result) => f(result).apply(newLoc)
      case ParseFailure(newLoc,msg) => ParseFailure(newLoc,msg)
    }
  }


object Parser {
  def repeat[A](p: Parser[A]): Parser[List[A]] =
    p andThen repeat(p) map { case (x,xs) => x::xs} orElse {loc => ParseSuccess(loc, List[A]()) }

  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] = {
    val newN = n-1
    if (newN >= 0) { p andThen repeatN(newN)(p) map { case (x,xs) => x::xs} }
    else {loc => ParseSuccess(loc, List[A]()) }
  }

  def attempt[A](p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res) => ParseSuccess(newLoc,res)
      case ParseFailure(_,msg) => ParseFailure(loc,msg)
    }

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res) => ParseSuccess(newLoc,res)
      case ParseFailure(_,_) => ParseFailure(loc,msg)
    }

  def tag[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res) => ParseSuccess(newLoc,res)
      case ParseFailure(_,origMsg) => ParseFailure(loc,origMsg+msg)
    }

  implicit def char(c: Char): Parser[Char] =
    c.toString.r map (result => result.charAt(0))

  implicit def string(s: String): Parser[String] =
    s.r

  implicit def regex(r: Regex): Parser[String] =
    loc => loc.matchRegex(r) match {
      case Some((loc1, result)) => ParseSuccess(loc1, result)
      case None => ParseFailure(loc, "Expected "+loc.input.charAt(0))
    }

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)

  def digits: Parser[Int] =
    (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }
  }


