package parsing

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

trait Location{
  def position: Int
  def isTheEnd : Boolean = input.length == position
  def input : String
  // matchRegex just match the first element of a string and return an Option accordingly.
  // It uses the function findPrefixOf as provided by the Scala regex library
  def matchRegex(r:Regex): Option[(Location, String)] =
    r.findPrefixOf(this.input.substring(position)) match {
      case Some(str) => {
        val newLoc = new StringInput(this.position + str.length, this.input)
        Some(newLoc, str)
      }
      case None => None
    }
}

case class StringInput(position:Int, input: String) extends Location


object Location {
  implicit def fromString(s: String): Location = new StringInput(0, s)
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

private[parsing] sealed trait ParseState[+A]

case class ParseSuccess[A](loc: Location, result: A, isCommit: Boolean) extends ParseState[A]
case class ParseFailure(loc: Location, exMsg: String, isCommit: Boolean) extends ParseState[Nothing]{
  override def toString: String = exMsg
}

trait Parser[A] {
  self =>
  def apply(loc: Location): ParseState[A]

  // Parse is basically a wrap around apply and returns a Try[A] based on the ParseState[A] returned by the apply method
  // If ParseState is a success but the entire input is not parsed, then parse should fail
  def parse(loc: Location): Try[A] = {
    apply(loc) match {
      case ParseSuccess(newLoc, result, _) => {
        if (newLoc.isTheEnd) Success(result)
        else Failure(new Exception("Could not parse the whole string"))
      }
      case ParseFailure(_, msg, _) =>
        Failure(new Exception(msg))
    }
  }

  // Given two parsers pa1, pa2: Parser[A] the parser pa1 orElse pa2 first applies pa1 and returns its result on success.
  // If pa1 failrs, the composite parser applies pa2 on the same input sequence as pa1 and returns pa2's result.
  def orElse(p: => Parser[A]): Parser[A] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc, result, _) => ParseSuccess(newLoc, result, true)
      case ParseFailure(newLoc, msg, _) if (newLoc.position > loc.position) => ParseFailure(newLoc, msg, true)
      case ParseFailure(_, _, _) => p(loc)
    }

  // Given two parsers pa: Parser[A] and pb: Parser[B], the parser pa andThen pb works by first applying pa to parse the first part of the input sequence.
  // If pa succeeds with some result value a: A, it then applies pb to the remainder of the input.
  // If pb also succeeds with some result value b: B, then the composite parser returns the pair (a, b).
  // If either pa or pb fails, then the composite parser fails as well.
  def andThen[B](p: => Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- p
    } yield (a, b)

  // Given a parser pa: Parser[A] and a function f: A => B, the parser pa.map(f) applies pa to parse the input sequence producing a: A
  // as an intermediate result value if successful, and then computes f(a): B as the final result value of the parse.
  def map[B](f: A => B): Parser[B] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc, result, isCommit) => ParseSuccess(newLoc, f(result), isCommit)
      case ParseFailure(newLoc, msg, isCommit) => ParseFailure(newLoc, msg, isCommit)
    }

  // Given a parser pa: Parser[A] and a function f: A => Parser[B], the parser pa.flatMap(f) first applies pa to the input sequence
  // producing a partial parse with intermediate result a: A if successful. Using a it then constructs the parser f(a): Parser[B]
  // and uses this subparser to parse the remaining input. f(a) then produces the final result of type B. If pa or f(a) fail then
  // so does pa.flatMap(f).
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    loc => apply(loc) match {
      case ParseSuccess(newLoc, result, _) => f(result).apply(newLoc)
      case ParseFailure(newLoc, msg, isCommit) => ParseFailure(newLoc, msg, isCommit)
    }
}


object Parser {

  // Given a parser pa: Parser[A], the parser repeat(a) applies pa as many times as needed to parse the input sequence,
  // producing the list of result values of the subparses if successful
  def repeat[A](p: Parser[A]): Parser[List[A]] =
    attempt(p andThen repeat(p) map { case (x,xs) => x::xs}) orElse {loc => ParseSuccess(loc, List[A](),true) }

  // if the pattern accepted by the current parser repeats exactly n times for some given n
  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] = {
    val newN = n-1
    if (newN >= 0) { p andThen repeatN(newN)(p) map { case (x,xs) => x::xs} }
    else {loc => ParseSuccess(loc, List[A](), true) }
  }

  // parser attempt(p) should behave like p, except that if p fails,
  // its returned parse state is reset to an uncommitted state, indicating that it is backtrackable
  def attempt[A](p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res,_) => ParseSuccess(newLoc,res,true)
      case ParseFailure(_,msg,_) => ParseFailure(loc,msg,false)
    }

  // The parser label(msg)(p) should behave like p.
  // However, if p fails, then the error message contained in the error state produced by p should be replaced by msg.
  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res,_) => ParseSuccess(newLoc,res,true)
      case ParseFailure(_,_,_) => ParseFailure(loc,msg,true)
    }

  // The parser tag(msg)(p) should again behave like p.
  // However, if p fails, then the error message in p's final error state should be extended with msg.
  def tag[A](msg: String)(p: Parser[A]): Parser[A] =
    loc => p.apply(loc) match {
      case ParseSuccess(newLoc,res,_) => ParseSuccess(newLoc,res,true)
      case ParseFailure(_,origMsg,_) => ParseFailure(loc,origMsg+" "+msg,true)
    }

  // returns a parser that accepts only the input sequence consisting of the single character c
  implicit def char(c: Char): Parser[Char] =
    c.toString.r map (result => result.charAt(0))

  // returns a parser that accepts only the input sequence consisting of the single string s
  implicit def string(s: String): Parser[String] =
    s.r

  // returns a parser that accepts only the input sequence matching the regex r
  implicit def regex(r: Regex): Parser[String] =
    loc => loc.matchRegex(r) match {
      case Some((loc1, result)) => ParseSuccess(loc1, result, true)
      case None => ParseFailure(loc, "Error (" + loc.position + ") Found '" + loc.input.substring(loc.position) + "' but expected '" + r.regex + "'", false)
    }

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)

  def digits: Parser[Int] =
    (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }
  }


