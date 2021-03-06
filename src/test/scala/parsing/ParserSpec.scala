package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._

  //some ideas for unit tests

  "Char Parser" should "parse the character properly" in {
    assert('a'.parse('a'.toString).get === 'a')
    assert('b'.apply('a'.toString).toString === "Error (0) Found 'a' but expected 'b'")
    assert((label("b expected")('b')).apply('a'.toString).toString === "b expected")
    assert((tag("at end of line")('b')).apply('a'.toString).toString === "Error (0) Found 'a' but expected 'b' at end of line")
    assert((tag("at end of line")(label("b expected")('b')).apply('a'.toString).toString === "b expected at end of line"))
  }

  "String Parser" should "parse the string properly" in {
    assert("ab".parse("ab").get === "ab")
    assert("ab and cd".parse("ab and cd").get === "ab and cd")
    //assert("ab".apply('a'.toString).toString === "Error (0) Found 'ac' but expected 'ab'")
    assert((string("aabb") orElse string("aacc")).parse("aacc").get === "aacc")
  }

  "orElse Parser" should "work properly" in {
    assert(('a' orElse 'b').parse('a').get === 'a')
    assert(('a' orElse 'b').parse('b').get === 'b')
    assert(('b' orElse 'a').parse('a').get === 'a')
    assert((('a' andThen 'a') orElse ('a' andThen 'b')).apply("ab").toString === "Error (1) Found 'b' but expected 'a'")
    assert(((char('a')) andThen ((char('b')) orElse (char('a'))) andThen (char('c'))).parse("aac").get === (('a','a'),'c'))
    assert(((string("ab") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get ===("cd","ab"))
    assert(((string("cd") andThen string("ba")) orElse (string("cd") andThen string("ab"))).apply("cdab").toString ==="Error (2) Found 'ab' but expected 'ba'")
  }
  "andThen Parser" should "work properly" in {
    assert(('a' andThen 'b').parse("ab").get === ('a', 'b'))
    assert(('a' andThen ('b' orElse 'a')).parse("aa").get === ('a', 'a'))
  }

  "repeat Parser" should "work properly" in {
    assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))
    assert((repeat('a') map (_.size)).parse("aaa").get === 3)
    assert(repeat("[a-zA-Z]".r).parse("FlyHigh").get.mkString === "FlyHigh")
  }

  "repeatN Parser" should "work properly" in {
    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
  }

  "digit" should "work properly" in {
    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
    assert((double.parse("23.56").get === 23.56))
  }

  "list" should "work properly" in {
    assert( list( char('a'),char(':')).parse("a:a").get === List('a', 'a'))
    assert(("\\[" ~> list("abc", char(',')) <~ "\\]").parse("[abc,abc]").get === List("abc","abc"))
  }

  "attempt" should "work properly" in {
    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
    assert((attempt(string("cd") andThen string("ba")) orElse (string("cd") andThen string("ab"))).parse("cdab").get ===("cd","ab"))
  }
}
