package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._

  //some ideas for unit tests
  "Char Parser" should "parse the character properly" in {
    assert('a'.parse('a'.toString).get === 'a')
    //assert((label("b expected")('b')).parse('a'.toString).toString === "b expected")
  }
  assert("ab".parse("ab").get === "ab")
  assert(('a' orElse 'b').parse('a').get === 'a')

  assert(('a' orElse 'b').parse('b').get === 'b')

  assert(('a' andThen 'b').parse("ab").get === ('a', 'b'))

  assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))

  assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))

  assert((repeat('a') map (_.size)).parse("aaa").get === 3)

  "foo" should "do something useful" in {
    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
  }

  "foo2" should "do something useful" in {
    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
  }
}
