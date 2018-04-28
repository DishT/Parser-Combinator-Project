package json

import parsing._

object JSON {

  sealed abstract class JSON

  case object JNull extends JSON
  case class JBool(b: Boolean) extends JSON
  case class JNumber(n: Double) extends JSON
  case class JString(s: String) extends JSON
  case class JArray(a: IndexedSeq[JSON]) extends JSON
  case class JObject(a: Map[String, JSON]) extends JSON
//
/*
  def jsonParser: Parser[JSON] = new Parser[JSON] {
    import Parser._

    def apply(loc: Location) = jValue.apply(loc)

    def jValue: Parser[JSON] = ((((jBool orElse jNumber) orElse jNull) orElse jString) orElse jArray) orElse jObject

    def jNull: Parser[JNull.type] = string("null") map (_ => JNull)

    def jBool: Parser[JBool] = string("true") map (_ => JBool(true)) orElse(string("false") map (_ => JBool(false)))

    def jNumber: Parser[JNumber] = digits map (JNumber(_))

    def jString: Parser[JString] = repeat("[a-zA-Z]".r) map (_.mkString) map (JString(_))

    def jArray: Parser[JArray] = (char('[') ~> list(jValue, char(',')) <~ char(']')) map((vs: List[JSON]) => JArray(vs.toIndexedSeq))
//    [1,2,3] ~> 1,2,3] , <~ 1,2,3 = List(1,2,3)
    def jObject: Parser[JObject] = ???
//      ('{' ~> list(jValue, ',') <~ '}') map {case "{" ~> _ <~ "}" => JObject(Map() ++ _)}
//   {"num":1.0}  "num" : 1.0, Map("num" -> 1.0)
//    def member: Parser[(String, Any)] = ???
//      string( ) andThen ":" andThen jValue map { case name andThen ":" andThen jValue =>(name, jValue)}

  }
  
  def parse(s: String): JSON = jsonParser.parse(s).get
*/
}