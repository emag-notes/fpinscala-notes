package fpinscala.chapter09

import scala.language.{higherKinds, implicitConversions}

trait JSON

object JSON {

  case object JNull                          extends JSON
  case class JNumber(get: Double)            extends JSON
  case class JString(get: String)            extends JSON
  case class JBool(get: Boolean)             extends JSON
  case class JArray(get: Seq[JSON])          extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParseError, Parser[+ _]](P: Parsers[ParseError, Parser]): Parser[JSON] = {
    import P.{string => _, _}

    implicit def tok(s: String) = token(P.string(s))

    def array = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"

    def obj = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"

    def keyval = escapedQuoted ** (":" *> value)

    def lit: Parser[JSON] = ???
//    def lit = scope("literal") {
//      "null".as(JNull) |
//      double.map(JNumber) |
//      escapedQuoted.map(JString) |
//      "true".as(JBool(true)) |
//      "false".as(JBool(false))
//    }

    def value: Parser[JSON] = lit | obj | array

    root(whitespace *> (obj | array))
  }
}
