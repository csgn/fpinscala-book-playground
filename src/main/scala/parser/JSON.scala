package parser

import scala.util.matching.Regex

// https://www.json.org/json-en.html

enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

def jsonParser[Err, Parser[+_]](
    P: Parsers[Err, Parser]
): Parser[JSON] =
  import P.*

  def jToken(s: String): Parser[String] =
    string(s).attempt <* jWhitespace

  def jRegexToken(s: String): Parser[String] =
    regex(s.r).attempt <* jWhitespace

  def jDocument: Parser[JSON] =
    jWhitespace *> (jObject | jArray) <* jEof

  def jObject: Parser[JSON] = (
    jToken("{") *> jMap
      .sep(jToken(","))
      .map(kvs => JSON.JObject(kvs.toMap)) <* jToken("}")
  ).scope("object")

  def jArray: Parser[JSON] = (
    jToken("[") *> jValue
      .sep(jToken(","))
      .map(vs => JSON.JArray(vs.toIndexedSeq)) <* jToken("]")
  ).scope("array")

  def jWhitespace: Parser[String] = regex("\\s".r)

  def jNumber: Parser[Double] =
    jRegexToken("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?").map(_.toDouble)

  def jString: Parser[String] =
    jRegexToken("\\w+").map(_.toString)

  def jBool(b: Boolean): Parser[JSON] = jToken(b.toString).as(JSON.JBool(b))
  def jNull: Parser[JSON] = jToken("null").as(JSON.JNull)

  def jLiteral: Parser[JSON] = (
    jNull |
      jBool(true) |
      jBool(false) |
      jNumber.map(JSON.JNumber(_)) |
      jString.map(JSON.JString(_))
  ).scope("literal")

  def jValue: Parser[JSON] =
    jObject | jArray | jLiteral

  def jMap: Parser[(String, JSON)] =
    jString ** (jToken(":") *> jValue)

  def jEof: Parser[String] = regex("\\z".r)

  ???
