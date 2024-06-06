package parser

trait Parsers[ParseError, Parser[+_]]:
  // law: char(c).run(c.toString) == Right(c)
  // example: char('a').run('a'.toString) == Right('a')
  def char(c: Char): Parser[Char]

  // law: string(s).run(s) == Right(s)
  def string(s: String): Parser[String]

  extension [A](p: Parser[A])
    // law:
    //    string(s).or(string(s')).run(s) == Right(s)
    //    string(s).or(string(s')).run(s') == Right(s')
    // examples:
    //    string("abra").or(string("cadabra")).run("abra") == Right("abra")
    //    string("abra").or(string("cadabra")).run("cadabra") == Right("cadabra")
    //
    // law:
    //    (string(s) | string(s')).run(s) == Right(s)
    //    (string(s) | string(s')).run(s') == Right(s')
    // examples:
    //    (string("abra") | string("cadabra")).run("abra") == Right("abra")
    //    (string("abra") | string("cadabra")).run("cadabra") == Right("cadabra")
    infix def or(p2: Parser[A]): Parser[A]
    def |(p2: Parser[A]): Parser[A] = p.or(p2)

    def run(input: String): Either[ParseError, A]
