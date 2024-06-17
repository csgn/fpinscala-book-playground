package parser

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]:
  /* succeed(X) == Parser(X) */
  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  /* char(c) == Parser(c) */
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /* string(s) == Parser(s) */
  def string(s: String): Parser[String]

  /* unbiasL((char(c1) ** char(c2)) ** char(c3)) == (c1, c2, c3) */
  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p(0)(0), p(0)(1), p(1))

  /* unbiasL(char(c1) ** (char(c2) ** char(c3))) == (c1, c2, c3) */
  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p(0), p(1)(0), p(1)(1))

  def defer[A](p: => Parser[A]): Parser[A]

  def regex(r: Regex): Parser[String]

  extension [A](p1: Parser[A])
    /* char(c).many.slice.run(S) == Right(S) */
    def slice: Parser[String]

    def map[B](f: A => B): Parser[B] =
      flatMap(a => succeed(f(a)))

    def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
      for
        a <- p1
        b <- p2
      yield f(a, b)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    /*
     * c = 'a'
     * p = char(c).many.map(_.size)
     *
     * S = "aaa"
     * p.run(S) == Right(3)
     *
     * S = "aa"
     * p.run(S) == Right(2)
     *
     * S = "" or "b123"
     * p.run(S) == Right(0)
     *
     */
    def many: Parser[List[A]] =
      map2(defer(p1.many))(_ :: _) | succeed(Nil)

    /*
     * c = 'a'
     * p = char(c).many1.map(_.size)
     *
     * S = "aaa"
     * p.run(S) == Right(3)
     *
     * S = "aa"
     * p.run(S) == Right(2)
     *
     * S = "" or "b123"
     * p.run(S) == Left(ParseError)
     *
     * */
    def many1: Parser[List[A]] =
      map2(p1.many)(_ :: _)

    /*
     *
     * c1 = 'a'
     * c2 = 'b'
     * p = char(c1) ** char(c2)
     *
     * S = "bbb"
     * p.run(S) == Right((0, 3))
     *
     * S = "aaaab"
     * p.run(S) == Right((4, 1))
     *
     */
    infix def product[B](p2: Parser[B]): Parser[(A, B)] =
      for
        a <- p1
        b <- p2
      yield (a, b)

    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)

    /*
     * s1 = "ab"
     * s2 = "cad"
     * S = "ababcad"
     * N = 3
     *
     * p = (string(s1) | string(s2)).listOfN(N)
     *
     * p.run(S) == Right(List("ab", "ab", "cad"))
     *
     * S = "cadabab"
     * p.run(S) == Right(List("cad", "ab", "ab"))
     *
     * S = "ababab"
     * p.run(S) == Right(List("ab", "ab", "ab"))
     * */
    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else map2(p1.listOfN(n - 1))(_ :: _)

    /*
     * S = s1 or s2
     * (string(s1) | string(s2)).run(S) == Right(S)
     */
    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = or(p2)

    /*
     * char(c).run(c.toString) == Right(c)
     * string(s).run(s) == Right(s)
     */
    def run(input: String): Either[ParseError, A]
