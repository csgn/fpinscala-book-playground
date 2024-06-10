package parser

import pbtest.{Gen, Prop}
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]]:
  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): Parser[String]

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def defer[A](p: => Parser[A]): Parser[A]

  def regex(r: Regex): Parser[String]

  def fail(s: String): Parser[String]

  val nonNegativeInt: Parser[Int] =
    for
      nString <- regex("[0-9]+".r)
      n <- nString.toIntOption match
        case Some(n) => succeed(n)
        case None    => fail("expected an integer")
    yield n

  extension [A](p1: Parser[A])
    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = or(p2)

    infix def product[B](p2: => Parser[B]): Parser[(A, B)] =
      for
        a <- p1
        b <- p2
      yield (a, b)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def map[B](f: A => B): Parser[B] =
      flatMap(a => succeed(f(a)))

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for
        a <- p1
        b <- p2
      yield f(a, b)

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else map2(listOfN(n - 1))(_ :: _)

    def many: Parser[List[A]] =
      map2(p1.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      map2(p1.many)(_ :: _)

    def slice: Parser[String]

    def run(input: String): Either[ParseError, A]
