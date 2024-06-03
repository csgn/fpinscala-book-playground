package pbtest

import state.{RNG2, State}

import Gen.*
import Prop.*
import Prop.Position.{Left, Right}
import Prop.Result.{Passed, Falsified}
import scala.util.Failure

object Gen:
  type Gen[+A] = State[RNG2, A]

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG2.boolean)

  def stringN(n: Int): Gen[String] =
    choose(0, 127)
      .listOfN(n)
      .map(_.map(_.toChar).mkString)

  def double: Gen[Double] =
    State(RNG2.double)

  def doubleRange(start: Double, stopExclusive: Double): Gen[Double] =
    double.map(start + _ % (stopExclusive - start))

  def option[A](ga: Gen[A]): Gen[Option[A]] =
    boolean.flatMap: b =>
      if b then ga.map(Some(_))
      else unit(None)

  def fromOption[A](goa: Gen[Option[A]]): Gen[A] =
    goa.flatMap:
      case Some(a) => unit(a)
      case None    => unit(null.asInstanceOf[A])

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG2.nonNegativeInt)
      .map(start + _ % (stopExclusive - start))

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive)
      .map2(choose(start, stopExclusive))((_, _))

  def chooseInclusive(start: Int, stop: Int): Gen[Int] =
    choose(start, stop + 1)

  def chooseStartZero(stopExclusive: Int): Gen[Int] =
    choose(0, stopExclusive)

  def chooseStartZeroStopInclusive(stop: Int): Gen[Int] =
    choose(0, stop + 1)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    State.sequence(List.fill(n)(g))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap: b =>
      if b then g1
      else g2

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1(1).abs / (g1(1).abs + g2(1).abs)
    double.flatMap(w => if w < g1Threshold then g1(0) else g2(0))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(
      start,
      if stopExclusive % 2 == 0 then stopExclusive - 1
      else stopExclusive
    ).map: i =>
      if i % 2 != 0 then i + 1
      else i

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(
      start,
      if stopExclusive % 2 != 0 then stopExclusive - 1
      else stopExclusive
    ).map: i =>
      if i % 2 == 0 then i + 1
      else i

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)

    def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(gb)(f)

    def listOfN(n: Int): Gen[List[A]] =
      Gen.listOfN(n, self)

    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(listOfN)

end Gen

object Prop:
  opaque type Prop = (TestCases, RNG2) => Result

  opaque type SuccessCount = Int
  opaque type FailedCase = String

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  enum Position:
    case Left
    case Right

  enum Result:
    case Passed
    case Falsified(
        failure: FailedCase,
        successes: SuccessCount,
        position: Position = Left
    )

    def isFalsifed: Boolean = this match
      case Passed             => false
      case Falsified(_, _, _) => true

  private def randomLazyList[A](g: Gen[A])(rng: RNG2): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"""Test Case: $s
        |Generated an Exception: ${e.getMessage} 
        |Stack Trace: 
        |${e.getStackTrace.mkString("\n")}""".stripMargin

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Passed
              else Falsified(a.toString, i)
            catch
              case e: Exception =>
                Falsified(buildMsg(a, e), i)
        .find(_.isFalsifed)
        .getOrElse(Passed)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      (n, rng) =>
        self(n, rng) match
          case Passed =>
            that(n, rng) match
              case Passed => Passed
              case Falsified(f, s, _) =>
                Falsified(f, s, Right)
          case Falsified(f, s, _) =>
            Falsified(f, s, Left)

    def ||(that: Prop): Prop =
      (n, rng) =>
        self(n, rng) match
          case Passed => Passed
          case Falsified(f, s, _) =>
            that(n, rng) match
              case Passed => Passed
              case Falsified(f, s, _) =>
                Falsified(f, s, Right)

    def check: Result =
      val rng = RNG2.Simple2(System.nanoTime)
      self(1, rng)

end Prop
