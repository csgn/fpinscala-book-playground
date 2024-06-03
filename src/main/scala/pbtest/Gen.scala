package pbtest

import state.{RNG2, State}

import Gen.*
import Prop.*

object Gen:
  type Gen[+A] = State[RNG2, A]

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG2.boolean)

  def string(n: Int): Gen[String] =
    choose(0, 127)
      .listOfN(n)
      .map(l => l.map(_.toChar).mkString)

  def double(start: Double, stopExclusive: Double): Gen[Double] =
    State(RNG2.double).map(start + _ % (stopExclusive - start))

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
  opaque type Prop = Unit
  opaque type SuccessCount = Int
  opaque type FailedCase = String

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  def check: Either[(FailedCase, SuccessCount), SuccessCount] = ???
end Prop
