package pbtest

import state.{RNG2, State}
import parallel.LL

import Gen.*
import Prop.*
import SGen.*
import Prop.Result.{Passed, Falsified, Proved}
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

opaque type SGen[+A] = Int => Gen[A]
object SGen:
  extension [A](self: SGen[A])
    def map[B](f: A => B): SGen[B] =
      self(_).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(f(_)(n))
end SGen

opaque type Gen[+A] = State[RNG2, A]
object Gen:
  object `**`:
    def unapply[A, B](p: (A, B)) = Some(p)

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
    @annotation.targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] =
      map2(gb)((_, _))

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

    def list: SGen[List[A]] =
      print("list[O] ")
      n =>
        print("list[F] ")
        self.listOfN(n)

    def nonEmptyList: SGen[List[A]] =
      n => list(if n == 0 then n + 1 else n /*or n.max(1)*/ )

    def unsized: SGen[A] =
      _ => self
end Gen

opaque type Prop = (MaxSize, TestCases, RNG2) => Result
object Prop:
  opaque type SuccessCount = Int
  opaque type FailedCase = String

  opaque type TestCases = Int
  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x

  enum Result:
    case Passed
    case Falsified(
        failure: FailedCase,
        successes: SuccessCount
    )
    case Proved;

    def isFalsifed: Boolean = this match
      case Passed          => false
      case Falsified(_, _) => true
      case Proved          => false

  private def randomLazyList[A](g: Gen[A])(rng: RNG2): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"""Test Case: $s
        |Generated an Exception: ${e.getMessage} 
        |Stack Trace: 
        |${e.getStackTrace.mkString("\n")}""".stripMargin

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    print("forAll1[X] ")
    Prop:
      print("forAll1[O] ")
      (n, rng) =>
        print("forAll1[L] ")
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

  @annotation.targetName("forAllSized")
  def forAll[A](as: SGen[A])(f: A => Boolean): Prop =
    print("forAll2[L] ")
    (max, n, rng) =>
      print("forAll2[F] ")
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      // map functions run interleaved with each other
      val prop =
        LazyList
          .from(0)
          .take((n.toInt min max.toInt) + 1)
          // not evaluated immediately, and evaluated firstly
          .map(i => {
            print("MAP1[L] "); forAll(as(i))({ print("MAP1[F] "); f })
          })
          // not evaluated immediately, and evaluated secondly
          .map[Prop](p => {
            println("MAP2[L] ");
            (max, n, rng) => { print("MAP2[F] "); p(max, casesPerSize, rng) }
          })
          // starts to evaluate lazylist
          .toList
          .reduce((a, b) =>
            // &(p1, p2)
            // &(&(p1, p2), p3))
            // &(&(&(p1, p2), p3)), p4)
            print("REDUCE ")
            a && b
          )

      println("PROP")
      prop(max, n, rng)

  val executors: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => LL.Par[Boolean]): Prop =
    forAll(executors ** g):
      case s ** a =>
        f(a).run(s).get

  def apply(f: (TestCases, RNG2) => Result): Prop =
    print("APPLY[L] ")
    (_, n, rng) =>
      print("APPLY[F] ")
      f(n, rng)

  def equal[A](p1: LL.Par[A], p2: LL.Par[A]): LL.Par[Boolean] =
    print("equal[L] ")
    p1.map2(p2)(_ == _)

  def verify(p: => Boolean): Prop =
    print("verify[L] ")
    (_, _, _) =>
      print("verify[F] "); if p then Passed else Falsified("()", 0)

  def verifyPar(p: LL.Par[Boolean]): Prop =
    forAllPar(unit(()))(_ => p)

  extension (self: Prop)
    def &&(that: Prop): Prop =
      print("&&[L] ")
      (max, n, rng) =>
        print("&&[F] ")
        self.tag("and-left")(max, n, rng) match
          case Passed =>
            println("&&[I]")
            that.tag("and-right")(max, n, rng)
          case x => x

    def ||(that: Prop): Prop =
      (max, n, rng) =>
        self.tag("or-left")(max, n, rng) match
          case Falsified(_, _) => that.tag("or-right")(max, n, rng)
          case x               => x

    def tag(msg: String): Prop =
      (max, n, rng) =>
        self(max, n, rng) match
          case Falsified(f, s) =>
            Falsified(s"$msg($f)", s)
          case x => x

    def run(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG2 = RNG2.Simple2(System.currentTimeMillis)
    ): Unit =
      print("RUN ")
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(Console.RED + s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(Console.GREEN + s"+ OK, passed $testCases tests.")
        case Proved => println(Console.GREEN + s"+ OK, proved property.")

    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG2 = RNG2.Simple2(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)
end Prop
