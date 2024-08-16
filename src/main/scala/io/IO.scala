package io

object Program1 {
  import Program2.*

  case class Player(name: String, score: Int)

  def winner(p1: Player, p2: Player): Option[Player] = {
    if p1.score > p2.score then Some(p1)
    else if p1.score < p2.score then Some(p2)
    else None
  }

  def winnerMsg(p: Option[Player]): String = p
    .map { case Player(name, _) =>
      s"$name is the winner!"
    }
    .getOrElse("It's a draw.")

  def contest(p1: Player, p2: Player): Unit = {
    println(winnerMsg(winner(p1, p2)))
  }

  def contestPure(p1: Player, p2: Player): IO = {
    PrintLine(winnerMsg(winner(p1, p2)))
  }
}

object Program2 {
  import scala.io.StdIn.readLine

  trait IO { self =>
    def unsafeRun: Unit

    def ++(io: IO): IO = new {
      def unsafeRun: Unit =
        self.unsafeRun
        io.unsafeRun
    }
  }

  def PrintLine(msg: String): IO = new {
    def unsafeRun: Unit = println(msg)
  }

  def fahrenheitToCelsius(f: Double): Double = {
    (f - 32) * 5.0 / 9.0
  }

  def converterImperative: Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  def converterFunctional: IO = {
    PrintLine("Enter a temperature in degrees Fahrenheit")
    // what are we gonna do now ?
  }
}

object Program3 {
  import monads.Monad2
  import scala.io.StdIn.readLine
  import io.Program2.fahrenheitToCelsius

  trait IO[A] { self =>
    def unsafeRun: A

    def map[B](f: A => B): IO[B] = new {
      def unsafeRun: B = f(self.unsafeRun)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new {
      def unsafeRun: B = f(self.unsafeRun).unsafeRun
    }
  }

  object IO {
    def apply[A](a: => A): IO[A] = new {
      def unsafeRun: A = a
    }

    given monad: Monad2[IO] with {
      def unit[A](a: => A): IO[A] = IO(a)
      extension [A](fa: IO[A])
        override def flatMap[B](f: A => IO[B]): IO[B] =
          fa.flatMap(f)
    }
  }

  def ReadLine: IO[String] = IO(readLine)

  def PrintLine(msg: String): IO[Unit] = IO(println(msg))

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()
}

object Program4 {
  import Expr.*

  enum Expr[A] {
    case Pure(a: A)
    case If(cond: Expr[Boolean], thenBranch: Expr[A], elseBranch: Expr[A])
    case FlatMap[A, B](sub: Expr[A], k: A => Expr[B]) extends Expr[B]
  }

  object Expr {
    def pure[A](a: A): Expr[A] = Pure(a)

    def ifExpr[A](
        cond: Expr[Boolean],
        thenBranch: Expr[A],
        elseBranch: Expr[A],
    ): Expr[A] =
      If(cond, thenBranch, elseBranch)

    def flatMap[A, B](sub: Expr[A], k: A => Expr[B]): Expr[B] =
      FlatMap(sub, k)

    def eval[A](expr: Expr[A]): A = expr match {
      case Pure(a) => a
      case If(cond, thenBranch, elseBranch) =>
        val c = eval(cond)
        if c then eval(thenBranch) else eval(elseBranch)
      case FlatMap(sub, k) =>
        val a = eval(sub)
        eval(k(a))
    }
  }

  def compute(x: Int): Option[Int] = {
    if x > 0 then {
      Some(x * 2).flatMap(y => Some(y + 3))
    } else {
      None
    }
  }

  def computeAsDataConstructor(x: Int): Expr[Int] = {
    ifExpr(
      pure(x > 0), // condition
      flatMap(pure(x * 2), y => pure(y + 3)), // then branch
      pure(0), // else branch
    )
  }
}

object Program5 {
  enum IO[A] {
    case Return(a: A)
    case Suspend(resume: () => A)
    case FlatMap[A, B](
        sub: IO[A],
        k: A => IO[B],
    ) extends IO[B]

    def flatMap[B](f: A => IO[B]): IO[B] = {
      FlatMap(this, f)
    }

    def map[B](f: A => B): IO[B] = {
      flatMap(a => Return(f(a)))
    }

    @annotation.tailrec
    final def unsafeRun: A = this match {
      case Return(a)  => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>
        x match
          case Return(a)     => f(a).unsafeRun
          case Suspend(r)    => f(r()).unsafeRun
          case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun
    }
  }

  object IO {
    def apply[A](a: => A): IO[A] = {
      suspend(Return(a))
    }

    def suspend[A](ioa: => IO[A]): IO[A] = {
      Suspend(() => ioa).flatMap(identity)
    }
  }
}

object Program6 {
  import TailRec.*

  enum TailRec[A] {
    case Return(a: A)
    case Suspend(resume: () => TailRec[A])
    case FlatMap[A, B](
        sub: TailRec[A],
        k: A => TailRec[B],
    ) extends TailRec[B]

    def flatMap[B](f: A => TailRec[B]): TailRec[B] = {
      FlatMap(this, f)
    }

    def map[B](f: A => B): TailRec[B] = {
      flatMap(a => Return(f(a)))
    }

    @annotation.tailrec
    final def unsafeRun: A = this match {
      case Return(a)  => a
      case Suspend(r) => r().unsafeRun
      case FlatMap(x, f) =>
        x match
          case Return(a)     => f(a).unsafeRun
          case Suspend(r)    => FlatMap(r(), f).unsafeRun
          case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).unsafeRun
    }
  }

  object TailRec {
    def apply[A](a: => A): TailRec[A] = {
      FlatMap(
        Suspend(() => Return(a)),
        (x: A) => Return(x),
      )
    }
  }

  object Examples {
    /*FlatMap(r(), f))
     * factorial(5)
     * FlatMap(
     *   Return(1),
     *   x => Flatmap(
     *     Return(1 * x), x => Flatmap(
     *       Return(2 * x), x => Flatmap(
     *         Return(3 * x), x => Flatmap(
     *           Return(4 * x), x => Return(5 * x)
     *         )
     *       )
     *     )
     *   )
     * )
     */
    def factorial(n: Int): TailRec[Int] = {
      if n == 0 then Return(1)
      else
        FlatMap[Int, Int](
          Suspend(() => factorial(n - 1)),
          x => Return(n * x),
        )
    }
  }
}
