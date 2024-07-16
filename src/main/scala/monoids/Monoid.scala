package monoids

import monoids.MonoidSyntax.*
import monoids.MonoidInstances.*
import pbtest.{Gen, Prop}
import Gen.`**`

trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object MonoidSyntax:
  def switchDual[A](m: Monoid[A]): Monoid[A] = new:
    def empty: A = m.empty
    def combine(x: A, y: A): A = m.combine(y, x)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldMapViaGiven[A, B](as: List[A])(f: A => B)(using m: Monoid[B]): B =
    foldMap(as, m)(f)

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, switchDual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapBalanced[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.size <= 0 then m.empty
    else if as.size == 1 then f(as(0))
    else
      val (l, r) = as.splitAt(as.size / 2)
      m.combine(
        foldMapBalanced(l, m)(f),
        foldMapBalanced(r, m)(f),
      )

  def foldMapBalancedViaGiven[A, B](as: IndexedSeq[A])(f: A => B)(using
      m: Monoid[B]
  ): B =
    foldMapBalanced(as, m)(f)

object MonoidInstances:
  def stringMonoid: Monoid[String] = new:
    def empty: String = ""
    def combine(x: String, y: String): String = x + y

  def listMonoid[A]: Monoid[List[A]] = new:
    def empty: List[A] = Nil
    def combine(x: List[A], y: List[A]): List[A] = x ++ y

  def intAdditionMonoid: Monoid[Int] = new:
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y

  def intMultiplicationMonoid: Monoid[Int] = new:
    def empty: Int = 1
    def combine(x: Int, y: Int): Int = x * y

  def boolDisjunctionMonoid: Monoid[Boolean] = new:
    def empty: Boolean = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y

  def boolConjuctionMonoid: Monoid[Boolean] = new:
    def empty: Boolean = true
    def combine(x: Boolean, y: Boolean): Boolean = x && y

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def empty: Option[A] = None
    def combine(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)

  // def optionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new:
  //   def empty: Option[A] = None
  //   def combine(x: Option[A], y: Option[A]): Option[A] = x.map2(y)(f)

  def lastOptionMonoid[A]: Monoid[Option[A]] = switchDual(optionMonoid)

  def endoMonoid[A]: Monoid[A => A] = new:
    def empty: A => A = identity
    def combine(f: A => A, g: A => A): A => A = f.andThen(g)

object MonoidLaws:
  def monoidGenLaw[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case a ** b ** c =>
          m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
      .tag("associativity")

    val identity = Prop
      .forAll(gen): a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      .tag("idenity")

    associativity && identity

object MonoidGivens:
  given Monoid[Int] with
    def empty: Int = 0
    def combine(x: Int, y: Int): Int = x + y

  given Monoid[String] with
    def empty: String = ""
    def combine(x: String, y: String): String = x + y
