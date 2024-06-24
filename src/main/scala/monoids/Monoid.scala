package monoids

import pbtest.{Gen, Prop}
import parallel.NonBlockLL

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  val empty: A

def stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String): String = a1 + a2
  val empty = ""

def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  val empty = Nil

val intAddition: Monoid[Int] = new:
  def combine(a1: Int, a2: Int): Int = a1 + a2
  val empty = 0

val intMultiplication: Monoid[Int] = new:
  def combine(a1: Int, a2: Int): Int = a1 * a2
  val empty = 1

val booleanOr: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
  val empty = false

val booleanAnd: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
  val empty = true

def firstOptionMonoid[A]: Monoid[Option[A]] = new:
  def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  val empty = None

def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

// def optionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new:
//   def combine(a1: Option[A], a2: Option[A]): Option[A] = a1.map2(a2)(f)
//   val empty = None

def dual[A](m: Monoid[A]): Monoid[A] = new:
  def combine(a1: A, a2: A): A = m.combine(a2, a1)
  val empty = m.empty

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
  val empty = identity

def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

def combineAll[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.empty)(m.combine)

def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
  as.foldLeft(m.empty)((acc, a) => m.combine(acc, f(a)))

def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
  foldMap(as, dual(endoMonoid))(f.curried)(acc)

def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
  foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
  if as.length == 0 then m.empty
  else if as.length == 1 then f(as(0))
  else
    val (l, r) = as.splitAt(as.length / 2)
    m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

def parMonoid[A](m: Monoid[A]): Monoid[NonBlockLL.Par[A]] = new:
  def combine(a1: NonBlockLL.Par[A], a2: NonBlockLL.Par[A]): NonBlockLL.Par[A] =
    a1.map2(a2)(m.combine)

  val empty: NonBlockLL.Par[A] = NonBlockLL.unit(m.empty)

def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(
    f: A => B
): NonBlockLL.Par[B] =
  NonBlockLL
    .parMap(as)(f)
    .flatMap(bs => foldMapV(bs, parMonoid(m))(b => NonBlockLL.lazyUnit(b)))

enum WC:
  case Stub(chars: String)
  case Part(lStub: String, words: Int, rStub: String)

def wcMonoid: Monoid[WC] = new:
  def combine(a1: WC, a2: WC): WC = (a1, a2) match
    case (WC.Stub(s1), WC.Stub(s2))         => WC.Stub(s1 + s2)
    case (WC.Stub(s1), WC.Part(l2, w2, r2)) => WC.Part(s1 + l2, w2, r2)
    case (WC.Part(l1, w1, r1), WC.Stub(s2)) => WC.Part(l1, w1, r1 + s2)
    case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) =>
      WC.Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

  val empty: WC = WC.Stub("")

def wordCounter(s: String): Int =
  def wc(c: Char): WC =
    if c.isWhitespace then WC.Part("", 0, "")
    else WC.Stub(c.toString)

  def unstub(s: String): Int = if s.isEmpty then 0 else 1

  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
    case WC.Stub(chars) => unstub(chars)
    case WC.Part(lStub, words, rStub) =>
      unstub(lStub) + words + unstub(rStub)
