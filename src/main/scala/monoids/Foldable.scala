package monoids

import monoids.MonoidSyntax.*
import monoids.MonoidInstances.*

trait Foldable[F[_]]:
  extension [A](as: F[A])
    def foldRight[B](acc: B)(f: (A, B) => B): B =
      foldMap(f.curried)(using switchDual(endoMonoid[B]))(acc)

    def foldLeft[B](acc: B)(f: (B, A) => B): B =
      foldMap(a => b => f(b, a))(using endoMonoid[B])(acc)

    def foldMap[B](f: A => B)(using m: Monoid[B]): B =
      foldRight(m.empty)((a, b) => m.combine(f(a), b))

    def combineAll(using m: Monoid[A]): A =
      foldLeft(m.empty)(m.combine)

    def toList: List[A] =
      foldRight(List.empty[A])(_ :: _)

object FoldableInstances:
  given Foldable[List] with
    extension [A](as: List[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

  given Foldable[IndexedSeq] with
    extension [A](as: IndexedSeq[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

      override def foldMap[B](f: A => B)(using m: Monoid[B]): B =
        foldMapBalanced(as, m)(f)

  given Foldable[LazyList] with
    extension [A](as: LazyList[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B =
        as.foldRight(acc)(f)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B =
        as.foldLeft(acc)(f)

  given Foldable[Option] with
    extension [A](as: Option[A])
      override def foldRight[B](acc: B)(f: (A, B) => B): B = as match
        case None    => acc
        case Some(a) => f(a, acc)

      override def foldLeft[B](acc: B)(f: (B, A) => B): B = as match
        case None    => acc
        case Some(a) => f(acc, a)

      override def foldMap[B](f: A => B)(using m: Monoid[B]): B = as match
        case None    => m.empty
        case Some(a) => f(a)

object FoldableExamples:
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    import FoldableInstances.given
    import MonoidGivens.given
    as.foldMap(a => Map(a -> 1))
