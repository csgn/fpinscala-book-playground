package monads

import monoids.Semigroup

import java.time.LocalDate
import Validated.*

case class NonEmptyList[+A](head: A, tail: List[A]):
  def toList: List[A] = head :: tail

object NonEmptyList:
  def apply[A](head: A, tail: A*): NonEmptyList[A] =
    NonEmptyList(head, tail.toList)

  given nonEmptyListSemigroup[A]: Semigroup[NonEmptyList[A]] with
    def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] =
      NonEmptyList(x.head, x.tail ++ y.toList)

enum Validated[+E, +A]:
  case Valid(get: A) extends Validated[Nothing, A]
  case Invalid(error: E) extends Validated[E, Nothing]

given validatedApplicative[E: Semigroup]: Applicative[[x] =>> Validated[E, x]]
with
  def unit[A](a: => A): Validated[E, A] = Valid(a)
  extension [A](fa: Validated[E, A])
    override def map2[B, C](fb: Validated[E, B])(
        f: (A, B) => C
    ): Validated[E, C] = {
      (fa, fb) match
        case (Valid(a), Valid(b)) => Valid(f(a, b))
        case (Invalid(ea), Invalid(eb)) =>
          Invalid(summon[Semigroup[E]].combine(ea, eb))
        case (e @ Invalid(_), _) => e
        case (_, e @ Invalid(_)) => e
    }

object WebForm:
  case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

  def validName(name: String): Validated[NonEmptyList[String], String] =
    if name != "" then Valid(name)
    else Invalid(NonEmptyList("Name cannot be empty"))

  def validBirthdate(
      birthdate: String
  ): Validated[NonEmptyList[String], LocalDate] =
    try Valid(LocalDate.parse(birthdate))
    catch
      case _: java.time.format.DateTimeParseException =>
        Invalid(NonEmptyList("Birthdate must be in the form yyy-MM-dd"))

  def validPhone(phoneNumber: String): Validated[NonEmptyList[String], String] =
    if phoneNumber.matches("[0-9]{10}") then Valid(phoneNumber)
    else Invalid(NonEmptyList("Phone number must be 10 digits"))

  def validateWebForm(
      name: String,
      birthdate: String,
      phoneNumber: String,
  ): Validated[NonEmptyList[String], WebForm] =
    validName(name).map3(
      validBirthdate(birthdate),
      validPhone(phoneNumber),
    )(WebForm(_, _, _))

trait Monad2[F[_]] extends Applicative[F]:

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    // in terms of flatMap
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] =
      // in terms of flatMap
      ffa.flatMap(identity)

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      // in terms of map and join
      fa.map(f).join

    override def map[B](f: A => B): F[B] =
      // in terms of flatMap and unit
      fa.flatMap(a => unit(f(a)))

    override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      // in terms of flatMap and map
      fa.flatMap(a => fb.map(b => f(a, b)))

trait Applicative[F[_]] extends Functor[F]:
  self =>
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B], fa: F[A]): F[B] =
    // in terms of map2
    fab.map2(fa)((f, a) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    // in terms of map2 and unit
    as.foldRight(unit(List.empty[B]))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    // in terms of traverse
    traverse(fas)(identity)

    // in terms of map2 and unit
    fas.foldRight(unit(List.empty[A]))((a, acc) => a.map2(acc)(_ :: _))

  def sequenceMap[K, V](ofv: Map[K, F[V]]): F[Map[K, V]] =
    ofv.foldRight(unit(Map.empty[K, V])) { case ((k, fv), acc) =>
      acc.map2(fv)((m, v) => m + (k -> v))
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    // in terms of sequence
    sequence(List.fill(n)(fa))

    // in terms of map2 and unit
    List
      .fill(n)(fa)
      .foldRight(unit(List.empty[A]))((a, acc) => a.map2(acc)(_ :: _))

  def product2[G[_]](G: Applicative[G]): Applicative[[x] =>> (F[x], G[x])] =
    new:
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](
          fs: (F[A => B], G[A => B]),
          p: (F[A], G[A]),
      ): (F[B], G[B]) =
        (self.apply(fs(0), p(0)), G.apply(fs(1), p(1)))

  def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
    def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

    extension [A](fga: F[G[A]])
      override def map2[B, C](fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga)(fgb)(G.map2(_)(_)(f))

  extension [A](fa: F[A])
    def map4[B, C, D, Z](fb: F[B], fc: F[C], fd: F[D])(
        f: (A, B, C, D) => Z
    ): F[Z] =
      // in terms of apply and unit
      apply(apply(apply(apply(unit(f.curried), fa), fb), fc), fd)

      // in terms of product and map2
      fa.product(fb).map2(fc.product(fd)) { case ((a, b), (c, d)) =>
        f(a, b, c, d)
      }

    def map3[B, C, Z](fb: F[B], fc: F[C])(f: (A, B, C) => Z): F[Z] =
      // in terms of apply and unit
      apply(apply(apply(unit(f.curried), fa), fb), fc)

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      // in terms of apply and unit
      apply(apply(unit(f.curried), fa), fb)

    def map[B](f: A => B): F[B] =
      // in terms of map2 and unit
      fa.map2(unit(()))((a, _) => f(a))

      // in terms of apply and unit
      apply(unit(f), fa)

    def product[B](fb: F[B]): F[(A, B)] =
      // in terms of map2
      fa.map2(fb)((_, _))
