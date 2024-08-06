package monads

import parallel.LL
import parallel.LL.Par
import state.State

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def join[A](ffa: F[F[A]]): F[A] =
    ffa.flatMap(identity)

object MonadInstances:
  given optionMonad: Monad[Option] with
    def unit[A](a: A): Option[A] = None

    extension [A](fa: Option[A])
      def flatMap[B](f: A => Option[B]): Option[B] =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: A): List[A] = List(a)

    extension [A](fa: List[A])
      def flatMap[B](f: A => List[B]): List[B] =
        fa.flatMap(f)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: A): LazyList[A] = LazyList(a)

    extension [A](fa: LazyList[A])
      def flatMap[B](f: A => LazyList[B]): LazyList[B] =
        fa.flatMap(f)

  given parMonad: Monad[LL.Par] with
    def unit[A](a: A): Par[A] = LL.unit(a)

    extension [A](fa: Par[A])
      def flatMap[B](f: A => Par[B]): Par[B] =
        LL.flatMap(fa)(f)

  given stateMonad[S]: Monad[[x] =>> State[S, x]] with
    def unit[A](a: A): State[S, A] = State.unit(a)

    extension [A](fa: State[S, A])
      def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(fa)(f)
