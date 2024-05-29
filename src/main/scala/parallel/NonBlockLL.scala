package parallel

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Callable

object NonBlockLL:
  opaque type Future[+A] = (A => Unit) => Unit
  opaque type Par[+A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    es => cb => cb(a)

  def lazyUnit[A](a: => A): Par[A] =
    fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // non-strict version of unit
  def delay[A](a: => A): Par[A] =
    es => cb => cb(a)

  def fork[A](a: => Par[A]): Par[A] =
    es => cb => eval(es)(a(es)(cb))

  def async[A](f: (A => Unit) => Unit): Par[A] =
    es => cb => f(cb)

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    as.foldRight(unit(List.empty[A]))((a, acc) => a.map2(acc)(_ :: _))

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    fork:
      if as.isEmpty then unit(IndexedSeq.empty[A])
      else if as.size == 1 then as.head.map(IndexedSeq(_))
      else
        val (l, r) = as.splitAt(as.size / 2)
        sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  def sequenceViaSequenceBalanced[A](as: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(as.toIndexedSeq).map(_.toList)

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match
      case Nil    => unit(Nil)
      case h :: t => h.map2(fork(sequenceRight(t)))(_ :: _)

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
    sequence(as.map(asyncF(f)))

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      cb =>
        cond(es): b =>
          if b then eval(es)(t(es)(cb))
          else eval(es)(f(es)(cb))

  def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
    es => cb => p(es): i =>
      eval(es)(ps(i % ps.size)(es)(cb))

  def choiceViaChoiceN[A](
      cond: Par[Boolean]
  )(t: Par[A], f: Par[A]): Par[A] =
    choiceN(cond.map(b => if b then 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(ps: Map[K, Par[V]]): Par[V] =
    es => cb => key(es): k =>
      ps(k)(es)(cb)

  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    p.flatMap(f)

  def choiceViaFlatMap[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    p.flatMap(b => if b then t else f)

  def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    p.flatMap(i => choices(i))

  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => cb => ppa(es)(pa => eval(es)(pa(es)(cb)))

  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
    ppa.flatMap(identity)

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(pa.map(f))

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(
      new Callable[Unit]:
        def call(): Unit = r
    )

  extension [A](pa: Par[A])
    def run(es: ExecutorService): A =
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      pa(es): a =>
        ref.set(a)
        latch.countDown()

      latch.await()
      ref.get

    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        cb =>
          var ar: Option[A] = None
          var br: Option[B] = None

          val combiner = Actor[Either[A, B]](es):
            case Left(a) =>
              if br.isDefined then eval(es)(cb(f(a, br.get)))
              else ar = Some(a)
            case Right(b) =>
              if ar.isDefined then eval(es)(cb(f(ar.get, b)))
              else br = Some(b)

          pa(es)(a => combiner ! Left(a))
          pb(es)(b => combiner ! Right(b))

    def map[B](f: A => B): Par[B] =
      es => cb => pa(es)(a => eval(es)(cb(f(a))))

    def flatMap[B](f: A => Par[B]): Par[B] =
      es => cb => pa(es)(a => f(a)(es)(cb))

    def zip[B](pb: Par[B]): Par[(A, B)] = map2(pb)((_, _))
