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

  def fork[A](a: => Par[A]): Par[A] =
    es => cb => eval(es)(a(es)(cb))

  def async[A](f: (A => Unit) => Unit): Par[A] =
    es => cb => f(cb)

  private def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(
      new Callable[Unit]:
        def call(): Unit = r
    )

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es => cb =>
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

  extension [A](p: Par[A])
    def run(es: ExecutorService): A =
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es): a =>
        ref.set(a)
        latch.countDown()

      latch.await()
      ref.get
