package parallel

import java.util.concurrent.*

object LL:
  opaque type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A]:
    def isDone: Boolean = true
    def get(timeout: Long, unit: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A]:
      def call: A = a(es).get
    )
  
  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    // fbs will block the main thread 
    // if we do not wrap with fork because map operation
    // could be computationally expensive.
    fork:
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]()))((a, acc) => a.map2(acc)(_ :: _))

  def sequenceViaSequenceBalanced[A](ps: List[Par[A]]): Par[List[A]] =
    sequenceBalanced(ps.toIndexedSeq).map(_.toList)

  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
    // also we can implement using divide & conquer
    if pas.isEmpty then unit(IndexedSeq.empty)
    else if pas.size == 1 then pas.head.map(a => IndexedSeq(a))
    else 
      val (l, r) = pas.splitAt(pas.size / 2)
      sequenceBalanced(l).map2(sequenceBalanced(r))(_ ++ _)

  extension [A](parList: Par[List[Int]])
    def sortPar: Par[List[Int]] =
      parList.map(_.sorted)

  extension [A](pa: Par[A])
    def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es =>
        val futureA = pa(es)
        val futureB = pb(es)
        UnitFuture(f(futureA.get, futureB.get))

    def map2Timeouts[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
      es => new Future[C]:
        private val futureA = pa(es)
        private val futureB = pb(es)

        @volatile private var cache: Option[C] = None

        def isDone: Boolean = cache.isDefined
        def get: C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

        def get(timeout: Long, unit: TimeUnit): C = 
          val timeoutNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
          val started = System.nanoTime
          val a = futureA.get(timeoutNanos, TimeUnit.NANOSECONDS)
          val elapsed = System.nanoTime - started
          val b = futureB.get(timeoutNanos - elapsed, TimeUnit.NANOSECONDS)
          val c = f(a, b)
          cache = Some(c)
          c

        def isCancelled: Boolean = futureA.isCancelled || futureB.isCancelled
        def cancel(mayInterruptIfRunning: Boolean): Boolean = 
          futureA.cancel(mayInterruptIfRunning) || futureB.cancel(mayInterruptIfRunning)

    def map[B](f: A => B): Par[B] =
      pa.map2(unit(()))((a, _) => f(a))

    def run(s: ExecutorService): Future[A] = pa(s)







