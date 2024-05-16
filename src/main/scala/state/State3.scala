package state

opaque type State[S, +A] = S => (A, S)
object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) =
      underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- s2
      yield f(a, b)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit(Nil: List[A]))((f, acc) => f.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

trait RNG3:
  def nextInt: (Int, RNG3)

object RNG3:
  type Rand3[+A] = RNG3 => (A, RNG3)
  // type Rand3[A] = State[RNG3, A]

  case class Simple2(seed: Long) extends RNG3:
    override def nextInt: (Int, RNG3) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG3 = Simple2(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG3)

  def nonNegativeInt: Rand3[Int] =
    rng =>
      val (i, r) = rng.nextInt
      (if i < 0 then -(i + 1) else i, r)

  def int: Rand3[Int] = _.nextInt

  /*
   * between 0 - 1, not including 1
   */
  def double: Rand3[Double] =
    rng =>
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble: Rand3[(Int, Double)] =
    rng =>
      val (i, r) = rng.nextInt
      val (i2, r2) = double(r)
      ((i, i2), r2)

  def doubleInt: Rand3[(Double, Int)] =
    rng =>
      val ((i1, i2), r) = intDouble(rng)
      ((i2, i1), r)

  def double3: Rand3[(Double, Double, Double)] =
    rng =>
      val (i1, r1) = double(rng)
      val (i2, r2) = double(r1)
      val (i3, r3) = double(r2)
      ((i1, i2, i3), r3)

  def ints(count: Int): Rand3[List[Int]] =
    rng =>
      @scala.annotation.tailrec
      def go(n: Int, acc: List[Int], r2: RNG3): (List[Int], RNG3) =
        if n >= count then (acc, r2)
        else
          val (i1, r3) = r2.nextInt
          go(n + 1, i1 :: acc, r3)

      go(0, List(), rng)

  def unit[A](a: A): Rand3[A] =
    rng => (a, rng)

  def map[A, B](s: Rand3[A])(f: A => B): Rand3[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand3[Int] =
    map(nonNegativeInt)(a => a - (a % 2))

  def doubleViaMap: Rand3[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand3[A], rb: Rand3[B])(f: (A, B) => C): Rand3[C] =
    rng =>
      val (i1, r1) = ra(rng)
      val (i2, r2) = rb(rng)
      (f(i1, i2), r2)

  def both[A, B, C](ra: Rand3[A], rb: Rand3[B]): Rand3[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaMap2: Rand3[(Int, Double)] =
    both(int, double)

  def doubleIntViaMap2: Rand3[(Double, Int)] =
    both(double, int)

  def doubleDoubleViaMap2: Rand3[(Double, Double)] =
    both(double, double)

  // my answer
  def sequence[A](rs: List[Rand3[A]]): Rand3[List[A]] =
    rng =>
      def go(as: List[Rand3[A]], acc: List[A], rng2: RNG3): (List[A], RNG3) =
        if as.isEmpty then (acc, rng2)
        else
          val h = as.head
          val t = as.tail
          val (i, rng3) = h(rng2)
          go(t, i :: acc, rng3)

      go(rs, List(), rng)

  // my answer
  def intsViaSequence(n: Int): Rand3[List[Int]] =
    sequence(List.fill(n)(int))

  def sequenceViaFoldRight[A](rs: List[Rand3[A]]): Rand3[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequenceFoldRight(n: Int): Rand3[List[Int]] =
    sequenceViaFoldRight(List.fill(n)(int))

  def nonNegativeLessThan(n: Int): Rand3[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)

  def flatMap[A, B](r: Rand3[A])(f: A => Rand3[B]): Rand3[B] =
    rng0 =>
      val (i, rng1) = r(rng0)
      f(i)(rng1)

  def mapViaFlatMap[A, B](s: Rand3[A])(f: A => B): Rand3[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand3[A], rb: Rand3[B])(
      f: (A, B) => C
  ): Rand3[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
