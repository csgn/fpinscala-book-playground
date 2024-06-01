package state

trait RNG2:
  def nextInt: (Int, RNG2)

object RNG2:
  type Rand2[+A] = RNG2 => (A, RNG2)

  case class Simple2(seed: Long) extends RNG2:
    override def nextInt: (Int, RNG2) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG2 = Simple2(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG2)

  def nonNegativeInt: Rand2[Int] =
    rng =>
      val (i, r) = rng.nextInt
      (if i < 0 then -(i + 1) else i, r)

  def int: Rand2[Int] = _.nextInt

  /*
   * between 0 - 1, not including 1
   */
  def double: Rand2[Double] =
    rng =>
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + 1), r)

  def boolean: Rand2[Boolean] =
    rng =>
      rng.nextInt match
        case (i, rng2) => (i % 2 == 0, rng2)

  def intDouble: Rand2[(Int, Double)] =
    rng =>
      val (i, r) = rng.nextInt
      val (i2, r2) = double(r)
      ((i, i2), r2)

  def doubleInt: Rand2[(Double, Int)] =
    rng =>
      val ((i1, i2), r) = intDouble(rng)
      ((i2, i1), r)

  def double3: Rand2[(Double, Double, Double)] =
    rng =>
      val (i1, r1) = double(rng)
      val (i2, r2) = double(r1)
      val (i3, r3) = double(r2)
      ((i1, i2, i3), r3)

  def ints(count: Int): Rand2[List[Int]] =
    rng =>
      @scala.annotation.tailrec
      def go(n: Int, acc: List[Int], r2: RNG2): (List[Int], RNG2) =
        if n >= count then (acc, r2)
        else
          val (i1, r3) = r2.nextInt
          go(n + 1, i1 :: acc, r3)

      go(0, List(), rng)

  def unit[A](a: A): Rand2[A] =
    rng => (a, rng)

  def map[A, B](s: Rand2[A])(f: A => B): Rand2[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeEven: Rand2[Int] =
    map(nonNegativeInt)(a => a - (a % 2))

  def doubleViaMap: Rand2[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand2[A], rb: Rand2[B])(f: (A, B) => C): Rand2[C] =
    rng =>
      val (i1, r1) = ra(rng)
      val (i2, r2) = rb(rng)
      (f(i1, i2), r2)

  def both[A, B, C](ra: Rand2[A], rb: Rand2[B]): Rand2[(A, B)] =
    map2(ra, rb)((_, _))

  def intDoubleViaMap2: Rand2[(Int, Double)] =
    both(int, double)

  def doubleIntViaMap2: Rand2[(Double, Int)] =
    both(double, int)

  def doubleDoubleViaMap2: Rand2[(Double, Double)] =
    both(double, double)

  // my answer
  def sequence[A](rs: List[Rand2[A]]): Rand2[List[A]] =
    rng =>
      def go(as: List[Rand2[A]], acc: List[A], rng2: RNG2): (List[A], RNG2) =
        if as.isEmpty then (acc, rng2)
        else
          val h = as.head
          val t = as.tail
          val (i, rng3) = h(rng2)
          go(t, i :: acc, rng3)

      go(rs, List(), rng)

  // my answer
  def intsViaSequence(n: Int): Rand2[List[Int]] =
    sequence(List.fill(n)(int))

  def sequenceViaFoldRight[A](rs: List[Rand2[A]]): Rand2[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequenceFoldRight(n: Int): Rand2[List[Int]] =
    sequenceViaFoldRight(List.fill(n)(int))

  def nonNegativeLessThan(n: Int): Rand2[Int] =
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)

  def flatMap[A, B](r: Rand2[A])(f: A => Rand2[B]): Rand2[B] =
    rng0 =>
      val (i, rng1) = r(rng0)
      f(i)(rng1)

  def mapViaFlatMap[A, B](s: Rand2[A])(f: A => B): Rand2[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand2[A], rb: Rand2[B])(
      f: (A, B) => C
  ): Rand2[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
