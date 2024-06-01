package state

trait RNG3:
  def nextInt: (Int, RNG3)

object RNG3:
  // type Rand3[+A] = RNG3 => (A, RNG3)
  type Rand3[A] = State[RNG3, A]

  case class Simple2(seed: Long) extends RNG3:
    override def nextInt: (Int, RNG3) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG3 = Simple2(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG3)

  def nonNegativeInt: Rand3[Int] =
    State: rng =>
      val (i, r) = rng.nextInt
      (if i < 0 then -(i + 1) else i, r)

  def int: Rand3[Int] = State(_.nextInt)

  /*
   * between 0 - 1, not including 1
   */
  def double: Rand3[Double] =
    nonNegativeInt.map(i => (i / (Int.MaxValue.toDouble + 1)))

  def boolean: Rand3[Boolean] =
    State: rng =>
      rng.nextInt match
        case (i, rng2) => (i % 2 == 0, rng2)

  def intDouble: Rand3[(Int, Double)] =
    int.map2(double)((_, _))

  def doubleInt: Rand3[(Double, Int)] =
    double.map2(int)((_, _))

  def double3: Rand3[(Double, Double, Double)] =
    double.map2(double)((_, _)).map2(double)((t, d) => (t._1, t._2, d))

  def ints(count: Int): Rand3[List[Int]] =
    State.sequence(List.fill(count)(int))

  def unit[A](a: A): Rand3[A] =
    State.unit(a)

  def map[A, B](s: Rand3[A])(f: A => B): Rand3[B] =
    s.map(f)

  def nonNegativeEven: Rand3[Int] =
    nonNegativeInt.map(a => a - (a % 2))

  def doubleViaMap: Rand3[Double] =
    nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand3[A], rb: Rand3[B])(f: (A, B) => C): Rand3[C] =
    ra.map2(rb)(f)

  def both[A, B, C](ra: Rand3[A], rb: Rand3[B]): Rand3[(A, B)] =
    ra.map2(rb)((_, _))

  def intDoubleViaMap2: Rand3[(Int, Double)] =
    both(int, double)

  def doubleIntViaMap2: Rand3[(Double, Int)] =
    both(double, int)

  def doubleDoubleViaMap2: Rand3[(Double, Double)] =
    both(double, double)

  // my answer
  def sequence[A](rs: List[Rand3[A]]): Rand3[List[A]] =
    State.sequence(rs)

  // my answer
  def intsViaSequence(n: Int): Rand3[List[Int]] =
    sequence(List.fill(n)(int))

  def sequenceViaFoldRight[A](rs: List[Rand3[A]]): Rand3[List[A]] =
    rs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))

  def intsViaSequenceFoldRight(n: Int): Rand3[List[Int]] =
    sequenceViaFoldRight(List.fill(n)(int))

  def nonNegativeLessThan(n: Int): Rand3[Int] =
    nonNegativeInt.flatMap: i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeLessThan(n)

  def flatMap[A, B](r: Rand3[A])(f: A => Rand3[B]): Rand3[B] =
    r.flatMap(f)

  def mapViaFlatMap[A, B](s: Rand3[A])(f: A => B): Rand3[B] =
    s.flatMap(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand3[A], rb: Rand3[B])(
      f: (A, B) => C
  ): Rand3[C] =
    ra.flatMap(a => map(rb)(b => f(a, b)))
