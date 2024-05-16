package state

trait RNG:
  def nextInt: (Int, RNG)

object RNG:
  case class Simple(seed: Long) extends RNG:
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r)

  /*
   * between 0 - 1, not including 1
   */
  def double(rng: RNG): (Double, RNG) =
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, r) = rng.nextInt
    val (i2, r2) = double(r)
    ((i, i2), r2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i1, i2), r) = intDouble(rng)
    ((i2, i1), r)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (i1, r1) = double(rng)
    val (i2, r2) = double(r1)
    val (i3, r3) = double(r2)
    ((i1, i2, i3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @scala.annotation.tailrec
    def go(n: Int, acc: List[Int], r2: RNG): (List[Int], RNG) =
      if n >= count then (acc, r2)
      else
        val (i1, r3) = r2.nextInt
        go(n + 1, i1 :: acc, r3)

    go(0, List(), rng)
