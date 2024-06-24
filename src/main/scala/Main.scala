import monoids.*

@main def main(): Unit =
  val s = "lorem ipsum dolor sit amet, "
  val count = wordCounter(s, wcMonoid)
  println(count)
