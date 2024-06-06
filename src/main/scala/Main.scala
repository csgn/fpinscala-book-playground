import pbtest.*
import pbtest.Prop.{MaxSize, TestCases}
import state.RNG2
import state.RNG2.Simple2

@main def main(): Unit =
  val rng = RNG2.Simple2(System.currentTimeMillis)
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(smallInt.nonEmptyList): l =>
    print("maxProp[F] ")
    print(l.toString + " ")
    val max = l.max
    l.forall(_ <= max)

  print("maxPropRUN ")
  maxProp.run(MaxSize.fromInt(1), TestCases.fromInt(3), rng = rng)
  println("DONE")
