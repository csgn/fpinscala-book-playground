import pbtest.*
import pbtest.Prop.{MaxSize, TestCases}
import state.RNG2.Simple2

@main def main(): Unit =
  val smallInt = Gen.choose(-10, 10)
  val maxProp = Prop.forAll(smallInt.listOfN(5)): l =>
    val max = l.max
    l.forall(_ <= max)

  maxProp.run(MaxSize.fromInt(3), TestCases.fromInt(3), Simple2(System.currentTimeMillis))


