package pbtest

import state.{RNG2, State}
import Gen.listOfN

class PbtestSuite extends munit.FunSuite:
  test("1"):
    val rng = RNG2.Simple2(System.nanoTime())
    val s =
      Gen.chooseStartZeroStopInclusive(1).listOfN(Gen.choose(10, 15)).run(rng)
    println(s._1)
end PbtestSuite
