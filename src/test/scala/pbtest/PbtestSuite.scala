package pbtest

import state.{RNG2, State}
import Gen.listOfN

class PbtestSuite extends munit.FunSuite:
  test("1"):
    val rng = RNG2.Simple2(System.nanoTime())
    // val s =
    //   Gen.chooseStartZeroStopInclusive(1).listOfN(Gen.choose(10, 15)).run(rng)

    // val s =
    //   Gen
    //     .weighted((Gen.choose(0, 5), 0.9), (Gen.choose(5, 10), 0.1))
    //     .listOfN(10)
    //     .run(rng)

    // val s = Gen.odd(0, 8).run(rng)

    /* println(Prop.buildMsg(Gen.choose(0, 5), Exception("hello world"))) */

    val g1 = Gen.listOfN(5, Gen.choose(0, 5))
    val g2 = Gen.listOfN(5, Gen.choose(5, 10))
    val p = Prop.forAll(g1)(a => 1 == 1)
      && Prop.forAll(g2)(b => 1 != 1)

    p.run
end PbtestSuite
