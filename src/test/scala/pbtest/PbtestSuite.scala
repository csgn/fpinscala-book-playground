package pbtest

import parallel.LL
import state.{RNG2, State}
import java.util.concurrent.{ExecutorService, Executors}

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

    // val g1 = Gen.listOfN(5, Gen.choose(0, 5))
    // val g2 = Gen.listOfN(5, Gen.choose(5, 10))
    // val p = Prop.forAll(g1)(a => 1 == 1)
    //   && Prop.forAll(g2)(b => 1 == 1)
    // p.run

    // val smallInt = Gen.choose(-10, 10)
    // val maxProp = Prop.forAll(smallInt.nonEmptyList): l =>
    //   val max = l.max
    //   !l.exists(_ > max)

    val es = Executors.newCachedThreadPool

    // val p1 = Prop.forAll(Gen.unit(LL.unit(1))): pi =>
    //   pi.map(_ + 1).run(es).get == LL.unit(2).run(es).get

    // val p2 = Prop.verify:
    //   val p1 = LL.unit(1).map(_ + 1)
    //   val p2 = LL.unit(2)
    //   p1.run(es).get == p2.run(es).get

    // val p3 = Prop.verify:
    //   Prop
    //     .equal(
    //       LL.unit(1).map(_ + 1),
    //       LL.unit(2)
    //     )
    //     .run(es)
    //     .get

    val p4 = Prop.verifyPar:
      Prop.equal(
        LL.unit(1).map(_ + 1),
        LL.unit(2)
      )

    p4.run()

end PbtestSuite
