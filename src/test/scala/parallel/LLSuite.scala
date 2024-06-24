package parallel

import scala.util.Random
import java.util.concurrent.*

class LLSuite extends munit.FunSuite:
  private val es = Executors.newFixedThreadPool(4)

  test("Sorting parallel list of integers"):
    val unsortedParListOfInt = LL.unit(List(3, 2, 1))
    val sortedParListOfInt = LL.unit(List(1, 2, 3))

    val executedSortedParListOfInt = LL.sortPar(unsortedParListOfInt)

    val obtained =
      executedSortedParListOfInt.run(es).get
    val expected = sortedParListOfInt.run(es).get

    assertEquals(obtained, expected)

  test("Sequence parallel list of integers"):
    val listOfParIntegers = List(LL.unit(1), LL.unit(2), LL.unit(3))
    val parListOfIntegers = LL.unit(List(1, 2, 3))

    val sequencedList = LL.sequence(listOfParIntegers)

    val obtained = sequencedList.run(es).get
    val expected = parListOfIntegers.run(es).get

    assertEquals(obtained, expected)

  test("Filter odd integers numbers"):
    val listOfIntegers = List(1, 2, 3, 4, 5)
    val expected = List(1, 3, 5)
    val obtained = LL
      .parFilter(listOfIntegers)(_ % 2 != 0)
      .run(es)
      .get(500, TimeUnit.MILLISECONDS)

    assertEquals(obtained, expected)

  test("map2"):
    val pa = LL.unit(1)
    val pb = LL.unit(2)

    val expected = 3
    val obtained = pa.map2(pb)((a, b) => a + b).run(es).get

    assertEquals(obtained, expected)

  test("map3"):
    val pa = LL.unit(1)
    val pb = LL.unit(2)
    val pc = LL.unit(3)

    val expected = 6
    val obtained = pa.map3(pb, pc)((a, b, c) => a + b + c).run(es).get

    assertEquals(obtained, expected)

  test("map4"):
    val pa = LL.unit(1)
    val pb = LL.unit(2)
    val pc = LL.unit(3)
    val pd = LL.unit(4)

    val expected = 10
    val obtained =
      pa.map4(pb, pc, pd)((a, b, c, d) => a + b + c + d).run(es).get

    assertEquals(obtained, expected)

  test("map5"):
    val pa = LL.unit(1)
    val pb = LL.unit(2)
    val pc = LL.unit(3)
    val pd = LL.unit(4)
    val pe = LL.unit(5)

    val expected = 15
    val obtained =
      pa.map5(pb, pc, pd, pe)((a, b, c, d, e) => a + b + c + d + e).run(es).get

    assertEquals(obtained, expected)

  test("choice"):
    val l = List(1, 22, 3)
    val cond = math.random < 0.25
    val expected = if cond then 1 else 22
    val obtained =
      LL.choice(LL.unit(cond))(LL.unit(l.min), LL.unit(l.max)).run(es).get

    assertEquals(obtained, expected)

  test("choice in terms of choiceN"):
    val l = List(1, 22, 3)
    val cond = math.random < 0.25
    val expected = if cond then 1 else 22
    val obtained =
      LL.choice(LL.unit(cond))(LL.unit(l.min), LL.unit(l.max)).run(es).get

    assertEquals(obtained, expected)

  test("choiceN"):
    val l = List(LL.unit(1), LL.unit(2), LL.unit(3))
    val n = 1
    val expected = l(n).run(es).get
    val obtained = LL.choiceN(LL.unit(n))(l).run(es).get

    assertEquals(obtained, expected)

end LLSuite
