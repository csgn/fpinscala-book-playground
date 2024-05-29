package parallel

import NonBlockLL.*
import java.util.concurrent.*

class NonBlockLLSuite extends munit.FunSuite:
  private val es = Executors.newFixedThreadPool(1)

  test("unit"):
    val expected = 1 + 1
    val obtained = unit(1 + 1).run(es)

    assertEquals(obtained, expected)

  test("map2"):
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)

    val expected = l1.sum + l2.sum
    val obtained = unit(l1).map2(unit(l2))(_.sum + _.sum).run(es)

    assertEquals(obtained, expected)

  test("map"):
    val l1 = List(1, 2, 3)

    val expected = l1.sum
    val obtained = unit(l1).map(_.sum).run(es)

    assertEquals(obtained, expected)

  test("flatMap"):
    val l1 = List(1, 2, 3)

    val expected = List(2, 3, 4)
    val obtained = unit(l1).flatMap(a => unit(a.map(_ + 1))).run(es)

    assertEquals(obtained, expected)

  test("zip"):
    val key = "a"
    val value = 1

    val expected = Tuple2(key, value)
    val obtained = unit(key).zip(unit(value)).run(es)

    assertEquals(obtained, expected)

  test("Sequence non-blocking parallel list of integers"):
    val listOfParIntegers = List(unit(1), unit(2), unit(3))
    val parListOfIntegers = unit(List(1, 2, 3))

    val sequencedList = sequence(listOfParIntegers)

    val obtained = sequencedList.run(es)
    val expected = parListOfIntegers.run(es)

    assertEquals(obtained, expected)

  test("Sequence non-blocking parallel list of integers with SequenceBalanced"):
    val listOfParIntegers = List(unit(1), unit(2), unit(3))
    val parListOfIntegers = unit(List(1, 2, 3))

    val sequencedList = sequenceViaSequenceBalanced(listOfParIntegers)

    val obtained = sequencedList.run(es)
    val expected = parListOfIntegers.run(es)

    assertEquals(obtained, expected)

  test("choice"):
    val l = List(1, 22, 3)
    val cond = math.random < 0.25
    val expected = if cond then 1 else 22
    val obtained = choice(unit(cond))(unit(l.min), unit(l.max)).run(es)

    assertEquals(obtained, expected)

end NonBlockLLSuite
