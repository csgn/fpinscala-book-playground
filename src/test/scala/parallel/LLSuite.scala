package parallel

import parallel.*
import java.util.concurrent.*

class LLSuite extends munit.FunSuite:
  val es = Executors.newSingleThreadExecutor()

  test("Sorting parallel list of integers"):
    val unsortedParListOfInt = LL.unit(List(3, 2, 1))
    val sortedParListOfInt = LL.unit(List(1, 2, 3))

    val executedSortedParListOfInt = unsortedParListOfInt.sortPar

    val futureA =
      executedSortedParListOfInt.run(es)
    val futureB = sortedParListOfInt.run(es)

    assertEquals(futureA.get, futureB.get)

  test("Sequence parallel list of integers"):
    val listOfParIntegers = List(LL.unit(1), LL.unit(2), LL.unit(3))
    val parListOfIntegers = LL.unit(List(1, 2, 3))

    val sequencedList = LL.sequence(listOfParIntegers)

    val futureA = parListOfIntegers.run(es)
    val futureB = sequencedList.run(es)

    assertEquals(futureA.get, futureB.get)

  test("t"):
    val listOfOperations = List(1, 2, 3, 4, 5)
    LL.parMap(listOfOperations)(_ + 1)
      .run(es)
      .get(3000, TimeUnit.MILLISECONDS)

end LLSuite
