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

end NonBlockLLSuite
