import monads.*

import io.{Program1 => p1}
import io.{Program2 => p2}
import io.{Program3 => p3}
import io.{Program4 => p4}
import io.{Program5 => p5}
import io.{Program6 => p6}
import io.Program6.TailRec

@main def main(): Unit = {
  val result = p6.Examples.factorial(3)
  println(result.unsafeRun)
}
