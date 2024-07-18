import monoids.{MonoidSyntax, MonoidInstances, MonoidGivens}
import MonoidSyntax.*
import MonoidInstances.*
import MonoidGivens.given

@main def main(): Unit =
  val im =
    foldMapBalancedViaGiven(IndexedSeq("a", "b", "c", "d"))(identity)
  println(im)

  val f: Int => String = (n: Int) => s"$n t"
  val g: String => Int = (s: String) => s.length

  println(f.andThen(g)(2))
