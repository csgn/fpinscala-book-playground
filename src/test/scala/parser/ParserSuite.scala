package parser

class ParserSuite extends munit.FunSuite:
  val o: Option[Int] = Some(2)

  o match {
    case Some(x) => println(x)
    case None    => println("NONE")
  }

  o match {
    case x @ Some(_) => println(x)
    case None        => println("NONE")
  }

end ParserSuite
