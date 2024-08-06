import monads.*


@main def main(): Unit =
  val form = WebForm.validateWebForm(
    "sergen",
    "2001-10-",
    "398463298"
  )

  println(form)


