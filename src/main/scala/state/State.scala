package state

opaque type State[S, +A] = S => (A, S)
object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) =
      underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- s2
      yield f(a, b)

  def sequence[S, A](actions: List[State[S, A]]): State[S, List[A]] =
    actions.foldRight(unit(Nil: List[A]))((f, acc) => f.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] =
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def unit[S, A](a: A): State[S, A] =
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def get[S]: State[S, S] = s => (s, s)
  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

