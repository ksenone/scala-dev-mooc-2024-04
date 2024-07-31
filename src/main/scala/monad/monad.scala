package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  sealed trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] = x match {
      case null => EmptyWrap
      case _ => NonEmptyWrap(x)
    }

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = this match {
      case NonEmptyWrap(value) => f(value)
      case _ => EmptyWrap
    }

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = this.flatMap(v => pure(f(v)))

    def withFilter(f: A => Boolean): Wrap[A] = this match {
      case wrap @ NonEmptyWrap(value) if f(value) => wrap
      case _ => EmptyWrap
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element

}