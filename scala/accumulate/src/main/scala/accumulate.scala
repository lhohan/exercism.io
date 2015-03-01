class Accumulate {
  def accumulate[A, B](f: A => B, as: List[A]): List[B] = as match {
    case Nil => Nil
    case x :: xs => f(x) :: accumulate(f, xs)
  }
}