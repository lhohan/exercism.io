class Accumulate {
  def accumulate[A, B](f: A => B, xs: List[A]) = xs.map(f)
}