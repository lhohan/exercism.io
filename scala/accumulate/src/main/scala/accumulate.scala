import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: A => B, as: List[A]): List[B] = {
    @tailrec
    def loop(ass: List[A], acc: List[B]): List[B] = ass match {
      case Nil => acc
      case x :: xs => loop(xs, acc :+ f(x))
    }
    loop(as, Nil)
  }
}

// Non-tail recursive solution

//class Accumulate {
//  def accumulate[A, B](f: A => B, as: List[A]): List[B] = as match {
//    case Nil => Nil
//    case x :: xs => f(x) :: accumulate(f, xs)
//  }
//}