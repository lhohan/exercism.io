import scala.annotation.tailrec

class Sublist {

  def sublist[A](xs: List[A], ys: List[A]): Sublist.Value =
    (isSuperlist(xs, ys), isSuperlist(ys, xs)) match {
      case (true, true)   => Sublist.Equal
      case (true, false)  => Sublist.Superlist
      case (false, true)  => Sublist.Sublist
      case (false, false) => Sublist.Unequal
    }

  @tailrec
  private def isSuperlist[A](xs: List[A], ys: List[A]): Boolean =
    if (startsWith(xs, ys)) {
      true
    } else if (xs.size <= ys.size) {
      false
    } else {
      isSuperlist(xs.tail, ys)
    }

  /** xs startsWith ys */
  def startsWith[A](xs: List[A], ys: List[A]): Boolean =
    (xs, ys) match {
      case (_, Nil)                       => true
      case (Nil, _)                       => false
      case (x :: xss, y :: yss) if x == y => startsWith(xss, yss)
      case _                              => false
    }
}

object Sublist extends Enumeration {
  val Sublist, Superlist, Unequal, Equal = Value
}