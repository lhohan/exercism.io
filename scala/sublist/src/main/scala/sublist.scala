class Sublist {

  def sublist[A](xs: List[A], ys: List[A]): Sublist.Value =
    (isSublist(xs, ys), isSublist(ys, xs)) match {
      case (true, true)   => Sublist.Equal
      case (true, false)  => Sublist.Superlist
      case (false, true)  => Sublist.Sublist
      case (false, false) => Sublist.Unequal
    }

  private def isSublist[A](xs: List[A], ys: List[A]) = xs.containsSlice(ys)

}

object Sublist extends Enumeration {
  val Sublist, Superlist, Unequal, Equal = Value
}