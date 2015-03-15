import scala.annotation.tailrec

case class Binary(b: String) {

  def toDecimal2: Int = {
    @tailrec
    def toDecimal(binary: List[Char], count: Int, total: Int): Int = binary match {
      case Nil       => total
      case '0' :: bs => toDecimal(bs, count + 1, total)
      case '1' :: bs => toDecimal(bs, count + 1, total + pow2(count))
      case _ :: bs   => 0
    }
    toDecimal(b.reverse.toList, 0, 0)
  }

  def toDecimal: Int = b.toStream.reverse.zipWithIndex.map(Some(_)).foldLeft(Option(0)) { (total, ob) =>
    ob match {
      case Some(('0', _)) => total
      case Some(('1', i)) => total.map(_ + pow2(i))
      case _              => None
    }
  }.getOrElse(0)

  @tailrec
  private def pow2(n: Int, t: Int = 1): Int =
    if (n <= 0) {
      t
    } else {
      pow2(n - 1, 2 * t)
    }


}