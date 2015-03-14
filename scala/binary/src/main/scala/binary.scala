import scala.annotation.tailrec

case class Binary(b: String) {
  def toDecimal: Int = {
    def pow2(n: Int): Int = {
      @tailrec
      def pow2(n: Int, acc: Int): Int =
        if (n <= 0) {
          acc
        } else {
          pow2(n - 1, 2 * acc)
        }
      pow2(n, 1)
    }

    @tailrec
    def toDecimal(binary: List[Char], count: Int, total: Int): Int = binary match {
      case Nil       => total
      case '0' :: bs => toDecimal(bs, count + 1, total)
      case '1' :: bs => toDecimal(bs, count + 1, total + pow2(count))
      case _ :: bs   => 0
    }
    toDecimal(b.reverse.toList, 0, 0)
  }
}