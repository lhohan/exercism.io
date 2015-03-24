import scala.annotation.tailrec

object PrimeFactors {

  def apply() = this

  def primeFactors(number: Long): List[Long] = {

    def from(long: Long): Stream[Long] = long #:: from(long + 1)

    @tailrec
    def factors(n: Long, acc: List[Long], ps: Stream[Long], currentPrime: Long): List[Long] =
      (n, n % currentPrime) match {
        case (1, _) => acc
        case (_, 0) => factors(n / currentPrime, acc :+ currentPrime, ps, currentPrime)
        case _      => factors(n, acc, ps.tail, ps.head)
      }

    lazy val primes: Stream[Long] = 2 #:: from(3).filter(_ % primes.head != 0)
    factors(number, List.empty[Long], primes.tail, primes.head)
  }
}