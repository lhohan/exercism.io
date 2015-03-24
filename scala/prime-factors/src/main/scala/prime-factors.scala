import scala.annotation.tailrec

object PrimeFactors {

  def apply() = this

  def primeFactors(number: Long): List[Long] = {

    def from(long: Long): Stream[Long] = Stream.cons(long, from(long + 1))

    @tailrec
    def factors(n: Long, acc: List[Long], ps: Stream[Long], currentPrime: Long = 2): List[Long] = {
      if (n == 1) {
        acc
      } else {
        val remainder = n % currentPrime
        if (remainder == 0) {
          val quotient = n / currentPrime
          factors(quotient, acc :+ currentPrime, ps, currentPrime)
        } else {
          factors(n, acc, ps.tail, ps.head)
        }
      }
    }

    lazy val primes: Stream[Long] = 2 #:: from(3).filter(_ % primes.head != 0)
    factors(number, List.empty[Long], primes)
  }
}