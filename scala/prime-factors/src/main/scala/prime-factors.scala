import scala.annotation.tailrec

object PrimeFactors {

  def apply() = this

  type Prime = (Long, PrimeGenerator)

  case class PrimeGenerator(sieve: Stream[Long]) {
    def nextPrime: Prime = {
      (sieve.head, PrimeGenerator(sieve.filter(_ % sieve.head != 0)))
    }
  }

  def primeFactors(number: Long): List[Long] = {

    def from(long: Long): Stream[Long] = Stream.cons(long, from(long + 1))

    @tailrec
    def factors(n: Long, acc: List[Long], primeGenerator: PrimeGenerator, currentPrime: Long = 2): List[Long] = {
      if (n == 1) {
        acc
      } else {
        println(s"prime is $currentPrime")
        val remainder = n % currentPrime
        if (remainder == 0) {
          val quotient = n / currentPrime
          factors(quotient, acc :+ currentPrime, primeGenerator, currentPrime)
        } else {
          val (nextPrime, generator) = primeGenerator.nextPrime
          factors(n, acc, generator, nextPrime)
        }
      }
    }
    factors(number, List.empty[Long], new PrimeGenerator(from(2)))
  }
}