import scala.util.Random

class Robot {

  import Robot._

  private var current = newName

  def name = current

  def reset(): Unit = current = newName

}


object Robot {

  private var remainingNames = allNames

  private def newName: String = remainingNames.headOption match {
    case Some(n) =>
      remainingNames = remainingNames.tail
      n
    case None    => throw new UnsupportedOperationException("No more names available")
  }

  private lazy val allNames: Stream[String] =
    for {
      l1 <- letters
      l2 <- letters
      d1 <- digits
      d2 <- digits
      d3 <- digits
    } yield s"$l1$l2$d1$d2$d3"

  private def letters = Random.shuffle(('A' to 'Z').toStream)

  private def digits = Random.shuffle(('0' to '9').toStream)

}

/* for testing purposes - test case for exhausted list
private def allNames: Stream[String] = Random.shuffle(
  for {
    d <- digits
  } yield s"$d")
*/