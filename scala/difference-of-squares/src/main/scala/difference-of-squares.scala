case class Squares() {
  
  def squareOfSums(n: Int) = {
    val x = (1 to n).sum
    x * x
  }

  def sumOfSquares(n: Int) = (1 to n).map(x=> x*x).sum

  def difference(n: Int) =  squareOfSums(n) - sumOfSquares(n)

}