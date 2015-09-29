import Allergen.Allergen

object Allergen extends Enumeration {
  type Allergen = Value
  val Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats = Value
}

object Allergies {
  def apply() = this

  def isAllergicTo(allergen: Allergen, score: Int): Boolean = (score & toScore(allergen)) != 0

  def allergies(score: Int): List[Allergen] = Allergen.values.filter(a => isAllergicTo(a, score)).toList

  private lazy val toScore: Map[Allergen, Int] = {
    lazy val pow2: Stream[Int] = Stream.cons(1, pow2.map(_ * 2))
    val scoresLowToHigh = pow2.take(Allergen.values.size).toList
    Allergen.values.zip(scoresLowToHigh).toMap
  }
}

