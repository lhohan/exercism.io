object DNA {
  val Nucleotides = Set('A', 'T', 'C', 'G')
}

class DNA(dna: String) {

  import DNA._

  dna.foreach(validate)

  def count(c: Char): Int = {
    validate(c)
    dna.count(_ == c)
  }

  lazy val nucleotideCounts: Map[Char, Int] =
    Nucleotides.map { c => (c, count(c))}.toMap

  private def validate(c: Char) = require(Nucleotides contains c)

}