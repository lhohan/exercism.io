object Dna {
  def apply() = this

  private val Mapping = Map(
    'G' -> 'C',
    'C' -> 'G',
    'T' -> 'A',
    'A' -> 'U')

  def toRna(dna: String) = dna.map(Mapping(_))
}