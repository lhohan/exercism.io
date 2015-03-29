object Raindrops {
  def apply() = this
  def raindrops = (3, "Pling") :: (5, "Plang") :: (7, "Plong") :: Nil

  def convert(n: Int) = {
    val sounds = raindrops.map(x => if (n % x._1 == 0) x._2 else "").mkString
    if (sounds.nonEmpty) sounds else n.toString
  }
}