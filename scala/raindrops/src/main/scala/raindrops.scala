object Raindrops {
  def apply() = this

  def raindrops = (3, "Pling") :: (5, "Plang") :: (7, "Plong") :: Nil

  def convert(n: Int) = {
    val converted = raindrops.foldLeft("")((acc, raindrop) =>
      if (n % raindrop._1 == 0) acc + raindrop._2 else acc
    )
    if (converted.isEmpty) n.toString else converted
  }
}