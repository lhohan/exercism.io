object Hamming {
  def compute(x: String, y: String): Int = x.zip(y).count(xy => xy._1 != xy._2)
}
