object ETL {
  def transform(in: Map[Int, Seq[String]]): Map[String, Int] = for {
    (value, words) <- in
    word <- words
  } yield (word.toLowerCase, value)
}