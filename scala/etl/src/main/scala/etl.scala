object ETL {
  def transform(in: Map[Int, Seq[String]]): Map[String, Int] = {
    in.flatMap { case (value, strings) =>
      strings.map(str => str.toLowerCase -> value)
    }
  }
}