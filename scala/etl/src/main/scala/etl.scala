object ETL {
  def transform(in: Map[Int, Seq[String]]): Map[String, Int] = {
    in.foldLeft(Map.empty[String, Int]) { (stringsToValue, valueToStrings) =>
      val (value, strings) = valueToStrings
      strings.foldLeft(stringsToValue) { (stringsForValue, str) =>
        stringsForValue + (str.toLowerCase -> value)
      }
    }
  }

}