object ETL {
  def transform(in: Map[Int, Seq[String]]): Map[String, Int] = {
    in.foldLeft(Map.empty[String, Int]){(acc, el) =>
      val (i:Int, xs) = el
      xs.foldLeft(acc){(acc1, el1) =>
        acc1 + (el1.toLowerCase -> i)
      }
    }
  }

}