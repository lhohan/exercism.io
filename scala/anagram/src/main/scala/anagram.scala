class Anagram(a: String) {

  val fa = frequencies(a)

  private def frequencies(w: String): Map[Char, Int] = {
    w.groupBy(s => s.toLower).mapValues(_.size)
  }

  private def isAnagram(w: String): Boolean = {
    !w.equalsIgnoreCase(a) && frequencies(w) == fa
  }

  def matches(ws: Seq[String]) = ws filter isAnagram

}