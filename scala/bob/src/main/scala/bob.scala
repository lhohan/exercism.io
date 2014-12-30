class Bob {
  def hey(text: String): String = {
    def shouting(s: String) = {
      val letters = s.filter(_.isLetter)
      letters.nonEmpty && letters.forall(_.isUpper)
    }
    text match {
      case t if t.trim.isEmpty => "Fine. Be that way!"
      case t if shouting(t) => "Whoa, chill out!"
      case t if t.endsWith("?") => "Sure."
      case _ => "Whatever."
    }
  }
}
