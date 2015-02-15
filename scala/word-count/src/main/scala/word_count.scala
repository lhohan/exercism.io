class Phrase(private val phrase: String) {

  def wordCount = {
    val words = phrase.split("[^\\w']").filter(_.nonEmpty)
    val groupedWords = words.groupBy(_.toLowerCase)
    groupedWords.mapValues(_.size)
  }

}