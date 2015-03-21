import scala.util.parsing.combinator.RegexParsers
import java.io.Reader

object Util {

  // don't do these words, they are common probably irrelavent
  val simpleWords = List("the", "for", "and").map(_.toUpperCase)
  def simpleWord(w: String) = w.length < 3 || simpleWords.contains(w.toUpperCase)

  object WordParser extends RegexParsers {
    val word = """[^\s]+""".r
    val words = rep1(word)
    def apply(r: Reader) = parseAll(words, r).get.filterNot(simpleWord _).map(cleanWord _)
  }

  val nonWordChars = """[^A-Za-z]"""
  def cleanWord(w: String) = w.replaceAll(nonWordChars, "").toUpperCase
}
