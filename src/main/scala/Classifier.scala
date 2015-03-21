import java.io.{InputStream, FileInputStream}
import scala.io.Source

class Classifier(val corpus: Map[String, List[Classification]]) {
//  def score(word: String): List[(String, Int)] =
}

case class CSVLine(word: String, soc: String, weight: Int)
object CSVLine {
  private val numberOfFields = 3
  def apply(line: String) = {
    val fields = line.split(",")
    assert(fields.length == numberOfFields, s"line ($line) should have $numberOfFields fields")
    new CSVLine(fields(0), fields(1), fields(2).toInt)
  }
}

case class Classification(code: String, weight: Int)
object Classification {
  def fromLine(line: CSVLine) = Classification(line.soc, line.weight)
}

object Classifier {
  def read(stream: InputStream): Classifier = {
    val in = Source.fromInputStream(stream).getLines
    val corpus = in.foldLeft(Map.empty[String, List[Classification]]) { (corpusAcc, rawline) =>
      val line    = CSVLine(rawline)
      val classes = corpusAcc.getOrElse(line.word, Nil)
      corpusAcc.updated(line.word, (Classification fromLine line) :: classes)
    }
    new Classifier(corpus)
  }

  def read(fname: String): Classifier = read(new FileInputStream(fname))

}
