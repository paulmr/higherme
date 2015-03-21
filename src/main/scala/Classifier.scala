import java.io.{InputStream, FileInputStream}
import scala.io.Source
import Util.cleanWord

class Classifier(corpus: Map[String, Classifications]) {
  def score(word: String): Classifications = {
    val res = corpus.getOrElse(cleanWord(word), Classifications.empty)
    if(!res.isEmpty) { println(s"$word matches " + res) }
    res
  }

  def classify(words: Seq[String]) =
    words.foldLeft(Classifications.empty) { (acc, word) =>
      acc ++ score(word)
    }
}

object Classifier {
  def read(stream: InputStream): Classifier = {
    val in = Source.fromInputStream(stream).getLines
    val corpus = in.foldLeft(Map.empty[String, Classifications]) { (corpusAcc, rawline) =>
      val line = CSVLine(rawline)
      val cl = corpusAcc.getOrElse(line.word, Classifications.empty)
      corpusAcc + (line.word -> (cl + Classification(CSVLine(rawline))))
    }
    new Classifier(corpus)
  }

  def read(fname: String): Classifier = read(new FileInputStream(fname))
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

class Classifications(private val data: Map[String, Int]) {
  def isEmpty = data.isEmpty
  def +(cl: Classification): Classifications =
    new Classifications(data + (cl.code -> cl.weight))

  // merges
  def ++(that: Classifications): Classifications =
    new Classifications(
      that.data.keys.foldLeft(data) { (acc, thatKey) =>
        acc + (thatKey -> (acc.getOrElse(thatKey, 0) + that.data(thatKey)))
      }
    )

  def sorted: Seq[(String, Int)] = data.toList sortBy { case (_, weight) => weight }

  override def toString =
    sorted.take(5).map { case (code, weight) => s"$code -> $weight" } mkString ", "

}

object Classifications {
  def empty = new Classifications(Map.empty[String, Int])
  def apply(c: Classification) = new Classifications(Map(c.code -> c.weight))
}

case class Classification(code: String, weight: Int)
object Classification {
  def apply(line: CSVLine): Classification = Classification(line.soc, line.weight)
}
