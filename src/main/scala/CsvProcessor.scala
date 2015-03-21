import scala.io.Source
import java.io.FileInputStream
import scala.collection.immutable.VectorBuilder
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.StreamReader
import java.io.{StringReader, FileReader, PrintStream}

object CsvParser extends RegexParsers {

  /* e.g. header */
  val skipLines = 1

  /* names for the fields that I want to access */
  object Fields {
    def field(i: Int) = (l: List[String]) => l.lift(i)
    def soc  = field(6)
    def name = field(8)
  }

  val nonWordChars = """[^A-Za-z]"""
  val wordSep = """[ \t]+"""

  def csv = rep1sep(line, lsep)
  def line = rep1sep(field, fsep)
  def field = quoted | """[^,"\n\r]+""".r | ""
  def quoted = """"[^"]+"""".r
  def fsep = ","

  override def skipWhitespace = false

  def lsep = rep1("""[\n\r]+""".r)

  def parseFile(r: FileReader) =
    parseAll(csv, StreamReader(r))

  def parseString(s: String) =
    parseAll(csv, StreamReader(new StringReader(s)))

  def cleanWord(w: String) = w.replaceAll(nonWordChars, "").toUpperCase

  // don't do these words, they are common probably irrelavent
  val simpleWords = List("the", "for", "a", "in", "at", "and", "").map(_.toUpperCase)
  def simpleWord(w: String) = simpleWords.contains(w.toUpperCase)

  def transform(in: List[List[String]], out: PrintStream): Unit =
    for(line <- in.drop(skipLines)) {
      for {
        name <- Fields.name(line)
        soc  <- Fields.soc(line)
        nameWords = name.split(wordSep).map(cleanWord _).filterNot(simpleWord _)
      } {
        assert(soc.indexOf(',') == -1)
        nameWords foreach { word => out.println(s"$word,$soc") }
      }
    }


  def main(args: Array[String]) = {
    if(args.length < 2) {
      println("Args: infile outfile")
    } else {
      val (infile, outfile) = (args(0), args(1)) match {
        case (in, "-") => (new FileReader(in), System.out)
        case (in, out) => (new FileReader(in), new PrintStream(out))
      }
      parseFile(infile) match {
        case Success(l, _) => println(s"success: ${l.length}"); transform(l, outfile)
        case f: NoSuccess => System.err.println(s"failure $f")
      }
    }
  }
}
