import scala.io.Source
import java.io.FileInputStream
import scala.collection.immutable.VectorBuilder
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.StreamReader
import java.io.{StringReader, FileReader, PrintStream}

object CsvParser extends RegexParsers {

  val desired = List(6,8)

  def csv = rep1sep(line, lsep)
  def line = rep1sep(field, fsep)
  def field = quoted | """[^,"\n\r]+""".r | ""
  def quoted = """"[^"]+"""".r
  def fsep = ","

  override def skipWhitespace = false

//  def separator = accept(',')

  def lsep = rep1("""[\n\r]+""".r)

//  def eof = Parser(in => if(in.atEnd) Success((), in) else Error("expected eof", in))

  // def processCsv(in: Source): IndexedSeq[IndexedSeq[String]] = {
  //   val csv = new VectorBuilder[String]
  //   val line = new VectorBuilder[String]

  //   in.foreach {
  //     case '\n' => println("new line")
  //     case _    => println("not recog")
  //   }
  //   Vector.empty
  // }

  def parseFile(fname: String) =
    parseAll(csv, StreamReader(new FileReader(fname)))

  def parseString(s: String) =
    parseAll(csv, StreamReader(new StringReader(s)))

  def transform(in: List[List[String]], out: PrintStream): Unit =
    for(line <- in) {
      out.println(desired.map(line.lift(_).getOrElse("-")).mkString(","))
    }

  def main(args: Array[String]) = {
    if(args.length < 2) {
      println("Args: infile outfile")
    } else {
      val (infile, outfile) = (args(0), args(1))
      parseFile(infile) match {
        case Success(l, _) => println(s"success: ${l.length}"); transform(l, new PrintStream(outfile))
        case f: NoSuccess => System.err.println(s"failure $f")
      }
    }
  }
}
