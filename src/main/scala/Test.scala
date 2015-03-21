import java.io.FileReader

object Test extends App {
  val words = Util.WordParser(new FileReader("data/job-advert.txt"))
  words.foreach(w => println(s"[$w]"))
}
