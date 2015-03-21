import java.io.FileReader

object Test extends App {
  val words = Util.WordParser(new FileReader("data/job-advert.txt"))
  val cl = Classifier.read("data/soc-processed.csv")
  println(cl classify words)
}
