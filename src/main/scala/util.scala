object Util {

  val nonWordChars = """[^A-Za-z]"""
  def cleanWord(w: String) = w.replaceAll(nonWordChars, "").toUpperCase

}
