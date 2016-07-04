import scala.io.Source

val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")

// create a list of words and filter those words contains non letter characters such as '-'
// because our mnemonics map does not contain a mapping from a number to '-'
val words = in.getLines.toList filter { _ forall { _.isLetter } }

// our mnemonics map
val mnem: Map[Char, String] = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

// invert the mnemonics map to give a map from chars 'A-Z' to numbers '2-9'
val charCode: Map[Char, Char] =
  for {
    (num, chars) <- mnem
    char <- chars
  } yield char -> num

// maps a word to the digit string it can represent. e.g. "Java" to "5282"
def wordCode(word: String): String = word.toUpperCase map charCode

// a map from digit strings to the words that represent them.
// e.g. "5282" -> List("Java", "Kata", ...)
// a missing number is mapped to the empty list. e.g. "1111"
val codeWords: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

// return all ways to decode a number as a list of words
def decode(code: String): Set[List[String]] = {
  if (code.isEmpty) Set(List())
  else {
    for {
      n <- 1 to code.length
      word <- codeWords(code take n)
      rest <- decode(code drop n)
    } yield word :: rest
  }.toSet
}

def translate(code: String): Set[String] =
  decode(code) map (_ mkString " ")

translate("7225247386") foreach println
