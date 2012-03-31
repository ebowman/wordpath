import collection.immutable.{Set, Map}

object Wordpath extends App {
  def difference(a: String, b: String): Int = {
    var i = 0
    var sum = 0
    while (i < a.length) {
      if (a(i) != b(i)) {
        sum += 1
      }
      i += 1
    }
    sum
  }

  val words = io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).toSet

  val allPairs = for {
    word <- words
    other <- words if other != word && difference(word, other) == 1
  } yield {
    (word, other)
  }
  val lookup: Map[String, Set[String]] = allPairs.groupBy(_._1).map(a => a._1 -> a._2.map(_._2))

  val start = "boil"
  val target = "fast"

  var cursor = (start :: Nil) :: Nil
  while (!cursor.head.isEmpty && !cursor.head.exists(difference(_, target) == 0)) {
    val step1 = cursor.head.map(lookup(_).toList.map(other => (other, difference(target, other)))).flatten
    val min = step1.map(_._2).min
    cursor ::= step1.filter(_._2 == min).map(_._1)
  }
  println("solution = " + cursor.reverse)

}
