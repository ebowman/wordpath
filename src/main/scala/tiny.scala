object Tiny extends App {
  val words = io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).map(_.toLowerCase).toSet
  val lookup = (for (word <- words; other <- words if diff(word, other) == 1) yield (word, other)).groupBy(_._1).map(a => a._1 -> a._2.map(_._2))
  val visited = collection.mutable.Set[String](args(0))
  println(search(List(List(args(0)))).reverse)

  def search(paths: Seq[Seq[String]]): Seq[String] = {
    val nextPaths = for (path <- paths; next <- lookup(path.head) if !visited.contains(next)) yield {
      visited += next; next +: path
    }
    nextPaths.find(_.head == args(1)).getOrElse(if (nextPaths.isEmpty) Nil else search(nextPaths))
  }

  def diff(a: String, b: String): Int = (0 until a.length).map(i => if (a(i) == b(i)) 0 else 1).sum
}
