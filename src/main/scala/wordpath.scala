
object WordPath extends App {

  import Utils.time

  implicit val lookup = Dictionary.lookup
  import Solvers._
  import Utils.{pretty, prettys}

  /* this is more idiomatic and easier to understand & maintain, but slow (takes >8000 ms)
  import Utils.diff
  time("old lookup generation") {
    val allPairs: Set[(String, String)] = for {
      word <- words
      other <- words if other != word && diff(word, other) == 1
    } yield {
      (word, other)
    }

    // Map[String, Set[(String, String)]]
    val lookup: Map[String, Set[String]] = allPairs.groupBy(_._1).map(a => a._1 -> a._2.map(_._2))
  }
  */

  def solveAll(start: String, finish: String) {
    println("%s => %s".format(start, finish))
    println(prettys(time("solveAllShortestBFS")(solveAllShortestBFS(start, finish))))
    println(pretty(time("solveDFS")(solveDFS(start, finish))))
    println(pretty(time("solveAnyShortestBFS")(solveAnyShortestBFS(start, finish))))
    println(pretty(time("solveAnyShortestBFSAndrew")(solveAnyShortestBFSAndrew(start, finish))))
  }

  solveAll(args(0), args(1))
  sys.exit(0);

  // println(solveAll("head", "tail"))
  // sys.exit()

  /**for finding long paths*/
  // val count = 4000 //00000
  val manySolutions = time("computing %s in parallel".format(lookup.size * (lookup.size - 1))) {
    (for (a <- lookup.keys.view; b <- lookup.keys.view if a != b) yield (a, b)).par.map {
      case (a: String, b: String) => (a, b, solveDFS(a, b).size)
    }
  }

  val best = manySolutions.foldLeft("", "", 0)((a, b) => if (b._3 > a._3) b else a)

  println(best)
  println(pretty(solveDFS(best._1, best._2)))
  sys.exit(0)
  /**/

  /* for finding the longest shortest path /**/
  val manySolutions = time("computing %s in parallel".format(lookup.size * lookup.size - 1)) {
    (for (a <- lookup.keys.view; b <- lookup.keys.view if a != b) yield (a, b)).par.map {
      case (a: String, b: String) => (a, b, solveAnyShortestBFS(a, b).size)
    }
  }
  val longest = manySolutions.foldLeft("", "", 0)((a, b) => if (b._3 > a._3) b else a)

  println(longest)
  println(pretty(solveAnyShortestBFS(longest._1, longest._2)))
  println(pretty(solveAnyShortestBFSAndrew(longest._1, longest._2)))
  sys.exit(0)
  */


  for {
    a <- lookup.keys
    b <- lookup.keys if b != a
  } {
    solveAll(a, b)
  }


}
