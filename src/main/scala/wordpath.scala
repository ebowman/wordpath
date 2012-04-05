import annotation.tailrec
import collection.mutable.HashMap

object WordPath extends App {
  def diff(a: String, b: String): Int = (0 until a.length).map(i => if (a(i) == b(i)) 0 else 1).sum

  def pieces(word: String) = Seq("_" + word.drop(1), word.take(1) + "_" + word.drop(2), word.take(2) + "_" + word.drop(3), word.take(3) + "_")

  def time[T](label: String)(f: => T): T = {
    val start = System.currentTimeMillis
    val result = f
    val end = System.currentTimeMillis
    println("%s took %s ms".format(label, end - start))
    result
  }

  val words = time("load")(io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).map(_.toLowerCase).toSet)
  val lookup: Map[String, Set[String]] = time("build lookup") {
    class String2SetMap extends HashMap[String, Set[String]] {
      override def default(key: String) = Set()
    }

    // Set((bult,List(_ult, b_lt, bu_t, bul_)), (clop,List(_lop, c_op, cl_p, clo_)), ...
    val mangle = words.map(word => word -> pieces(word))

    // e.g., Map(ri_l -> Set(rial, rill), see_ -> Set(seen, seed, seer, seek, seel, seep, seem),
    val unmangle = new String2SetMap
    mangle.foreach(a => a._2.foreach(b => unmangle.put(b, (unmangle(b) + a._1))))

    // e.g., Map(pavy -> Set(paty, pave, davy, paly, navy, tavy, wavy, pavo, cavy),
    val lookup = new String2SetMap
    unmangle.values.foreach(set => set.foreach(word => lookup.put(word, lookup(word) ++ (set - word))))

    lookup.toMap // convert it to an immutable map
  }

  /* this is more idiomatic and easier to understand & maintain, but slow (takes >8000 ms)
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

  /**
   * Recursive algorithm guaranteed to find a shortest path, if there is one.
   * This is essentially Dijkstra's algorithm.
   */
  def solveAnyShortestBFS(start: String, target: String): Seq[String] = {
    import collection.mutable.Set
    val visited = Set[String]()
    visited += start
    @tailrec
    def recurse(paths: Seq[Seq[String]]): Seq[String] = {
      val nextPaths = for {
        path <- paths
        next <- lookup(path.head) if !visited.contains(next)
      } yield {
        visited += next
        next +: path
      }
      if (nextPaths.isEmpty) {
        Nil
      } else {
        nextPaths.find(p => p.head == target) match {
          case Some(path) => path
          case None => recurse(nextPaths)
        }
      }
    }
    require(lookup.contains(start), "%s is not a word".format(start))
    require(lookup.contains(target), "%s is not a word".format(target))
    recurse(List(List(start))).reverse
  }


  /**
   * Another variant of Dijkstra, using a queue instead of recursion.
   */
  def solveAnyShortestBFSAndrew(start: String, target: String): Seq[String] = {
    import collection.mutable.{Map, Queue}

    val previous = Map[String, Either[String, String]]()
    previous.put(start, Left(start)) // prevent backtracking to start

    def pathTo(tail: List[String]): List[String] = {
      previous(tail.head) match {
        case Left(_) => tail
        case Right(v) => pathTo(v :: tail)
      }
    }

    val q = new Queue[String]
    q.enqueue(start)

    while (!q.isEmpty) {
      val vertex = q.dequeue()

      if (vertex == target) {
        return pathTo(List(target))
      }

      for (neighbor <- lookup(vertex) if !previous.contains(neighbor)) {
        q.enqueue(neighbor)
        previous.put(neighbor, Right(vertex))
      }
    }

    Seq.empty

  }

  /**
   * A variation of Dijkstra's algorithm that will reconsider nodes, and as a result
   * find all shortest paths, but isn't guaranteed to terminate in a reasonable time
   * or with reasonable space requirements.
   */
  def solveAllShortestBFS(start: String, target: String): Seq[Seq[String]] = {
    @tailrec
    def recurse(paths: Seq[Seq[String]]): Seq[Seq[String]] = {
      val nextPaths = for {
        path <- paths
        next <- lookup(path.head) if !path.contains(next)
      } yield {
        next +: path
      }
      if (nextPaths.isEmpty) {
        Nil
      } else {
        val foundSolution = nextPaths.filter(_.head == target)
        if (foundSolution.isEmpty) {
          recurse(nextPaths)
        } else {
          foundSolution
        }
      }
    }
    require(lookup.contains(start), "%s is not a word".format(start))
    require(lookup.contains(target), "%s is not a word".format(target))
    recurse(List(List(start))).map(_.reverse)
  }


  /**
   * A depth-first search for any path, which has the interesting property
   * of tending to find really long paths.
   */
  def solveDFS(start: String, target: String): Seq[String] = {

    import scala.collection.JavaConverters._
    val visited = new java.util.HashSet[String]().asScala
    visited += start
    def recurse(path: Seq[String]): Option[Seq[String]] = {
      for {
        next <- lookup(path.head) if !visited.contains(next)
      } yield {
        visited += next
        if (next == target) {
          return Some(next +: path)
        } else {
          val solution = recurse(next +: path)
          if (solution.isDefined) {
            return solution
          }
        }
      }
      None
    }
    require(lookup.contains(start), "%s is not a word".format(start))
    require(lookup.contains(target), "%s is not a word".format(target))
    recurse(Seq(start)).map(_.reverse).getOrElse(Seq.empty)
  }

  // generates a nice string representation of a sequence of words
  def pretty(solution: Seq[String]): String = {
    if (solution.isEmpty) {
      "No solution found"
    } else {
      solution.mkString(" -> ")
    }
  }

  // generates a nice string representation of a sequence of words
  def prettys(solution: Seq[Seq[String]]): String = {
    if (solution.isEmpty) {
      "No solution found"
    } else {
      solution.map(pretty).mkString("\n")
    }
  }

}
