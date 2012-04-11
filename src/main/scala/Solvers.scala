import annotation.tailrec

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 4/11/12 9:48 AM
 */

object Solvers {

  /**
   * Recursive algorithm guaranteed to find a shortest path, if there is one.
   * This is essentially Dijkstra's algorithm.
   */
  def solveAnyShortestBFS(start: String, target: String)(implicit lookup: Map[String, Set[String]]): Seq[String] = {
    val visited = new java.util.HashSet[String]()
    visited.add(start)
    @tailrec
    def recurse(paths: Seq[Seq[String]]): Seq[String] = {
      val nextPaths = for {
        path <- paths
        next <- lookup(path.head) if !visited.contains(next)
      } yield {
        visited.add(next)
        next +: path
      }
      if (nextPaths.isEmpty) {
        Nil
      } else {
        nextPaths.find(_.head == target) match {
          case Some(path) => path
          case None => recurse(nextPaths)
        }
      }
    }
    recurse(List(List(start))).reverse
  }

  /**
   * Another variant of Dijkstra, using a queue instead of recursion.
   */
  def solveAnyShortestBFSAndrew(start: String, target: String)(implicit lookup: Map[String, Set[String]]): Seq[String] = {
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
  def solveAllShortestBFS(start: String, target: String)(implicit lookup: Map[String, Set[String]]): Seq[Seq[String]] = {
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
  def solveDFS(start: String, target: String)(implicit lookup: Map[String, Set[String]]): Seq[String] = {

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

}
