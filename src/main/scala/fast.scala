import akka.dispatch.{ExecutionContext, Await, Future}
import akka.util.Duration
import annotation.tailrec
import collection.mutable.HashMap
import java.io.{FileWriter, BufferedWriter, PrintWriter}
import java.util.ArrayList
import java.util.concurrent.{Executors, LinkedBlockingQueue}

object Fast extends App {
  val words = io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).map(_.toLowerCase).toSet
  val lookup: Map[String, Set[String]] = {
    class String2SetMap extends HashMap[String, Set[String]] {
      override def default(key: String) = Set()
    }

    val mangle = words.map(word => word -> pieces(word))
    val unmangle = new String2SetMap
    mangle.foreach(a => a._2.foreach(b => unmangle.put(b, (unmangle(b) + a._1))))
    val lookup = new String2SetMap
    unmangle.values.foreach(set => set.foreach(word => lookup.put(word, lookup(word) ++ (set - word))))
    lookup.toMap
  }

  val queue = new LinkedBlockingQueue[String]
  val pairs = Vector().par ++ io.Source.fromFile(args(0)).getLines().map(_.split("\t"))
  pairs.map(pair => solveAnyShortestBFS(pair(0), pair(1))).seq.foreach {
    case (result: Seq[_]) =>
      queue.put(result.mkString(" -> "))
  }
  queue.put("done")
  val executor = Executors.newSingleThreadExecutor
  implicit val executionContext = ExecutionContext.fromExecutor(executor)
  val future = Future {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter("results.txt")))
    val buffer = new ArrayList[AnyRef](1024)
    var done = false
    while (!done) {
      buffer.clear()
      queue.drainTo(buffer)
      import collection.JavaConverters._
      buffer.asScala.foreach {
        line =>
          if (line == "done") {
            writer.close()
            done = true
          } else {
            writer.println(line)
          }
      }
    }
  }
  Await.result(future, Duration.Inf)
  executor.shutdown()

  def solveAnyShortestBFS(start: String, target: String): Seq[String] = {
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

  def diff(a: String, b: String): Int = (0 until a.length).map(i => if (a(i) == b(i)) 0 else 1).sum

  def pieces(word: String) =
    Seq("_" + word.drop(1), word.take(1) + "_" + word.drop(2), word.take(2) + "_" + word.drop(3), word.take(3) + "_")
}
