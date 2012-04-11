import akka.dispatch.{ExecutionContext, Await, Future}
import akka.util.Duration
import java.io.{FileWriter, BufferedWriter, PrintWriter}
import java.util.ArrayList
import java.util.concurrent.{Executors, LinkedBlockingQueue}

object Fast extends App {
  implicit val lookup = Dictionary.lookup

  val solver = Solvers.solveAnyShortestBFSAndrew _

  val queue = new LinkedBlockingQueue[String]
  val pairs = Vector().par ++ io.Source.fromFile(args(0)).getLines().map(_.split("\t"))
  pairs.map(pair => solver(pair(0), pair(1))).seq.foreach {
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
}
