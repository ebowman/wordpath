/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 4/11/12 5:00 PM
 */

object Checker extends App {

  val lookup = Dictionary.lookup
  io.Source.fromFile(args(0)).getLines().foreach { line =>
    val path = line.split(" -> ")
    require(path.zip(path.tail).forall(item => lookup(item._1).contains(item._2)), line + " did not verify.")
  }
  println("%s checks out ok." format args(0))
}
