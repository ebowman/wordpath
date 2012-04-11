/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 4/11/12 9:57 AM
 */

object Utils {
  def time[T](label: String)(f: => T): T = {
    val start = System.currentTimeMillis
    val result = f
    val end = System.currentTimeMillis
    println("%s took %s ms".format(label, end - start))
    result
  }

  def diff(a: String, b: String): Int = (0 until a.length).map(i => if (a(i) == b(i)) 0 else 1).sum

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
