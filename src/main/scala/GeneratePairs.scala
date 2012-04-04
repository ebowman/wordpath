import java.io.{PrintWriter, FileWriter}

/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 4/4/12 10:54 AM
 */

object GeneratePairs extends App {
  val words = io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).map(_.toLowerCase).toSet.toArray

  val writer = new PrintWriter(new FileWriter("pairs.txt"))
  for (i <- 1 to 100000) {
    writer.println("%s\t%s".format(words(util.Random.nextInt(words.size)), words(util.Random.nextInt(words.size))))
  }
  writer.close()

}
