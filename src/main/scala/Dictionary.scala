
/**
 * Document Me.
 *
 * @author Eric Bowman
 * @since 4/11/12 9:46 AM
 */

object Dictionary {
  val lookup: Map[String, Set[String]] = {
    import collection.mutable.HashMap
    def pieces(word: String) =
      Seq("_" + word.drop(1), word.take(1) + "_" + word.drop(2), word.take(2) + "_" + word.drop(3), word.take(3) + "_")
    val words = io.Source.fromFile("/usr/share/dict/words").getLines().filter(_.length == 4).map(_.toLowerCase).toSet
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

}
