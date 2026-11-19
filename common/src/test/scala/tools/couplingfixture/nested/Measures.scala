package tools.couplingfixture.nested

// CouplingSpec's fixture: an object that is a collaborator (it memoises: state) holding a nested data type with a
// cached reading (a lazy val of its own fields). A class using only the nested type uses data; one calling the
// object's own function uses the collaborator.

object Measures {
  final case class Film(title: String) { lazy val key: String = title.trim.toLowerCase(java.util.Locale.ROOT) }

  private val memo = scala.collection.mutable.Map.empty[String, String]
  def relation(a: Film, b: Film): String = memo.getOrElseUpdate(a.key + "|" + b.key, if (a.key == b.key) "same" else "apart")
}

/** Uses only the nested data type. */
final class FilmCount { def count(films: Seq[Measures.Film]): Int = films.map(_.key).distinct.size }

/** Calls the object's own function. */
final class Relating { def same(a: Measures.Film, b: Measures.Film): Boolean = Measures.relation(a, b) == "same" }
