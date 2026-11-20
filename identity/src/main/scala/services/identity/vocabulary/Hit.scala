package services.identity.vocabulary

/** One film a lookup NAMED: a search result or a filmography credit. Only what the list itself
 *  carries — the film's own record is an `Film`. */
final case class Hit(tmdbId: Int, title: String, originalTitle: Option[String], year: Option[Int], popularity: Double)

object Hit {
  /** The hit standing for its film among `hits` naming it: the most popular, ties by title, original title and year. */
  def best(hits: Seq[Hit]): Option[Hit] = hits.minByOption(h => (-h.popularity, h.title, h.originalTitle.getOrElse(""), h.year.getOrElse(0)))
}
