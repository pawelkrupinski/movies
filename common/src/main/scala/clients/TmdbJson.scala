package clients

import play.api.libs.json.JsValue

import scala.util.Try

/** How a TMDB response's fields read, wherever they are read — the client's parsers and the identity
 *  model's film record alike, so the two cannot drift on what a field means. */
object TmdbJson {

  /** The year of a TMDB `release_date` ("2019-05-01"), or `None` when it is missing, short or not a
   *  number — one malformed date leaves its film's year unknown, never fails the response. */
  def releaseYear(date: Option[String]): Option[Int] =
    date.filter(_.length >= 4).flatMap(d => Try(d.take(4).toInt).toOption)

  def releaseYear(film: JsValue): Option[Int] = releaseYear((film \ "release_date").asOpt[String])

  /** The crew holding one of `jobs`: each caller names its own, since a co-director counts for some. */
  def crewWith(crew: Seq[JsValue], jobs: Set[String]): Seq[JsValue] =
    crew.filter(c => (c \ "job").asOpt[String].exists(jobs.contains))

  /** Their names, in TMDB's order, each once. */
  def crewNames(crew: Seq[JsValue], jobs: Set[String]): Seq[String] =
    crewWith(crew, jobs).flatMap(c => (c \ "name").asOpt[String]).distinct

  val Director: Set[String] = Set("Director")
}
