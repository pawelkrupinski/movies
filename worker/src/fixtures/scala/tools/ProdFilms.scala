package tools

import play.api.libs.json._

import java.nio.file.{Files, Path, Paths}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.util.Using

/**
 * One film production was SCREENING when a corpus was recorded — what [[ProdCoverageBaseline]] counts,
 * kept per film so a leg that drifts from the band can say WHICH films it drifted on.
 *
 * `key` is the row's stored lookup key (`sanitize(title)|year`), the nearest thing to a title a
 * `movies` document carries. `covered` names the coverage axes the row holds a value on, under the
 * names [[ProdCoverageBaseline.metrics]] uses. `slots` are the row's screening slot keys
 * (`"<cinema>␟<titleKey>"`, `CinemaShowing.keyFor`): the one identity a replayed film and a
 * production film share, so the per-film diff ([[FilmCoverageDiff]]) joins on them.
 */
final case class ProdFilm(id: String, key: String, tmdbId: Option[Int], tmdbBasis: Option[String], imdbId: Option[String],
                          covered: Set[String], slots: Seq[String])

object ProdFilm {
  implicit val format: OFormat[ProdFilm] = Json.format[ProdFilm]
}

/** Production's screening films, recorded beside the corpus and its [[ProdCoverageBaseline]] (`RecordCorpusFixture`)
 *  and shipped in the same archive. Optional for a reader: a corpus recorded before this file existed has none, and
 *  the per-film diff says so rather than failing a leg over a missing diagnostic. */
object ProdFilms {

  /** The coverage axes a `movies` row holds a value on, by the field names [[ProdCoverageBaseline.metrics]] uses —
   *  every metric but `films`, which is the row itself. */
  val Axes: Seq[String] = Seq("tmdbId", "imdbId", "imdbRating", "filmwebRating", "metascore", "rottenTomatoes")

  def pathFor(code: String): Path = Paths.get(s"test/resources/fixtures/corpus/prod-films-$code.json.gz")

  def write(code: String, films: Seq[ProdFilm]): Path = {
    val path = pathFor(code)
    Files.createDirectories(path.getParent)
    Using.resource(new GZIPOutputStream(Files.newOutputStream(path)))(_.write(Json.toBytes(Json.toJson(films))))
    path
  }

  def read(code: String): Option[Seq[ProdFilm]] =
    Option.when(Files.exists(pathFor(code)))(readFrom(pathFor(code)))

  /** The films a recording laid out elsewhere holds (its `corpus/prod-films-<cc>.json.gz`). */
  def readFrom(path: Path): Seq[ProdFilm] =
    Using.resource(new GZIPInputStream(Files.newInputStream(path)))(in => Json.parse(in).as[Seq[ProdFilm]])
}
