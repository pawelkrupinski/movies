package tools

import play.api.libs.json._
import services.identity.TakeJudgements

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
 *
 * `modelTook` says which stage made the row's TMDB take: `Some(true)` when prod's identity model itself named that film
 * at one of the row's listings (its stored per-listing decision, `identity_traces`), `Some(false)` when none did — the
 * take a stage after the model made (the agreement's agreed, poster-vote, broadcast, fill or catalogue take, a
 * correction's), which a hermetic leg never replays ([[ProdFilms.takenPastTheModel]]). `None` where it was not recorded:
 * a row with no TMDB id, one none of whose listings the model holds, or a recording from before the field.
 */
final case class ProdFilm(id: String, key: String, tmdbId: Option[Int], tmdbBasis: Option[String], imdbId: Option[String],
                          covered: Set[String], slots: Seq[String], modelTook: Option[Boolean] = None)

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

  /**
   * Production's films whose TMDB take a stage AFTER its identity model made ([[ProdFilm.modelTook]] false): the
   * agreement's takes over the families' answers, its poster votes and fills, a correction's. A hermetic leg replays
   * none of what those stages read (its agreement takes nothing a family must agree on, [[FiledAnswers]]), so the band
   * compares like with like only without them — on BOTH sides ([[ProdCoverageBaseline.without]], [[runSideOf]]).
   * Poland's prod held 19 such films the morning after recording 43654865568, nearly all of them films that recording's
   * replay took nothing for: most of the 5.4% its band read off (Verify scrape recording #31). A recording from before
   * the field withdraws none.
   */
  def takenPastTheModel(prod: Seq[ProdFilm]): Seq[ProdFilm] = prod.filter(film => film.tmdbId.isDefined && film.modelTook.contains(false))

  /** The ids of `run`'s films that are `withdrawn`'s on the replay's side: those every listing of which bills a slot of
   *  a withdrawn production film — a split's every part with it. A run film billing any other slot too stays counted, as
   *  the other film it holds is no withdrawn one's. */
  def runSideOf(withdrawn: Seq[ProdFilm], run: Seq[RunFilm]): Set[String] = {
    val slots = withdrawn.flatMap(_.slots).toSet
    run.filter(film => film.listings.nonEmpty && film.listings.forall(listing => slots(listing.slotKey))).map(_.id).toSet
  }

  /** Production's films whose TMDB take a hand judgement calls wrong at EVERY listing of `run` sharing their slots:
   *  the takes the projection withdraws (`TakeJudgements.withdraw`) that production served still — its worker reads
   *  the judgements it was last deployed with, the replay main's. A film some listing still takes keeps its
   *  coverage, as the withdrawal leaves that listing's take. */
  def judgedWrong(prod: Seq[ProdFilm], run: Seq[RunFilm], judgements: TakeJudgements.Index): Seq[ProdFilm] = {
    val listingsBySlot = run.flatMap(_.listings).groupBy(_.slotKey)
    prod.filter(film => film.tmdbId.exists { id =>
      val listings = film.slots.flatMap(listingsBySlot.getOrElse(_, Nil))
      listings.nonEmpty && listings.forall(listing => judgements.denied(listing.venue, listing.raw)(id))
    })
  }
}
