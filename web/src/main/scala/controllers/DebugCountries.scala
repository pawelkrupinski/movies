package controllers

import java.util.Locale

import models.{CityScreening, Country, ResolvedMovie}
import play.api.mvc.{Cookie, RequestHeader}
import services.MirrorFreshness
import services.attempts.EnrichmentAttemptReader
import services.cadence.RatingCadenceReader
import services.movies.{MovieRepository, StoredMovieRecord}
import services.tasks.TaskQueue

import java.time.Instant

/**
 * Everything a `/debug` page reads, bound to ONE country's Mongo database — the
 * corpus (`movies`), the task queue, the
 * rating-cadence collection, and the read-model dump.
 *
 * Every one of those reads can be a SNAPSHOT: with `MONGODB_MOVIES_MIRROR_URI`
 * set they all resolve against the local mirror, which serves a page that looks
 * live whatever state the sync is in — hence [[MirrorFreshness]], which the
 * navbar renders so the page states its own age.
 *
 * The two whole-collection reads — the corpus listing and the read-model dump —
 * are functions returning a [[DebugSnapshot]], so the wiring decides per stack
 * whether one is read per request ([[DebugSnapshot.readNow]]: the boot country's
 * WARM in-memory [[services.readmodel.WebReadModel]], every spec) or served from a
 * [[RefreshingSnapshot]] (anything read off the mirror, where the read scales with
 * the corpus). The default listing reads per request.
 *
 * The read-model dump carries per-film screening COUNTS, not the screenings: the
 * US read model holds ~100k screening documents, and rendering every showtime
 * inline OOM'd the dev server (2026-10-03). A row's screenings are fetched on
 * expand through `readModelScreeningsFor` — `None` when that read failed, so a
 * failure is never shown as "no screenings".
 */
final class DebugStack(
  val country:               Country,
  val movieRepository:       MovieRepository,
  val taskQueue:             TaskQueue,
  val ratingCadenceReader:   RatingCadenceReader,
  val attemptReader:         EnrichmentAttemptReader,
  val readModel:              () => DebugSnapshot[ReadModelDump],
  val readModelScreeningsFor: String => Option[Seq[CityScreening]],
  // How far behind the local read-mirror this stack reads through is. Defaults
  // to "not mirrored" — prod, and every test that wires a stack by hand, read
  // their data straight from the source and so have no copy that could be stale.
  val mirrorFreshness:        MirrorFreshness = MirrorFreshness.notMirrored,
  corpusListing:              Option[() => DebugSnapshot[CorpusListing]] = None,
  ratingCadenceSnapshot:      Option[() => DebugSnapshot[Seq[(String, services.cadence.RatingChangeStats)]]] = None,
) {
  /** The `movies` corpus without showtimes — the `/debug` table, and the titles
   *  `/debug/cadence` names its films by. */
  val listing: () => DebugSnapshot[CorpusListing] =
    corpusListing.getOrElse(DebugSnapshot.readNow(mirrorFreshness)(CorpusListing.read(movieRepository)))

  /** Every `rating_cadence` record — `/debug/cadence`. */
  val ratingCadence: () => DebugSnapshot[Seq[(String, services.cadence.RatingChangeStats)]] =
    ratingCadenceSnapshot.getOrElse(DebugSnapshot.readNow(mirrorFreshness)(ratingCadenceReader.all()))
}

/** The `/debug` corpus table's rows, RENDERED: a row is a pure function of its record,
 *  and rendering ~2k of them (every derived `MovieRecord` field per row) was most of
 *  a `/debug` load once the read itself was snapshotted — so a snapshot renders them
 *  once, off the request path. */
final case class DebugCorpusTable(size: Int, rows: String)

object DebugCorpusTable {
  def of(records: Seq[StoredMovieRecord], normalizer: services.movies.TitleNormalizer)(implicit city: models.City): DebugCorpusTable =
    // Flattened to ONE string: a `fill`ed `Html` is a tree of thousands of fragments that
    // every page render walks again to rebuild the same text.
    DebugCorpusTable(records.size,
      play.twirl.api.HtmlFormat.fill(records.sortBy(_.title.toLowerCase(Locale.ROOT)).map(views.html._debugRow(_, normalizer))).body)
}

/** One read of the corpus, in the two shapes the debug pages use it. */
final case class CorpusListing(table: DebugCorpusTable, titleByTmdb: Map[Int, String])

object CorpusListing {
  def read(movieRepository: MovieRepository): CorpusListing = {
    val records = movieRepository.findAllForListing()
    // The table is the global corpus; its only use for a city is the /movie fallback
    // link on a row with no live showtimes anywhere — the default city, as before.
    CorpusListing(DebugCorpusTable.of(records, movieRepository.normalizer)(using models.City.all.head),
      records.flatMap(r => r.record.tmdbId.map(_ -> r.title)).toMap)
  }
}

/** One film's screening documents in the read model, summarised for its row. */
final case class ScreeningCounts(docs: Int, cinemas: Int, showtimes: Int)

/** The read model as `/debug/readmodel` lists it: every `web_movies` doc sorted by
 *  title, and per film how many `web_screenings` docs, cinemas and showtimes it has. */
final case class ReadModelDump(movies: Seq[ResolvedMovie], counts: Map[String, ScreeningCounts], lastModified: Instant) {
  def screeningCount: Int = counts.valuesIterator.map(_.docs).sum
  def showtimeCount:  Int = counts.valuesIterator.map(_.showtimes).sum
}

object ReadModelDump {
  val empty: ReadModelDump = ReadModelDump(Seq.empty, Map.empty, Instant.EPOCH)

  /** The WARM in-memory model the app actually serves from. */
  def of(model: services.readmodel.WebReadModel): ReadModelDump =
    of(model.allMovies(), model.allScreenings().foreach, model.lastModified)

  /** One film's screening docs in the warm model. */
  def screeningsOf(model: services.readmodel.WebReadModel)(filmId: String): Option[Seq[CityScreening]] =
    Some(model.allScreenings().filter(_.filmId == filmId))

  /** Count while streaming, so a caller paging `web_screenings` never holds them all. */
  def of(movies: Seq[ResolvedMovie], foreachScreening: (CityScreening => Unit) => Unit, lastModified: Instant): ReadModelDump = {
    val docs      = scala.collection.mutable.HashMap.empty[String, Int]
    val cinemas   = scala.collection.mutable.HashMap.empty[String, Set[String]]
    val showtimes = scala.collection.mutable.HashMap.empty[String, Int]
    foreachScreening { s =>
      docs.updateWith(s.filmId)(n => Some(n.getOrElse(0) + 1))
      cinemas.updateWith(s.filmId)(c => Some(c.getOrElse(Set.empty) + s.cinema))
      showtimes.updateWith(s.filmId)(n => Some(n.getOrElse(0) + s.showtimes.size))
    }
    val counts = docs.iterator.map { case (film, n) => film -> ScreeningCounts(n, cinemas(film).size, showtimes(film)) }.toMap
    ReadModelDump(movies.sortBy(_.title.toLowerCase(Locale.ROOT)), counts, lastModified)
  }
}

/**
 * Which country's data the `/debug` pages read, per request.
 *
 * In prod (and every controller test) there is a single stack — the
 * deployment's own country — and the switch is off: a `?country=` param is
 * ignored. Locally in Dev the wiring builds one stack per switchable country so
 * the debug pages can hop between countries' corpora SAME-ORIGIN via
 * `?country=xx`, instead of the navbar navigating to the other country's
 * production deployment (which serves prod-mode and 404s every `/debug` route).
 *
 * Resolution order in Dev: the `?country=` query param wins (and is stamped into
 * a sticky cookie so the plain `<a>` tab links keep the selection), then the
 * cookie, then the boot country. An unknown or not-wired code falls back to the
 * boot country.
 */
final class DebugCountries private (
  bootCountry: Country,
  stacks:      Map[Country, DebugStack],
  devMode:     Boolean,
) {
  /** Whether the Dev-only same-origin switch is live: Dev mode AND more than one
   *  country wired. The debug navbar renders the `?country=` switcher only then;
   *  otherwise it keeps the prod cross-deployment links. */
  val switchable: Boolean = devMode && stacks.sizeIs > 1

  /** The country this request's `/debug` view should read. Always the boot
   *  country when the switch is off. */
  def resolve(request: RequestHeader): Country =
    if (!switchable) bootCountry
    else queryCountry(request).orElse(cookieCountry(request)).getOrElse(bootCountry)

  def stackFor(country: Country): DebugStack = stacks.getOrElse(country, stacks(bootCountry))

  /** When the request carried an explicit `?country=`, the cookie to persist that
   *  selection across the plain tab links; `None` otherwise (nothing to stamp). */
  def selectionCookie(request: RequestHeader): Option[Cookie] =
    if (!switchable) None
    else queryCountry(request).map(c => Cookie(DebugCountries.CookieName, c.code, httpOnly = false))

  private def queryCountry(request: RequestHeader): Option[Country] =
    request.getQueryString("country").flatMap(Country.byCode).filter(stacks.contains)

  private def cookieCountry(request: RequestHeader): Option[Country] =
    request.cookies.get(DebugCountries.CookieName).flatMap(c => Country.byCode(c.value)).filter(stacks.contains)
}

object DebugCountries {
  /** Client-readable cookie (not httpOnly) that persists the switched country
   *  across the debug navbar's plain tab links + the SSE stream request. */
  val CookieName = "debugCountry"

  /** The single-country holder — prod and every controller test. One stack, no
   *  switching: a `?country=` param is ignored. */
  def single(bootStack: DebugStack): DebugCountries =
    new DebugCountries(bootStack.country, Map(bootStack.country -> bootStack), devMode = false)

  /** The boot stack plus any Dev-only extra per-country stacks. `devMode` gates
   *  the switch; when false only the boot stack is ever selected. */
  def of(bootStack: DebugStack, extras: Map[Country, DebugStack], devMode: Boolean): DebugCountries =
    new DebugCountries(bootStack.country, extras + (bootStack.country -> bootStack), devMode)
}
