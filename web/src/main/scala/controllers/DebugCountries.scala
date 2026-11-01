package controllers

import java.util.Locale

import models.Country
import play.api.mvc.{Cookie, RequestHeader}
import services.MirrorFreshness
import services.attempts.EnrichmentAttemptReader
import services.cadence.RatingCadenceReader
import services.movies.{MovieRepository, StoredMovieRecord}
import services.tasks.TaskQueue


/**
 * Everything a `/debug` page reads, bound to ONE country's Mongo database — the
 * corpus (`movies`), the task queue and the rating-cadence collection. The
 * read-model dump is [[DebugReadModels]]'.
 *
 * Every one of those reads can be a SNAPSHOT: with `MONGODB_MOVIES_MIRROR_URI`
 * set they all resolve against the local mirror, which serves a page that looks
 * live whatever state the sync is in — hence [[MirrorFreshness]], which the
 * navbar renders so the page states its own age.
 *
 * The whole-collection reads — the corpus listing and the cadence records — are
 * functions returning a [[DebugSnapshot]], so the wiring decides per stack whether
 * one is read per request ([[DebugSnapshot.readNow]]: every spec) or served from a
 * [[RefreshingSnapshot]] (anything read off the mirror, where the read scales with
 * the corpus). The default listing reads per request.
 */
final class DebugStack(
  val country:               Country,
  val movieRepository:       MovieRepository,
  val taskQueue:             TaskQueue,
  val ratingCadenceReader:   RatingCadenceReader,
  val attemptReader:         EnrichmentAttemptReader,
  // How far behind the local read-mirror this stack reads through is. Defaults
  // to "not mirrored" — prod, and every test that wires a stack by hand, read
  // their data straight from the source and so have no copy that could be stale.
  val mirrorFreshness:        MirrorFreshness = MirrorFreshness.notMirrored,
  corpusListing:              Option[() => DebugSnapshot[CorpusListing]] = None,
  ratingCadenceSnapshot:      Option[() => DebugSnapshot[Seq[(String, services.cadence.RatingChangeStats)]]] = None,
)(using services.movies.LatestTitleYear) {
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
  def of(records: Seq[StoredMovieRecord], normalizer: services.movies.TitleNormalizer)(implicit city: models.City, latestYear: services.movies.LatestTitleYear): DebugCorpusTable =
    // Flattened to ONE string: a `fill`ed `Html` is a tree of thousands of fragments that
    // every page render walks again to rebuild the same text.
    DebugCorpusTable(records.size,
      play.twirl.api.HtmlFormat.fill(records.sortBy(_.title.toLowerCase(Locale.ROOT)).map(views.html._debugRow(_, normalizer, latestYear))).body)
}

/** One read of the corpus, in the two shapes the debug pages use it. */
final case class CorpusListing(table: DebugCorpusTable, titleByTmdb: Map[Int, String])

object CorpusListing {
  def read(movieRepository: MovieRepository)(using latestYear: services.movies.LatestTitleYear): CorpusListing = {
    val records = movieRepository.findAllForListing()
    // The table is the global corpus; its only use for a city is the /movie fallback
    // link on a row with no live showtimes anywhere — the default city, as before.
    CorpusListing(DebugCorpusTable.of(records, movieRepository.normalizer)(using models.City.all.head, latestYear),
      records.flatMap(r => r.record.tmdbId.map(_ -> r.title)).toMap)
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
