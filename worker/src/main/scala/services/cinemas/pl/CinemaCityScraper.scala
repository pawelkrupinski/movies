package services.cinemas.pl

import models._
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, FilmDetail}

import java.time.LocalDate

/**
 * Adapter for `CinemaCityClient`, whose `fetch(cinemaId, cinema)` serves
 * multiple cinema variants. Each `CinemaCityScraper` instance captures one
 * (cinemaId, cinema) pair.
 *
 * It is the per-venue `DetailEnricher` for the *listing* side (this venue's
 * showtimes + `filmUrl`), but the per-film detail is shared chain-wide: every
 * venue uses one `"cinema-city"` `detailGroup` (so a film is fetched + parsed
 * once per network per freshness window — the queue's unique index + freshness
 * drop the sibling venues' duplicate schedules) and writes the result into a
 * single `CinemaCityChain` network source rather than each venue's own slot.
 * `MovieRecord`'s merged accessors are film-level, so one shared slot surfaces
 * the synopsis/cast/genres at every venue. Enrichment health reports once as
 * "Cinema City Enrichment" instead of one row per venue.
 */
class CinemaCityScraper(
  client:   CinemaCityClient,
  cinemaId: String,
  val cinema: Cinema
) extends ChunkedCinemaScraper with DetailEnricher {
  // Chunked per-WEEK. The quickbook API has no range/multi-cinema endpoint (only
  // per-date `/at-date`), so the 30–60 per-date calls are irreducible — but they
  // don't each need their own task. We group the screening dates into runs of
  // `DaysPerChunk` (one chunk key = a comma-joined run of dates); each ScrapeChunk
  // fetches that run's days and they're merged by Cinema City film id. This keeps
  // the fan-out's slot-relief + run-id coordination while cutting the chunk-task +
  // `scrape_chunks` write count ~7× versus per-date (those tiny coordination
  // writes — per venue, per refresh — were themselves Mongo write load). The
  // chain-shared deferred detail path (detailGroup "cinema-city") is orthogonal.
  def planChunks(): Seq[String] =
    client.dates(cinemaId).map(_.toString).grouped(CinemaCityScraper.DaysPerChunk).map(_.mkString(",")).toSeq
  // A throw on any day reschedules the whole run (run-level retry); fixtures
  // resolve every day, so the synchronous `fetch()` output is unchanged.
  def fetchChunk(key: String): Seq[CinemaMovie] =
    key.split(",").toSeq.flatMap(d => client.fetchDay(cinemaId, cinema, LocalDate.parse(d)))
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    client.reduce(chunks.values.flatten.toSeq)
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(CinemaCityClient.BaseApiUrl)
  // The numeric `cinemaId` is Cinema City's externalCode (e.g. 1078 = Poznań
  // Plaza); the public venue page is `/kina/<slug>/<id>` where the id is
  // load-bearing and the slug cosmetic, so any slug + the right id resolves.
  override def sourceUrl: Option[String] = Some(s"https://www.cinema-city.pl/kina/cinema-city/$cinemaId")
  override val detailGroup: String = "cinema-city"
  override def detailTarget: Source = CinemaCityChain
  override def enrichmentServiceOverride: Option[String] = Some("Cinema City Enrichment")
  override def fetchFilmDetail(ref: String): Option[FilmDetail] = client.fetchFilmDetail(ref)
  override def chain: Boolean = true
}

object CinemaCityScraper {
  /** How many consecutive screening-dates one chunk task covers. 7 ≈ a week:
   *  enough to cut the chunk/write count ~7× vs per-date, small enough that a
   *  chunk's slot time and its retry blast-radius stay bounded. */
  val DaysPerChunk: Int = 7
}
