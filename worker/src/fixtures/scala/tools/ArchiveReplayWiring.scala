package tools

import clients.TmdbClient
import models.{Cinema, CinemaMovie, Country}
import modules.WorkerWiring
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.movies.{InMemoryMovieRepository, InMemoryScreeningsRepository, InMemorySlotsRepository}
import services.readmodel.{InMemoryReadModelRepository, ReadModelReader, ReadModelWriter}
import services.scrapes.ScrapeArchiveRepository

/**
 * Replays a country's pipeline from ARCHIVED scrape data rather than from
 * recorded HTTP — the `cinema_scrapes` collection is read back and each row
 * becomes a `PreScrapedCinemaScraper`, so the corpus enters through exactly the
 * seam a chunked scrape's reduced result already uses.
 *
 * This is the wiring the per-country convergence specs run on, and it is the
 * first consumer of the archive: a country whose HTTP fixtures don't exist
 * (every country but Poland) can still be driven end to end.
 *
 * `WorkerWiring` takes its country as a constructor argument and `TestWiring` is
 * a trait, so a trait alone could never supply one — hence a class that extends
 * the composition root directly and mixes the test seams in on top. That is the
 * whole reason this type exists rather than a `FixtureTestWiring` subclass.
 */
class ArchiveReplayWiring(
  country:  Country,
  archive:  ScrapeArchiveRepository
) extends WorkerWiring(country) with TestWiring {

  /** No network at all. Every cinema listing comes from the archive, and any
   *  enrichment call (TMDB and friends) is a deliberate miss: this spec asserts
   *  that the SCRAPE→fold→settle loop reaches a fixpoint, and a live or recorded
   *  metadata lookup would make the answer depend on data the country may not
   *  have. Failing loudly beats silently reaching the real network. */
  override lazy val httoFetch: HttpFetch = OfflineHttpFetch
  override lazy val enrichmentFetch: HttpFetch = httoFetch
  override lazy val multikinoFetch: HttpFetch  = httoFetch
  override lazy val biletynaFetch: HttpFetch   = httoFetch
  override lazy val zyteFetch: HttpFetch       = httoFetch
  override lazy val flicksFetch: HttpFetch     = httoFetch

  // No API key either, so the TMDB client short-circuits rather than shaping a
  // request it can never send.
  override lazy val tmdbClient: TmdbClient = new TmdbClient(enrichmentFetch, apiKey = None)

  // Production's storage shape, minus Mongo — showtimes in `screenings`, slots in
  // `movie_slots`, neither inlined on the `movies` row. Same reasoning as
  // `FixtureTestWiring`: a merge is a rename, and a fake that inlines everything
  // carries showtimes across a rename for free and so can't express the bug.
  override lazy val screeningsRepository = new InMemoryScreeningsRepository
  override lazy val slotsRepository      = new InMemorySlotsRepository
  override lazy val movieRepository =
    new InMemoryMovieRepository(screenings = Some(screeningsRepository), slots = Some(slotsRepository))
  override lazy val readModelRepository: ReadModelReader & ReadModelWriter = new InMemoryReadModelRepository()

  /** The archived listing per cinema, read ONCE. Every tick re-serves this same
   *  `Seq[CinemaMovie]`, which is precisely the "identical re-scrape" the
   *  convergence assertion needs — a prod tick that finds nothing changed. */
  lazy val archivedListings: Map[Cinema, Seq[CinemaMovie]] =
    archive.findAll()
      .filter(row => countryCinemas.contains(row.cinema))
      .map(row => row.cinema -> row.films)
      .filter(_._2.nonEmpty)
      .toMap

  private lazy val countryCinemas: Set[Cinema] = CountryScrapeCorpus.cinemasOf(country).toSet

  /** One scraper per archived cinema, replacing the catalogue's HTTP clients.
   *  `PreScrapedCinemaScraper` is production code — the same wrapper
   *  `ScrapeChunkReduceHandler` uses to push a reduced chunk set back through the
   *  runner — so the corpus enters the pipeline exactly where a real one does. */
  override lazy val cinemaScrapers: Seq[CinemaScraper] =
    archivedListings.toSeq
      .sortBy(_._1.displayName)
      .map { case (cinema, films) =>
        new PreScrapedCinemaScraper(cinema, Set.empty, isChain = false, () => films, listingComplete = true)
      }
}

/** An `HttpFetch` that refuses every call. Used where a spec must prove it is
 *  hermetic: any client that slips through announces itself as a failure naming
 *  the URL, rather than quietly reaching the network. */
object OfflineHttpFetch extends HttpFetch {
  private def refuse(url: String): Nothing =
    throw new UnsupportedOperationException(s"offline replay: no HTTP allowed, but something requested $url")

  override def get(url: String): String            = refuse(url)
  override def getBytes(url: String): Array[Byte]  = refuse(url)
  override def post(url: String, body: String, contentType: String): String = refuse(url)
  override def getAsync(url: String): java.util.concurrent.CompletableFuture[String] = {
    val failed = new java.util.concurrent.CompletableFuture[String]()
    failed.completeExceptionally(new UnsupportedOperationException(s"offline replay: no HTTP allowed, but something requested $url"))
    failed
  }
}
