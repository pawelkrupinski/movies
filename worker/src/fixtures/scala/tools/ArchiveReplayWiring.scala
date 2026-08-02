package tools

import clients.TmdbClient
import models.{Cinema, CinemaMovie, Country}
import modules.WorkerWiring
import services.cinemas.common.{CinemaScraper, PreScrapedCinemaScraper}
import services.readmodel.{ReadModelReader, ReadModelWriter}
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
  country:          Country,
  archive:          ScrapeArchiveRepository,
  enrichmentCache:  Option[EnrichmentCache],
  // REQUIRED, and always a real database. There is no in-memory storage to default
  // to any more: a fixpoint proved over a map says nothing about codecs, paged scans
  // or a transactional staging fold, and every one of those has shipped a bug here.
  storage:          ConvergenceStorage
) extends WorkerWiring(country) with TestWiring {

  /** The tree both chains replay from and record into — named by the environment when a
   *  run wants a particular one, and by the country otherwise. Read ONCE so the two
   *  chains, the cache that lives beside it and the freshness prune can never be looking
   *  at different directories.
   *
   *  LAZY, and it has to be: `WorkerWiring`'s own initialisation forces `httoFetch`, which
   *  is this subclass's override, which reads this field. As a strict `val` it is still
   *  `null` at that point and both chains silently bind to
   *  `test/resources/fixtures/null` — a tree that exists, records, and replays, so the
   *  next run answers from it and never reaches the live leg at all. */
  private lazy val fixtureDirectory: String = ArchiveReplayWiring.fixtureDirectory(country)

  /**
   * The scrape side: recorded DETAIL pages first, live behind them, and whatever the
   * live leg fetched written back into the same tree.
   *
   * It used to be a fetch that refused every call, on the grounds that every listing comes
   * from the archive so a scraper reaching for HTTP is a bug. The listing half of that
   * still holds and is enforced by construction: `cinemaScrapers` is replaced with
   * `PreScrapedCinemaScraper`, so no scraper's fetch is ever called. What the catalogue
   * DOES do here is per-film detail — 25 Polish cinema clients implement `DetailEnricher`
   * — and refusing that was quietly costing the suite its enrichment.
   *
   * Polish listings carry a title and showtimes and nothing else; the year, director,
   * synopsis and poster live on a per-film detail page, and `StagingSteps.detailReady`
   * gates TMDB resolution on that fetch precisely because it supplies the hints
   * resolution needs. With the fetch refused, rows reached TMDB yearless, fell to the
   * tier that demands an exact title match, and `Amadeusz` never matched `Amadeus`:
   * Poland resolved 36% against prod's 78%, while Germany — whose listings are
   * self-sufficient and whose clients implement no enricher at all — sat at 96% on the
   * same code.
   *
   * Same tree as the enrichment fixtures, because cinema hosts cannot collide with
   * TMDB/IMDb/RT ones and one artifact is easier to keep honest than two. `strict = true`
   * for the same reason it is there: a synthesised empty answer would be
   * indistinguishable from a real one and the live leg would never be reached.
   */
  override lazy val httoFetch: HttpFetch =
    new FallbackHttpFetch(Seq(
      "detail-fixtures" -> new clients.tools.FakeHttpFetch(fixtureDirectory, strict = true, foldYear = false),
      "detail-live"     -> new clients.tools.RecordingHttpFetch(
        fixtureDirectory, phaseFetch(services.metrics.WorkerHttpMetrics.Phase.Scrape), foldYear = false)))
  override lazy val multikinoFetch: HttpFetch  = httoFetch
  override lazy val biletynaFetch: HttpFetch   = httoFetch
  override lazy val zyteFetch: HttpFetch       = httoFetch
  override lazy val flicksFetch: HttpFetch     = httoFetch

  /** Enrichment runs against the per-country cache when one is supplied.
   *
   *  Live enrichment used to be refused here on the grounds that a metadata lookup
   *  answers on its own schedule and would make the fixpoint depend on data the
   *  country may not have. The cache is what retires that objection: the first
   *  pass fills it, every later pass replays it — failures included — so the
   *  enrichment fields are as reproducible as the scraped ones and can take part
   *  in the convergence claim rather than sitting it out as `None`.
   *
   *  The cache wraps the FULL enrich-phase chain (metering, throttle, breaker,
   *  rate limit) rather than sitting under it, so a live fill is throttled exactly
   *  as production would throttle it while a hit costs nothing. */
  lazy val cachedEnrichmentFetch: Option[CachingEnrichmentFetch] =
    enrichmentCache.map(cache =>
      new CachingEnrichmentFetch(cache, phaseFetch(services.metrics.WorkerHttpMetrics.Phase.Enrich)))

  /**
   * On-disk enrichment fixtures FIRST, live behind them, and whatever the live leg
   * fetched recorded on the way through.
   *
   * The leg replays every answer already recorded in the tree — no Mongo, no tunnel, no
   * third-party call — and only genuinely new URLs reach the live chain. Those are then
   * written into the same tree, so a run both consumes and extends the corpus and a
   * partial fixture set converges on a complete one instead of having to be recorded in
   * one perfect pass. An ABSENT tree is just the empty case of that: every URL is new,
   * every answer is recorded, and the next run starts warm. It is not a mode.
   *
   * Three details are load-bearing, and each was a bug first:
   *
   *  - `strict = true`. `FakeHttpFetch` normally SYNTHESISES an empty TMDB search
   *    result on a miss so a client falls through to its next strategy. Behind a
   *    fallback that is indistinguishable from success, so the live leg would never
   *    be reached and every unrecorded query would resolve to nothing — the
   *    "everything is tmdbNoMatch" failure, arriving via the replay layer.
   *  - `foldYear = false` on BOTH sides. The default key folds `year` away, which is
   *    right for cinema scrapes and wrong here: `?query=Cicha+noc&year=2026` returns
   *    0 results where the yearless form returns 16, and `TmdbClient` depends on
   *    exactly that difference.
   *  - The recorder wraps ONLY the live leg. Wrapping the whole chain wrote every
   *    fixture HIT straight back to disk byte-identically — `RecordingHttpFetch` writes
   *    on every call — so a fully recorded corpus still paid a disk write per URL per
   *    pass, and the order-independence test runs four enrichment sweeps over the same
   *    films. Only a live fetch has anything new to record. (`RecordingHttpFetch`'s own
   *    doc argues for wrapping a whole chain, but that is about the Zyte-routed SCRAPE
   *    chain, where an inner leg fetches through its own client and would otherwise
   *    bypass the recorder. There is no such leg here.)
   */
  override lazy val enrichmentFetch: HttpFetch = {
    // The live leg is the CACHE when there is one, and the enrich-phase chain
    // itself when there isn't. Without that second case "no cache" meant "offline
    // behind the fixtures", so every URL the tree didn't already hold failed
    // outright and was never recorded — which is precisely the state CI is now in,
    // with the Mongo cache removed from the leg and the tree carrying determinism
    // on its own. A tree that cannot grow re-misses the same URLs for ever.
    val live   = cachedEnrichmentFetch.getOrElse(phaseFetch(services.metrics.WorkerHttpMetrics.Phase.Enrich))
    val replay = new clients.tools.FakeHttpFetch(fixtureDirectory, strict = true, foldYear = false)
    // Named for what it IS. Labelled "live", every cached 404 was reported as
    // `live: HTTP 404`, so a run answering entirely from remembered verdicts looked
    // exactly like one re-fetching every one of them.
    new FallbackHttpFetch(Seq(
      "enrichment-fixtures" -> replay,
      "remembered-or-live"  -> new clients.tools.RecordingHttpFetch(fixtureDirectory, live, foldYear = false)))
  }

  /** The real key and the country's own language — the enrichment is meant to be the one
   *  production would do. Overridden because `TestWiring` pins a stub key and the DEFAULT
   *  language, and a replay canonicalising against the wrong locale is a divergence the
   *  suite would report as a pipeline bug.
   *
   *  There was a second, keyless branch here for the case where enrichment had nowhere to
   *  ask, gated on `enrichmentCache.isDefined`. It outlived the condition it described —
   *  the fixture tree became a source in its own right — and the drift was invisible,
   *  because `search` is `authHeader.flatMap` and a keyless client returns `None` without
   *  ever reaching the fetch: a leg with 6,906 recorded fixtures and no cache resolved 0
   *  of 892 films in 55 seconds, all three specs GREEN over a corpus with no metadata in
   *  it. There is no "nowhere to ask" any more, so there is no branch to drift. */
  override lazy val tmdbClient: TmdbClient = new TmdbClient(enrichmentFetch, language = country.language)

  // Production's storage SHAPE either way — showtimes in `screenings`, slots in
  // `movie_slots`, neither inlined on the `movies` row. A fake that inlined everything
  // would carry showtimes across a rename for free, and a merge is a rename, so it
  // could not express the bug. Which IMPLEMENTATION sits behind that shape is the
  // storage's choice, so the same assertions run over memory and over Mongo.
  override lazy val screeningsRepository = storage.screenings
  override lazy val slotsRepository      = storage.slots
  override lazy val movieRepository      = storage.movies
  override lazy val readModelRepository: ReadModelReader & ReadModelWriter = storage.readModel

  // `TestWiring` pins these in memory because its Mongo is disabled; with a real
  // database they must follow the storage, or the staging fold — the half of the
  // pipeline that needs a transaction — silently stays fake while everything around
  // it is real.
  override lazy val mongoConnection      = storage.connection
  override lazy val stagingRepository    = storage.staging
  override lazy val stagingFolder        = storage.stagingFolder(movieRepository)

  // The collections production keeps BESIDE the pipeline's own state. None of them is
  // what an assertion reads, which is why they were the easiest things to leave faked —
  // and why leaving them faked narrowed the claim without looking as though it did: a
  // settle that enqueues a task, stamps freshness, coordinates a chunk or records an
  // OMDb attempt does real writes in production and none at all against a fake.
  override lazy val taskQueue            = storage.tasks
  override lazy val freshnessStore       = storage.freshness
  override lazy val chunkScrapeStore     = storage.chunkScrape
  override lazy val omdbAttemptStore     = storage.omdbAttempt

  // The ONE store deliberately left faked on BOTH paths, inherited from `TestWiring`:
  // `resolutionCache` stays a passthrough. A shared, stateful resolution cache fixes the
  // value for a hint key by whichever row populates it first, so a shuffled re-enrich
  // sweep would resolve differently from an unshuffled one and the order-independence
  // test would fail for a reason that is not an ordering bug. Production really does
  // carry that statefulness, so this is a genuine narrowing of the claim rather than an
  // equivalent substitute — named here rather than left to be discovered.

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

object ArchiveReplayWiring {

  /** Points a run at a PARTICULAR tree — a scratch one to prove recording works, a
   *  shared one restored from a CI cache. Only ever an override: unset means the
   *  country's own tree, not a different way of running. */
  val FixturesVar = "KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES"

  /**
   * The fixture tree a country's replay reads and records: `enrichment-pl`,
   * `enrichment-de`, `enrichment-uk` under `test/resources/fixtures/` (or under
   * `KINOWO_FIXTURE_ROOT`), which is exactly what CI and `scripts/convergence-local.sh`
   * have always passed explicitly.
   *
   * There is no "no tree" answer, and that is the point. An unconfigured run used to get
   * a wiring that refused HTTP outright: TMDB was handed no key, every cinema DETAIL
   * fetch threw, and the leg ran to completion having enriched nothing — a fixpoint over
   * a corpus with no metadata in it, reported as a pass. A directory that does not exist
   * yet is simply an empty one; the first run fills it and every later run replays it.
   */
  def fixtureDirectory(country: Country): String =
    Env.get(FixturesVar).filter(_.nonEmpty).getOrElse(s"enrichment-${country.code}")
}
