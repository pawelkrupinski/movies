package services.movies

import clients.TmdbClient
import controllers.{FilmSchedule, MovieControllerService}
import models.{Cinema, Country, MovieRecord, Showtime}
import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.MovieDetailsComplete
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository, ScrapeAttempt}
import services.titlerules.TitleRuleSet
import tools.{ArchiveReplayWiring, CorpusFixture, CountryScrapeCorpus, EnrichmentCache, EnrichmentCacheStore,
  Env, FileEnrichmentCacheStore, MongoEnrichmentCacheStore, SameThreadExecutionBudget}

import java.time.{Instant, LocalDateTime}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.{Random, Try}

/**
 * The per-country fixpoint guard. `ReScrapeIdempotencySpec` asks this question of
 * Poland against the recorded HTTP corpus; this asks it of a country's OWN
 * catalogue and country-scoped title rules, driven from `cinema_scrapes`.
 *
 * Deliberately asserts NOTHING about which films or showtimes come out — a
 * generated corpus has no business claiming a repertoire. It asserts only the
 * shape of the pipeline's behaviour over time:
 *
 *   1. the first settle after boot CONVERGES — a further settle changes no key,
 *      moves no film's cinemas, folds no row and writes nothing;
 *   2. identical re-scrape ticks are CHURN-FREE — no row re-folded, no known film
 *      re-diverted into staging, on any tick;
 *   3. the corpus reaches an emission-free FIXPOINT — two consecutive ticks with
 *      zero persisted writes, within a bounded number of ticks. A pipeline that
 *      oscillates (the square-wave class of bug) never gets there, so the bound
 *      is the discriminator;
 *   4. NOTHING IS SILENTLY LOST: every cinema, every showtime and every film the
 *      archive holds comes out the far end, in the rows the web would render;
 *   5. and it is ORDER-INDEPENDENT: several independent passes, each taking the
 *      cinemas in a different random order AND each cinema's films in a different
 *      random order, land on byte-identical `movies` records, byte-identical
 *      `screenings`, and a byte-identical rendered read model.
 *
 * Each country runs in its OWN JVM — one spec class per country, one CI leg each —
 * because the leg installs that country's `TitleRuleSet` process-globally. Two
 * countries in one JVM would overwrite each other's normalisation, so there is
 * deliberately no alias that runs them together.
 *
 * Requires MONGODB_URI: the corpus is written to and read back from a real
 * `cinema_scrapes` collection, so the archive's own round-trip is on the path.
 * The database is uniquely named per run (see `IsolatedMongoDatabase`), which is
 * what lets the three legs — and anything else on the `it` layer — run at once.
 *
 * ENRICHMENT is on the path too when a real `TMDB_API_KEY` is present, replayed
 * through this country's `EnrichmentCache` so what a pass sees is fixed rather than
 * whatever the live services felt like saying that minute. Without a key — which is
 * what CI gets — the replay stays offline and the enrichment fields sit the claim
 * out as `None`, exactly as they did before the cache existed. See
 * `enrichmentCacheStore` below.
 */
abstract class CountryConvergenceBehaviour(country: Country) extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    enrichmentCacheStore.foreach(_.close())
    super.afterAll()
  }

  /**
   * This country's enrichment cache — the one thing in this suite that deliberately
   * OUTLIVES the run.
   *
   * Everything else here is thrown away: the corpus goes into a `kinowo_isolated_*`
   * database that is dropped in `finally`, which is exactly right for data whose
   * whole purpose is to be reconstructed identically next time. The enrichment
   * answers are the opposite — expensive to obtain, stable for a day, and the only
   * way the enrichment fields can take part in a fixpoint claim at all. So they
   * live in a fixed `convergence_test` database with a per-country collection and a
   * 1-day TTL, and nothing here drops it.
   *
   * Present only when a real `TMDB_API_KEY` is. Without a key `TmdbClient.search`
   * short-circuits to `None`, so nothing downstream would resolve and there would be
   * nothing to cache — the replay stays offline exactly as it was before, which is
   * also what CI's `country-convergence` workflow gets, since it sets `MONGODB_URI`
   * and no key.
   *
   * That gate is silent by design, and the trap it sets is worth naming: `Env` reads
   * `.env.local` from the WORKING DIRECTORY, so a run from a fresh worktree finds no
   * key and quietly takes the offline path. To exercise the cached one, give the
   * worktree the key and point Mongo at a throwaway:
   *
   * {{{
   *   ln -s /path/to/movies/.env.local .env.local     # gitignored
   *   MONGODB_URI="mongodb://127.0.0.1:28017/?directConnection=true" sbt convergencePoland
   * }}}
   *
   * Expect ~25 min on a cold cache (~3.3k live fills for Poland) and ~1 min warm —
   * the run prints both the preload count and the hit/fill split, so which one
   * happened is never a guess.
   */
  /**
   * The FILE store wins wherever a fixture tree is configured, and Mongo is the
   * fallback for a run that has no tree at all.
   *
   * That order round, deliberately. The file cache lives inside the tree, so it
   * travels in the same artifact and is warm on the next run by construction — no
   * URI, no cluster, no tunnel, and nothing that can be pointed at a socket nobody
   * is listening to. Mongo first meant a `MONGODB_URI` sitting in someone's
   * `.env.local` silently took over from it: a local run then spent its whole sweep
   * logging `not authorized on convergence_test` and wrote no cache at all, while the
   * store designed for exactly that run went unused.
   */
  private lazy val enrichmentCacheStore: Option[EnrichmentCacheStore] =
    if (TmdbClient.ApiKey.isEmpty) None
    else fixtureDirectory.map(dir => new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(dir)))
      .orElse(cacheUri.map(uri => MongoEnrichmentCacheStore.open(uri, country)))

  /** A Mongo cache, for a run with no fixture tree to keep one beside. */
  private def cacheUri: Option[String] =
    Env.get("KINOWO_CONVERGENCE_CACHE_URI").orElse(Env.get("MONGODB_URI"))

  private def fixtureDirectory: Option[String] =
    Env.get("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES").filter(_.nonEmpty)

  /**
   * ONE cache for every replay in the suite, preloaded whole before the first of
   * them boots. Shared deliberately: the order-independence test drives three
   * concurrent passes over the same corpus, and three private caches would let them
   * each fill the same URL from the live service — any disagreement between those
   * answers would read as an order-dependence that isn't one.
   *
   * The preload is NOT skipped when the fixture tree is present, though it was, on
   * the reasoning that the tree already holds the answers. It holds the answers that
   * ARRIVED. `RecordingHttpFetch` writes down a successful fetch; a 404 is not a
   * successful fetch, and roughly half a country's films never resolve to a TMDB id
   * and still cost three or four rating-slug guesses apiece that all miss. Those are
   * the entries only a cache has ever held — and with the preload skipped and the
   * Mongo cache unreachable, every one of them was re-asked live, paced and
   * single-file, on every run. A profile of a 22-minute Poland leg finds its one
   * application thread parked in `RealHttpFetch.getBytes` in all 15 samples, and the
   * fixture tree five files larger at the end than at the start.
   */
  private lazy val enrichmentCache: Option[EnrichmentCache] = enrichmentCacheStore.map { store =>
    val cache  = new EnrichmentCache(store)
    val loaded = step("preloadEnrichmentCache")(cache.preload())
    info(s"${country.displayName}: enrichment cache preloaded with $loaded entries from ${describe(store)}")
    cache
  }

  private def describe(store: EnrichmentCacheStore): String = store match {
    case mongo: MongoEnrichmentCacheStore => s"${MongoEnrichmentCacheStore.DatabaseName}.${mongo.collectionName}"
    case file:  FileEnrichmentCacheStore  => file.root.toString
    case other                            => other.getClass.getSimpleName
  }

  /** Independent random-order passes compared against each other. Three is enough
   *  to catch an order dependency while keeping the heaviest country (Germany,
   *  1,533 venues) inside its CI leg's budget. */
  private val Passes = 3

  /** Fixed, so an order-dependent regression fails the same way every run rather
   *  than surfacing as a flake. */
  private val OrderSeed = 0x2026_07_28L

  /** The instant the rendered rows are taken at. Pinned so a row can never differ
   *  between passes merely because the wall clock moved mid-test — the corpus's
   *  showtimes are all after it, so every pass renders the same window.
   *
   *  The generated corpus builds its showtimes around a fixed day, so the constant
   *  is right for it. A REAL corpus's showtimes sit around whenever it was scraped,
   *  and rendering a live dump at a hard-coded 2026-08-01 would put the whole
   *  repertoire in the past and emit nothing — so that case takes its instant from
   *  the corpus instead (see `realCorpusRenderAt`). Still a fixed value for the
   *  run's duration either way, which is all the passes need. */
  private lazy val renderAt: LocalDateTime = realCorpusRenderAt.getOrElse(LocalDateTime.of(2026, 8, 1, 0, 0))

  /** Counts merges by reason so a per-tick delta is observable. */
  private final class CountingMergeMetrics extends MergeMetrics {
    private val counts = MergeReason.all.map(_ -> new AtomicInteger(0)).toMap
    def recordMerge(reason: MergeReason, victims: Int): Unit = counts(reason).addAndGet(victims)
    def total: Int = counts.values.map(_.get).sum
    def byReason: Map[MergeReason, Int] = counts.view.mapValues(_.get).toMap
  }

  private def keySet(w: ArchiveReplayWiring): Set[(String, Option[Int])] =
    w.movieCache.snapshot().map(r => (r.title, r.year)).toSet

  private def cinemasByFilm(w: ArchiveReplayWiring): Map[String, Set[String]] =
    w.movieRepository.findAll().map(r =>
      StoredMovieRecord.idOf(r) -> r.record.cinemaData.keySet.map(_.displayName)).toMap

  /** ONE seeded archive + booted corpus, shared by the convergence test and the
   *  no-loss test.
   *
   *  Both want the same thing — this country's archive replayed and settled, with
   *  the read model projected — and booting it twice doubled the leg for nothing.
   *  That is not free at this scale: Germany replays 1,533 venues and ~18k
   *  listings per boot, and five boots per leg is what pushed its CI job past the
   *  45-minute ceiling.
   *
   *  Safe to share by construction, the same argument `ReScrapeIdempotencySpec`
   *  makes: the convergence test asserts the corpus is a FIXPOINT — a further
   *  settle changes nothing and identical re-scrapes write nothing — so whichever
   *  test runs first hands the other exactly the state it expected. Merge counts
   *  are read as deltas inside each test, so a no-op pass cannot pollute the
   *  other's baseline. The database is dropped when the suite ends. */
  private lazy val shared: (ArchiveReplayWiring, CountingMergeMetrics, ScrapeArchiveRepository) = {
    // In-memory archive: the leg no longer needs a Mongo at all.
    //
    // The corpus used to be READ from `cinema_scrapes`, so routing it back through a
    // real collection put the archive's BSON round-trip on the path for free. The
    // corpus now comes from a fixture file, so that round-trip tests
    // `MongoScrapeArchiveRepository`'s codecs rather than anything about the corpus —
    // and `ScrapeArchiveIntegrationSpec` already owns exactly that, plus the keyset
    // page-boundary case. The "content is never replaced by nothing" rule lives in
    // `ScrapeArchiveRepository.record` ABOVE the storage seam, so the in-memory
    // implementation still exercises it.
    //
    // What this removes from every leg: a mongo:7 container, a replica-set init, a
    // uniquely-named throwaway database per run, and the `MONGODB_URI` requirement
    // that has broken more runs this week than the archive round-trip ever caught.
    val archive = new services.scrapes.InMemoryScrapeArchiveRepository
    val seeded   = seedArchive(archive)
    val merges   = new CountingMergeMetrics
    val w = new ArchiveReplayWiring(country, archive, enrichmentCache) {
      override lazy val movieCache = new CaffeineMovieCache(
        movieRepository, eventBus, staging = Some(stagingRepository),
        retrigger = enrichmentRetrigger, mergeMetrics = merges)
    }
    withClue(s"the archive round-trip lost cinemas: seeded $seeded, replayed ${w.cinemaScrapers.size}\n") {
      w.cinemaScrapers.size shouldBe seeded
    }
    info(s"${country.displayName}: $seeded cinemas replayed from cinema_scrapes, " +
         s"${w.archivedListings.values.map(_.size).sum} film listings")
    bootSettled(w)
    enrichmentCache.foreach(cache => info(s"${country.displayName}: enrichment cache after boot — ${cache.statistics}"))
    info(s"${country.displayName}: enrichment coverage — ${enrichmentCoverage(w)}")
    requireEnrichmentReached(w)
    (w, merges, archive)
  }

  /**
   * A run that HAS an enrichment source must actually have enriched something.
   *
   * The coverage line above was informational, and that let the suite pass green
   * having proved nothing: with the TMDB key gated on the wrong condition, a leg
   * resolved 0 of 892 films and all three specs still passed — the fixpoint holds
   * trivially over a corpus with no metadata in it. The offline run is still allowed
   * to resolve nothing (it has nowhere to ask); a run with a cache or a fixture tree
   * is not.
   *
   * Deliberately a floor of ONE rather than a ratio. Any real collapse — a missing
   * key, a stale tree, a resolver that stopped answering — lands at exactly zero,
   * and a ratio would need re-tuning per country as each corpus drifts, which is how
   * a guard becomes a flake and then gets deleted.
   */
  private def requireEnrichmentReached(w: ArchiveReplayWiring): Unit =
    if (w.enrichmentAvailable) {
      val resolved = w.movieRepository.findAll().count(_.record.tmdbId.isDefined)
      withClue(s"${country.displayName} has an enrichment source but resolved NOTHING — " +
               s"${enrichmentCoverage(w)}. A fixpoint over an unenriched corpus proves nothing: ") {
        resolved should be > 0
      }
    }

  /** How far each enrichment source actually got across the settled corpus.
   *
   *  Printed on every run, including the offline one, where the all-zero line is
   *  itself the assertion's context: it says out loud that the fixpoint just proved
   *  was proved with no metadata in it. With the cache on, the ladder is legible —
   *  a source can only reach the rows the source above it resolved, so a collapse
   *  between two rungs localises which resolver stopped answering.
   */
  private def enrichmentCoverage(w: ArchiveReplayWiring): String = {
    val records = w.movieRepository.findAll().map(_.record)
    def count(predicate: MovieRecord => Boolean): Int = records.count(predicate)
    s"${records.size} films — tmdbId ${count(_.tmdbId.isDefined)}, tmdbNoMatch ${count(_.tmdbNoMatch)}, " +
    s"imdbId ${count(_.imdbId.isDefined)}, imdbRating ${count(_.imdbRating.isDefined)}, " +
    s"filmwebRating ${count(_.filmwebRating.isDefined)}, metascore ${count(_.metascore.isDefined)}, " +
    s"rottenTomatoes ${count(_.rottenTomatoes.isDefined)}"
  }

  /** Boot the corpus to the steady state production reaches, settle it, and get
   *  it into the read model.
   *
   *  The conclude pass AFTER the settles is load-bearing, not tidiness: a row the
   *  settle created was never concluded by `bootCorpus`, and an unconcluded row
   *  fails `readyToProject` and is silently skipped by the projector — which is
   *  exactly how 32 of 80 films, 44 cinemas and 360 screenings went missing from
   *  the read model while the corpus itself was complete. */
  private def bootSettled(w: ArchiveReplayWiring): Unit = {
    step("bootCorpus")(w.bootCorpus())
    // ONE settle, deliberately. Settling twice here would let a corpus that needs
    // two passes to stop moving look identical to one that never moved, because
    // the assertion below only ever sees the state after the last of them.
    //
    // The settle is the PAIR, though — `settle()` then `canonicalizeBySanitize()`,
    // exactly what the periodic settle runs in production and exactly what the
    // fixpoint assertion below re-applies. Booting with only the first half left the
    // corpus in a state production never rests in, so the assertion's canonicalize
    // was the FIRST one the corpus had ever seen and legitimately collapsed three
    // stranded same-film duplicates ("Ghost in shell" / "Ghost in the Shell -
    // Ponownie Na Wielkim Ekranie" / "Uwierz w ducha"). That read as the pipeline
    // failing to converge when it was the boot never finishing a settle — and it was
    // unreachable for as long as the TMDB key was gated wrong and nothing enriched,
    // because the duplicates are only discoverable once a shared `tmdbId` exists.
    step("settle")(w.movieService.settle())
    step("canonicalize")(w.movieCache.canonicalizeBySanitize())
    step("drainStaging")(w.drainStaging())
    step("concludeEnrichment")(w.concludeEnrichment())
    step("project")(w.readModelProjector.reconcile())
    step("reloadReadModel")(w.webReadModel.reload())
  }

  /**
   * Announce a phase to STDOUT as it starts and finishes, with its duration.
   *
   * `println`, not `info`: ScalaTest buffers `info` until the test COMPLETES, so a
   * leg that spends twenty minutes booting a country prints nothing at all until it
   * is over — and a CI log with a twenty-minute silence is indistinguishable from a
   * hang. That ambiguity cost real time this week, twice: once on a leg that was
   * genuinely wedged on a dead tunnel, once on a leg that was working fine. The
   * elapsed time is what separates the two, so it goes to the stream that flushes.
   */
  private def step[A](label: String)(body: => A): A = {
    val started = System.nanoTime()
    println(s"[${country.code}] $label …")
    val result = body
    println(f"[${country.code}] $label done in ${(System.nanoTime() - started) / 1e9}%.1fs")
    result
  }

  /**
   * A REAL `cinema_scrapes` collection to replay instead of the generated corpus,
   * when `KINOWO_CONVERGENCE_SCRAPES_URI` (+ `_DB`) names one.
   *
   * The generated corpus encodes the flap-prone title SHAPES but no real film, so
   * enrichment can only ever refuse it: every generated row carries
   * `director = Seq("Some Director")`, which sends resolution down the
   * director-walk branch and guarantees a miss whatever the title says. Pointing
   * the suite at a production dump is what lets the enrichment fields carry weight
   * — real titles, real years, real directors.
   *
   * Read-only: the rows are copied into the run's own isolated archive and the
   * source is never written to, so a live mirror can safely be the source.
   */
  private lazy val realScrapeSource: Option[ScrapeArchiveRepository] =
    Env.get("KINOWO_CONVERGENCE_SCRAPES_URI").map { uri =>
      // Tuned for the tunnel — see TunnelTunedUri. Without it a proxy restart costs
      // 30s of server selection per attempt and the corpus read stalls at 0% CPU.
      val database = MongoClient(tools.TunnelTunedUri(uri)).getDatabase(
        Env.get("KINOWO_CONVERGENCE_SCRAPES_DB").getOrElse(country.mongoDb))
      new MongoScrapeArchiveRepository(Some(database))
    }

  /**
   * The real corpus, read ONCE and shared by everything that needs it.
   *
   * Read ONCE, because the read is expensive and doing it twice — as an earlier
   * version did, separately for the render instant and for the seeding — doubles a
   * whole-collection round trip for nothing.
   *
   * It FAILS LOUDLY when the result is empty. The archive's reads are best-effort
   * by design — a failure is logged and yields nothing — which is right for the
   * production archive, where a scrape must not die because its side-record didn't
   * save, and exactly wrong for a corpus source. A failed read is not data: an
   * empty corpus would let the suite pass having verified nothing. That guard is
   * what turned Germany's silent "0 venues, 0 listings" into a diagnosis, and it
   * stays even though the read underneath it is now sound.
   */
  private lazy val realScrapeRows: Seq[services.scrapes.ArchivedScrape] =
    if (CorpusFixture.exists(country.code)) {
      // The checked-in corpus. Preferred over the live read whenever it exists: it
      // needs no tunnel, costs milliseconds, and — unlike prod — does not move
      // under the test. Prod drifts as venues rescrape (the same Polish corpus
      // measured 7,044, then 7,055, then 7,063 listings inside an hour), so a
      // divergence found against the live read could not be re-examined afterwards.
      val rows = step("readCorpusFixture")(CorpusFixture.read(country.code))
      info(s"${country.displayName}: replayed ${rows.size} archived scrapes from ${CorpusFixture.pathFor(country.code)}")
      rows
    } else fetchAndCaptureCorpus

  /** Fall back to FETCHING the corpus from a live archive, and write the fixture on
   *  the way through so the next run does not have to.
   *
   *  Self-healing rather than a chore: a country with no fixture yet, or one whose
   *  fixture was deliberately deleted to refresh it, pays the tunnel once and
   *  leaves the file behind. */
  private def fetchAndCaptureCorpus: Seq[services.scrapes.ArchivedScrape] = realScrapeSource.toSeq.flatMap { source =>
    val known = CountryScrapeCorpus.cinemasOf(country).toSet
    val rows  = step("fetchCorpusFromArchive")(
      source.findAll().filter(row => known.contains(row.cinema) && row.films.nonEmpty))

    if (rows.isEmpty)
      throw new IllegalStateException(
        s"KINOWO_CONVERGENCE_SCRAPES_URI is set but ${country.displayName}'s cinema_scrapes read came back " +
        s"empty across all ${known.size} of the catalogue's cinemas. The archive's reads are best-effort — a " +
        "failure is logged and yields nothing — so this is far more likely a slow or dropped connection " +
        "than a genuinely empty archive, and seeding an empty corpus would let the suite pass having " +
        "verified nothing.")

    // Only a COMPLETE read is worth capturing — the guard above already refused an
    // empty one, and a short read would bake a truncated corpus into the repo.
    val path = CorpusFixture.write(country.code, rows)
    info(s"${country.displayName}: read ${rows.size} archived scrapes from ${known.size} catalogue cinemas — " +
         s"captured ${CorpusFixture.renderedBytes(rows) / 1048576} MB of JSON to $path " +
         s"(${java.nio.file.Files.size(path) / 1048576} MB gzipped); future runs replay it without a tunnel")
    rows
  }

  /** The real corpus's own render instant: the start of the day its OLDEST listed
   *  showtime falls on, so every screening the dump holds is in the future and the
   *  read model renders all of it. Taken from the corpus rather than the clock so
   *  the three passes still agree. */
  private lazy val realCorpusRenderAt: Option[LocalDateTime] =
    // Same condition as `seedArchive`: a real corpus is a real corpus whether it came
    // from a fixture or a live archive. Gating on `realScrapeSource` alone left a
    // fixture-driven run rendering at the hard-coded generated-corpus instant, which
    // put 12 venues' entire repertoire in the past and reported them as "never reach
    // the read model" — a loss that was really a clock mismatch.
    Option.when(CorpusFixture.exists(country.code) || realScrapeSource.isDefined)(realScrapeRows).flatMap { rows =>
      rows.flatMap(_.films).flatMap(_.showtimes).map(_.dateTime).minOption.map(_.toLocalDate.atStartOfDay)
    }

  /** Seed the archive with this country's corpus, exactly as a real scrape would
   *  have filed it — through `ScrapeAttempt`, so the archive's own "content only"
   *  rule and its BSON round-trip are both on the path to the pipeline. */
  private def seedArchive(archive: ScrapeArchiveRepository): Int =
    // A committed fixture is enough on its own — it must NOT need a live source
    // configured alongside it. Dispatching on `realScrapeSource` alone meant a
    // checked-in corpus was silently ignored unless the tunnel env var happened to be
    // set too, which is backwards: the fixture exists precisely so a run needs no
    // tunnel.
    if (CorpusFixture.exists(country.code) || realScrapeSource.isDefined) seedFromRealScrapes(archive)
    else seedGeneratedCorpus(archive)

  /** Copy the real `cinema_scrapes` dump in. Already restricted to the cinemas this
   *  country's catalogue still knows (see [[realScrapeRows]]) — a dump can outlive a
   *  venue, and `ArchiveReplayWiring` drops what the catalogue no longer lists, which
   *  would otherwise make the seeded/replayed counts disagree for a reason that isn't
   *  loss. */
  private def seedFromRealScrapes(archive: ScrapeArchiveRepository): Int = {
    val rows = realScrapeRows
    // Concurrent, with a bounded pool: each venue is an independent `replaceOne` on
    // its own `_id`, so there is no ordering between them, and sequentially they are
    // 282-1,533 round-trips of latency for no reason. Deliberately still through
    // `record` rather than a bulk write — that is what keeps the archive's own
    // "content is never replaced by nothing" rule and its BSON round-trip on the
    // path, which is the entire reason the corpus goes through Mongo at all.
    step(s"seed ${rows.size} venues into the archive") {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(8)
    try {
      implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(pool)
      scala.concurrent.Await.result(scala.concurrent.Future.traverse(rows) { row =>
        scala.concurrent.Future(archive.record(ScrapeAttempt(
        cinema          = row.cinema,
        city            = row.city.orElse(Cinema.cityOf(row.cinema)),
        at              = row.lastSuccess.map(_.at).getOrElse(Instant.parse("2026-07-28T06:00:00Z")),
        listingComplete = row.lastSuccess.exists(_.listingComplete),
        films           = row.films
        )))
      }, 10.minutes)
    } finally pool.shutdown()
    }
    info(s"${country.displayName}: seeded from REAL cinema_scrapes — ${rows.size} venues, " +
         s"${rows.map(_.films.size).sum} listings, rendering at $renderAt")
    rows.size
  }

  private def seedGeneratedCorpus(archive: ScrapeArchiveRepository): Int = {
    val listings = CountryScrapeCorpus.listings(country, LocalDateTime.of(2026, 8, 1, 0, 0))
    listings.foreach { case (cinema, films) =>
      archive.record(ScrapeAttempt(
        cinema          = cinema,
        city            = Cinema.cityOf(cinema),
        at              = Instant.parse("2026-07-28T06:00:00Z"),
        listingComplete = true,
        films           = films
      ))
    }
    listings.size
  }

  /** One production-shaped tick: re-serve every cinema's archived listing in a
   *  shuffled order, then drain and settle. Returns the set of `(cinema, title)`
   *  diversions the scrape phase pushed into staging — a KNOWN film landing back
   *  in `pending_movies` is the churn we care about. */
  private def settleTick(w: ArchiveReplayWiring, rnd: Random): Set[(String, String)] = {
    val before = w.stagingRepository.findAll()
      .map(r => (r.cinema.displayName, TitleNormalizer.sanitize(r.title))).toSet
    val ready = mutable.ListBuffer.empty[MovieDetailsComplete]

    // Shuffled per tick so the fixpoint is asserted independent of the order
    // cinemas re-report, not merely in catalogue order. `rnd` is caller-seeded,
    // so an order-dependent regression fails deterministically, never as a flake.
    rnd.shuffle(w.cinemaScrapers.toList).foreach { scraper =>
      Try(scraper.fetch()).toOption.foreach { films =>
        try {
          val touched = w.movieCache.recordCinemaScrape(scraper.cinema, films)
          ready ++= w.cinemaScrapeRunner.classify(scraper.cinema, touched)
        } catch { case _: Exception => () }
      }
    }

    val after = w.stagingRepository.findAll()
      .map(r => (r.cinema.displayName, TitleNormalizer.sanitize(r.title))).toSet
    ready.foreach(w.eventBus.publish)
    w.drainServices()
    w.drainStaging()
    w.movieService.settle()
    after -- before
  }

  s"the ${country.displayName} pipeline" should
    "converge on the first settle and stay churn-free under identical re-scrapes" in {
    // Put the JVM on THIS country's title rules. Without it the leg runs on
    // `TitleNormalizer.active`, which defaults to the sole country named by the
    // environment and — with none named — to Poland: a German or British leg would
    // exercise its own catalogue under POLISH normalisation and quietly prove
    // nothing about its own.
    //
    // Installed HERE, in the test body, and deliberately NOT in the constructor:
    // ScalaTest INSTANTIATES every discovered suite in order to read its tags, so
    // a constructor-level swap fires even in a run that excludes this spec by tag
    // — which is how a German rule set reached `FilmScheduleEndToEndSpec` and
    // failed it. A test body runs only when the test does.
    TitleNormalizer.installRules(TitleRuleSet.forCountry(country))
    {
      val (w, merges, _) = shared

      // ── 1) The settle is a fixpoint of itself ────────────────────────────────
      val before        = keySet(w)
      val cinemasBefore = cinemasByFilm(w)
      val recordsBefore = w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
      val screeningsBefore = w.screeningsRepository.findAll()
      val mergesBefore  = merges.total
      info(s"${country.displayName}: settled corpus of ${before.size} films")
      before should not be empty

      val emissions = new AtomicInteger(0)
      w.movieRepository.watchChanges(_ => { emissions.incrementAndGet(); () }, _ => { emissions.incrementAndGet(); () })

      w.movieService.settle()
      w.movieCache.canonicalizeBySanitize()

      val after        = keySet(w)
      val cinemasAfter = cinemasByFilm(w)
      val recordsAfter = w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
      val screeningsAfter = w.screeningsRepository.findAll()
      withClue(
        s"a settle on a settled ${country.displayName} corpus folded ${merges.total - mergesBefore} row(s); " +
          s"keys APPEARED=${(after -- before).take(8).mkString(", ")} " +
          s"VANISHED=${(before -- after).take(8).mkString(", ")}\n") {
        (merges.total - mergesBefore) shouldBe 0
        after shouldBe before
      }
      val moved = (cinemasBefore.keySet ++ cinemasAfter.keySet)
        .filter(k => cinemasBefore.get(k) != cinemasAfter.get(k))
      withClue(
        s"a settle on a settled ${country.displayName} corpus MOVED cinemas on ${moved.size} film(s):\n" +
          moved.take(6).map(k =>
            s"  $k lost=${cinemasBefore.getOrElse(k, Set.empty) -- cinemasAfter.getOrElse(k, Set.empty)} " +
            s"gained=${cinemasAfter.getOrElse(k, Set.empty) -- cinemasBefore.getOrElse(k, Set.empty)}").mkString("\n") + "\n") {
        moved shouldBe empty
      }
      // Keys and cinema sets say the SHAPE is unchanged; these say the DATA is.
      // A settle that rewrote a synopsis, a runtime, or a slot's showtimes in
      // place would leave every key and every cinema set exactly where it was.
      withClue(s"a settle on a settled ${country.displayName} corpus CHANGED the stored records:\n" +
               s"${CorpusDiff.records(recordsBefore, recordsAfter, "before", "after")}\n") {
        recordsAfter shouldBe recordsBefore
      }
      withClue(s"a settle on a settled ${country.displayName} corpus CHANGED the screenings:\n" +
               s"${CorpusDiff.slots(screeningsBefore, screeningsAfter, "before", "after")}\n") {
        screeningsAfter shouldBe screeningsBefore
      }
      withClue(s"a settle on a ${country.displayName} corpus that has cleared staging wrote " +
               s"${emissions.get} time(s) — it should have had nothing to do\n") {
        emissions.get shouldBe 0
      }

      // ── 2+3) Identical re-scrapes are churn-free and reach a fixpoint ─────────
      val settledKeys = before
      val rnd      = new Random(0x2026_07_28L)
      val churn    = mutable.ListBuffer.empty[String]
      val keyDrift = mutable.ListBuffer.empty[String]
      val perTick  = mutable.ListBuffer.empty[Int]
      // A FIXED two ticks, both of which must be completely clean. The bounded
      // search this replaced ("keep ticking until two consecutive quiet ones,
      // give up after twelve") could not tell a corpus that never moved from one
      // that thrashed for ten ticks and then went quiet — and the whole question
      // is whether the FIRST identical re-scrape is already a no-op. The second
      // tick is there to catch a two-state oscillation, not to grant slack.
      (1 to 2).foreach { t =>
        val mergesBeforeTick    = merges.byReason
        val emissionsBeforeTick = emissions.get
        val diversions   = settleTick(w, rnd)
        val mergesDelta  = MergeReason.all.map(r => r -> (merges.byReason(r) - mergesBeforeTick(r))).filter(_._2 > 0)
        val emissionsDelta = emissions.get - emissionsBeforeTick
        val keysNow  = keySet(w)
        val appeared = keysNow -- settledKeys
        val vanished = settledKeys -- keysNow

        perTick += emissionsDelta
        mergesDelta.foreach { case (r, n) => churn += f"tick $t%d: $n%3d merge(s) reason=${r.label}" }
        if (diversions.nonEmpty)
          churn += s"tick $t: ${diversions.size} known film(s) RE-DIVERTED to staging: ${diversions.take(12).mkString(", ")}"
        if (emissionsDelta != 0)
          churn += s"tick $t: $emissionsDelta persisted write(s) — an identical re-scrape must write nothing"
        if (appeared.nonEmpty) keyDrift += s"tick $t: keys APPEARED: ${appeared.take(8).mkString(", ")}"
        if (vanished.nonEmpty) keyDrift += s"tick $t: keys VANISHED: ${vanished.take(8).mkString(", ")}"
      }
      info(s"${country.displayName}: per-tick change-stream emissions: ${perTick.mkString(", ")}")
      if (keyDrift.nonEmpty)
        info(s"${country.displayName}: key-spelling drift (informational):\n${keyDrift.mkString("\n")}")

      withClue(
        s"A settled ${country.displayName} corpus must not re-fold or re-divert under identical " +
          s"re-scrape, but:\n${churn.mkString("\n")}\n") {
        churn.toList shouldBe empty
      }
    }
  }

  /** One whole-corpus pass in a seeded-random order, returning everything a
   *  divergence could hide in: the persisted film records, the per-slot
   *  screenings, and the rows the web would actually render.
   *
   *  Two orders are shuffled, because they fail differently. CINEMA order is the
   *  one production varies every tick (the reaper enqueues by due-time, not by
   *  catalogue position). FILM order WITHIN a cinema is the one a scraper varies
   *  whenever a site reorders its listing — and it decides which of a venue's two
   *  spellings of the same film reaches the shared slot key first, which is the
   *  seam the same-slot ping-pong rode in on.
   *
   *  `SameThreadExecutionBudget` pins enrichment to the calling thread so the
   *  only nondeterminism left is the seeded shuffle — otherwise a thread race,
   *  not an order dependency, would decide the outcome and the test would flake
   *  rather than fail. */
  private def replay(archive: ScrapeArchiveRepository, seed: Long)
      : (Seq[StoredMovieRecord], Map[String, Map[String, Seq[Showtime]]], Seq[FilmSchedule]) = {
    val rnd = new Random(seed)
    val w = new ArchiveReplayWiring(country, archive, enrichmentCache) {
      override lazy val backgroundBudget: tools.ExecutionBudget = new SameThreadExecutionBudget
    }
    val ready = mutable.ListBuffer.empty[MovieDetailsComplete]
    rnd.shuffle(w.cinemaScrapers.toList).foreach { scraper =>
      Try(scraper.fetch()).toOption.foreach { films =>
        val touched = w.movieCache.recordCinemaScrape(scraper.cinema, rnd.shuffle(films.toList))
        ready ++= w.cinemaScrapeRunner.classify(scraper.cinema, touched)
      }
    }
    // Publish in a shuffled order too: production publishes inline as each cinema
    // lands, so the enrichment stage sees an arbitrary cross-film order.
    rnd.shuffle(ready.toList).foreach(w.eventBus.publish)
    w.drainServices()
    w.drainStaging()
    w.movieService.settle()
    w.drainStaging()
    w.movieService.settle()
    w.concludeEnrichment()
    w.readModelProjector.reconcile()
    w.webReadModel.reload()

    val records = w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))
    val screenings = w.screeningsRepository.findAll()
    val service = new MovieControllerService(w.webReadModel)
    val rows = country.cities.sortBy(_.slug).flatMap(c => service.toSchedules(c, renderAt))
    (records, screenings, rows)
  }

  s"the ${country.displayName} corpus" should
    "come out identical — films, screenings and rendered rows — whatever order it arrives in" in {
    TitleNormalizer.installRules(TitleRuleSet.forCountry(country))
    {
      val archive = new services.scrapes.InMemoryScrapeArchiveRepository
      seedArchive(archive)

      // Concurrently: the passes are independent whole-corpus replays and running
      // them back-to-back made this the leg's long pole (three boots serially, on
      // top of the shared one). Same helper the fixture determinism specs use.
      val passes = ParallelReplays((0 until Passes).map(i => OrderSeed + i.toLong))(replay(archive, _))
      val (records0, screenings0, rows0) = passes.head
      info(s"${country.displayName}: $Passes passes over ${records0.size} films, " +
           s"${screenings0.values.map(_.size).sum} slots, ${rows0.size} rendered rows")
      records0 should not be empty
      rows0     should not be empty

      val divergences = mutable.ListBuffer.empty[String]
      (1 until Passes).foreach { i =>
        val (recordsI, screeningsI, rowsI) = passes(i)
        if (recordsI != records0) divergences += s"FILMS differ on pass $i:\n${CorpusDiff.records(records0, recordsI, "pass0", s"pass$i")}"
        if (screeningsI != screenings0) divergences += s"SCREENINGS differ on pass $i:\n${CorpusDiff.slots(screenings0, screeningsI, "pass0", s"pass$i")}"
        if (rowsI != rows0)
          divergences += s"RENDERED ROWS differ on pass $i (${rows0.size} vs ${rowsI.size}):\n" +
                         CorpusDiff.rows(rows0, rowsI, "pass0", s"pass$i")
      }
      // Name the seeds: each pass's arrival order is a pure function of its seed, so
      // quoting them makes a CI-only divergence reproducible on a laptop instead of
      // something you re-run and hope to see again.
      val seeds = (0 until Passes).map(i => s"pass$i=0x${(OrderSeed + i.toLong).toHexString}").mkString(", ")
      withClue(s"${divergences.size} order-dependent divergence(s) [$seeds]:\n${divergences.take(10).mkString("\n")}\n") {
        divergences.toList shouldBe empty
      }
    }
  }


  /** Every cinema, showtime and film the DATABASE holds, versus what the web
   *  would actually render.
   *
   *  The two specs above are both blind to loss: a pipeline that dropped half the
   *  corpus on the floor would still settle to a fixpoint, and would still do so
   *  identically whatever order it read things in. Convergence says the corpus
   *  stops changing; it says nothing about the corpus being COMPLETE. This is the
   *  one that would have caught a projection that quietly served fewer films than
   *  it was given.
   *
   *  The expectation is read back out of `cinema_scrapes` rather than from the
   *  generator, so what is being compared is what the database actually holds
   *  against what the read model actually emits — the whole path, end to end.
   *
   *  Subset, not equality, in both directions that matter: the read model may
   *  legitimately hold MORE (a folded film carries several venues' spellings into
   *  one row) but never less. */
  s"the ${country.displayName} read model" should
    "emit every cinema, showtime and film the archive holds" in {
    TitleNormalizer.installRules(TitleRuleSet.forCountry(country))
    {
      val (w, _, archive) = shared

      // ── what the DATABASE holds ──────────────────────────────────────────────
      val stored          = archive.findAll()
      val storedCinemas   = stored.filter(_.films.nonEmpty).map(_.cinema.displayName).toSet
      val storedScreenings = stored.flatMap(row =>
        row.films.flatMap(_.showtimes.map(st => (row.cinema.displayName, st.dateTime)))).toSet
      val storedTitles    = stored.flatMap(_.films.map(_.movie.title)).toSet

      // ── what the WEB would render ────────────────────────────────────────────
      val rows = country.cities.sortBy(_.slug).flatMap(c =>
        new MovieControllerService(w.webReadModel).toSchedules(c, renderAt))
      val shown        = rows.flatMap(_.showings.flatMap(_._2))
      val shownCinemas = shown.map(_.cinema.displayName).toSet
      val shownScreenings = shown.flatMap(cs => cs.showtimes.map(st => (cs.cinema.displayName, st.dateTime))).toSet
      val shownTitles  = rows.map(_.movie.title).toSet

      info(s"${country.displayName}: archive holds ${storedCinemas.size} cinemas / " +
           s"${storedScreenings.size} distinct screenings / ${storedTitles.size} scraped titles; " +
           s"read model emits ${shownCinemas.size} cinemas / ${shownScreenings.size} screenings / ${shownTitles.size} films")
      storedCinemas should not be empty

      val lostCinemas = storedCinemas -- shownCinemas
      withClue(s"${lostCinemas.size} cinema(s) in cinema_scrapes never reach the read model: " +
               s"${lostCinemas.toList.sorted.take(10).mkString(", ")}\n") {
        lostCinemas shouldBe empty
      }

      val lostScreenings = storedScreenings -- shownScreenings
      withClue(s"${lostScreenings.size} of ${storedScreenings.size} screening(s) never reach the read model; " +
               s"first: ${lostScreenings.toList.sortBy(p => (p._1, p._2)).take(8).mkString(", ")}\n") {
        lostScreenings shouldBe empty
      }

      // A scraped title need not survive VERBATIM — folding is the point, and
      // "Diuna (dubbing)" is meant to come out as "Diuna". What must survive is
      // the FILM: every title the archive holds has to be represented by some
      // emitted film that it folded into, so nothing vanishes without a home.
      // Compared on the canonical key, not the spelling: the projection derives a
      // film's DISPLAY title, so a corpus row stored as "Cicha garden ii" is
      // legitimately emitted as "Cicha Garden II". Matching raw strings reported
      // six such films as lost when every one of them was on the page.
      val settled  = w.movieCache.snapshot().map(r => TitleNormalizer.sanitize(r.title)).toSet
      val emitted  = shownTitles.map(TitleNormalizer.sanitize)
      val homeless = settled -- emitted
      withClue(s"${homeless.size} settled film(s) exist in the corpus but are emitted by nothing: " +
               s"${homeless.toList.sorted.take(10).mkString(", ")}\n") {
        homeless shouldBe empty
      }
    }
  }

}
