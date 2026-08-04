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
import tools.{ArchiveReplayWiring, ConvergenceStorage, CorpusFixture, CountryScrapeCorpus, EnrichmentCache,
  EnrichmentFreshness, Env, FileEnrichmentCacheStore, ProdCoverageBaseline,
  SameThreadExecutionBudget}

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
 * Needs no database. The corpus comes from a committed fixture and is replayed through
 * an in-memory archive; the enrichment answers come from a recorded fixture tree with a
 * remembered-verdict cache beside it. Every Mongo the suite once required — a container
 * for the corpus, a tunnelled cluster for the cache — is gone, along with the failures
 * they caused.
 *
 * ENRICHMENT is always on the path, replayed so that what a pass sees is fixed rather
 * than whatever the live services felt like saying that minute. A real `TMDB_API_KEY` is
 * required and checked before the run starts, because without one nothing resolves and
 * the leg would spend an hour proving a fixpoint over a corpus with no metadata in it.
 */
abstract class CountryConvergenceBehaviour(
  country: Country,
  /**
   * Which recorded corpus to replay: the country's whole catalogue, or the ~100-film
   * SAMPLE captured beside it.
   *
   * The sample leg exists to fail FAST. These assertions are the only ones that can
   * see a whole class of enrichment regression — a fallback chain that turned a 404
   * into an outage cost every country its Metacritic and Rotten Tomatoes ladders, and
   * nothing else in the suite noticed — but the full legs take 12 to 73 minutes to
   * say so, and the UK's is the one most likely to be cancelled on a budget. A
   * hundred films exercise the same code on the same shapes in a couple of minutes,
   * so the matrix runs them first and the long legs only start once they are green.
   *
   * Everything else is identical. The sample is not a weaker experiment: it runs the
   * same fixpoint, order-independence, no-loss and production-band assertions over a
   * smaller corpus, so a failure means the same thing here as there.
   */
  corpusKey: String
) extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    enrichmentCacheStore.close()
    passStorages.synchronized(passStorages.foreach(p => Try(p.close())))
    storage.close()
    super.afterAll()
  }

  /**
   * Where this run keeps the state it makes claims about: a real MongoDB when
   * `MONGODB_URI` names one, memory otherwise. CI names one, so the persistence layer
   * — codecs, keyset-paged full scans, transactional staging folds — is on the path the
   * assertions run over; a local run gets whichever it asks for.
   *
   * Every assertion is identical across the two. That is the point: if a claim holds in
   * memory and not in Mongo, the difference IS the finding, and until now the suite
   * could not produce it because the database was never there to disagree.
   */
  private lazy val storage: ConvergenceStorage =
    ConvergenceStorage.fromEnv(s"convergence-$corpusKey", TitleNormalizer.forCountry(country))

  /** The per-pass databases, so `afterAll` can drop them. Each is isolated; none may
   *  outlive the run. */
  private val passStorages = mutable.ListBuffer.empty[ConvergenceStorage]

  /**
   * This country's enrichment cache — the one thing in this suite that deliberately
   * OUTLIVES the run.
   *
   * Everything else here is thrown away: the corpus is rebuilt from a committed fixture
   * every time, which is exactly right for data whose whole purpose is to be
   * reconstructed identically. The enrichment answers are the opposite — expensive to
   * obtain, and the only way the enrichment fields can take part in a fixpoint claim at
   * all. So they live in a directory INSIDE the fixture tree, which travels between runs
   * in the release asset the leg publishes, and expire on [[EnrichmentFreshness.Ttl]].
   *
   * There is no Mongo option any more, and no URI to point anywhere. The cache began in
   * a `convergence_test` database reachable only over a `flyctl proxy`, and that tunnel
   * caused every serious failure this suite has had: three legs cancelled at the
   * 75-minute ceiling paying a 5-second server-selection timeout per cache miss, a
   * preload that never once completed inside its 120-second ceiling, and a local run
   * that spent its whole sweep logging `not authorized`. A directory beside the fixtures
   * needs no cluster, no credential and no proxy, and is warm on the next run by
   * construction.
   *
   * Requires a real `TMDB_API_KEY`, and says so HERE rather than letting the run find
   * out. Without a key `TmdbClient.search` short-circuits before it reaches any fetch, so
   * neither the tree nor the cache can rescue it: the leg resolves exactly zero and dies
   * at `requireEnrichmentReached` — after scraping, folding and projecting a whole
   * country to prove nothing. The keyless run used to be a supported mode, with the cache
   * simply absent and the enrichment assertion excused along with it.
   *
   * The trap worth naming: `Env` reads `.env.local` from the WORKING DIRECTORY, so a run
   * from a fresh worktree finds no key. Symlink it in:
   *
   * {{{
   *   ln -s /path/to/movies/.env.local .env.local     # gitignored
   *   sbt convergencePoland
   * }}}
   *
   * The run prints the preload count and the hit/fill split, so whether it was warm is
   * never a guess.
   */
  private lazy val enrichmentCacheStore: FileEnrichmentCacheStore = {
    if (TmdbClient.ApiKey.isEmpty)
      throw new IllegalStateException(
        s"TMDB_API_KEY is not set, so ${country.displayName} would resolve nothing: TmdbClient.search " +
        "short-circuits on a missing key without reaching the fixture tree at all. Symlink .env.local into " +
        "the working directory (see this suite's scaladoc) and re-run.")
    new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(fixtureDirectory))
  }

  /** The same tree [[ArchiveReplayWiring]] replays and records into, asked the same way,
   *  so the cache lands BESIDE the corpus it belongs to rather than beside whichever
   *  directory this file happened to name. */
  private def fixtureDirectory: String = ArchiveReplayWiring.fixtureDirectory(country)

  /** Age the recorded responses out before anything reads them, so a rating captured
   *  once isn't replayed for ever. The verdict cache expires itself on read; this is the
   *  half nothing used to expire at all. */
  private lazy val expireStaleFixtures: Int =
    step("expireStaleEnrichment")(
      EnrichmentFreshness.prune(java.nio.file.Paths.get(clients.tools.FakeHttpFetch.rootFor(fixtureDirectory))))

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
  private lazy val enrichmentCache: EnrichmentCache = {
    val store   = enrichmentCacheStore
    val expired = expireStaleFixtures
    if (expired > 0)
      info(s"${country.displayName}: expired $expired recorded response(s) older than " +
           s"${EnrichmentFreshness.Ttl.toDays}d — they refetch and record fresh")
    // Successes are never persisted here: `RecordingHttpFetch` already writes every
    // response into the fixture tree and the tree is consulted first, so a copy in the
    // cache could not be read — it only made the artifact three times larger. What the
    // cache is for is the half the tree cannot hold, the remembered FAILURES.
    val cache  = new EnrichmentCache(store, persistSuccesses = false)
    val loaded = step("preloadEnrichmentCache")(cache.preload())
    info(s"${country.displayName}: enrichment cache preloaded with $loaded entries from ${store.root}")
    cache
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
   *  It used to fall back to a hard-coded 2026-08-01 — right for the GENERATED corpus,
   *  which built its showtimes around a fixed day. That corpus is gone (a run without a
   *  real one is refused by `requireCorpusFixture`), and the constant with it: applied to
   *  a real dump it put the whole repertoire in the past and emitted nothing, which is
   *  how 12 venues were once reported as "never reach the read model". */
  private lazy val renderAt: LocalDateTime =
    realScrapeRows.flatMap(_.films).flatMap(_.showtimes).map(_.dateTime).minOption
      .map(_.toLocalDate.atStartOfDay)
      .getOrElse(throw new IllegalStateException(
        s"${country.displayName}'s corpus holds no showtimes at all, so there is no instant to render at."))

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
      StoredMovieRecord.idOf(r, w.movieRepository.normalizer) -> r.record.cinemaData.keySet.map(_.displayName)).toMap

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
    val archive = storage.archive
    val seeded   = seedArchive(archive)
    val merges   = new CountingMergeMetrics
    val w = new ArchiveReplayWiring(country, archive, Some(enrichmentCache), storage) {
      // `mergeMetrics` is the ONLY thing this override exists to change — everything
      // else must stay as `WorkerWiring` builds it. `enrichmentLanguage` went missing
      // here and nowhere else: the cache the replay passes use (see `replay`) keeps
      // prod's, so the shared boot and the passes were canonicalising country names
      // against different locales, and the leg this spec reports coverage from was
      // the one running on the default.
      override lazy val movieCache = new CaffeineMovieCache(
        movieRepository, eventBus, staging = Some(stagingRepository),
        retrigger = enrichmentRetrigger, mergeMetrics = merges,
        // `CountryConvergenceBehaviour.this` — inside the anonymous `ArchiveReplayWiring`
        // both this spec's `country` and the wiring's are in scope, and they are the same
        // value; naming the spec's is what disambiguates.
        enrichmentLanguage = CountryConvergenceBehaviour.this.country.language,
        normalizer = TitleNormalizer.forCountry(CountryConvergenceBehaviour.this.country))
    }
    withClue(s"the archive round-trip lost cinemas: seeded $seeded, replayed ${w.cinemaScrapers.size}\n") {
      w.cinemaScrapers.size shouldBe seeded
    }
    info(s"${country.displayName}: storage — ${storage.describe}")
    info(s"${country.displayName}: $seeded cinemas replayed from cinema_scrapes, " +
         s"${w.archivedListings.values.map(_.size).sum} film listings")
    bootSettled(w)
    info(s"${country.displayName}: enrichment cache after boot — ${enrichmentCache.statistics}")
    info(s"${country.displayName}: enrichment coverage — ${enrichmentCoverage(w)}")
    info(s"${country.displayName}: ratings given a tmdbId — ${ratingsGivenTmdbId(w)}")
    info(s"${country.displayName}: unresolved — ${unresolvedFilms(w)}")
    requireEnrichmentReached(w)
    (w, merges, archive)
  }

  /**
   * A run must actually have enriched something. Unconditionally: every run has a tree to
   * replay and a live chain behind it, so "nowhere to ask" is not a state any more.
   *
   * The coverage line above was informational, and that let the suite pass green
   * having proved nothing: with the TMDB key gated on the wrong condition, a leg
   * resolved 0 of 892 films and all three specs still passed — the fixpoint holds
   * trivially over a corpus with no metadata in it. This is also where a missing
   * `TMDB_API_KEY` now lands: the key is the one input nothing can replay around, and a
   * keyless run resolves exactly zero.
   *
   * Deliberately a floor of ONE rather than a ratio. Any real collapse — a missing
   * key, a stale tree, a resolver that stopped answering — lands at exactly zero,
   * and a ratio would need re-tuning per country as each corpus drifts, which is how
   * a guard becomes a flake and then gets deleted.
   */
  private def requireEnrichmentReached(w: ArchiveReplayWiring): Unit = {
    val resolved = w.movieRepository.findAll().count(_.record.tmdbId.isDefined)
    withClue(s"${country.displayName} resolved NOTHING — ${enrichmentCoverage(w)}. " +
             s"A fixpoint over an unenriched corpus proves nothing; check TMDB_API_KEY is " +
             s"readable from the working directory: ") {
      resolved should be > 0
    }
  }

  /**
   * Refuse to run without this country's recorded corpus.
   *
   * There used to be a silent fallback to the GENERATED corpus, and it cost eleven runs
   * and a wrong diagnosis. The generated titles are synthetic — "Long ogród", "Der lange
   * podróż + spotkanie z twórcami" — so TMDB matches none of them and every row concludes
   * `tmdbNoMatch`. That is correct behaviour on nonsense input, but it reads exactly like
   * a broken enrichment pipeline: I chased a "poisoned" fixture tree that was in fact
   * accurately caching negatives for films that do not exist, and deleted 8,306 valid
   * entries doing it.
   *
   * A leg without a corpus is not a weaker version of this suite, it is a different
   * experiment wearing its name: the fixpoint it proves is over a repertoire nobody ships.
   * So it fails, and says what is missing and how to produce it.
   */
  private def requireCorpusFixture(): Unit =
    withClue(s"no corpus fixture for ${country.code} and no KINOWO_CONVERGENCE_SCRAPES_URI to " +
             s"record one from. This leg would otherwise replay the " +
             s"GENERATED corpus, whose synthetic titles cannot enrich — a fixpoint over a " +
             s"repertoire that does not exist. Record one:\n" +
             s"  KINOWO_COUNTRY=${country.code} KINOWO_CONVERGENCE_SCRAPES_URI=<prod mongo> \\\n" +
             s"    sbt 'worker/Test/runMain scripts.RecordCorpusFixture'\n") {
      CorpusFixture.exists(corpusKey) shouldBe true
    }

  /** How far each enrichment source actually got across the settled corpus.
   *
   *  Printed on every run, and legible as a ladder — a source can only reach the rows
   *  the source above it resolved, so a collapse between two rungs localises which
   *  resolver stopped answering.
   */
  /** How many unresolved titles the report names before truncating. Enough to spot a
   *  pattern (a banner family, a language, a decoration) without burying the run's other
   *  findings; the total is always reported. */
  private val UnresolvedFilmsReported = 400

  /** The stored records in a stable order — the same snapshot the settle assertion
   *  takes, so a churn tick can be diffed against itself with `CorpusDiff.records`. */
  private def recordSnapshot(w: ArchiveReplayWiring): Seq[StoredMovieRecord] =
    w.movieRepository.findAll().sortBy(r => (r.title, r.year.map(_.toString).getOrElse("")))

  private def enrichmentCoverage(w: ArchiveReplayWiring): String = {
    val records = w.movieRepository.findAll().map(_.record)
    def count(predicate: MovieRecord => Boolean): Int = records.count(predicate)
    s"${records.size} films — tmdbId ${count(_.tmdbId.isDefined)}, tmdbNoMatch ${count(_.tmdbNoMatch)}, " +
    s"imdbId ${count(_.imdbId.isDefined)}, imdbRating ${count(_.imdbRating.isDefined)}, " +
    s"filmwebRating ${count(_.filmwebRating.isDefined)}, metascore ${count(_.metascore.isDefined)}, " +
    s"rottenTomatoes ${count(_.rottenTomatoes.isDefined)}"
  }

  /** This run's coverage in the shape production's was recorded in, so the two can
   *  be compared field by field. */
  private def coverageOf(w: ArchiveReplayWiring): ProdCoverageBaseline = {
    val records = w.movieRepository.findAll().map(_.record)
    def count(predicate: MovieRecord => Boolean): Int = records.count(predicate)
    ProdCoverageBaseline(
      recordedAt     = Instant.EPOCH,   // unused on this side; the comparison is field-wise
      films          = records.size,
      tmdbId         = count(_.tmdbId.isDefined),
      imdbId         = count(_.imdbId.isDefined),
      imdbRating     = count(_.imdbRating.isDefined),
      filmwebRating  = count(_.filmwebRating.isDefined),
      metascore      = count(_.metascore.isDefined),
      rottenTomatoes = count(_.rottenTomatoes.isDefined))
  }

  /** How far this run may sit from production on any one axis before it is a
   *  regression rather than noise.
   *
   *  5% of prod's own share, not 5 percentage points: an axis prod resolves 97% of
   *  and one it resolves 42% of should not get the same absolute licence.
   *
   *  Measured before it was chosen. On the day the baseline was first recorded every
   *  axis of all three countries sat within ~2.5%, so 5% is a doubling of the observed
   *  spread rather than a number picked to make today pass — which is what keeps this
   *  a regression detector instead of a snapshot of one afternoon. */
  private val ProdTolerance = 0.05

  /** Whether production is still PROJECTING this country — i.e. whether the
   *  baseline the band scores against is a live reference or a headstone.
   *
   *  Derived from the country's `webUrl` rather than a per-spec flag, because that
   *  is already this codebase's single "is this country deployed" lever
   *  (`Country.switchable` reads the same field). Restoring a country therefore
   *  re-arms its band automatically, with no second switch to remember.
   *
   *  Why it has to exist at all: when a country's worker is stopped, `cinema_scrapes`
   *  and the read model freeze at the SAME instant — but not at the same POINT. The
   *  archive keeps every scrape the worker had recorded, including the ones it had
   *  not yet folded, so the replay legitimately projects more films than prod ever
   *  emitted. Germany froze at 103 replayed against 84 projected: 22.6% out of a 5%
   *  band, and constant, because neither side moves again. That is not the pipeline
   *  regressing — it is the reference being gone, and no honest tolerance makes it
   *  pass. The band is SKIPPED there rather than widened; widening it would blind
   *  the countries that still have a live baseline, which is the opposite of what
   *  this assertion is for. Every other claim in this suite scores the run against
   *  the ARCHIVE, needs no live production, and keeps running for every country. */
  protected def productionIsLive: Boolean = country.webUrl.isDefined

  /** Ratings coverage CONDITIONED on having resolved a tmdbId.
   *
   *  The headline counts conflate two different failures. A film with no tmdbId has
   *  nothing to look a rating up by, so it drags every rating count down without saying
   *  anything about the rating sources; a film that HAS a tmdbId and still has no rating
   *  is a different problem entirely — the source refused, 404'd, or was never asked.
   *  Reporting the conditional rate separates "we could not identify the film" from "we
   *  identified it and could not rate it", which is the difference between chasing title
   *  rules and chasing an HTTP failure. */
  private def ratingsGivenTmdbId(w: ArchiveReplayWiring): String = {
    val resolved = w.movieRepository.findAll().map(_.record).filter(_.tmdbId.isDefined)
    if (resolved.isEmpty) "no films resolved a tmdbId"
    else {
      def rate(label: String, predicate: MovieRecord => Boolean): String = {
        val n = resolved.count(predicate)
        f"$label $n%d (${100.0 * n / resolved.size}%.1f%%)"
      }
      s"of ${resolved.size} films WITH a tmdbId — " + Seq(
        rate("imdbId", _.imdbId.isDefined),
        rate("imdbRating", _.imdbRating.isDefined),
        rate("filmwebRating", _.filmwebRating.isDefined),
        rate("metascore", _.metascore.isDefined),
        rate("rottenTomatoes", _.rottenTomatoes.isDefined)).mkString(", ")
    }
  }

  /** The FILMS that resolved to nothing, named.
   *
   *  The counts above say how big the gap is and nothing about what it is made of, and
   *  the per-row resolution logging is actively misleading as a substitute: a film with
   *  several decorated rows ("X", "Cykl Y: X", "X + spotkanie z reżyserem") logs a
   *  no-match for each row that failed while the film itself resolved through another,
   *  so counting log lines overstates the gap and points at titles that are already
   *  fine. Chasing that cost a round of rules aimed at rows whose films had resolved.
   *
   *  Sorted and capped so the report stays diffable between runs rather than dumping a
   *  few hundred lines; the count is always reported in full. */
  private def unresolvedFilms(w: ArchiveReplayWiring): String = {
    val unresolved = w.movieRepository.findAll()
      .filter(_.record.tmdbId.isEmpty)
      .map(stored => stored.year.fold(stored.title)(year => s"${stored.title} ($year)"))
      .sorted
    val shown = unresolved.take(UnresolvedFilmsReported)
    s"${unresolved.size} film(s) with no tmdbId" +
      (if (unresolved.isEmpty) "" else s"; first ${shown.size}: ${shown.mkString(" | ")}")
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
  /** Shared with the harness's own phases (`TestWiring.bootCorpus`), so a run's timings
   *  all read the same way whichever layer emitted them. */
  private def step[A](label: String)(body: => A): A = tools.PhaseTimer.timed(country.code, label)(body)

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
    if (CorpusFixture.exists(corpusKey)) {
      // The checked-in corpus. Preferred over the live read whenever it exists: it
      // needs no tunnel, costs milliseconds, and — unlike prod — does not move
      // under the test. Prod drifts as venues rescrape (the same Polish corpus
      // measured 7,044, then 7,055, then 7,063 listings inside an hour), so a
      // divergence found against the live read could not be re-examined afterwards.
      val rows = step("readCorpusFixture")(CorpusFixture.read(corpusKey))
      info(s"${country.displayName}: replayed ${rows.size} archived scrapes from ${CorpusFixture.pathFor(corpusKey)}")
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
    val path = CorpusFixture.write(corpusKey, rows)
    info(s"${country.displayName}: read ${rows.size} archived scrapes from ${known.size} catalogue cinemas — " +
         s"captured ${CorpusFixture.renderedBytes(rows) / 1048576} MB of JSON to $path " +
         s"(${java.nio.file.Files.size(path) / 1048576} MB gzipped); future runs replay it without a tunnel")
    rows
  }

  /** Seed the archive with this country's corpus, exactly as a real scrape would
   *  have filed it — through `ScrapeAttempt`, so the archive's own "content only"
   *  rule and its BSON round-trip are both on the path to the pipeline. */
  private def seedArchive(archive: ScrapeArchiveRepository): Int = {
    // A committed fixture is enough on its own — it must NOT need a live source
    // configured alongside it. Dispatching on `realScrapeSource` alone meant a
    // checked-in corpus was silently ignored unless the tunnel env var happened to be
    // set too, which is backwards: the fixture exists precisely so a run needs no
    // tunnel.
    if (!CorpusFixture.exists(corpusKey) && realScrapeSource.isEmpty) requireCorpusFixture()
    seedFromRealScrapes(archive)
  }

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


  /** One production-shaped tick: re-serve every cinema's archived listing in a
   *  shuffled order, then drain and settle. Returns the set of `(cinema, title)`
   *  diversions the scrape phase pushed into staging — a KNOWN film landing back
   *  in `pending_movies` is the churn we care about. */
  private def settleTick(w: ArchiveReplayWiring, rnd: Random): Set[(String, String)] = {
    val before = w.stagingRepository.findAll()
      .map(r => (r.cinema.displayName, w.stagingRepository.normalizer.sanitize(r.title))).toSet
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
      .map(r => (r.cinema.displayName, w.stagingRepository.normalizer.sanitize(r.title))).toSet
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

      val splitsBefore = w.movieService.mixedFilmSplits
      w.movieService.settle()
      w.movieCache.canonicalizeBySanitize()

      // The settle also SPLITS a row found to hold two different films, and over a
      // real country's corpus it must find none. A handful of genuine title
      // collisions exist in production ("Joanna d'Arc" carrying both Besson's 1999
      // film and Pálmason's 2025 one), but they are rare and none is in these
      // replays — so a split firing here means the detector has begun reading
      // ORDINARY data as two films, which costs a good row its cinemas. That
      // failure mode is not hypothetical: a director disagreement (cinemas credit
      // different roles), an uncorroborated title difference (one film named in two
      // languages) and a screening year printed for a repertory title each had to
      // be abandoned as evidence, and each was caught only by counting.
      withClue(s"settle split ${w.movieService.mixedFilmSplits - splitsBefore} cinema slot(s) out of " +
               s"${country.displayName}'s settled corpus as belonging to a second film — " +
               s"on a corpus that holds no mixed row, so the detector is reading ordinary data as two films: ") {
        w.movieService.mixedFilmSplits shouldBe splitsBefore
      }

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
        // Snapshot the records so a tick that writes can say WHAT it wrote. The count
        // alone gives you nothing to check a hypothesis against — it cost two rounds of
        // work on causes that turned out to leave the count at exactly 31.
        val recordsBeforeTick   = recordSnapshot(w)
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
        if (emissionsDelta != 0) {
          val recordsAfterTick = recordSnapshot(w)
          churn += s"tick $t: $emissionsDelta persisted write(s) — an identical re-scrape must write nothing" +
                   (if (recordsAfterTick == recordsBeforeTick)
                      " (the stored records came out IDENTICAL — the write changed nothing, so this is a " +
                      "re-write of unchanged data, not a corpus still moving)"
                    else s"\n${CorpusDiff.records(recordsBeforeTick, recordsAfterTick, s"before-tick$t", s"after-tick$t")}")
        }
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
    // Its OWN Mongo database, one per pass. The passes run concurrently over the same
    // corpus, so they cannot share collections — but they no longer run in memory either.
    // That mattered: order-independence was the ONE claim never tested against real
    // persistence, so a divergence introduced by re-keys, upsert ordering or transaction
    // interleaving could not have been caught by the assertion written to catch exactly
    // that.
    // Short on purpose: the database name carries a pid and a nanosecond stamp, and
    // Mongo caps the whole thing at 63 characters.
    val passStorage = ConvergenceStorage.fromEnv(s"${country.code}p${seed - OrderSeed}", TitleNormalizer.forCountry(country))
    passStorages.synchronized(passStorages += passStorage)
    val w = new ArchiveReplayWiring(country, archive, Some(enrichmentCache), passStorage) {
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
      // One archive per run, in Mongo like everything else. The passes each get their
      // own database (see `replay`), so they still cannot tread on each other.
      val archive = storage.archive
      seedArchive(archive)

      // Concurrently: the passes are independent whole-corpus replays and running
      // them back-to-back made this the leg's long pole (three boots serially, on
      // top of the shared one). Same helper the fixture determinism specs use.
      val passes = ParallelReplays((0 until Passes).map(i => OrderSeed + i.toLong))(replay(archive, _))
      val (records0, screenings0, rows0) = passes.head
      info(s"${country.displayName}: $Passes passes over ${records0.size} films, " +
           s"${screenings0.values.map(_.size).sum} slots, ${rows0.size} rendered rows")
      records0 should not be empty
      rows0    should not be empty
      // NOTE: the screenings comparison below is currently VACUOUS. `screenings0` is empty
      // because `MongoConvergenceStorage` builds `movies` without the `screenings`/`slots`
      // read-split that `WorkerWiring` wires in prod, so showtimes stay embedded in the film
      // document and the side collection is never written. Two empty maps compare equal on
      // every pass, so this axis passes without testing anything. The films and rendered-rows
      // axes are real; only this one is not.
      //
      // Do NOT "fix" it by passing `screenings = Some(...), slots = Some(...)` — that was
      // tried (2026-07-31) and it empties the pipeline outright: the read model went to
      // 0 cinemas / 0 screenings / 0 films and an identical re-scrape churned 3,079 writes,
      // while `movies` still held all 773 films. The read-split is a protocol (write order,
      // re-stitch on read, change-stream fan-out), not two constructor arguments, and
      // turning it on here needs that protocol traced first.

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


  /**
   * What the pipeline identifies, against what PRODUCTION identifies for the same
   * repertoire.
   *
   * Every other assertion in this suite is self-referential: the corpus is a fixpoint
   * of itself, arrives order-independently, and loses nothing. All three hold
   * perfectly over a pipeline that has quietly stopped enriching — `requireEnrichmentReached`
   * exists because exactly that shipped, resolving 0 of 892 films with three specs
   * green. That guard is a floor of ONE, deliberately, so it catches a collapse and
   * nothing subtler. This is the assertion with an external reference: it fails when
   * the pipeline drifts away from what prod actually achieves, in either direction.
   *
   * BOTH directions, and the upward one is not paranoia. The harness's rating sweep
   * drove Filmweb for every country while production gates it to Poland, so the
   * German and British legs reported 972 and 1293 Filmweb ratings against prod's
   * zero — a fabricated number that read as enrichment. A one-sided band would have
   * called that a pass.
   *
   * The baseline ships with the corpus and is captured from the same connection at
   * the same instant (see `ProdCoverageBaseline`), so this needs no production
   * access and stays offline and reproducible like the rest of the suite.
   *
   * Being the assertion with an EXTERNAL reference is also the one way it can stop
   * being answerable: it is skipped, loudly, for a country production no longer
   * projects. See `productionIsLive` — that is a dead reference, not a wide one, so
   * the band itself is never relaxed and every live country is scored exactly as
   * before. A MISSING baseline still fails hard below; only a frozen one cancels.
   */
  s"the ${country.displayName} pipeline's coverage" should
    "stay within 5% of what production achieves on the same repertoire" in {
    TitleNormalizer.installRules(TitleRuleSet.forCountry(country))
    {
      val (w, _, _) = shared
      val baseline = ProdCoverageBaseline.read(corpusKey).getOrElse(
        // Loud, not skipped. A silent pass here would restore precisely the failure
        // this assertion exists to catch: the suite reporting green while nothing
        // checks the numbers it produces.
        fail(s"no production coverage baseline for ${country.code} — it is captured beside the corpus, so " +
             s"re-record both:\n  gh workflow run \"Record scrape fixtures\""))

      info(s"${country.displayName}: production baseline recorded ${baseline.recordedAt} — " +
           s"${baseline.films} films, tmdbId ${baseline.tmdbId}, imdbId ${baseline.imdbId}, " +
           s"imdbRating ${baseline.imdbRating}, filmwebRating ${baseline.filmwebRating}, " +
           s"metascore ${baseline.metascore}, rottenTomatoes ${baseline.rottenTomatoes}")

      val mine = coverageOf(w)
      // Printed whether or not it passes. A band that only speaks when it breaks hides
      // an axis drifting TOWARDS the line — Poland's identification sat at 5.0% of a 5%
      // band while the rating axes it feeds were the ones failing.
      info(s"${country.displayName}: coverage against production —\n  " +
           ProdCoverageBaseline.report(mine, baseline, ProdTolerance).mkString("\n  "))

      val offBand = ProdCoverageBaseline.divergences(mine, baseline, ProdTolerance)

      // Reported above either way; only the ASSERTION is conditional. A country
      // whose worker is stopped has a frozen baseline that the replay can no
      // longer be scored against (see `productionIsLive`), so the numbers stay in
      // the log to eyeball and the leg keeps every other claim it makes.
      if (!productionIsLive)
        cancel(
          s"${country.displayName} is not deployed, so its production baseline is frozen and this band " +
          s"cannot mean anything: prod stopped being projected while the archive kept the scrapes it had " +
          s"not yet folded, so the replay is expected to exceed it (currently ${offBand.size} axis/axes " +
          s"outside ${f"${100 * ProdTolerance}%.0f"}%). Every other assertion in this leg still ran. " +
          s"Restoring the country's webUrl re-arms this one.")

      withClue(
        s"${offBand.size} coverage axis/axes drifted from production by more than " +
        f"${100 * ProdTolerance}%.0f%%:\n${offBand.mkString("\n")}\n\n" +
        s"A regression here means this pipeline now identifies or rates less of the corpus than prod does " +
        s"(or MORE — a source prod does not run for this country). If prod itself moved, re-record the " +
        s"corpus and its baseline together.\n") {
        offBand shouldBe empty
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
      val settled  = w.movieCache.snapshot().map(r => w.movieCache.normalizer.sanitize(r.title)).toSet
      val emitted  = shownTitles.map(w.movieCache.normalizer.sanitize)
      val homeless = settled -- emitted
      withClue(s"${homeless.size} settled film(s) exist in the corpus but are emitted by nothing: " +
               s"${homeless.toList.sorted.take(10).mkString(", ")}\n") {
        homeless shouldBe empty
      }
    }
  }

}
