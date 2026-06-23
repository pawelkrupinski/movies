package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.MovieDetailsComplete
import tools.FixtureTestWiring

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/**
 * TEMPORAL idempotency / fixpoint guard — the axis `ScrapeOrderDeterminismSpec`
 * does NOT cover. That spec proves the settled corpus is identical regardless of
 * the order things happen *within one boot*. This one proves the settled corpus
 * is CHURN-FREE under repeated IDENTICAL scrape ticks: once the corpus has
 * settled, re-feeding the very same cinema fixtures (the next 5-min prod tick)
 * must NOT re-fold a row (every fold orphans a row's tmdbId-keyed freshness →
 * re-enrichment load, the 2026-06-19 re-key wave) and must NOT re-divert a known
 * film back into staging.
 *
 * The dominant flap this caught: a decorated edition (a programme-prefix screening
 * like "Plenerowe Pałacowe: Ścieżki życia", enriched off the base film via the
 * apiQuery prefix strip) carries the BASE title as a TMDB alias. `concludedKeyFor`
 * and the staging-divert gate used to match that alias WITHOUT the `isBareFilmTitle`
 * guard the settle's `groupByFilm` applies — so a bare "Ścieżki życia" scrape got
 * pulled onto the decorated row (canonicalRank's tiebreak), splitting the bare film
 * and re-diverting it every tick. Gating all three paths on the SAME
 * `FilmCanonicalizer.isBareFilmTitle` predicate fixed it (see MovieCache).
 *
 * Signals captured per tick, AFTER the corpus has settled:
 *   - merge churn: `MergeMetrics` increments during the tick's settle/fold,
 *   - staging diversions: a known film re-pushed into `pending_movies`.
 * Both must be zero once settled (asserted). Key-spelling drift (a settled title
 * re-cased by the scrape path) is reported informationally — no merge/freshness
 * cost — and tracked as a separate, smaller follow-up.
 */
@CorpusReplay // CI runs this heavy spec on its own parallel e2e shard — see CorpusReplay.java
class ReScrapeIdempotencySpec extends AnyFlatSpec with Matchers {

  private val Fixture = "08-06-2026"

  /** Counts merges by reason so a per-tick delta is observable. */
  private final class CountingMergeMetrics extends services.movies.MergeMetrics {
    private val counts = MergeReason.all.map(_ -> new AtomicInteger(0)).toMap
    def recordMerge(reason: MergeReason, victims: Int): Unit = counts(reason).addAndGet(victims)
    def total: Int = counts.values.map(_.get).sum
    def byReason: Map[MergeReason, Int] = counts.view.mapValues(_.get).toMap
  }

  private def keySet(w: FixtureTestWiring): Set[(String, Option[Int])] =
    w.movieCache.snapshot().map(r => (r.title, r.year)).toSet

  /** One production-shaped scrape tick that REPLICATES `runOneScrapeTick` but
   *  observes the staging sink right after the scrape phase (before it drains),
   *  so a known film re-diverted to `pending_movies` is visible. Returns the set
   *  of `(cinema, sanitize(title))` diversions this tick produced. */
  private def scrapeTickObservingStaging(w: FixtureTestWiring): Set[(String, String)] = {
    val stagingBefore = w.stagingRepository.findAll()
      .map(r => (r.cinema.displayName, services.movies.TitleNormalizer.sanitize(r.title))).toSet
    val ready = mutable.ListBuffer.empty[MovieDetailsComplete]
    w.cinemaScrapers.foreach { scraper =>
      try {
        val touched = w.movieCache.recordCinemaScrape(scraper.cinema, scraper.fetch())
        ready ++= w.cinemaScrapeRunner.classify(scraper.cinema, touched)
      } catch { case _: Exception => () }
    }
    val stagingAfter = w.stagingRepository.findAll()
      .map(r => (r.cinema.displayName, services.movies.TitleNormalizer.sanitize(r.title))).toSet
    w.enrichDetailsSync()
    ready.foreach(w.eventBus.publish)
    stagingAfter -- stagingBefore
  }

  /** A full settle tick: scrape (observing staging) + drain + fold + settle.
   *  Returns the staging diversions the scrape phase produced; merge churn is
   *  read separately off the injected `CountingMergeMetrics`. */
  private def settleTick(w: FixtureTestWiring): Set[(String, String)] = {
    val diversions = scrapeTickObservingStaging(w)
    w.drainServices()
    w.drainStaging()
    w.movieService.settle()
    diversions
  }

  /** Boot the real pipeline and settle to the steady state production reaches,
   *  with a `CountingMergeMetrics` wired into the cache so a later tick's fold
   *  churn is observable. `bootStartup` doesn't run the convergence collapse, so
   *  settle explicitly (twice, with a staging drain between) to reach the
   *  fixpoint — not a mid-settle transient. */
  private def bootSettled(): (FixtureTestWiring, CountingMergeMetrics) = {
    val merges = new CountingMergeMetrics
    val w = new FixtureTestWiring(Fixture) {
      override lazy val movieCache = new services.movies.CaffeineMovieCache(
        movieRepository, eventBus, staging = Some(stagingRepository),
        retrigger = enrichmentRetrigger, mergeMetrics = merges)
    }
    w.bootStartup()
    w.movieService.settle()
    w.drainStaging()
    w.movieService.settle()
    (w, merges)
  }

  // ONE boot shared by both tests below. The full-pipeline `bootSettled()` is
  // ~110s; booting it per-test made this the single heaviest e2e spec (two
  // boots ≈ 5.5 min) and the CI long pole. Sharing is order-independent and
  // safe BY CONSTRUCTION: both tests assert the settled corpus is a fixpoint
  // (an extra settle is a no-op; identical re-scrape ticks are churn-free), so
  // neither test mutates the shared key set — whichever runs first leaves the
  // corpus in exactly the state the other expects. `merges` deltas are captured
  // fresh inside each test, so a no-op pass can't pollute the other's baseline.
  private lazy val settled: (FixtureTestWiring, CountingMergeMetrics) = bootSettled()

  // ── Settle is idempotent ────────────────────────────────────────────────────
  // The cheapest temporal invariant: re-running the settle (`canonicalizeBySanitize`)
  // on an ALREADY-settled corpus must be a pure no-op — no fold, no re-key. A
  // settle that keeps finding rows to collapse means its own output isn't a
  // fixpoint of its own rule (the partition/canonical-key choice isn't stable),
  // which is the seam the re-scrape flap rode in on.
  "an already-settled corpus" should "be unchanged by a further settle pass (settle is idempotent)" in {
    val (w, merges) = settled
    val before       = keySet(w)
    val mergesBefore = merges.total
    info(s"settled corpus: ${before.size} films")

    w.movieService.settle()

    val after = keySet(w)
    withClue(
      s"a settle on a settled corpus folded ${merges.total - mergesBefore} row(s); " +
        s"keys APPEARED=${(after -- before).take(8).mkString(", ")} VANISHED=${(before -- after).take(8).mkString(", ")}\n") {
      (merges.total - mergesBefore) shouldBe 0
      after shouldBe before
    }
  }

  // ── Re-scrape is a no-op ─────────────────────────────────────────────────────
  "a settled corpus" should
    "be churn-free under an identical re-scrape: no re-divert of known films, no re-fold" in {
    val (w, merges) = settled
    val settledKeys = keySet(w)
    info(s"settled corpus: ${settledKeys.size} films")

    // Now run several IDENTICAL re-scrape ticks. We assert the prod-meaningful
    // CHURN invariant: once settled, an identical re-scrape must trigger NO
    // merges (each fold orphans a row's freshness → re-enrichment load) and NO
    // re-diversion of a known film back into staging. Key-spelling drift (a
    // settled title re-cased by the scrape path) is reported informationally —
    // it carries no merge/freshness cost and is tracked separately.
    val Ticks = 4
    val churn    = mutable.ListBuffer.empty[String]
    val keyDrift = mutable.ListBuffer.empty[String]
    (1 to Ticks).foreach { t =>
      val mergesBefore = merges.byReason
      val diversions = settleTick(w)
      val mergesDelta = MergeReason.all.map(r => r -> (merges.byReason(r) - mergesBefore(r))).filter(_._2 > 0)
      val keysNow = keySet(w)
      val added = keysNow -- settledKeys
      val removed = settledKeys -- keysNow

      mergesDelta.foreach { case (r, n) => churn += f"tick $t%d: ${n}%3d merge(s) reason=${r.label}" }
      if (diversions.nonEmpty)
        churn += s"tick $t: ${diversions.size} known film(s) RE-DIVERTED to staging: ${diversions.take(12).mkString(", ")}"
      if (added.nonEmpty)   keyDrift += s"tick $t: keys APPEARED: ${added.take(8).mkString(", ")}"
      if (removed.nonEmpty) keyDrift += s"tick $t: keys VANISHED: ${removed.take(8).mkString(", ")}"
    }

    if (keyDrift.nonEmpty)
      info(s"key-spelling drift (informational, no merge/freshness cost):\n${keyDrift.mkString("\n")}")

    withClue(
      s"A settled corpus must be churn-free under identical re-scrape, but merges/re-diversions were observed:\n" +
        s"${churn.mkString("\n")}\n") {
      churn.toList shouldBe empty
    }
  }
}
