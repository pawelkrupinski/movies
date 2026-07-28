package services.movies

import models.{Cinema, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.MovieDetailsComplete
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository, ScrapeAttempt}
import services.titlerules.TitleRuleSet
import tools.{ArchiveReplayWiring, CountryScrapeCorpus, Env, IsolatedMongoDatabase}

import java.time.{Instant, LocalDateTime}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
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
 *      is the discriminator.
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
 */
abstract class CountryConvergenceBehaviour(country: Country) extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  /** Bound on re-scrape ticks before we declare the corpus non-convergent. */
  private val MaxTicks = 12

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

  /** Seed the archive with this country's corpus, exactly as a real scrape would
   *  have filed it — through `ScrapeAttempt`, so the archive's own "content only"
   *  rule and its BSON round-trip are both on the path to the pipeline. */
  private def seedArchive(archive: ScrapeArchiveRepository): Int = {
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
    IsolatedMongoDatabase.withDatabase(Env.get("MONGODB_URI").get, s"convergence-${country.code}") { database =>
      val archive = new MongoScrapeArchiveRepository(Some(database))
      val seeded  = seedArchive(archive)

      val merges = new CountingMergeMetrics
      val w = new ArchiveReplayWiring(country, archive) {
        override lazy val movieCache = new CaffeineMovieCache(
          movieRepository, eventBus, staging = Some(stagingRepository),
          retrigger = enrichmentRetrigger, mergeMetrics = merges)
      }

      withClue(s"the archive round-trip lost cinemas: seeded $seeded, replayed ${w.cinemaScrapers.size}\n") {
        w.cinemaScrapers.size shouldBe seeded
      }
      info(s"${country.displayName}: $seeded cinemas replayed from cinema_scrapes, " +
           s"${w.archivedListings.values.map(_.size).sum} film listings")

      // Boot to the steady state production reaches. `bootCorpus` doesn't run the
      // convergence collapse, so settle explicitly (twice, with a staging drain
      // between) to reach the fixpoint rather than a mid-settle transient.
      w.bootCorpus()
      w.movieService.settle()
      w.drainStaging()
      w.movieService.settle()

      // ── 1) The settle is a fixpoint of itself ────────────────────────────────
      val before        = keySet(w)
      val cinemasBefore = cinemasByFilm(w)
      val mergesBefore  = merges.total
      info(s"${country.displayName}: settled corpus of ${before.size} films")
      before should not be empty

      val emissions = new AtomicInteger(0)
      w.movieRepository.watchChanges(_ => { emissions.incrementAndGet(); () }, _ => { emissions.incrementAndGet(); () })

      w.movieService.settle()
      w.movieCache.canonicalizeBySanitize()

      val after        = keySet(w)
      val cinemasAfter = cinemasByFilm(w)
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
      withClue(s"a settle on a ${country.displayName} corpus that has cleared staging wrote " +
               s"${emissions.get} time(s) — it should have had nothing to do\n") {
        emissions.get shouldBe 0
      }

      // ── 2+3) Identical re-scrapes are churn-free and reach a fixpoint ─────────
      val settledKeys = before
      val rnd         = new Random(0x2026_07_28L)
      val churn       = mutable.ListBuffer.empty[String]
      val keyDrift    = mutable.ListBuffer.empty[String]
      val perTick     = mutable.ListBuffer.empty[Int]
      var consecutiveZeroEmission = 0
      var t = 0
      while (consecutiveZeroEmission < 2 && t < MaxTicks) {
        t += 1
        val mergesBeforeTick    = merges.byReason
        val emissionsBeforeTick = emissions.get
        val diversions   = settleTick(w, rnd)
        val mergesDelta  = MergeReason.all.map(r => r -> (merges.byReason(r) - mergesBeforeTick(r))).filter(_._2 > 0)
        val emissionsDelta = emissions.get - emissionsBeforeTick
        val keysNow  = keySet(w)
        val appeared = keysNow -- settledKeys
        val vanished = settledKeys -- keysNow

        perTick += emissionsDelta
        consecutiveZeroEmission = if (emissionsDelta == 0) consecutiveZeroEmission + 1 else 0
        mergesDelta.foreach { case (r, n) => churn += f"tick $t%d: $n%3d merge(s) reason=${r.label}" }
        if (diversions.nonEmpty)
          churn += s"tick $t: ${diversions.size} known film(s) RE-DIVERTED to staging: ${diversions.take(12).mkString(", ")}"
        if (appeared.nonEmpty) keyDrift += s"tick $t: keys APPEARED: ${appeared.take(8).mkString(", ")}"
        if (vanished.nonEmpty) keyDrift += s"tick $t: keys VANISHED: ${vanished.take(8).mkString(", ")}"
      }
      info(s"${country.displayName}: per-tick change-stream emissions until fixpoint: ${perTick.mkString(", ")}")
      if (keyDrift.nonEmpty)
        info(s"${country.displayName}: key-spelling drift (informational):\n${keyDrift.mkString("\n")}")

      withClue(
        s"A settled ${country.displayName} corpus must not re-fold or re-divert under identical " +
          s"re-scrape, but:\n${churn.mkString("\n")}\n") {
        churn.toList shouldBe empty
      }
      withClue(
        s"${country.displayName} never reached a two-tick emission-free fixpoint within $MaxTicks ticks " +
          s"(per-tick emissions: ${perTick.mkString(", ")}) — something is rewritten on every tick.\n") {
        consecutiveZeroEmission should be >= 2
      }
    }
  }
}
