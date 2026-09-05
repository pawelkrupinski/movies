package services.tasks

import models.{CinemaMovie, Country, KinoApollo, Movie, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import services.schedule.{InMemoryScheduledRunStore, NeverClaimScheduledRunStore}
import services.resolution.TmdbBasis

import java.time.{Instant, LocalDateTime}
import scala.collection.mutable
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

class UnresolvedTmdbReaperSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-18T00:00:00Z").toEpochMilli

  private def newCache() = new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus(), normalizer = titleNormalizer)

  private def seedRow(cache: CaffeineMovieCache, title: String)(edit: MovieRecord => MovieRecord): Unit = {
    cache.recordCinemaScrape(KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
  }

  /** A recording retry seam — the reaper owns the schedule/gate; the per-row
   *  re-try (clear negative + dispatch) is `MovieService.retryResolve` in prod. */
  private def recorder(): (mutable.Buffer[CacheKey], CacheKey => Unit) = {
    val seen = mutable.Buffer.empty[CacheKey]
    (seen, key => { seen += key; () })
  }

  /** Drive a full period of contiguous 5-min ticks (the default tick interval),
   *  returning the per-tick re-try counts. */
  private def runOnePeriod(reaper: UnresolvedTmdbReaper, period: FiniteDuration = 24.hours): Seq[Int] = {
    val delta = UnresolvedTmdbReaper.DefaultTickInterval
    val ticks = (period.toMillis / delta.toMillis).toInt
    (1 to ticks).map(k => reaper.tick(t0 + k * delta.toMillis))
  }

  // ── eligibility (independent of phase: assert over a whole period) ────────────

  "UnresolvedTmdbReaper" should "re-try an unresolved row exactly once per period" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity) // tmdbId = None → unresolved
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 1
    seen.map(_.cleanTitle).distinct should contain only cache.keyOf("Bare", None).cleanTitle
  }

  it should "never re-try a TMDB-resolved row" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Resolved")(_.copy(tmdbId = Some(42)))
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  // The third kind. Prod 2026-09-05: five rows sat resolved to a film their OWN
  // venues contradicted — "Vivaldi i ja" on an 18-minute STABAT MATER concert
  // short while 46 venues advertised 110 minutes and named Damiano Michieletto;
  // "Das Phantom der Oper" on the 1925 silent while 10 venues published 2004,
  // 140 minutes and Joel Schumacher. `resolveTmdbId` never re-runs once a tmdbId
  // is set, so each was permanent — every one of them needed a hand repair. The
  // evidence to catch them was on the rows the whole time.
  it should "force a re-resolve when the film's runtime is a fraction of what the venues advertise" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Vivaldi i ja") { r =>
      r.copy(tmdbId = Some(1667002),
        data = r.data
          + ((Tmdb: Source) -> SourceData(title = Some("STABAT MATER RV621"), runtimeMinutes = Some(18)))
          + ((KinoApollo: Source) -> SourceData(title = Some("Vivaldi i ja"), runtimeMinutes = Some(110))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 1
    forced.map(_.cleanTitle) should contain(cache.keyOf("Vivaldi i ja", None).cleanTitle)
  }

  it should "force a re-resolve when no venue names any of the resolved film's directors" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Das Phantom der Oper") { r =>
      r.copy(tmdbId = Some(964),
        data = r.data
          + ((Tmdb: Source) -> SourceData(title = Some("Das Phantom der Oper"), runtimeMinutes = Some(93),
               director = Seq("Rupert Julian")))
          + ((KinoApollo: Source) -> SourceData(title = Some("Das Phantom der Oper"), runtimeMinutes = Some(140),
               director = Seq("Joel Schumacher"))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 1
    forced should have size 1
  }

  // S2: a conclusion reached on a bare title, on a row that has SINCE acquired a
  // director or a year, is weaker than the evidence now available. Nothing made it
  // look again — `needsTmdbResolution` re-verifies only when the triggering event
  // carries a director, and a later detail refresh publishes none. So the row keeps
  // a guess it could now improve on, which is every one of the five prod films.
  it should "force a re-resolve when a title-only guess now has a director to check" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Homo sapiens") { r =>
      r.copy(tmdbId = Some(891655), tmdbBasis = Some(TmdbBasis.TitleOnly.toString),
        data = r.data
          + ((Tmdb: Source) -> SourceData(title = Some("Homo sapiens"), runtimeMinutes = Some(95),
               director = Seq("Mariano Cohn")))
          + ((KinoApollo: Source) -> SourceData(title = Some("Homo sapiens"), runtimeMinutes = Some(95),
               director = Seq("Mariano Cohn"))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 1
    forced should have size 1
  }

  it should "leave a title-only guess alone while nothing better has arrived" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Still Bare") { r =>
      r.copy(tmdbId = Some(42), tmdbBasis = Some(TmdbBasis.TitleOnly.toString),
        data = r.data + ((Tmdb: Source) -> SourceData(title = Some("Still Bare"), runtimeMinutes = Some(100))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "not keep re-resolving a row already concluded on strong evidence" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Verified") { r =>
      r.copy(tmdbId = Some(7), tmdbBasis = Some(TmdbBasis.DirectorWalk.toString),
        data = r.data
          + ((Tmdb: Source) -> SourceData(title = Some("Verified"), runtimeMinutes = Some(100), director = Seq("A Director")))
          + ((KinoApollo: Source) -> SourceData(title = Some("Verified"), runtimeMinutes = Some(100), director = Seq("A Director"))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "leave a row alone when the venues corroborate it" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Lalka") { r =>
      r.copy(tmdbId = Some(1321666),
        data = r.data
          + ((Tmdb: Source) -> SourceData(title = Some("Lalka"), runtimeMinutes = Some(162),
               director = Seq("Maciej Kawalski")))
          // Multikino advertises this 162-minute film at 147 and names its director.
          + ((KinoApollo: Source) -> SourceData(title = Some("Lalka"), runtimeMinutes = Some(147),
               director = Seq("Maciej Kawalski"))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 0
    forced shouldBe empty
  }

  // Abstention is the whole discipline here: a venue that publishes nothing must
  // never be read as disagreement, or the sweep force-re-resolves the corpus.
  it should "leave a row alone when the venues published nothing to contradict it" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedRow(cache, "Silent") { r =>
      r.copy(tmdbId = Some(42),
        data = r.data + ((Tmdb: Source) -> SourceData(title = Some("Silent"), runtimeMinutes = Some(120),
          director = Seq("Someone"))))
    }
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "never re-try a row still awaiting detail enrichment" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Pending")(_.copy(detailPending = true)) // unresolved but detail owed
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry)).sum shouldBe 0
    seen shouldBe empty
  }

  // ── stale enrichment language ────────────────────────────────────────────────
  // `fullDetails` is fetched only at resolve time, so a row resolved before its
  // deployment enriched in its own language stays frozen — Berlin showed the Polish
  // "Familijny, Komedia, Przygodowy" for Der Super Mario Galaxy Film. These pin the
  // detect-and-force-re-resolve behaviour that unfreezes it.

  /** A resolved row whose Tmdb slot carries `genres`/`title` fetched under `language`
   *  (`None` = unstamped, i.e. written before the stamp existed → legacy pl-PL). */
  private def seedResolvedTmdbRow(
    cache: CaffeineMovieCache, title: String, genres: Seq[String], language: Option[String]
  ): Unit =
    seedRow(cache, title)(r => r.copy(
      tmdbId = Some(42),
      data   = r.data + ((Tmdb: Source) -> SourceData(title = Some(title), genres = genres, language = language))))

  it should "force a re-resolve of a row whose Tmdb slot was enriched in another language" in {
    val cache = newCache(); val (retried, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedResolvedTmdbRow(cache, "Der Super Mario Galaxy Film", Seq("Familijny", "Komedia"), Some("pl-PL"))
    val reaper = new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Germany)
    runOnePeriod(reaper).sum shouldBe 1
    // The row IS resolved, so the plain retry seam would no-op — it must take the forced path.
    forced.map(_.cleanTitle) shouldBe Seq(cache.keyOf("Der Super Mario Galaxy Film", None).cleanTitle)
    retried shouldBe empty
  }

  it should "treat an UNSTAMPED Tmdb slot as the legacy Polish enrichment" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    // Written before the language stamp existed — the rows this fix has to reach.
    seedResolvedTmdbRow(cache, "Die Odyssee", Seq("Przygodowy", "Akcja"), language = None)
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Germany))
    forced.map(_.cleanTitle) shouldBe Seq(cache.keyOf("Die Odyssee", None).cleanTitle)
  }

  it should "leave an unstamped Tmdb slot alone on the Polish deployment" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    // The whole PL corpus is unstamped and already correct — it must not churn.
    seedResolvedTmdbRow(cache, "Chłopi", Seq("Dramat", "Historyczny"), language = None)
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Poland)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "leave a row already enriched in the deployment's own language alone" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    seedResolvedTmdbRow(cache, "Die Odyssee", Seq("Abenteuer", "Action"), Some("de-DE"))
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Germany)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "not force a Tmdb slot that carries no localized text to re-resolve" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    // A slot the details fetch never filled has nothing stale to correct; forcing it
    // would re-run a search that already concluded.
    seedRow(cache, "Empty Slot")(r => r.copy(
      tmdbId = Some(7), data = r.data + ((Tmdb: Source) -> SourceData(language = None))))
    runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Germany)).sum shouldBe 0
    forced shouldBe empty
  }

  it should "spread a wrong-language backlog across the period rather than bursting" in {
    val cache = newCache(); val (_, retry) = recorder(); val (forced, forceRetry) = recorder()
    // The real DE shape: a whole corpus frozen at once by a single pre-fix deploy.
    (0 until 120).foreach(i => seedResolvedTmdbRow(cache, f"Stale$i%03d", Seq("Komedia"), None))
    val perTick = runOnePeriod(new UnresolvedTmdbReaper(cache, retry, forceRetry = forceRetry, country = Country.Germany))
    perTick.sum shouldBe 120
    forced.size shouldBe 120
    perTick.max should be <= 6 // a flat trickle, not one spike that pins the CPU credit
  }

  // ── the spread property ──────────────────────────────────────────────────────

  it should "spread a synchronized unresolved backlog across the period rather than bursting" in {
    val cache = newCache(); val (_, retry) = recorder()
    val n = 150
    (0 until n).foreach(i => seedRow(cache, f"Stuck$i%03d")(identity)) // all unresolved, all "born" together
    val reaper = new UnresolvedTmdbReaper(cache, retry)
    val perTick = runOnePeriod(reaper)

    perTick.sum shouldBe n                  // every unresolved row re-tried exactly once over the period
    perTick.max should be <= 20             // no burst (the old daily sweep put all 150 in one tick)
    perTick.count(_ > 0) should be >= 100   // genuinely spread across most ticks
  }

  // ── recovery-burst cap ───────────────────────────────────────────────────────

  it should "cap re-tries per tick so a bunched backlog drains over several ticks" in {
    val cache = newCache(); val (_, retry) = recorder()
    (0 until 50).foreach(i => seedRow(cache, f"Stuck$i%03d")(identity))
    // period == tickInterval → every unresolved row is due on every tick, so the
    // cap is what bounds the batch (the leftover stays due next tick).
    val reaper = new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval), maxEnqueuePerTick = 10)
    reaper.tick(t0) shouldBe 10
    reaper.tick(t0 + UnresolvedTmdbReaper.DefaultTickInterval.toMillis) shouldBe 10
  }

  // ── cluster occurrence claim ─────────────────────────────────────────────────

  "UnresolvedTmdbReaper.tickIfClaimed" should "not re-try when another machine claimed the window" in {
    val cache = newCache(); val (seen, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval),
      runStore = NeverClaimScheduledRunStore).tickIfClaimed() shouldBe 0
    seen shouldBe empty
  }

  it should "re-try when it wins the claim" in {
    val cache = newCache(); val (_, retry) = recorder()
    seedRow(cache, "Bare")(identity)
    new UnresolvedTmdbReaper(cache, retry,
      dueWindow = new DueWindow(UnresolvedTmdbReaper.DefaultTickInterval),
      runStore = new InMemoryScheduledRunStore).tickIfClaimed() shouldBe 1
  }
}
