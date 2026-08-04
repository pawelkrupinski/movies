package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.titlerules.TitleRuleSet

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Fast (in-memory, ms) reproduction of the re-scrape FLAPPING the heavy
 * `ReScrapeIdempotencySpec` found on the real corpus: once a film has settled to
 * its canonical `(cleanTitle, year)` key, re-feeding the SAME cinema scrapes (the
 * next prod tick) must leave the row untouched — same key, no merge/rekey churn.
 *
 * `CanonicalSpellingSpec` runs scrape → canonicalize ONCE, so it can't see a
 * flap that only shows on the SECOND identical tick. This drives the cycle
 * (scrape → canonicalize → re-scrape → canonicalize) and asserts the corpus is a
 * fixpoint, counting merges to catch churn invisible at the key-set boundary.
 */
class CanonicalKeyFixpointSpec extends AnyFlatSpec with Matchers {

  private final class CountingMergeMetrics extends MergeMetrics {
    private val n = new AtomicInteger(0)
    def recordMerge(reason: MergeReason, victims: Int): Unit = n.addAndGet(victims)
    def count: Int = n.get
  }

  private def cm(cinema: Cinema, title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title, releaseYear = year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), None))
    )

  private def keys(cache: MovieCache): Set[(String, Option[Int])] =
    cache.snapshot().map(r => (r.title, r.year)).toSet

  /** Settle a set of cinema reports, mark every row TMDB-concluded (as the boot's
   *  `concludeEnrichment` does for no-match films), then re-scrape the IDENTICAL
   *  reports and settle again. Returns (keys after first settle, keys after the
   *  re-scrape settle, merges during the re-scrape tick). */
  private def reScrape(reports: Seq[CinemaMovie]): (Set[(String, Option[Int])], Set[(String, Option[Int])], Int) =
    reScrapeFull(reports)._1

  /** The cycle, plus the two things the key set cannot show: how many WRITES the settled
   *  tick performed, and whether the corpus still holds its showtimes.
   *
   *  Returns ((settledKeys, afterKeys, merges), writesOnSettledTick, showtimesAfter). */
  private def reScrapeFull(reports: Seq[CinemaMovie])
      : ((Set[(String, Option[Int])], Set[(String, Option[Int])], Int), Int, Int) = {
    val merges     = new CountingMergeMetrics
    // Screenings WIRED: the production storage split, so a fold that strands a film's
    // showtimes under a retired id is visible here instead of only against real Mongo.
    val screenings = new InMemoryScreeningsRepository
    val repository = new InMemoryMovieRepository(screenings = Some(screenings))
    val cache      = new CaffeineMovieCache(repository, mergeMetrics = merges, normalizer = titleNormalizer)
    reports.foreach(r => cache.recordCinemaScrape(r.cinema, Seq(r)))
    cache.canonicalizeBySanitize()
    // Mark concluded (no-TMDB), the state the real flapping films are in.
    cache.snapshot().foreach(sr => cache.put(cache.keyOf(sr.title, sr.year), sr.record.copy(tmdbNoMatch = true)))
    cache.canonicalizeBySanitize()
    val settled = keys(cache)
    // Measured HERE — immediately after the settle and BEFORE the re-scrape tick, which
    // would re-supply showtimes and mask a settle that had just thrown them away. That
    // masking is why the first version of this assertion passed against the bug.
    val showtimesAfterSettle = screenings.findAll().values.flatMap(_.values).map(_.size).sum
    val mergesBefore  = merges.count
    val writesBefore  = repository.upserts.size + repository.deletes.size
    reports.foreach(r => cache.recordCinemaScrape(r.cinema, Seq(r)))
    cache.canonicalizeBySanitize()
    val writes = repository.upserts.size + repository.deletes.size - writesBefore
    ((settled, keys(cache), merges.count - mergesBefore), writes, showtimesAfterSettle)
  }

  "a settled casing-variant film" should "be a fixpoint under an identical re-scrape" in {
    // All-caps variant sorts FIRST by raw cleanTitle (0x4F 'O' < 0x6F 'o'), so
    // canonicalRank prefers "ZOO" while canonical() (isAllCaps) prefers "Zoo".
    val ((settled, after, merges), writes, showtimes) = reScrapeFull(Seq(
      cm(Helios,   "Zoo", Some(2026)),
      cm(KinoMuza, "ZOO", Some(2026))))
    withClue(s"settled=$settled  afterReScrape=$after  merges=$merges  writes=$writes  showtimes=$showtimes\n") {
      after shouldBe settled
      merges shouldBe 0
      writes shouldBe 0
      showtimes should be > 0
    }
  }

  "a settled year/yearless film" should "be a fixpoint under an identical re-scrape" in {
    val ((settled, after, merges), writes, showtimes) = reScrapeFull(Seq(
      cm(Helios,   "Sycamore", Some(2025)),
      cm(KinoMuza, "Sycamore", None)))
    withClue(s"settled=$settled  afterReScrape=$after  merges=$merges  writes=$writes  showtimes=$showtimes\n") {
      after shouldBe settled
      merges shouldBe 0
      writes shouldBe 0
      showtimes should be > 0
    }
  }

  // THE two blind spots this spec had, stated as their own cases so a future refactor
  // cannot quietly drop them.
  //
  // 1. QUIESCENCE, not just convergence. The spec asserted the key SET and the MERGE
  //    count. Prod satisfied both while deleting and re-inserting 735 of 941 rows every
  //    30 minutes under IDENTICAL ids (measured 2026-07-27: merges 0-2/30min, deletes
  //    735/30min, id set byte-identical across the burst). A converged corpus that
  //    rewrites itself forever is not settled — so count the writes.
  //
  // 2. The corpus must still HOLD its showtimes. With screenings inline (the old fake)
  //    a fold unions the records and carries them for free; under the production split
  //    they live under the film id, and a rename that doesn't move them destroys them.
  "an identical re-scrape of a settled corpus" should "perform NO writes and keep every showtime" in {
    val ((_, _, merges), writes, showtimes) = reScrapeFull(Seq(
      cm(Helios,   "Quiescent", Some(2026)),
      cm(KinoMuza, "QUIESCENT", Some(2026)),
      cm(KinoMuranow, "Quiescent", None)))
    withClue(s"a settled corpus rewrote itself: writes=$writes merges=$merges\n")(writes shouldBe 0)
    withClue(s"the settle lost the corpus's showtimes: $showtimes\n")(showtimes should be > 0)
  }

  "a bare scrape" should "fold a decorated edition sharing the base tmdbId into one record (split into cards in display)" in {
    // The "Ścieżki życia" case. A decorated edition (Plenerowe Pałacowe: …) is
    // enriched off the base film via the apiQuery programme-prefix strip, so it
    // carries the base tmdbId — it is the SAME film and now folds onto ONE record;
    // the read-model projection splits it back into its own CARD by shown title.
    // Both shown titles survive as cinema slots so the split can recover them.
    val tmdbId = 1127625
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository, normalizer = titleNormalizer)
    cache.put(cache.keyOf("Ścieżki życia", Some(2025)),
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        (Tmdb: Source)   -> SourceData(title = Some("Ścieżki życia"), releaseYear = Some(2025)),
        (Helios: Source) -> SourceData(title = Some("Ścieżki życia"), releaseYear = Some(2025)))))
    cache.put(cache.keyOf("Plenerowe Pałacowe: Ścieżki życia", Some(2025)),
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        (Tmdb: Source)     -> SourceData(title = Some("Ścieżki życia"), releaseYear = Some(2025)),
        (KinoMuza: Source) -> SourceData(title = Some("Plenerowe Pałacowe: Ścieżki życia"), releaseYear = Some(2025)))))

    // A bare re-scrape from another venue.
    cache.recordCinemaScrape(KinoMuranow, Seq(cm(KinoMuranow, "Ścieżki życia", Some(2025))))
    cache.canonicalizeBySanitize()

    // ONE record (same tmdbId), keyed on the dominant bare title — both shown titles
    // survive as cinema slots, and the bare re-scrape lands on the merged record.
    val snap = cache.snapshot()
    withClue(s"rows: ${snap.map(r => (r.title, r.year))}\n")(snap should have size 1)
    val rec = snap.head.record
    rec.tmdbId        shouldBe Some(tmdbId)
    rec.cinemaTitles  should contain allOf ("Ścieżki życia", "Plenerowe Pałacowe: Ścieżki życia")
    rec.cinemaData.keySet should contain (KinoMuranow: Cinema)
  }

  "a re-scrape of a resolved film at an off-by-2 year" should
    "keep the authoritative TMDB-derived key, not re-case it to the cinema's raw spelling" in {
    // The real "Północ, północny zachód" (North by Northwest, tmdbId=213) flap.
    // Its only cinema (Kino Muzeum) lists it ALL-CAPS at a 2-years-off year (1957),
    // while TMDB resolved it as "Północ, północny zachód" / 1959. The settled row
    // therefore keys on the properly-cased TMDB title at 1959. On the next tick the
    // cinema's 1957 is >±1 off, so `concludedKeyFor` misses and the redirect path
    // runs — it must NOT promote the resolved row onto the cinema's ALL-CAPS / 1957
    // key. A resolved row's key is authoritative.
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository, normalizer = titleNormalizer)
    cache.put(cache.keyOf("Północ, północny zachód", Some(1959)),
      MovieRecord(tmdbId = Some(213), data = Map[Source, SourceData](
        (Tmdb: Source)     -> SourceData(title = Some("Północ, północny zachód"), releaseYear = Some(1959)),
        (KinoMuza: Source) -> SourceData(title = Some("PÓŁNOC, PÓŁNOCNY ZACHÓD"), releaseYear = Some(1957)))))

    cache.recordCinemaScrape(KinoMuza, Seq(cm(KinoMuza, "PÓŁNOC, PÓŁNOCNY ZACHÓD", Some(1957))))

    val rows = cache.snapshot().map(r => (r.title, r.year)).toSet
    withClue(s"rows after re-scrape: $rows\n") {
      rows shouldBe Set(("Północ, północny zachód", Some(1959)))
    }
  }

  "a settled German title containing ' & '" should "be a fixpoint under an identical re-scrape" in {
    // The German "Minions & Monster" flap, from the recorded CinemaxX Würzburg
    // (Filmstarts theater A0263) payload. The canonical " & " → " i " rule is POLISH
    // and used to run everywhere, keying this German row `minionsimonster` — a key
    // its own cinema slot can never produce, so each settle re-canonicalised it and
    // orphaned its screenings (25 showtimes vanishing for 32 min of every hour in
    // prod; see DzienObjawieniaFlapSpec for the full trace).
    //
    // Under the country-scoped rule set the row keys on the cinema's own spelling
    // and the hourly re-scrape lands on it rather than forking a second row.
    val german    = new TitleNormalizer(TitleRuleSet.forCountry(Country.Germany))
    val wuerzburg = new GermanCinema("CinemaxX Würzburg", "CinemaxX Würzburg")
    val cache     = new CaffeineMovieCache(
      new InMemoryMovieRepository(normalizer = german), normalizer = german)
    cache.put(cache.keyOf("Minions & Monster", None),
      MovieRecord(tmdbId = Some(1315772), data = Map[Source, SourceData](
        (Tmdb: Source)      -> SourceData(title = Some("Minions & Monster")),
        (wuerzburg: Source) -> SourceData(title = Some("Minions & Monster")))))

    cache.recordCinemaScrape(wuerzburg, Seq(CinemaMovie(
      movie     = Movie("Minions & Monster", originalTitle = Some("Minions & Monsters")),
      cinema    = wuerzburg,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 7, 11, 11, 30), None)))))

    val rows = cache.snapshot().map(r => (r.title, r.year)).toSet
    withClue(s"rows after re-scrape: $rows\n") {
      rows shouldBe Set(("Minions & Monster", None))
    }
  }

  "a settled programme-prefix film" should "be a fixpoint under an identical re-scrape" in {
    // The Plenerowe Pałacowe / Filmowy Klub Seniora shape: a decorated edition
    // some cinemas report with a year and some without.
    val (settled, after, merges) = reScrape(Seq(
      cm(Helios,        "Plenerowe Pałacowe: Ścieżki życia", Some(2025)),
      cm(KinoMuza,      "Plenerowe Pałacowe: Ścieżki życia", None),
      cm(KinoMuranow,   "PLENEROWE PAŁACOWE: ŚCIEŻKI ŻYCIA", None)))
    withClue(s"settled=$settled  afterReScrape=$after  merges=$merges\n") {
      after shouldBe settled
      merges shouldBe 0
    }
  }
}
