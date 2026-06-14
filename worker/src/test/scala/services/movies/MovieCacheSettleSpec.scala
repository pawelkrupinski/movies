package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Drives `MovieCache.canonicalizeBySanitize` directly over hand-built corpora
 * to pin the ±1-year film-identity collapse the settle/hydrate pass performs.
 *
 * A single `sanitize(title)` group can legitimately cover several films — a
 * remake carrying the original's name, or one film cinemas date a year apart
 * (production vs theatrical). The old rule BAILED on any group holding >1
 * distinct tmdbId (left it fully split) and otherwise collapsed the WHOLE group
 * to one row — so it could neither separate two genuine films nor cap an
 * unbounded run of adjacent years. The new rule sub-clusters per film: resolved
 * rows split by tmdbId, year-bearing unresolved rows attach within ±1 of a
 * resolved cluster, the rest pack into greedy 2-year windows, and yearless rows
 * fold into the canonical cluster. All deterministic — a pure function of the
 * row set. (`CaffeineMovieCache` + `InMemoryMovieRepository` live in worker test
 * scope, beside `CanonicalSpellingSpec`/`CorpusSettleSpec`, so this spec sits
 * here rather than in common's testkit-less test config.)
 */
class MovieCacheSettleSpec extends AnyFlatSpec with Matchers {

  private def cache = new CaffeineMovieCache(new InMemoryMovieRepository)

  // A cinema-only (unresolved) row: one cinema slot at the given year, no tmdbId.
  private def cinemaRow(c: MovieCache, title: String, cinema: Cinema, year: Option[Int]): Unit =
    c.put(c.keyOf(title, year),
      MovieRecord(data = Map[Source, SourceData]((cinema: Source) -> SourceData(title = Some(title), releaseYear = year))))

  // A TMDB-resolved row: a Tmdb slot carries the resolved year + the given
  // tmdbId, plus one cinema slot.
  private def resolvedRow(c: MovieCache, title: String, cinema: Cinema, year: Int, tmdbId: Int): Unit =
    c.put(c.keyOf(title, Some(year)),
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        (Tmdb: Source)    -> SourceData(title = Some(title), releaseYear = Some(year)),
        (cinema: Source)  -> SourceData(title = Some(title), releaseYear = Some(year)))))

  private def rows(c: MovieCache): Set[(String, Option[Int])] =
    c.snapshot().map(r => (r.title, r.year)).toSet

  "canonicalizeBySanitize" should
    "collapse same-title adjacent-year rows sharing ONE tmdbId into a single row" in {
    val c = cache
    resolvedRow(c, "Dzień objawienia", Helios,   2025, 1275779)
    // A second cinema reports the theatrical year a year off — unresolved, so it
    // sits as a separate row until settle. The ±1 collapse must fold it onto the
    // resolved sibling's TMDB year.
    cinemaRow(c, "Dzień objawienia", KinoMuza, Some(2026))
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    r.head.year shouldBe Some(2025)                  // TMDB's resolved year
    r.head.record.tmdbId shouldBe Some(1275779)
    r.head.record.cinemaData.keySet shouldBe Set(Helios, KinoMuza)
  }

  it should "keep same-title rows resolved to DISTINCT tmdbIds as two separate rows" in {
    val c = cache
    resolvedRow(c, "Diuna", Helios,   1984, 100)  // the 1984 adaptation
    resolvedRow(c, "Diuna", KinoMuza, 2021, 200)  // the 2021 remake
    c.canonicalizeBySanitize()
    rows(c) shouldBe Set(("Diuna", Some(1984)), ("Diuna", Some(2021)))
  }

  it should "split same-title no-tmdbId years {2024,2025,2026} into exactly two ≤2-year clusters" in {
    val c = cache
    cinemaRow(c, "Festiwal", Helios,                Some(2024))
    cinemaRow(c, "Festiwal", KinoMuza,              Some(2025))
    cinemaRow(c, "Festiwal", CinemaCityPoznanPlaza, Some(2026))
    c.canonicalizeBySanitize()
    // Greedy window from the lowest year: {2024,2025} then {2026}. NEVER one
    // 3-year blob, NEVER {2025,2026} (which a chain-from-2025 would give).
    rows(c) shouldBe Set(("Festiwal", Some(2024)), ("Festiwal", Some(2026)))
  }

  it should "absorb a no-tmdbId year-bearing row within ±1 of a resolved cluster's TMDB year" in {
    val c = cache
    resolvedRow(c, "Erupcja", Helios,   2026, 555)
    cinemaRow (c, "Erupcja", KinoMuza, Some(2025))  // production year, ±1 of 2026
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    r.head.year shouldBe Some(2026)                 // collapses onto TMDB's year
    r.head.record.tmdbId shouldBe Some(555)
    r.head.record.cinemaData.keySet shouldBe Set(Helios, KinoMuza)
  }

  it should "NOT absorb a no-tmdbId year-bearing row two or more years off a resolved cluster" in {
    val c = cache
    resolvedRow(c, "Sny o słoniach", Helios,   2026, 777)
    cinemaRow (c, "Sny o słoniach", KinoMuza, Some(2023))  // 3 years off → its own cluster
    c.canonicalizeBySanitize()
    rows(c) shouldBe Set(("Sny o słoniach", Some(2026)), ("Sny o słoniach", Some(2023)))
  }

  it should "fold a yearless+idless row into the resolved canonical of the same title" in {
    val c = cache
    resolvedRow(c, "Bez wyjścia", Helios,   2026, 999)
    cinemaRow (c, "Bez wyjścia", KinoMuza, None)  // no year, no tmdbId
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    r.head.year shouldBe Some(2026)
    r.head.record.tmdbId shouldBe Some(999)
    r.head.record.cinemaData.keySet shouldBe Set(Helios, KinoMuza)
  }

  it should "leave a LONE yearless+idless row as its own row (not delete it)" in {
    val c = cache
    cinemaRow(c, "Maraton horrorów", KinoMuza, None)
    c.canonicalizeBySanitize()
    rows(c) shouldBe Set(("Maraton horrorów", None))
  }
}
