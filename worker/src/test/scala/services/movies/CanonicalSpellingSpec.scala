package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * Fast guard for the order-dependent STORED SPELLING flake the whole-corpus
 * determinism spec surfaced. `CacheKey` equality is by `sanitize(cleanTitle)`
 * + year (case/separator-insensitive), so the same film reported by different
 * cinemas under different casings ("Władcy Wszechświata" vs "Władcy
 * wszechświata" vs "WŁADCY WSZECHŚWIATA") — some with a year, some without —
 * collapses into one row whose surviving title string used to depend on which
 * cinema scraped first. `canonicalizeBySanitize` must re-assert the canonical
 * (min `canonicalRank`) spelling deterministically. Runs in‑memory, no TMDB,
 * so it's milliseconds — iterate here, not on the 1000‑film corpus.
 */
class CanonicalSpellingSpec extends AnyFlatSpec with Matchers {

  private def cm(cinema: Cinema, title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title, releaseYear = year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), None))
    )

  // One film, four reported variants: two casings at the present year, plus two
  // yearless casings. Canonical = min canonicalRank = ("Władcy Wszechświata",
  // Some(2026)) (year-present beats yearless; "W" sorts before "w").
  private val variants = Seq(
    cm(CinemaCityPoznanPlaza, "Władcy Wszechświata", Some(2026)),
    cm(Helios,                "Władcy wszechświata", Some(2026)),
    cm(KinoMuza,              "Władcy wszechświata", None),
    cm(KinoMuranow,           "WŁADCY WSZECHŚWIATA", None)
  )

  private def finalRow(order: Seq[CinemaMovie]): (String, Option[Int], Set[Cinema]) = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    order.foreach(v => cache.recordCinemaScrape(v.cinema, Seq(v)))
    cache.canonicalizeBySanitize()
    val rows = cache.snapshot()
    withClue(s"expected ONE row, got ${rows.map(r => (r.title, r.year))}\n") { rows.size shouldBe 1 }
    (rows.head.title, rows.head.year, rows.head.record.cinemaData.keySet)
  }

  "canonicalizeBySanitize" should "store the same canonical (cleanTitle, year) + slots regardless of scrape order" in {
    val results = variants.permutations.map(finalRow).toList
    withClue(s"distinct outcomes across ${results.size} orderings: ${results.distinct}\n") {
      results.distinct.size shouldBe 1
    }
    val (title, year, cinemas) = results.head
    title  shouldBe "Władcy Wszechświata"
    year   shouldBe Some(2026)
    cinemas shouldBe Set(CinemaCityPoznanPlaza, Helios, KinoMuza, KinoMuranow)
  }

  // Yearless case variants (the remaining whole-corpus tail: Maraton horrorów,
  // Żywot Briana, the Federico Fellini retrospective, …).
  private val yearlessCase = Seq(
    cm(Helios,                "Maraton horrorów", None),
    cm(KinoMuza,              "Maraton Horrorów", None),
    cm(CinemaCityPoznanPlaza, "MARATON HORRORÓW", None)
  )

  it should "canonicalise yearless case variants deterministically" in {
    val results = yearlessCase.permutations.map(finalRow).toList
    withClue(s"yearless distinct outcomes: ${results.distinct}\n") { results.distinct.size shouldBe 1 }
    results.head._1 shouldBe "Maraton Horrorów" // prefer normally-cased over the all-caps variant
  }

  // Separator variants (Monterey Pop | DKF  vs  Monterey Pop_DKF).
  private val separatorCase = Seq(
    cm(Helios,   "Monterey Pop | DKF", None),
    cm(KinoMuza, "Monterey Pop_DKF",   None)
  )

  it should "canonicalise separator variants deterministically" in {
    val results = separatorCase.permutations.map(finalRow).toList
    withClue(s"separator distinct outcomes: ${results.distinct}\n") { results.distinct.size shouldBe 1 }
  }

  // TMDB's resolved year is authoritative: when cinemas disagree on a film's
  // year (production vs theatrical — "Dzień objawienia" reported 2025 by some,
  // 2026 by others, TMDB dated 2026), the merged row must key at TMDB's year,
  // NOT the lowest cinema-reported one the old rule picked.
  it should "key a resolved film at TMDB's year, overriding a lower cinema-reported year" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo)
    // Resolved row — TMDB dated this film 2026.
    cache.put(cache.keyOf("Dzień objawienia", Some(2026)),
      MovieRecord(tmdbId = Some(1275779), data = Map[Source, SourceData](
        (Tmdb: Source)   -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)),
        (Helios: Source) -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)))))
    // A cinema variant reporting the production year 2025 — the lowest present
    // year, which the old "min year" rule would have made canonical.
    cache.put(cache.keyOf("Dzień objawienia", Some(2025)),
      MovieRecord(data = Map[Source, SourceData](
        (KinoMuza: Source) -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2025)))))

    cache.canonicalizeBySanitize()

    val rows = cache.snapshot()
    withClue(s"expected ONE row, got ${rows.map(r => (r.title, r.year))}\n")(rows.size shouldBe 1)
    rows.head.year shouldBe Some(2026) // TMDB's year, not the lower cinema 2025
  }
}
