package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmCanonicalizerSpec extends AnyFlatSpec with Matchers {

  private def key(title: String, year: Option[Int]): CacheKey = CacheKey(title, year)

  /** A resolved row: carries a tmdbId and a Tmdb slot whose releaseYear IS the
   *  cluster's authoritative tmdbYear. */
  private def resolved(title: String, tmdbId: Int, tmdbYear: Int, cinema: Source): (CacheKey, MovieRecord) =
    key(title, Some(tmdbYear)) -> MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(releaseYear = Some(tmdbYear)),
        cinema -> SourceData(title = Some(title), releaseYear = Some(tmdbYear))
      )
    )

  /** An unresolved, year-bearing cinema row. */
  private def unresolved(title: String, year: Option[Int], cinema: Source): (CacheKey, MovieRecord) =
    key(title, year) -> MovieRecord(
      data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), releaseYear = year))
    )

  "canonical" should "collapse a ±1-year unresolved + resolved cluster onto the resolved year and unioned cinemas" in {
    // Helios resolved the film to TMDB year 2026; Multikino stranded a 2025
    // (production-year) unresolved row beside it.
    val cluster = Seq(
      resolved("Dzień objawienia", tmdbId = 99, tmdbYear = 2026, cinema = Helios),
      unresolved("Dzień objawienia", Some(2025), cinema = Multikino)
    )

    val (canonicalKey, merged) = FilmCanonicalizer.canonical(cluster)

    // TMDB's resolved year is authoritative, overriding the cinema-reported 2025.
    canonicalKey.year shouldBe Some(2026)
    // unionAll picks the tmdbId-bearing row as the base, so the resolution survives.
    merged.tmdbId shouldBe Some(99)
    // Both cinemas' slots are unioned in — no showtime/slot loss.
    merged.cinemaData.keySet shouldBe Set(Helios, Multikino)
  }

  it should "prefer a normally-cased spelling over a SHOUTING variant" in {
    val cluster = Seq(
      unresolved("SAVAGE HOUSE", Some(2024), cinema = Helios),
      unresolved("Savage House", Some(2024), cinema = Multikino)
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster)

    canonicalKey.cleanTitle shouldBe "Savage House"
  }

  it should "not let a yearless variant win the spelling for an all-unresolved cluster" in {
    // A yearless all-caps variant sits beside the year-bearing normally-cased one.
    // It must NOT win the spelling just because the year fallback picks 2024.
    val cluster = Seq(
      unresolved("Savage House", Some(2024), cinema = Helios),
      unresolved("SAVAGE HOUSE", None, cinema = Multikino)
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster)

    // Year falls back to the lowest present year (no tmdbYear anywhere).
    canonicalKey.year shouldBe Some(2024)
    // Spelling is the non-shouting one, considered across ALL variants.
    canonicalKey.cleanTitle shouldBe "Savage House"
  }

  it should "be yearless when no row carries any year" in {
    val cluster = Seq(
      unresolved("Mystery Film", None, cinema = Helios),
      unresolved("Mystery Film", None, cinema = Multikino)
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster)

    canonicalKey.year shouldBe None
  }
}
