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

  "clusterByFilm" should "fold an unresolved same-title row into a resolved sibling a full two years off" in {
    // The "Zawieście czerwone latarnie" flake: every cinema's row resolves to one
    // TMDB film at year 1991, but Kino Muzeum reports it uppercase with the
    // PRODUCTION year 1989 — two years off the resolved year, just past the old ±1
    // window. While that row is still unresolved (its TMDB lookup hasn't landed
    // yet, or never will in a hermetic run), it must STILL fold into the resolved
    // 1991 cluster: a ±2 gap is a cinema's production-vs-release-year
    // disagreement, not a second film.
    val rows = Seq(
      resolved  ("Zawieście czerwone latarnie", tmdbId = 31273, tmdbYear = 1991, cinema = KinoMuza),
      unresolved("ZAWIEŚCIE CZERWONE LATARNIE", Some(1989), cinema = KinoMuzeumGdansk)
    )
    // Both insertion orders must land on ONE cluster carrying both cinemas.
    Seq(rows, rows.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered)
      withClue(s"clusters for order ${ordered.map(_._1.year)}: ${clusters.map(_.map(_._1))}\n") {
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(KinoMuza, KinoMuzeumGdansk)
      }
    }
  }

  it should "still keep two DISTINCT resolved tmdbIds far apart as separate films" in {
    // The over-merge guard: a real remake carrying the same title (each resolved
    // to its OWN tmdbId, years far apart) must stay two clusters. The fold above
    // only pulls in UNRESOLVED rows, never two resolved films.
    val clusters = FilmCanonicalizer.clusterByFilm(Seq(
      resolved("Diuna", tmdbId = 100, tmdbYear = 1984, cinema = KinoMuza),
      resolved("Diuna", tmdbId = 200, tmdbYear = 2021, cinema = KinoMuzeumGdansk)
    ))
    clusters should have size 2
  }

  it should "keep a decorated variant's own spelling, not the base title its Tmdb slot carries" in {
    // A dubbed variant resolved to the base film's tmdbId, so its Tmdb slot
    // title is the BARE base "Straszny film" — a DIFFERENT sanitize. Canonicalising
    // to that cross-sanitize title would re-key the dub onto the base and collapse
    // it; the slot title must be ignored because it doesn't match the variant.
    val cluster = Seq(
      key("Straszny film ukraiński dubbing", Some(2026)) -> MovieRecord(
        tmdbId = Some(12345),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(title = Some("Straszny film"), releaseYear = Some(2026)),
          Helios -> SourceData(title = Some("Straszny film ukraiński dubbing"), releaseYear = Some(2026))))
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster)

    // The dub keeps its own spelling; it does NOT collapse onto the base "Straszny film".
    TitleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe
      TitleNormalizer.sanitize("Straszny film ukraiński dubbing")
    canonicalKey.cleanTitle should include ("dubbing")
  }
}
