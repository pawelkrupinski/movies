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

  /** A resolved row keyed under `keyTitle` for a film TMDB knows as `tmdbTitle`
   *  (Polish) / `originalTitle`, reported by `cinema` as `cinemaTitle`. Used to
   *  build the cross-title (translation) duplicates the merge must fold. */
  private def aliased(
    keyTitle: String, tmdbId: Int, tmdbYear: Int,
    tmdbTitle: String, originalTitle: String, cinema: Source, cinemaTitle: String
  ): (CacheKey, MovieRecord) =
    key(keyTitle, Some(tmdbYear)) -> MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(title = Some(tmdbTitle), originalTitle = Some(originalTitle), releaseYear = Some(tmdbYear)),
        cinema -> SourceData(title = Some(cinemaTitle), releaseYear = Some(tmdbYear))
      )
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

  it should "keep a yearless-key unresolved row yearless, ignoring its deferred-detail slot year" in {
    // A deferred-detail cinema scrapes a film YEARLESS (yearless key); its detail
    // later adds a production year to the SLOT only. Folding alone, the row must
    // NOT adopt that provisional slot year as its key — that would make it a
    // year-bearing movies row its resolved siblings can no longer absorb (the
    // order-dependent "Głos Hind Rajab" / Kino Amondo split). It stays yearless.
    val row = key("Głos Hind Rajab", None) -> MovieRecord(
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Głos Hind Rajab"), releaseYear = Some(2022))))
    val (canonicalKey, _) = FilmCanonicalizer.canonical(Seq(row))
    canonicalKey.year shouldBe None
  }

  it should "fold a yearless-key slot-yeared unresolved row into a resolved sibling, not split it off by its slot year" in {
    // Kino Amondo reports "Głos Hind Rajab" yearless; its detail adds a 2022
    // production year to the slot — Δ3 from the resolved 2025 film, which would
    // split if that slot year keyed the row. Yearless, it folds in (rule 4),
    // regardless of insertion (fold) order.
    val group = Seq(
      resolved("Głos Hind Rajab", tmdbId = 1480382, tmdbYear = 2025, cinema = KinoMuza),
      key("Głos Hind Rajab", None) -> MovieRecord(
        data = Map[Source, SourceData](KinoMuzeumGdansk -> SourceData(title = Some("Głos Hind Rajab"), releaseYear = Some(2022))))
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(KinoMuza, KinoMuzeumGdansk)
      }
    }
  }

  "groupByFilm" should "fold a film keyed under two languages (same tmdbId, bare titles) into one component, then one cluster" in {
    // "Tangled" (original) and "Zaplątani" (Polish) are the SAME film — same
    // tmdbId, both keys are TMDB aliases — but different sanitized titles, so the
    // old sanitize-only grouping left them as two rows. They must now share a
    // film-identity component and collapse to one cluster, either insertion order.
    val rows = Seq(
      aliased("Tangled",   tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Multikino, cinemaTitle = "Zaplątani"),
      aliased("Zaplątani", tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Helios,    cinemaTitle = "Zaplątani")
    )
    Seq(rows, rows.reverse).foreach { ordered =>
      val components = FilmCanonicalizer.groupByFilm(ordered)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 1
        val clusters = FilmCanonicalizer.clusterByFilm(components.head)
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(Multikino, Helios)
      }
    }
  }

  it should "NOT fold a programme/decorated edition that merely carries the base tmdbId" in {
    // "Zaproszenie | Kinoteka dla rodziców" resolves to the base film's tmdbId, but
    // its key is NOT a TMDB alias (it adds the programme banner) — it is separate
    // by design and must stay its own component, while the bare base folds normally.
    val rows = Seq(
      aliased("Zaproszenie",                       tmdbId = 9001, tmdbYear = 2022, tmdbTitle = "Zaproszenie", originalTitle = "The Invitation", cinema = Helios,   cinemaTitle = "Zaproszenie"),
      aliased("Zaproszenie | Kinoteka dla rodziców", tmdbId = 9001, tmdbYear = 2022, tmdbTitle = "Zaproszenie", originalTitle = "The Invitation", cinema = Kinoteka, cinemaTitle = "Zaproszenie | Kinoteka dla rodziców")
    )
    val components = FilmCanonicalizer.groupByFilm(rows)
    withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
      components should have size 2
    }
  }

  it should "keep a remake (two distinct tmdbIds sharing a title) as one component but two clusters" in {
    val components = FilmCanonicalizer.groupByFilm(Seq(
      resolved("Diuna", tmdbId = 100, tmdbYear = 1984, cinema = KinoMuza),
      resolved("Diuna", tmdbId = 200, tmdbYear = 2021, cinema = KinoMuzeumGdansk)
    ))
    components should have size 1                                  // same sanitized title → one component
    FilmCanonicalizer.clusterByFilm(components.head) should have size 2  // split back out by tmdbId
  }

  it should "key a cross-language cluster on the dominant cinema title, not the alphabetical min" in {
    // The churn guard: keying on the alphabetical min ("tangled") would leave an
    // _id no cinema reports, so every "Zaplątani" scrape would re-spawn the row.
    val (canonicalKey, _) = FilmCanonicalizer.canonical(Seq(
      aliased("Tangled",   tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Multikino, cinemaTitle = "Zaplątani"),
      aliased("Zaplątani", tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Helios,    cinemaTitle = "Zaplątani")
    ))
    TitleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe "zaplatani"
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
