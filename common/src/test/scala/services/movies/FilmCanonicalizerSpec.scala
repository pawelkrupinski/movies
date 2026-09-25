package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmCanonicalizerSpec extends AnyFlatSpec with Matchers {

  import CanonicalizerRows.{cacheKey, published, resolved, unresolved}

  /** A resolved row keyed under `keyTitle` for a film TMDB knows as `tmdbTitle`
   *  (Polish) / `originalTitle`, reported by `cinema` as `cinemaTitle`. Used to
   *  build the cross-title (translation) duplicates the merge must fold. */
  private def aliased(
    keyTitle: String, tmdbId: Int, tmdbYear: Int,
    tmdbTitle: String, originalTitle: String, cinema: Source, cinemaTitle: String,
    englishTitle: Option[String] = None
  ): (CacheKey, MovieRecord) =
    cacheKey(keyTitle, Some(tmdbYear)) -> MovieRecord(
      tmdbId = Some(tmdbId),
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(title = Some(tmdbTitle), originalTitle = Some(originalTitle),
                             englishTitle = englishTitle, releaseYear = Some(tmdbYear)),
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

    val (canonicalKey, merged) = FilmCanonicalizer.canonical(cluster, titleNormalizer)

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

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster, titleNormalizer)

    canonicalKey.cleanTitle shouldBe "Savage House"
  }

  it should "not let a yearless variant win the spelling for an all-unresolved cluster" in {
    // A yearless all-caps variant sits beside the year-bearing normally-cased one.
    // It must NOT win the spelling just because the year fallback picks 2024.
    val cluster = Seq(
      unresolved("Savage House", Some(2024), cinema = Helios),
      unresolved("SAVAGE HOUSE", None, cinema = Multikino)
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster, titleNormalizer)

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

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster, titleNormalizer)

    canonicalKey.year shouldBe None
  }

  it should "keep the tmdbId the cinemas' runtimes point at even when that film has several rows tying on it" in {
    // The imdbId fold hands `canonical` two TMDB records of one film, and the cinemas'
    // runtimes pick which id survives. The right film is USUALLY the one with more rows
    // (more venues found it), and every row of one tmdbId carries the same TMDB runtime
    // — so asking per ROW made the right film tie with itself, `strictNearest` refused,
    // and the fold fell back to the lower year: the 18-minute concert record here.
    def timed(title: String, tmdbId: Int, year: Int, cinema: Source, tmdbRuntime: Int): (CacheKey, MovieRecord) =
      cacheKey(title, Some(year)) -> MovieRecord(
        tmdbId = Some(tmdbId), imdbId = Some("tt43683692"),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(releaseYear = Some(year), runtimeMinutes = Some(tmdbRuntime)),
          cinema -> SourceData(title = Some(title), releaseYear = Some(year), runtimeMinutes = Some(110))))
    val rows = Seq(
      timed("Ghost 2",  1568069, 2025, KinoMuza,         tmdbRuntime = 18),
      timed("Ghost 2",  1693400, 2026, KinoMuzeumGdansk, tmdbRuntime = 110),
      timed("Ghost Two", 1693400, 2026, Helios,           tmdbRuntime = 110))

    rows.permutations.foreach { ordered =>
      withClue(s"order ${ordered.map(_._1)}: ") {
        FilmCanonicalizer.canonical(ordered, titleNormalizer)._2.tmdbId shouldBe Some(1693400)
      }
    }
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
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters for order ${ordered.map(_._1.year)}: ${clusters.map(_.map(_._1))}\n") {
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(KinoMuza, KinoMuzeumGdansk)
      }
    }
  }

  // The window's EDGE, pinned so it can only move on purpose: two years off attaches,
  // three does not. `YearWindow.ProductionToRelease` owns the number, and
  // `ScrapeLanding.concludedKeyFor` reads the same one so a listing lands exactly where
  // the settle would attach it (`DecoratedListingLandsSpec` pins that side).
  it should "attach an unresolved row at the edge of the year window and orphan one just past it" in {
    def clustersFor(cinemaYear: Int) = FilmCanonicalizer.clusterByFilm(Seq(
      resolved  ("Zawieście czerwone latarnie", tmdbId = 31273, tmdbYear = 1991, cinema = KinoMuza),
      unresolved("Zawieście czerwone latarnie", Some(cinemaYear), cinema = KinoMuzeumGdansk)), titleNormalizer)
    withClue("two years off, either side: ") {
      clustersFor(1989) should have size 1
      clustersFor(1993) should have size 1
    }
    withClue("three years off, either side: ") {
      clustersFor(1988) should have size 2
      clustersFor(1994) should have size 2
    }
  }

  it should "still keep two DISTINCT resolved tmdbIds far apart as separate films" in {
    // The over-merge guard: a real remake carrying the same title (each resolved
    // to its OWN tmdbId, years far apart) must stay two clusters. The fold above
    // only pulls in UNRESOLVED rows, never two resolved films.
    val clusters = FilmCanonicalizer.clusterByFilm(Seq(
      resolved("Diuna", tmdbId = 100, tmdbYear = 1984, cinema = KinoMuza),
      resolved("Diuna", tmdbId = 200, tmdbYear = 2021, cinema = KinoMuzeumGdansk)
    ), titleNormalizer)
    clusters should have size 2
  }

  it should "keep a yearless-key unresolved row yearless, ignoring its deferred-detail slot year" in {
    // A deferred-detail cinema scrapes a film YEARLESS (yearless key); its detail
    // later adds a production year to the SLOT only. Folding alone, the row must
    // NOT adopt that provisional slot year as its key — that would make it a
    // year-bearing movies row its resolved siblings can no longer absorb (the
    // order-dependent "Głos Hind Rajab" / Kino Amondo split). It stays yearless.
    val row = cacheKey("Głos Hind Rajab", None) -> MovieRecord(
      data = Map[Source, SourceData](Helios -> SourceData(title = Some("Głos Hind Rajab"), releaseYear = Some(2022))))
    val (canonicalKey, _) = FilmCanonicalizer.canonical(Seq(row), titleNormalizer)
    canonicalKey.year shouldBe None
  }

  it should "fold a yearless-key slot-yeared unresolved row into a resolved sibling, not split it off by its slot year" in {
    // Kino Amondo reports "Głos Hind Rajab" yearless; its detail adds a 2022
    // production year to the slot — Δ3 from the resolved 2025 film, which would
    // split if that slot year keyed the row. Yearless, it folds in (rule 4),
    // regardless of insertion (fold) order.
    val group = Seq(
      resolved("Głos Hind Rajab", tmdbId = 1480382, tmdbYear = 2025, cinema = KinoMuza),
      cacheKey("Głos Hind Rajab", None) -> MovieRecord(
        data = Map[Source, SourceData](KinoMuzeumGdansk -> SourceData(title = Some("Głos Hind Rajab"), releaseYear = Some(2022))))
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(KinoMuza, KinoMuzeumGdansk)
      }
    }
  }

  it should "refuse a rule-4 fold when the yearless-key row's OWN slot year contradicts the resolved film" in {
    // DE "Hope", 2026-09-16: a resolved 2026 Korean horror film absorbed a
    // yearless-key row whose only cinema slot reports the UNRELATED 2014
    // Cameroonian migration drama "Hope" (91 min) — nothing to compare by TITLE
    // (no originalTitle published), but the slot's own year is Δ12 from the
    // resolved film, well past `YearWindow.SlotYearImplausibility`. Must NOT fold
    // (contrast the Δ3 "Głos Hind Rajab" case above, which must).
    val group = Seq(
      resolved("Hope", tmdbId = 1058424, tmdbYear = 2026, cinema = Helios),
      cacheKey("Hope", None) -> MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Hope"), releaseYear = Some(2014), runtimeMinutes = Some(91))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 2
  }

  it should "let an agreeing director overrule a rule-4 runtime contradiction a venue's typo manufactured" in {
    // PL hard cluster, surfaced by the Avengers ratchet's new arrival order: Kino Parczew
    // lists "Vincent.legenda oceanu" at 9 minutes (the film runs 91) crediting Steven
    // Majaury. Folded after the resolved row had TMDB's runtime, rule 4 refused it as a
    // different film and it stood alone; folded before, it joined — and the next rescrape
    // landed it on the resolved row anyway. A shared director is the veto
    // `MixedFilmDetector` already applies to exactly this kind of runtime "evidence".
    def withDetails(row: CanonicalizerRows.Row, source: Source, minutes: Int, directors: Seq[String]): CanonicalizerRows.Row =
      row._1 -> row._2.copy(data = row._2.data.updatedWith(source)(_.map(_.copy(runtimeMinutes = Some(minutes), director = directors))))
    val vincent = withDetails(resolved("Vincent. Legenda oceanu", tmdbId = 677558, tmdbYear = 2026, cinema = Helios),
      Tmdb, 91, Seq("Steven Majaury", "Pavel Hruboš"))
    val parczew = cacheKey("Vincent.legenda oceanu", None) -> MovieRecord(data = Map[Source, SourceData](Multikino ->
      SourceData(title = Some("Vincent.legenda oceanu"), runtimeMinutes = Some(9), director = Seq("Pavel Hrubas", "Steven Majaury"))))
    FilmCanonicalizer.clusterByFilm(Seq(vincent, parczew), titleNormalizer) should have size 1
    // Without the shared credit the typo is still read as a different film.
    val stranger = parczew._1 -> parczew._2.copy(data = parczew._2.data.map { case (k, sd) => k -> sd.copy(director = Seq("Someone Else")) })
    FilmCanonicalizer.clusterByFilm(Seq(vincent, stranger), titleNormalizer) should have size 2
  }

  it should "fold a yearless-key row within the WIDER rule-4 slot-year tolerance, and refuse just past it" in {
    def clustersFor(slotYear: Int) = FilmCanonicalizer.clusterByFilm(Seq(
      resolved("Hope", tmdbId = 1058424, tmdbYear = 2026, cinema = Helios),
      cacheKey("Hope", None) -> MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Hope"), releaseYear = Some(slotYear))))
    ), titleNormalizer)
    withClue("five years off: ") { clustersFor(2021) should have size 1 }
    withClue("six years off: ")  { clustersFor(2020) should have size 2 }
  }

  it should "waive the rule-4 slot-year refusal when a yearless straggler's OWN runtime agrees closely with the resolved film's" in {
    // PL "Happy Together" (Wong Kar-wai, 1997), 2026-09-17: Kinoteka — a BRAND NEW
    // venue, with nothing yet on the resolved row to reclaim by shared text the way
    // rule 2b's "identical listing" check needs — reports "Data premiery:
    // 30.06.2026" (this rerelease's screening date, not the film's vintage) under a
    // bare, undecorated heading, so the row keys yearless but its slot carries a year
    // Δ29 from the resolved 1997 cluster, past even `YearWindow.SlotYearImplausibility`.
    // Its own `originalTitle`/`director` are published in Cantonese romanization
    // ("Chun gwong cha sit"/"Wong Kar Wai") against TMDB's Chinese-script originals
    // ("春光乍洩"/"王家衛"), so neither can serve as the override signal here — but
    // the runtime, 96 minutes on both sides, needs no script to compare (contrast
    // "Hope" above, whose stragglers published no runtime the resolved film agreed
    // with either).
    val group = Seq(
      cacheKey("Happy Together", Some(1997)) -> MovieRecord(
        tmdbId = Some(1013),
        data = Map[Source, SourceData](Tmdb -> SourceData(releaseYear = Some(1997), runtimeMinutes = Some(96)))),
      cacheKey("Happy Together", None) -> MovieRecord(
        data = Map[Source, SourceData](Kinoteka ->
          SourceData(title = Some("Happy Together"), releaseYear = Some(2026), runtimeMinutes = Some(96))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
      clusters should have size 1
    }
  }

  it should "still refuse the rule-4 fold on a wide slot-year gap when the straggler's runtime DISAGREES" in {
    // The mirror of the case above: a yearless straggler past the slot-year
    // tolerance does NOT get waived just because it names SOME runtime — only a
    // CLOSELY AGREEING one is evidence of the same film.
    val group = Seq(
      cacheKey("Happy Together", Some(1997)) -> MovieRecord(
        tmdbId = Some(1013),
        data = Map[Source, SourceData](Tmdb -> SourceData(releaseYear = Some(1997), runtimeMinutes = Some(96)))),
      cacheKey("Happy Together", None) -> MovieRecord(
        data = Map[Source, SourceData](Kinoteka ->
          SourceData(title = Some("Happy Together"), releaseYear = Some(2026), runtimeMinutes = Some(150))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 2
  }

  it should "refuse a rule-4 fold when the yearless-key row's cinema publishes a contradicting original title + runtime" in {
    // The "Obcy" shape (see `groupByFilm`'s containment-edge tests below), asked
    // of rule 4 instead: Kino Pionier's "I Was A Stranger" (103 min) beside the
    // resolved "L'étranger" (120 min) — MixedFilmDetector's own evidence, not a
    // year gap, is what refuses this one.
    val group = Seq(
      published("Obcy", tmdbId = 7183, tmdbYear = 2025, cinema = Helios, originalTitle = "L'étranger", runtime = 120),
      cacheKey("Obcy", None) -> MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Obcy"), originalTitle = Some("I Was A Stranger"), runtimeMinutes = Some(103))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 2
  }

  it should "reclaim a year-window orphan sharing an IDENTICAL cinema listing with a resolved sibling, however far the year gap" in {
    // UK "The Hunger Games", 2026-09-16: Odeon Belfast's rerelease-season page
    // re-lists the resolved 2012 film with no releaseYear of its own, so
    // EmbeddedYear stamps it with the season's current year — 2026, fourteen
    // years past the resolved cluster, outside even rule 2's ±2 window — but the
    // venue's OWN synopsis text is byte-identical to what it already publishes on
    // the resolved row. The gap must not matter when the listing itself is a
    // provable duplicate.
    val text = "Every year in the ruins of what was once North America..."
    val group = Seq(
      cacheKey("The Hunger Games", Some(2012)) -> MovieRecord(
        tmdbId = Some(70160),
        data = Map[Source, SourceData](Tmdb -> SourceData(releaseYear = Some(2012))),
        retainedSynopses = Map[Source, String](Helios -> text)),
      cacheKey("The Hunger Games", Some(2026)) -> MovieRecord(
        retainedSynopses = Map[Source, String](Helios -> text))
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 1
      }
    }
  }

  it should "not reclaim a year-window orphan on a shared ENRICHMENT synopsis — only a venue's own print is a duplicate listing" in {
    // Rule 2b's evidence is a CINEMA re-publishing its own listing under a new
    // year. Two rows carrying the same Filmweb (or IMDb/TMDB) blurb only says the
    // same lookup ran for both titles — a title-keyed enrichment landing both a
    // 1990 and a 2017 "It" on one entry is exactly the same-title confusion the
    // year window exists to keep apart, not proof of one film.
    val text = "A shared enrichment synopsis for a same-titled film."
    val group = Seq(
      cacheKey("It", Some(2017)) -> MovieRecord(
        tmdbId = Some(346364),
        data = Map[Source, SourceData](
          Tmdb    -> SourceData(releaseYear = Some(2017)),
          Helios  -> SourceData(title = Some("It"), releaseYear = Some(2017)),
          Filmweb -> SourceData(synopsis = Some(text)))),
      cacheKey("It", Some(1990)) -> MovieRecord(
        data = Map[Source, SourceData](
          Multikino -> SourceData(title = Some("It"), releaseYear = Some(1990)),
          Filmweb   -> SourceData(synopsis = Some(text))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
      clusters should have size 2
    }
  }

  it should "fold a rule-4 straggler onto the ONE resolved film, never onto an unresolved orphan that merely ranks earlier" in {
    // One resolved "Hope" (2026) and an unresolved 2010 orphan outside its window —
    // rule 3 gives the orphan its own cluster, and its lower key year ranks it
    // first. A bare, evidence-free "Hope" straggler is the case rule 4 documents as
    // folding onto the single RESOLVED film; ranking alone handed it to the
    // unresolved orphan instead, a film nothing has identified.
    val group = Seq(
      resolved("Hope", tmdbId = 1058424, tmdbYear = 2026, cinema = Helios),
      unresolved("Hope", Some(2010), cinema = Multikino),
      cacheKey("Hope", None) -> MovieRecord(
        data = Map[Source, SourceData](Kinoteka -> SourceData(title = Some("Hope"))))
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        val resolvedCluster = clusters.find(_.exists(_._2.tmdbId.contains(1058424))).get
        resolvedCluster.map(_._1.year) should contain (None)
      }
    }
  }

  it should "leave a rule-4 straggler alone when NO film is resolved and two unresolved year clusters could claim it" in {
    // Nothing is resolved; rule 3 gives the 1984 and the 2021 "Diuna" a window cluster
    // each. A bare, yearless listing is either film, so picking the lower year would
    // be a guess the row then inherits — refuse, exactly as rule 4 does for two
    // resolved films.
    val straggler = cacheKey("Diuna", None) -> MovieRecord(
      data = Map[Source, SourceData](Kinoteka -> SourceData(title = Some("Diuna"))))
    val group = Seq(
      unresolved("Diuna", Some(1984), cinema = Helios),
      unresolved("Diuna", Some(2021), cinema = Multikino),
      straggler
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 3
        clusters should contain (Seq(straggler))
      }
    }
  }

  it should "still fold a rule-4 straggler onto the ONLY unresolved year cluster when nothing is resolved" in {
    val group = Seq(
      unresolved("Diuna", Some(2021), cinema = Helios),
      unresolved("Diuna", Some(2022), cinema = Multikino),
      cacheKey("Diuna", None) -> MovieRecord(
        data = Map[Source, SourceData](Kinoteka -> SourceData(title = Some("Diuna"))))
    )
    FilmCanonicalizer.clusterByFilm(group, titleNormalizer) should have size 1
  }

  it should "refuse to reclaim an orphan whose identical text happens to sit on TWO resolved clusters (ambiguous)" in {
    val text = "Some verbatim synopsis text shared by coincidence."
    val group = Seq(
      cacheKey("Film X", Some(2000)) -> MovieRecord(
        tmdbId = Some(1), data = Map[Source, SourceData](Tmdb -> SourceData(releaseYear = Some(2000))),
        retainedSynopses = Map[Source, String](Helios -> text)),
      cacheKey("Film X", Some(2010)) -> MovieRecord(
        tmdbId = Some(2), data = Map[Source, SourceData](Tmdb -> SourceData(releaseYear = Some(2010))),
        retainedSynopses = Map[Source, String](Multikino -> text)),
      cacheKey("Film X", Some(2030)) -> MovieRecord(
        retainedSynopses = Map[Source, String](Helios -> text, Multikino -> text))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    // Three clusters: the two resolved films stay apart (different tmdbIds), and
    // the ambiguous orphan stands alone rather than guessing which one it belongs to.
    clusters should have size 3
  }

  it should "reclaim a year-window orphan whose OWN title DECORATES a resolved cluster's bare title, even at a cinema with no shared listing to compare" in {
    // PL "Opętanie" poster-tour promo, 2026-09-17 (ReadModelFilmsInvisibleWithScreenings):
    // Kino Spektrum published "Opętanie - plakatowa trasa: darmowy plakat dla
    // każdego widza!" tagged with the TOUR's own year (2026) — 45 years past the
    // resolved 1981 cluster, outside rule 2's ±2 window — at a cinema that carries
    // NONE of the resolved cluster's own listings, so rule 2b's identical-text
    // reclaim (the Hunger Games case above) has nothing to compare against and the
    // row was a permanent orphan: `readyToProject` never held, so the projector
    // pruned its card and the film 404'd with an upcoming showing on sale.
    // `groupByFilm`'s containment edge already trusts a bare title as a token-run
    // PREFIX of a longer one (with no MixedFilmDetector contradiction) to be the
    // SAME film; this asks `clusterByFilm` to honour that same relationship
    // instead of splitting it back apart on the year gap alone.
    val group = Seq(
      resolved("Opętanie", tmdbId = 21484, tmdbYear = 1981, cinema = Helios),
      unresolved("Opętanie - plakatowa trasa: darmowy plakat dla każdego widza!", Some(2026), cinema = Kinoteka)
    )
    Seq(group, group.reverse).foreach { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      withClue(s"clusters: ${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 1
      }
    }
  }

  it should "refuse to reclaim a year-window orphan whose decoration matches TWO resolved clusters (ambiguous)" in {
    // The decoration-match mirror of the identical-text ambiguity case above: a
    // same-titled remake pair must stay split even though the orphan's title
    // decorates BOTH of their bare titles.
    val group = Seq(
      resolved("Diuna", tmdbId = 1, tmdbYear = 1984, cinema = Helios),
      resolved("Diuna", tmdbId = 2, tmdbYear = 2021, cinema = Multikino),
      unresolved("Diuna - pokaz przedpremierowy", Some(2026), cinema = Kinoteka)
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 3
  }

  it should "not reclaim a year-window orphan whose title carries its OWN delimited year disagreeing with the resolved cluster" in {
    // "It (1990)" beside a resolved "It" (2017) — SameTitleTwoFilmsSpec's shape,
    // asked of clusterByFilm directly. The bare-title containment match alone
    // would decorate ("Casablanca 1942" is still Casablanca — years aren't a
    // sequel marker), but the orphan's OWN delimited year says it is a DIFFERENT,
    // specific same-titled film, which must refuse regardless of the title shape.
    // Keyed at 1990 (not yearless) — `EmbeddedYear` persists the delimited year as
    // the row's own lookup year at the scrape boundary, so this exercises the same
    // rule-2/2b year-window orphan path the "Opętanie" case above does, not rule 4.
    val group = Seq(
      resolved("It", tmdbId = 570670, tmdbYear = 2017, cinema = Helios),
      unresolved("It (1990)", Some(1990), cinema = Kinoteka)
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 2
  }

  it should "reclaim a rerelease whose OWN bracketed year disagrees when its published runtime matches the resolved film's" in {
    // UK hard cluster: Odeon's rerelease season brackets the SEASON's year onto a 2013
    // film — "The Hunger Games: Catching Fire (2026)", 146 minutes, as TMDB has it. Arriving
    // after the 2013 row settled it resolves alone, TMDB has no 2026 film of that name,
    // and the own-year guard above refused it: an unresolved duplicate card. The printed
    // year alone cannot tell a release year from an event year (829eb309d, 3e4cbb3c5);
    // the runtime can — Wallace's "It" runs 168 minutes against Muschietti's 135.
    def withRuntime(row: CanonicalizerRows.Row, source: Source, minutes: Int): CanonicalizerRows.Row =
      row._1 -> row._2.copy(data = row._2.data.updatedWith(source)(_.map(_.copy(runtimeMinutes = Some(minutes)))))
    val catchingFire = withRuntime(resolved("The Hunger Games: Catching Fire", tmdbId = 101299, tmdbYear = 2013, cinema = Helios), Tmdb, 146)
    val rerelease    = withRuntime(unresolved("The Hunger Games: Catching Fire (2026)", Some(2026), cinema = Kinoteka), Kinoteka, 146)
    Seq(Seq(catchingFire, rerelease), Seq(rerelease, catchingFire)).foreach { ordered =>
      FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer) should have size 1
    }
    // The same shape with the runtimes disagreeing is still two films.
    val it1990 = withRuntime(unresolved("It (1990)", Some(1990), cinema = Kinoteka), Kinoteka, 168)
    val it2017 = withRuntime(resolved("It", tmdbId = 346364, tmdbYear = 2017, cinema = Helios), Tmdb, 135)
    FilmCanonicalizer.clusterByFilm(Seq(it2017, it1990), titleNormalizer) should have size 2
  }

  it should "not reclaim a year-window orphan whose decoration match is refused by MixedFilmDetector's own contradiction" in {
    // The decoration-match mirror of the "Obcy" contradiction case above: a
    // cinema-published original title + runtime that plainly names a DIFFERENT
    // film must still refuse, even though the bare titles alone would decorate.
    // Year kept far apart (2025 vs 2026's tmdbYear... a WIDE gap, well past ±2) so
    // this exercises the decoration-reclaim path rather than the plain rule-2
    // window, which would otherwise attach it regardless of this test's point.
    val group = Seq(
      published("Obcy", tmdbId = 7183, tmdbYear = 1969, cinema = Helios, originalTitle = "L'étranger", runtime = 120),
      cacheKey("Obcy - pokaz przedpremierowy", Some(2026)) -> MovieRecord(
        data = Map[Source, SourceData](Kinoteka ->
          SourceData(title = Some("Obcy - pokaz przedpremierowy"), originalTitle = Some("I Was A Stranger"), runtimeMinutes = Some(103))))
    )
    val clusters = FilmCanonicalizer.clusterByFilm(group, titleNormalizer)
    clusters should have size 2
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
      val components = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 1
        val clusters = FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer)
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(Multikino, Helios)
      }
    }
  }

  // Nine days of prod re-key logs (2026-08-29 → 09-06): "The Hunger Games: Mockingjay
  // Pt 2 (2026 Re-Release)" folded onto the 2012 "The Hunger Games" twenty times, so UK
  // venues screening the re-release served the first film's poster, cast and ratings.
  // The containment edge saw the base title as a prefix run and nothing to refuse on —
  // UK slots publish an original title one time in nine. The ordinal is the refusal.
  it should "not fold a sequel onto the first film just because it carries its title" in {
    val rows = Seq(
      resolved("The Hunger Games", tmdbId = 70160, tmdbYear = 2012, cinema = Multikino),
      unresolved("The Hunger Games: Mockingjay Pt 2 (2026 Re-Release)", None, cinema = Helios))
    Seq(rows, rows.reverse).foreach { ordered =>
      val components = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 2
      }
    }
  }

  // UK convergence, 2026-09-16: the same "The Hunger Games" containment-edge
  // collision, but for a sequel that renames itself instead of numbering
  // itself — "Catching Fire" carries no ordinal or part-marker, so it isn't
  // the shape the ordinal-based guard above catches. `SequelMarker`'s curated
  // franchise-subtitle list is what refuses it here.
  it should "not fold a same-franchise sequel onto the first film when its subtitle isn't a number" in {
    val rows = Seq(
      resolved("The Hunger Games", tmdbId = 70160, tmdbYear = 2012, cinema = Multikino),
      unresolved("The Hunger Games: Catching Fire", None, cinema = Helios))
    Seq(rows, rows.reverse).foreach { ordered =>
      val components = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 2
      }
    }
  }

  it should "still fold a banner-decorated screening of the same film" in {
    val rows = Seq(
      resolved("Toy Story 5", tmdbId = 1, tmdbYear = 2026, cinema = Multikino),
      unresolved("Toddler Club: Toy Story 5", None, cinema = Helios))
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 1
  }

  it should "fold a non-Latin-original film keyed under its English title via the englishTitle alias" in {
    // The Taiwanese "左撇子女孩" screens in Poland as both the Polish TMDB title
    // "Left-Handed Girl. To była ręka… diabła!" and the plain English release
    // title "Left-Handed Girl". Its `originalTitle` is non-Latin, so WITHOUT the
    // englishTitle alias the English-keyed row matches no alias, fails the bare
    // check, and the duplicate never collapses. The englishTitle alias makes both
    // rows bare so the shared tmdbId folds them into one component.
    val polish = "Left-Handed Girl. To była ręka… diabła!"
    val rows = Seq(
      aliased("Left-Handed Girl", tmdbId = 999075, tmdbYear = 2025, tmdbTitle = polish,
        originalTitle = "左撇子女孩", cinema = Multikino, cinemaTitle = "Left-Handed Girl",
        englishTitle = Some("Left-Handed Girl")),
      aliased(polish, tmdbId = 999075, tmdbYear = 2025, tmdbTitle = polish,
        originalTitle = "左撇子女孩", cinema = Helios, cinemaTitle = polish,
        englishTitle = Some("Left-Handed Girl"))
    )
    Seq(rows, rows.reverse).foreach { ordered =>
      val components = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 1
        FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 1
      }
    }
  }

  it should "fold an UNRESOLVED straggler whose key matches a resolved row's englishTitle alias" in {
    // A cinema (Chemik) lists the film under its plain English title "The Mandalorian
    // and Grogu". That English-title TMDB search has no hit (only the Polish title
    // resolves), so the straggler never gets its own tmdbId — the tmdbId edge can't
    // connect it. The resolved Polish row carries "The Mandalorian and Grogu" as its
    // englishTitle alias, so the alias edge must fold the idless straggler onto it
    // regardless of order; without it the straggler sits in its own component and the
    // cinema's slot is lost from the canonical row (order-dependent in the full corpus).
    val polish = "Gwiezdne wojny: Mandalorian i Grogu"
    val rows = Seq(
      aliased(polish, tmdbId = 1228710, tmdbYear = 2026, tmdbTitle = polish,
        originalTitle = "The Mandalorian and Grogu", cinema = Helios, cinemaTitle = polish,
        englishTitle = Some("The Mandalorian and Grogu")),
      unresolved("The mandalorian and grogu", None, cinema = KinoMuza)
    )
    Seq(rows, rows.reverse).foreach { ordered =>
      val components = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer)
      withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
        components should have size 1
        val clusters = FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer)
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe Set(Helios, KinoMuza)
      }
    }
  }

  it should "fold an UNRESOLVED programme edition onto its base by SEARCH TITLE, not by resolution" in {
    // "Zaproszenie | Kinoteka dla rodziców" has no tmdbId of its own (its decorated
    // string can't be searched), but shares the base's SEARCH TITLE — `apiQuery`
    // strips the "| Kinoteka dla rodziców" programme banner → "Zaproszenie" — so the
    // search-title edge folds it onto the resolved base DETERMINISTICALLY, regardless
    // of whether it ever resolves its own id. That's what makes the staging fold
    // arrival-order-independent (StagingOrderDeterminismSpec): a director-less
    // edition no longer has to win a director-walk race to join its film. (A
    // genuinely different film that merely contains the base word — "Moja Ojczyzna"
    // vs "Ojczyzna" — has a DIFFERENT search title and is NOT folded.)
    val rows = Seq(
      aliased("Zaproszenie", tmdbId = 9001, tmdbYear = 2022, tmdbTitle = "Zaproszenie",
        originalTitle = "The Invitation", cinema = Helios, cinemaTitle = "Zaproszenie"),
      unresolved("Zaproszenie | Kinoteka dla rodziców", None, cinema = Kinoteka)
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 1
  }

  it should "fold an UNRESOLVED Cyrillic-titled row onto its Latin base by ROMANIZED search title" in {
    // A Ukrainian-dubbed screening lists under the Cyrillic title "Ваяна"; the
    // Polish listing under Latin "Vaiana" resolved to TMDB. The bare Cyrillic
    // string neither resolves on TMDB nor sanitizes to the Latin key, so it would
    // sit as its own orphan film (the live kinowo.net duplicate). `apiQuery`
    // romanizes the SEARCH title (Ваяна → Vaiana), so both rows now share
    // sanitize(apiQuery) and the search-title edge folds them into one component —
    // WITHOUT the Cyrillic row having to resolve its own tmdbId.
    val rows = Seq(
      aliased("Vaiana", tmdbId = 1108427, tmdbYear = 2026, tmdbTitle = "Vaiana",
        originalTitle = "Moana", cinema = Helios, cinemaTitle = "Vaiana"),
      unresolved("Ваяна", Some(2026), cinema = Multikino)
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 1
  }

  it should "fold prefix- and suffix-decorated unresolved editions onto their base via the token-run edge" in {
    // The token-run edge folds an UNRESOLVED decorated edition onto its resolved base
    // when the base tokens are a PREFIX or SUFFIX run of the edition — a decoration no
    // rule strips, so neither the search-title nor tmdbId edge applies; only the
    // token-run edge can fold. Exercises BOTH the first-token (prefix) and last-token
    // (suffix) index buckets the O(n²)→indexed rewrite introduced.
    val rows = Seq(
      resolved("Fight Club", tmdbId = 550, tmdbYear = 1999, cinema = Helios),
      unresolved("Fight Club Maraton Grozy", None, cinema = Multikino),   // base is a PREFIX run
      unresolved("Nocny Pokaz Fight Club", None, cinema = Kinoteka)       // base is a SUFFIX run
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 1
  }

  it should "REFUSE a token-run fold when the edition edge-matches two DIFFERENT resolved films (ambiguous)" in {
    // "Alfa Beta Gamma" starts with base A ("Alfa Beta", tmdb 1) and ends with base B
    // ("Beta Gamma", tmdb 2) — two distinct tmdbIds, so it attaches to NEITHER. It can
    // only interact with the bases via the token-run edge (no shared sanitize / tmdbId /
    // search title), so this directly guards the indexed candidate-gathering + the
    // distinct-tmdbId ambiguity refusal.
    val rows = Seq(
      resolved("Alfa Beta", tmdbId = 1, tmdbYear = 2020, cinema = Helios),
      resolved("Beta Gamma", tmdbId = 2, tmdbYear = 2021, cinema = Multikino),
      unresolved("Alfa Beta Gamma", None, cinema = Kinoteka)
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 3
  }

  it should "REFUSE a token-run fold when the edition's cinemas publish a CONTRADICTING film" in {
    // The production row this exists for: Kino Pionier lists "Ktoś całkiem obcy"
    // (Brandt Andersen's "I Was A Stranger", 103 min, 2024), whose tokens END with
    // the whole of the resolved one-token base "Obcy" (Ozon's "L'étranger", 122
    // min) — so the token-run edge adopted it, `clusterByFilm` attached it (2024 is
    // within ±2 of 2025), and the collapse put a second film on the row.
    //
    // That is the exact row `MixedFilmSplitter` then splits back out, which is why
    // the fold has to refuse: the split re-diverts the slot to staging, the fold
    // gives it a row of its own, this edge adopts it again, and the settle splits
    // again — forever. The Poland convergence leg failed on precisely that loop,
    // on the "a further settle splits nothing" assertion.
    val rows = Seq(
      cacheKey("Obcy", Some(2025)) -> MovieRecord(
        tmdbId = Some(1429348),
        data = Map[Source, SourceData](
          Tmdb      -> SourceData(releaseYear = Some(2025)),
          Multikino -> SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"),
                                  runtimeMinutes = Some(122), releaseYear = Some(2025)))),
      cacheKey("Ktoś całkiem obcy", Some(2024)) -> MovieRecord(
        data = Map[Source, SourceData](
          KinoPionier -> SourceData(title = Some("KTOŚ CAŁKIEM OBCY"), originalTitle = Some("I Was A Stranger"),
                                    runtimeMinutes = Some(103), releaseYear = Some(2024))))
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 2
  }

  it should "REFUSE a token-run fold when the edition's own title is a resolved film's title" in {
    // THE PRODUCTION SHAPE, which the contradiction guard above cannot see. Thirteen
    // Cinema City venues list "Ktoś całkiem obcy" (2024) at 104 minutes and publish NO
    // original title — and `MixedFilmDetector` reads its evidence only from slots that
    // publish one, so that row contradicts nothing however plainly its runtime
    // disagrees with the 122-minute "Obcy". The edge adopted it, `clusterByFilm`
    // attached it (2024 is within ±2 of 2025), and every re-scrape pulled the venues
    // back off Ozon's film — the churn the Poland convergence leg failed on twice.
    //
    // No cinema has to say anything for this to be answerable: the corpus holds a
    // RESOLVED row keyed by that very title (the 2007 *Perfect Stranger*), so it is a
    // film's own name rather than a decoration of the shorter "Obcy".
    val rows = Seq(
      cacheKey("Obcy", Some(2025)) -> MovieRecord(
        tmdbId = Some(1429348),
        data = Map[Source, SourceData](
          Tmdb      -> SourceData(releaseYear = Some(2025)),
          Multikino -> SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"),
                                  runtimeMinutes = Some(122), releaseYear = Some(2025)))),
      cacheKey("Ktoś całkiem obcy", Some(2007)) -> MovieRecord(
        tmdbId = Some(7183),
        data = Map[Source, SourceData](
          Tmdb     -> SourceData(releaseYear = Some(2007)),
          KinoLuna -> SourceData(title = Some("Ktoś całkiem obcy")))),
      // Exactly what Cinema City publishes: a year, a runtime, and no original title.
      cacheKey("Ktoś całkiem obcy", Some(2024)) -> MovieRecord(
        data = Map[Source, SourceData](
          CinemaCityArkadia -> SourceData(title = Some("Ktoś całkiem obcy"),
                                          runtimeMinutes = Some(104), releaseYear = Some(2024))))
    )

    val components = FilmCanonicalizer.groupByFilm(rows, titleNormalizer)
    withClue(s"components: ${components.map(_.map(r => s"${r._1.cleanTitle}|${r._1.year.getOrElse("-")}").mkString("+")).mkString(" / ")} ") {
      // Ozon's film stands alone; the two same-titled rows share a component, where the
      // year rules — not this edge — decide whether they are one film.
      components should have size 2
    }
    val obcy = components.find(_.exists(_._1.cleanTitle == "Obcy")).get
    withClue("the Cinema City venues must not be adopted onto Ozon's film: ") {
      obcy.map(_._1.cleanTitle) shouldBe Seq("Obcy")
    }
  }

  it should "still fold a token-run edition whose cinema AGREES with the base film" in {
    // The counterpart, and the reason the refusal is keyed on a positive
    // contradiction rather than on the base being one token: a genuine decorated
    // edition publishes the same original title and runtime as its base, so it
    // still folds — the edge keeps doing its job.
    val rows = Seq(
      cacheKey("Obcy", Some(2025)) -> MovieRecord(
        tmdbId = Some(1429348),
        data = Map[Source, SourceData](
          Tmdb      -> SourceData(releaseYear = Some(2025)),
          Multikino -> SourceData(title = Some("Obcy"), originalTitle = Some("L'étranger"),
                                  runtimeMinutes = Some(122), releaseYear = Some(2025)))),
      cacheKey("Nocny pokaz Obcy", None) -> MovieRecord(
        data = Map[Source, SourceData](
          KinoPionier -> SourceData(title = Some("Nocny pokaz Obcy"), originalTitle = Some("L'étranger"),
                                    runtimeMinutes = Some(122))))
    )
    FilmCanonicalizer.groupByFilm(rows, titleNormalizer) should have size 1
  }

  it should "fold a programme/decorated edition sharing the base tmdbId into one record" in {
    // "Zaproszenie | Kinoteka dla rodziców" resolves to the base film's tmdbId, so
    // it is the SAME film and folds onto one storage record — even though its key
    // is not a bare TMDB alias. The programme banner is no longer a separate ROW;
    // the read-model projection splits it back into its own CARD by shown title
    // (see ReadModelProjectionSpec's `projectAll` cases). Both cinemas' slots —
    // and their distinct titles — survive the fold so the split can recover them.
    val rows = Seq(
      aliased("Zaproszenie",                       tmdbId = 9001, tmdbYear = 2022, tmdbTitle = "Zaproszenie", originalTitle = "The Invitation", cinema = Helios,   cinemaTitle = "Zaproszenie"),
      aliased("Zaproszenie | Kinoteka dla rodziców", tmdbId = 9001, tmdbYear = 2022, tmdbTitle = "Zaproszenie", originalTitle = "The Invitation", cinema = Kinoteka, cinemaTitle = "Zaproszenie | Kinoteka dla rodziców")
    )
    val components = FilmCanonicalizer.groupByFilm(rows, titleNormalizer)
    withClue(s"components: ${components.map(_.map(_._1.cleanTitle))}\n") {
      components should have size 1
    }
    val clusters = FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer)
    clusters should have size 1                                       // one tmdbId → one film
    val (_, merged) = FilmCanonicalizer.canonical(clusters.head, titleNormalizer)
    merged.evidence.titles shouldBe Set("Zaproszenie", "Zaproszenie | Kinoteka dla rodziców")
  }

  it should "keep a remake (two distinct tmdbIds sharing a title) as one component but two clusters" in {
    val components = FilmCanonicalizer.groupByFilm(Seq(
      resolved("Diuna", tmdbId = 100, tmdbYear = 1984, cinema = KinoMuza),
      resolved("Diuna", tmdbId = 200, tmdbYear = 2021, cinema = KinoMuzeumGdansk)
    ), titleNormalizer)
    components should have size 1                                  // same sanitized title → one component
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 2  // split back out by tmdbId
  }

  // Prod, 2026-08-14: `ghost2bigtorig|2025` (tmdbId 1568069) and `ghost2bigtorig|2026`
  // (tmdbId 1693400) are ONE film TMDB holds twice — both carry imdbId tt43683692 — and
  // 44 cinema slots were filed under each. Splitting on tmdbId alone gave the film two
  // rows nothing could ever rejoin, so the read model projected a card each and the site
  // showed it twice under one slug. `scripts.DuplicateAudit` calls a shared imdbId the
  // gold standard for "same film"; the fold now acts on it.
  private def withImdb(row: (CacheKey, MovieRecord), imdbId: String): (CacheKey, MovieRecord) =
    row._1 -> row._2.copy(imdbId = Some(imdbId))

  it should "fold two tmdbIds that TMDB gave the SAME imdbId into one cluster" in {
    val components = FilmCanonicalizer.groupByFilm(Seq(
      withImdb(resolved("Ghost 2", tmdbId = 1568069, tmdbYear = 2025, cinema = KinoMuza), "tt43683692"),
      withImdb(resolved("Ghost 2", tmdbId = 1693400, tmdbYear = 2026, cinema = KinoMuzeumGdansk), "tt43683692")
    ), titleNormalizer)
    components should have size 1
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 1
  }

  it should "still keep two imdbId-sharing rows apart when their cinemas published different films" in {
    // The refuse-on-contradiction guard, the same one the containment edge carries: an
    // id agreement may never merge what `MixedFilmSplitter` would split straight back
    // out, or the two chase each other and the settle never reaches a fixpoint.
    val components = FilmCanonicalizer.groupByFilm(Seq(
      published("Joanna d'Arc", 1, 1999, KinoMuza,         "Joan of Arc",    160, imdbId = Some("tt43683692")),
      published("Joanna d'Arc", 2, 2025, KinoMuzeumGdansk, "Johanna af Ork", 108, imdbId = Some("tt43683692"))
    ), titleNormalizer)
    components should have size 1
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 2
  }

  it should "fold imdbId-sharing tmdbIds transitively, veto on published evidence, and leave the rest alone — one pinned partition" in {
    // A realistic component pinned IN FULL — cluster order and member order — so the
    // imdbId step of the fold can be rewritten against a byte-exact expectation. Mixed
    // evidence: tmdbIds 100 and 200 share tt1; 200 also carries tt2 (one film TMDB holds
    // twice, its rows resolved to different IMDb records on different days), which 300
    // shares — so 100 and 300 are one film ONLY through 200, a union the fold has to be
    // transitive to find. 400 carries no imdbId. 500 and 600 share tt3 but their cinemas
    // published different films (the veto above). A year-bearing unresolved row attaches
    // to the folded cluster (rule 2, 2024 within ±2 of its 2025 reference year); a
    // yearless one stands alone because the group holds several films (rule 4).
    val a  = withImdb(resolved("Ghost 2",  tmdbId = 100, tmdbYear = 2025, cinema = KinoMuza),         "tt1")
    val b  = withImdb(resolved("Ghost 2",  tmdbId = 200, tmdbYear = 2026, cinema = KinoMuzeumGdansk), "tt1")
    val b2 = withImdb(resolved("Ghost II", tmdbId = 200, tmdbYear = 2026, cinema = Helios),           "tt2")
    val c  = withImdb(resolved("Ghost 2",  tmdbId = 300, tmdbYear = 2027, cinema = Multikino),        "tt2")
    val d  = resolved("Ghost 2", tmdbId = 400, tmdbYear = 2019, cinema = Kinoteka)
    val e  = published("Ghost 2", 500, 2020, KinoPort, "Ghost Two",      100, imdbId = Some("tt3"))
    val f  = published("Ghost 2", 600, 2021, KinoZak,  "Spectre Second", 130, imdbId = Some("tt3"))
    val g  = unresolved("Ghost 2", Some(2024), cinema = KinoSpektrum)
    val h  = unresolved("GHOST 2", None,       cinema = KinoIkm)
    val rows = Seq(h, f, c, b2, a, e, d, g, b)   // deliberately scrambled

    val expected = Seq(Seq(a, b, b2, c, g), Seq(d), Seq(e), Seq(f), Seq(h)).map(_.map(_._1))
    FilmCanonicalizer.clusterByFilm(rows, titleNormalizer).map(_.map(_._1)) shouldBe expected

    // Every rank here is distinct, so not just the membership but the whole partition —
    // cluster order and member order — is a pure function of the row SET.
    (1 to 25).foreach { seed =>
      val shuffled = new scala.util.Random(seed).shuffle(rows)
      withClue(s"seed $seed, order ${shuffled.map(_._1.cleanTitle)}: ") {
        FilmCanonicalizer.clusterByFilm(shuffled, titleNormalizer).map(_.map(_._1)) shouldBe expected
      }
    }
  }

  it should "read a row's published identity once, however many siblings it is compared against" in {
    // `MixedFilmDetector` rebuilds a row's identity from its slots on every question, and
    // the fold asks about a tmdbId group's main row against EVERY sibling (and about every
    // cross-group pair sharing an imdbId) — so one row's identity was rebuilt once per
    // comparison. Counted at the normalizer: a word only the main row's cinema publishes
    // is sanitized the same number of times whether the row has one sibling or ten.
    class CountingNormalizer extends TitleNormalizer(titleNormalizer.rules) {
      var zebraphantHits = 0
      override def sanitize(title: String): String = {
        if (title == "Zebraphant") zebraphantHits += 1
        super.sanitize(title)
      }
    }
    def sanitizeHits(siblings: Int): Int = {
      val counting = new CountingNormalizer
      val main     = published("Ghost 2", 700, 2025, KinoMuza, "Zebraphant", 100)
      // Same film — a differing original title agreeing on runtime is not a contradiction.
      val rest     = (1 to siblings).map(i => published(s"Ghost 2 ($i)", 700, 2025, CinemaShowing(KinoMuzeumGdansk, s"v$i"), "Ghost Two", 100))
      FilmCanonicalizer.clusterByFilm(main +: rest, counting) should have size 1
      counting.zebraphantHits
    }
    sanitizeHits(1) should be > 0                 // the counter sees the identity being built at all
    sanitizeHits(10) shouldBe sanitizeHits(1)
  }

  it should "keep two rows sharing ONE tmdbId apart when their cinemas published different films" in {
    // The same refusal one rung down. A shared tmdbId is not permission to merge either:
    // a row can be holding an id that is not its film's, and merging a second row onto it
    // buries the disagreement inside one record — prod's "Mistyczka" absorbed Kino Klaps's
    // listing of a different 2026 film that way, and the merged row then had a venue naming
    // each film. The evidence is `MixedFilmDetector`'s, so a Polish title beside a foreign
    // original (73 of 572 PL films) still merges; only a CORROBORATED contradiction splits.
    val components = FilmCanonicalizer.groupByFilm(Seq(
      published("Mistyczka",                          1646379, 2026, KinoMuza,         "Mistyczka",             87),
      published("DOBRE Kino - Maryja. Matka Papieża", 1646379, 2026, KinoMuzeumGdansk, "Maryja. Matka Papieża", 62)
    ), titleNormalizer)
    components should have size 1                                  // the shared tmdbId links them
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 2
  }

  it should "still merge two rows sharing a tmdbId when nothing their cinemas published contradicts it" in {
    // The ordinary case the rule above must not touch: one film, two spellings.
    val components = FilmCanonicalizer.groupByFilm(Seq(
      resolved("Zaplątani", tmdbId = 38757, tmdbYear = 2010, cinema = KinoMuza),
      resolved("Tangled",   tmdbId = 38757, tmdbYear = 2010, cinema = KinoMuzeumGdansk)
    ), titleNormalizer)
    components should have size 1
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 1
  }

  it should "split two curated franchise siblings sharing a WRONG tmdbId, even with no originalTitle/runtime/year evidence" in {
    // UK prod, 2026-09-16: Odeon's rerelease-season listings for "Catching Fire" and
    // "The Ballad of Songbirds and Snakes" publish no originalTitle/runtime/year at
    // all (small-chain listings, title only) — `MixedFilmDetector` sees no evidence
    // either way and would wave the merge through, exactly as it does for the
    // legitimate "Zaplątani"/"Tangled" case right below. Only `SequelMarker`'s
    // curated Hunger Games siblings list can tell these apart from bare titles alone.
    val bareCinemaSlot = (title: String, cinema: Source) =>
      cacheKey(title, Some(2026)) -> MovieRecord(
        tmdbId = Some(1300968),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(releaseYear = Some(2026)),
          cinema -> SourceData(title = Some(title), releaseYear = Some(2026))))
    val components = FilmCanonicalizer.groupByFilm(Seq(
      bareCinemaSlot("The Hunger Games: Catching Fire", Helios),
      bareCinemaSlot("The Hunger Games: The Ballad of Songbirds and Snakes", Multikino)
    ), titleNormalizer)
    components should have size 1                                  // the shared (wrong) tmdbId links them
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 2
  }

  it should "also split two curated-base siblings that differ only by a numbered part" in {
    // `curatedSiblings` gates the check on a CURATED base prefix match, but once
    // that gate passes it still asks `namesAnotherEntry`'s general ordinal logic —
    // so "Mockingjay Part 1" vs "Part 2" splits too, correctly (they ARE different
    // films). What the curated gate protects against is an UNCURATED title merely
    // ending in a number, never this genuine same-franchise numbered pair.
    val bareCinemaSlot = (title: String, cinema: Source) =>
      cacheKey(title, Some(2026)) -> MovieRecord(
        tmdbId = Some(1300968),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(releaseYear = Some(2026)),
          cinema -> SourceData(title = Some(title), releaseYear = Some(2026))))
    val components = FilmCanonicalizer.groupByFilm(Seq(
      bareCinemaSlot("The Hunger Games: Mockingjay - Part 1", Helios),
      bareCinemaSlot("The Hunger Games: Mockingjay - Part 2", Multikino)
    ), titleNormalizer)
    components should have size 1
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 2
  }

  it should "still merge an UNCURATED title's incidental numbered suffix sharing a tmdbId (evidence-free)" in {
    // The guard `curatedSiblings` needs: unlike the Hunger Games base above, "Ghost 2"
    // is not a curated franchise prefix, so a coincidental "(1)"-style suffix in
    // ordinary cinema title text (a disambiguator, a re-run marker, anything) must
    // NOT be read as a sequel split — only a CURATED base's own known siblings may.
    val bareCinemaSlot = (title: String, cinema: Source) =>
      cacheKey(title, Some(2025)) -> MovieRecord(
        tmdbId = Some(700),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(releaseYear = Some(2025)),
          cinema -> SourceData(title = Some(title), releaseYear = Some(2025))))
    val components = FilmCanonicalizer.groupByFilm(Seq(
      bareCinemaSlot("Ghost 2", Helios),
      bareCinemaSlot("Ghost 2 (1)", Multikino)
    ), titleNormalizer)
    components should have size 1
    FilmCanonicalizer.clusterByFilm(components.head, titleNormalizer) should have size 1
  }

  it should "key a cross-language cluster on the dominant cinema title, not the alphabetical min" in {
    // The churn guard: keying on the alphabetical min ("tangled") would leave an
    // _id no cinema reports, so every "Zaplątani" scrape would re-spawn the row.
    val (canonicalKey, _) = FilmCanonicalizer.canonical(Seq(
      aliased("Tangled",   tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Multikino, cinemaTitle = "Zaplątani"),
      aliased("Zaplątani", tmdbId = 38757, tmdbYear = 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Helios,    cinemaTitle = "Zaplątani")
    ), titleNormalizer)
    titleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe "zaplatani"
  }

  it should "keep a decorated variant's own spelling, not the base title its Tmdb slot carries" in {
    // A dubbed variant resolved to the base film's tmdbId, so its Tmdb slot
    // title is the BARE base "Straszny film" — a DIFFERENT sanitize. Canonicalising
    // to that cross-sanitize title would re-key the dub onto the base and collapse
    // it; the slot title must be ignored because it doesn't match the variant.
    val cluster = Seq(
      cacheKey("Straszny film ukraiński dubbing", Some(2026)) -> MovieRecord(
        tmdbId = Some(12345),
        data = Map[Source, SourceData](
          Tmdb   -> SourceData(title = Some("Straszny film"), releaseYear = Some(2026)),
          Helios -> SourceData(title = Some("Straszny film ukraiński dubbing"), releaseYear = Some(2026))))
    )

    val (canonicalKey, _) = FilmCanonicalizer.canonical(cluster, titleNormalizer)

    // The dub keeps its own spelling; it does NOT collapse onto the base "Straszny film".
    titleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe
      titleNormalizer.sanitize("Straszny film ukraiński dubbing")
    canonicalKey.cleanTitle should include ("dubbing")
  }

  // Each order-sensitivity case above proves ONE fold rule on a 2-row cluster,
  // where `rows.reverse` is the only alternate ordering. The settle's internals,
  // though, are multi-row: union-find over sanitize/tmdbId/alias edges
  // (`groupByFilm`), a `LinkedHashMap` of ±-window attachments and an
  // index-tie-broken `minByOption` (`clusterByFilm` rules 2 & 4), and a
  // `foldLeft`-driven `unionAll` (`canonical`). A 2-row case can't expose a bug
  // that only bites once three or more rows interleave. This case assembles ONE
  // film reported five different ways across five cinemas — every fold rule at
  // once — and asserts the settle lands on the SAME single cluster (and the same
  // canonical key) for ALL 5! = 120 arrival orders, the full-corpus guarantee
  // `StagingOrderDeterminismSpec` checks end-to-end, pinned here at the unit seam.
  "the full settle (groupByFilm → clusterByFilm → canonical)" should
    "collapse one five-cinema film to one cluster + key for every arrival order" in {
    val polish = "Zaplątani"
    // tmdbId 38757 with PL title "Zaplątani" / original "Tangled" (also its
    // englishTitle alias). The five rows exercise, respectively: a bare resolved
    // PL row, a bare resolved EN row folded by the shared tmdbId edge, a ±2-year
    // unresolved straggler (rule 2), a yearless EN straggler folded by the
    // englishTitle alias edge then rule 4, and a yearless PL straggler (rule 4).
    val rows = Seq(
      aliased(polish,    tmdbId = 38757, tmdbYear = 2010, tmdbTitle = polish, originalTitle = "Tangled",
        cinema = Helios,    cinemaTitle = polish,    englishTitle = Some("Tangled")),
      aliased("Tangled", tmdbId = 38757, tmdbYear = 2010, tmdbTitle = polish, originalTitle = "Tangled",
        cinema = Multikino, cinemaTitle = "Tangled", englishTitle = Some("Tangled")),
      unresolved(polish,    Some(2012), cinema = KinoMuza),          // production-year +2, rule 2
      unresolved("Tangled", None,       cinema = KinoMuzeumGdansk),  // alias-edge + rule 4
      unresolved(polish,    None,       cinema = Kinoteka)           // rule 4
    )
    val allCinemas = Set[Source](Helios, Multikino, KinoMuza, KinoMuzeumGdansk, Kinoteka)

    val settled = rows.permutations.toList.map { ordered =>
      val clusters = FilmCanonicalizer.groupByFilm(ordered, titleNormalizer).flatMap(FilmCanonicalizer.clusterByFilm(_, titleNormalizer))
      withClue(s"order ${ordered.map(r => (r._1.cleanTitle, r._1.year))} → " +
               s"${clusters.map(_.map(c => (c._1.cleanTitle, c._1.year)))}\n") {
        clusters should have size 1
        clusters.head.flatMap(_._2.cinemaData.keySet).toSet shouldBe allCinemas
      }
      val (canonicalKey, merged) = FilmCanonicalizer.canonical(clusters.head, titleNormalizer)
      (canonicalKey, merged.tmdbId)
    }

    // The canonical identity is one value, not 120: same key + tmdbId every order.
    settled.distinct should have size 1
    val (canonicalKey, tmdbId) = settled.head
    canonicalKey.year shouldBe Some(2010)                          // TMDB year, not the 2012 straggler
    titleNormalizer.sanitize(canonicalKey.cleanTitle) shouldBe "zaplatani"  // dominant PL spelling (3 of 5)
    tmdbId shouldBe Some(38757)
  }

  "clusterByFilm rule-2 tie-break" should
    "attach an equidistant straggler to the same remake for every arrival order" in {
    // A remake keyed under one title: two resolved films two years apart, and an
    // unresolved straggler EXACTLY equidistant from both (Δ1 each). Rule 2 attaches
    // it to the NEAREST resolved cluster; on a tie it must break on a pure function
    // of the row set (the cluster's `minRank`, then its tmdbId-sorted index), NOT on
    // which film the straggler happened to arrive after. A yearless+idless row
    // alongside two distinct films can't be attributed to either, so rule 4 leaves
    // it standing alone. Both outcomes must hold for ALL 4! = 24 orders — the
    // index-tie-break is exactly the kind of "stable only because we sorted first"
    // invariant a single fixed ordering can silently pass while broken.
    val rows = Seq(
      resolved  ("Diuna", tmdbId = 100, tmdbYear = 2018, cinema = Helios),
      resolved  ("Diuna", tmdbId = 200, tmdbYear = 2020, cinema = Multikino),
      unresolved("Diuna", Some(2019), cinema = KinoMuza),       // Δ1 from BOTH → tie → lower tmdbId (100)
      unresolved("Diuna", None,       cinema = Kinoteka)        // yearless + 2 films → own cluster (rule 4)
    )

    val perOrder = rows.permutations.toList.map { ordered =>
      val clusters = FilmCanonicalizer.clusterByFilm(ordered, titleNormalizer)
      // Each cluster as its set of cinemas, the whole partition as a sorted set —
      // a representation independent of cluster/row emission order.
      clusters.map(_.flatMap(_._2.cinemaData.keySet).toSet).toSet
    }

    perOrder.distinct should have size 1   // the partition itself never depends on order
    perOrder.head shouldBe Set(
      Set[Source](Helios, KinoMuza),   // tmdbId 100 + the equidistant straggler (tie → lower id)
      Set[Source](Multikino),          // tmdbId 200, alone
      Set[Source](Kinoteka)            // yearless straggler, alone (two distinct films → rule 4 refuses)
    )
  }
}
