package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Drives `MovieCache.canonicalizeBySanitize` directly over hand-built corpora
 * to pin the ±2-year film-identity collapse the settle/hydrate pass performs.
 *
 * A single `sanitize(title)` group can legitimately cover several films — a
 * remake carrying the original's name, or one film cinemas date a couple of years
 * apart (production vs theatrical). The old rule BAILED on any group holding >1
 * distinct tmdbId (left it fully split) and otherwise collapsed the WHOLE group
 * to one row — so it could neither separate two genuine films nor cap an
 * unbounded run of adjacent years. The new rule sub-clusters per film: resolved
 * rows split by tmdbId, year-bearing unresolved rows attach within ±2 of a
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

  // A resolved row keyed under `keyTitle` for a film TMDB knows as `tmdbTitle`
  // (Polish) / `originalTitle`. Lets a film be keyed under its original/English
  // title while TMDB still identifies it by its Polish title + tmdbId.
  private def aliasedRow(
    c: MovieCache, keyTitle: String, cinema: Cinema, year: Int, tmdbId: Int,
    tmdbTitle: String, originalTitle: String, cinemaTitle: String): Unit =
    c.put(c.keyOf(keyTitle, Some(year)),
      MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
        (Tmdb: Source)   -> SourceData(title = Some(tmdbTitle), originalTitle = Some(originalTitle), releaseYear = Some(year)),
        (cinema: Source) -> SourceData(title = Some(cinemaTitle), releaseYear = Some(year)))))

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

  it should "preserve a rated row's ratings when a cross-language duplicate folds onto it" in {
    // The "missing & regaining ratings" flap. Since cross-language films are
    // merged by shared tmdbId (FilmCanonicalizer.groupByFilm), a freshly-resolved
    // but NOT-yet-rated translation duplicate ("Disclosure Day") clusters with the
    // already-rated Polish row ("Dzień objawienia"). The canonical pick orders by
    // `canonicalRank` → cleanTitle, so the alphabetically-earlier "Disclosure Day"
    // becomes the union base; if the merge takes ratings ONLY from the base, the
    // rated sibling's scores are dropped and the public card loses every rating but
    // the IMDb link — until a later rating refresh re-fetches them (the flap).
    val c = cache
    // Rated Polish row (the victim): full ratings, keyed under its Polish title.
    c.put(c.keyOf("Dzień objawienia", Some(2026)),
      MovieRecord(
        tmdbId = Some(1275779), imdbId = Some("tt15047880"),
        imdbRating = Some(6.8), metascore = Some(74), rottenTomatoes = Some(80),
        filmwebRating = Some(5.9357),
        filmwebUrl = Some("https://www.filmweb.pl/film/Dzie%C5%84+objawienia-2026-10060793"),
        metacriticUrl = Some("https://www.metacritic.com/movie/disclosure-day"),
        rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/disclosure_day"),
        data = Map[Source, SourceData](
          (Tmdb: Source)   -> SourceData(title = Some("Dzień objawienia"), originalTitle = Some("Disclosure Day"), releaseYear = Some(2026)),
          (Helios: Source) -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)))))
    // English-title duplicate (the canonical base): same tmdbId, just resolved,
    // ratings not fetched yet. Its key sanitizes to a TMDB alias, so it's a bare
    // film title and folds onto the Polish row by shared tmdbId.
    aliasedRow(c, "Disclosure Day", KinoMuza, 2026, 1275779, "Dzień objawienia", "Disclosure Day", "Disclosure Day")
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE merged row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    val merged = r.head.record
    withClue(s"ratings were dropped by the cross-language merge: $merged\n") {
      merged.imdbRating shouldBe Some(6.8)
      merged.metascore shouldBe Some(74)
      merged.rottenTomatoes shouldBe Some(80)
      merged.filmwebRating shouldBe Some(5.9357)
      merged.filmwebUrl shouldBe Some("https://www.filmweb.pl/film/Dzie%C5%84+objawienia-2026-10060793")
      merged.metacriticUrl shouldBe Some("https://www.metacritic.com/movie/disclosure-day")
      merged.rottenTomatoesUrl shouldBe Some("https://www.rottentomatoes.com/m/disclosure_day")
      merged.imdbId shouldBe Some("tt15047880")
    }
  }

  it should "re-kick the IMDb rating (as a retrigger) when a same-tmdbId merge fills a missing imdbId" in {
    // The cache must announce, per case, the enrichment a merge invalidated by
    // changing its input — here the imdbId, gained from the folded sibling.
    val captured = scala.collection.mutable.ListBuffer[Set[RetriggerKind]]()
    val recorder = new EnrichmentRetrigger {
      def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = { captured += kinds; () }
    }
    val c = new CaffeineMovieCache(new InMemoryMovieRepository, retrigger = recorder)
    // Same title + tmdbId under two YEARS (a production/theatrical drift) — distinct
    // keys the put-time tmdbId gate folds. The lower year sorts first (canonicalRank),
    // so the 2025 row — the one WITHOUT the imdb id — becomes canonical and gains the
    // id from the 2026 victim.
    c.put(c.keyOf("Film", Some(2025)), MovieRecord(tmdbId = Some(500),
      data = Map[Source, SourceData]((Tmdb: Source) -> SourceData(title = Some("Film"), releaseYear = Some(2025)))))
    c.put(c.keyOf("Film", Some(2026)), MovieRecord(tmdbId = Some(500), imdbId = Some("tt500"),
      data = Map[Source, SourceData]((Tmdb: Source) -> SourceData(title = Some("Film"), releaseYear = Some(2026)))))
    captured.flatten.toSet shouldBe Set(RetriggerKind.ImdbRating)
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

  it should "absorb a no-tmdbId year-bearing row within ±2 of a resolved cluster's TMDB year" in {
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

  it should "absorb a no-tmdbId production-year row a FULL two years off a resolved cluster" in {
    // The "Zawieście czerwone latarnie" flake: Kino Muzeum reports the film at its
    // 1989 production year while TMDB resolved it to the 1991 release — a 2-year
    // gap that the old ±1 window orphaned (so the row's visibility hinged on a
    // TMDB-resolution race). ±2 folds it onto the resolved cluster every time.
    val c = cache
    resolvedRow(c, "Zawieście czerwone latarnie", Helios,   1991, 31273)
    cinemaRow (c, "Zawieście czerwone latarnie", KinoMuza, Some(1989))  // 2 years off
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    r.head.year shouldBe Some(1991)
    r.head.record.cinemaData.keySet shouldBe Set(Helios, KinoMuza)
  }

  it should "NOT absorb a no-tmdbId year-bearing row three or more years off a resolved cluster" in {
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

  it should "collapse a film keyed under two languages (same tmdbId) into one row keyed on the Polish title" in {
    val c = cache
    // "Zaplątani" (Polish) and "Tangled" (original) are the same film: same tmdbId,
    // both keys are TMDB aliases. Their differing sanitized titles dodge the
    // put-time tmdbId gate, so two rows persist until settle folds them.
    aliasedRow(c, "Tangled",   Multikino, 2010, 38757, "Zaplątani", "Tangled", "Zaplątani")
    aliasedRow(c, "Zaplątani", Helios,    2010, 38757, "Zaplątani", "Tangled", "Zaplątani")
    rows(c) shouldBe Set(("Tangled", Some(2010)), ("Zaplątani", Some(2010)))
    c.canonicalizeBySanitize()
    val r = c.snapshot()
    withClue(s"expected ONE row, got ${r.map(x => (x.title, x.year))}\n")(r.size shouldBe 1)
    // Keyed on the cinema-reported Polish title, NOT the original "tangled" (which
    // no cinema reports — that would re-spawn the duplicate on the next scrape).
    TitleNormalizer.sanitize(r.head.title) shouldBe "zaplatani"
    r.head.record.tmdbId shouldBe Some(38757)
    r.head.record.cinemaData.keySet shouldBe Set(Multikino, Helios)
  }

  it should "not re-write a settled cross-language row on a SECOND canonicalize pass" in {
    // Regression for the worker CPU-credit churn: once "Tangled" + "Zaplątani"
    // fold to one row keyed on the Polish display title, that row still carries
    // BOTH cinema slot titles. A re-settle must recognise the corpus as already
    // canonical and write NOTHING — otherwise every backstop/rehydrate tick
    // re-upserts the same row to Mongo forever, pinning the worker at 90% steal.
    val repo = new InMemoryMovieRepository
    val c    = new CaffeineMovieCache(repo)
    // Multikino genuinely LISTS the film under its original title — slot title is
    // "Tangled", not just the row key — while Helios lists the Polish "Zaplątani".
    aliasedRow(c, "Tangled",   Multikino, 2010, 38757, "Zaplątani", "Tangled", "Tangled")
    aliasedRow(c, "Zaplątani", Helios,    2010, 38757, "Zaplątani", "Tangled", "Zaplątani")
    c.canonicalizeBySanitize()                 // pass 1: folds the two into one row
    c.snapshot().size shouldBe 1
    val writesAfterSettle = repo.upserts.size
    c.canonicalizeBySanitize()                 // pass 2: corpus already canonical
    withClue(s"second pass wrote ${repo.upserts.size - writesAfterSettle} time(s) to an already-settled corpus\n")(
      repo.upserts.size shouldBe writesAfterSettle)
  }

  it should "keep a decorated edition that carries the base tmdbId as its own row" in {
    val c = cache
    // The programme edition resolves to the base film's tmdbId but is separate by
    // design: its key is not a TMDB alias, so the cross-title merge leaves it alone.
    aliasedRow(c, "Zaproszenie", Helios, 2022, 9001, "Zaproszenie", "The Invitation", "Zaproszenie")
    aliasedRow(c, "Zaproszenie | Kinoteka dla rodziców", KinoMuza, 2022, 9001,
      "Zaproszenie", "The Invitation", "Zaproszenie | Kinoteka dla rodziców")
    c.canonicalizeBySanitize()
    rows(c).map(r => TitleNormalizer.sanitize(r._1)) shouldBe
      Set("zaproszenie", "zaproszeniekinotekadlarodzicow")
  }

  // --- merge metrics: each fold is counted, by reason --------------------------

  private class RecordingMergeMetrics extends MergeMetrics {
    val calls = scala.collection.mutable.ListBuffer[(MergeReason, Int)]()
    def recordMerge(reason: MergeReason, victims: Int): Unit = { calls += ((reason, victims)); () }
  }

  "the merge counter" should
    "record a Canonicalize fold (1 victim) when a cross-language same-tmdbId cluster collapses" in {
    val m = new RecordingMergeMetrics
    val c = new CaffeineMovieCache(new InMemoryMovieRepository, mergeMetrics = m)
    aliasedRow(c, "Tangled",   Multikino, 2010, 38757, "Zaplątani", "Tangled", "Zaplątani")
    aliasedRow(c, "Zaplątani", Helios,    2010, 38757, "Zaplątani", "Tangled", "Zaplątani")
    c.canonicalizeBySanitize()
    m.calls.toList shouldBe List(MergeReason.Canonicalize -> 1)
  }

  it should "record a TmdbIdentity fold (1 victim) when the put-time tmdbId gate collapses a duplicate" in {
    val m = new RecordingMergeMetrics
    val c = new CaffeineMovieCache(new InMemoryMovieRepository, mergeMetrics = m)
    // Same normalised title + tmdbId under two years — the put gate folds the second.
    c.put(c.keyOf("Film", Some(2025)), MovieRecord(tmdbId = Some(500),
      data = Map[Source, SourceData]((Tmdb: Source) -> SourceData(title = Some("Film"), releaseYear = Some(2025)))))
    c.put(c.keyOf("Film", Some(2026)), MovieRecord(tmdbId = Some(500),
      data = Map[Source, SourceData]((Tmdb: Source) -> SourceData(title = Some("Film"), releaseYear = Some(2026)))))
    m.calls.toList shouldBe List(MergeReason.TmdbIdentity -> 1)
  }

  it should "record a ResolvedSettle fold for a yearless stray absorbed by a year-bearing resolved row" in {
    val m = new RecordingMergeMetrics
    val c = new CaffeineMovieCache(new InMemoryMovieRepository, mergeMetrics = m)
    resolvedRow(c, "Dzień objawienia", Helios,    2026, 1275779)  // year-bearing, resolved
    cinemaRow (c, "Dzień objawienia", Multikino, None)            // a stranded yearless stray
    val resolved = MovieRecord(tmdbId = Some(1275779), data = Map[Source, SourceData](
      (Tmdb: Source)   -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026)),
      (Helios: Source) -> SourceData(title = Some("Dzień objawienia"), releaseYear = Some(2026))))
    c.settleResolved(c.keyOf("Dzień objawienia", Some(2026)), resolved)
    m.calls.toList shouldBe List(MergeReason.ResolvedSettle -> 1)
  }

  // ── restart guard against the per-deploy retrigger flap ──────────────────────
  // Re-key every settled row exactly as `StoredMovieRecord.fromStorage` rebuilds
  // it on hydrate (the `_id` stores only `sanitize(title)|year`, so the title is
  // re-derived via `displayTitle`), then re-settle — and NO enrichment may be
  // re-kicked. The settle keys a single-title cluster by `minSpelling` while the
  // hydrate key is `displayTitle`; when those differ only by case/punctuation the
  // re-key is harmless, but it must NOT count as a rating-input change. Before the
  // fix it re-kicked all three title-ratings for every such row on every boot, and
  // the next hydrate flipped the spelling back — the ~83 retriggers/boot rating
  // spike measured in prod (`CanonicalizeRetriggerFlapSpec`).
  private def assertRestartDoesNotReKickEnrichment(settled: MovieCache): Unit = {
    val retriggered = scala.collection.mutable.ListBuffer.empty[Set[RetriggerKind]]
    val rebooted = new CaffeineMovieCache(new InMemoryMovieRepository, retrigger = new EnrichmentRetrigger {
      def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = { retriggered += kinds; () }
    })
    settled.snapshot().foreach { r =>
      val recovered = StoredMovieRecord.fromStorage(StoredMovieRecord.idFor(r.title, r.year), r.record)
      rebooted.put(CacheKey(recovered.title, recovered.year), r.record)
    }
    rebooted.canonicalizeBySanitize()
    withClue(s"enrichment re-kicked on restart (the deploy flap): ${retriggered.toList}\n")(
      retriggered shouldBe empty)
  }

  "a settled corpus" should "not re-kick enrichment across a worker restart — decorated same-tmdbId editions don't flap ratings" in {
    val c = cache
    // Several cinemas list one film (tmdbId 439) under different decorated
    // spellings — the prod "Słodkie życie" / Fellini-retrospective shape that
    // flapped: displayTitle picks "Federico Fellini: Słodkie życie" while the old
    // min-spelling picked "…SŁODKIE ŻYCIE" (space sorts before colon).
    aliasedRow(c, "Federico Fellini: Słodkie życie", KinoMuza, 1960, 439, "Słodkie życie", "La dolce vita", "Federico Fellini SŁODKIE ŻYCIE")
    aliasedRow(c, "Słodkie życie",                    Helios,  1960, 439, "Słodkie życie", "La dolce vita", "Słodkie życie")
    c.canonicalizeBySanitize()
    assertRestartDoesNotReKickEnrichment(c)
  }
}
