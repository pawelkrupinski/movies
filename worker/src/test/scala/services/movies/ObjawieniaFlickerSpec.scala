package services.movies

import clients.TmdbClient
import models.{Cinema, CinemaCityKinepolis, CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.RoutingHttpFetch

import java.time.LocalDateTime

/**
 * Regression for the flickering "Dzień objawienia" — a film reported WITH a
 * release year by some cinemas (Helios → TMDB-resolved) and WITHOUT one by
 * others (Multikino). A concurrent scrape can strand the yearless reports in
 * their own `(title, None)` cache row beside the resolved `(title, Some(year))`
 * one; the yearless row is unresolved so the read-model projector holds it back
 * (`readyToProject == false`) — its cinemas vanish and the page shows only
 * Helios, until a later settle folds them back (then the next scrape
 * re-strands them → the flicker).
 *
 * Two fixes, both exercised here:
 *   - PART B: a later scrape of an already-concluded film lands its slot
 *     straight on the concluded row (`concludedKeyFor`, matched by title +
 *     optional year), so no held-back yearless variant is ever (re)spawned.
 *   - PART A: the moment TMDB resolution concludes (hit OR miss), the film
 *     settles its own `sanitize(title)` group in ONE merged write
 *     (`settleResolved`), folding any already-stranded sibling — so the row's
 *     first `readyToProject` upsert already carries every cinema (the read model
 *     never sees the single-cinema split).
 */
class ObjawieniaFlickerSpec extends AnyFlatSpec with Matchers {

  private val Title = "Dzień objawienia"
  private val When  = LocalDateTime.of(2026, 6, 14, 18, 0)

  private def slot(cinema: Cinema, year: Option[Int]): (Source, SourceData) =
    (cinema: Source) -> SourceData(
      title       = Some(Title),
      releaseYear = year,
      director    = Seq("Steven Spielberg"),
      showtimes   = Seq(Showtime(When, bookingUrl = None))
    )

  private def scrape(cinema: Cinema, year: Option[Int]): CinemaMovie =
    scrapeTitled(cinema, Title, year)

  private def scrapeTitled(cinema: Cinema, title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Seq("Steven Spielberg"),
      showtimes = Seq(Showtime(When, bookingUrl = None))
    )

  // What the read model would actually project: only `readyToProject` rows
  // reach `web_movies` (ReadModelProjector holds the rest back), so this is the
  // set of cinemas a user would SEE for the film.
  private def projectedCinemas(cache: MovieCache): Set[Cinema] =
    cache.snapshot().filter(_.record.readyToProject).flatMap(_.record.cinemaData.keys).toSet

  private def tmdbHit() = new TmdbClient(
    http = new RoutingHttpFetch(Map(
      "/search/movie" -> ("""{"results":[{"id":1275779,"title":"Dzień objawienia",""" +
        """"original_title":"The Revelation","release_date":"2026-01-01","popularity":99.0}]}"""),
      "/external_ids" -> """{"id":1275779,"imdb_id":"tt15047880"}""",
      "/credits"      -> """{"crew":[{"job":"Director","name":"Steven Spielberg"}]}"""
    ), getOnly = true),
    apiKey = Some("stub")
  )

  private def tmdbMiss() = new TmdbClient(
    http = new RoutingHttpFetch(Map("/search/movie" -> """{"results":[]}"""), getOnly = true),
    apiKey = Some("stub")
  )

  private def service(cache: MovieCache, tmdb: TmdbClient): MovieService =
    new MovieService(cache, new InProcessEventBus, tmdb)

  // ── PART B: a re-scrape folds into the concluded row, by title + opt. year ──

  "a later yearless scrape" should "fold into an already-resolved sibling instead of stranding a held-back row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)

    // The resolved, year-bearing row (Helios reported 2026, TMDB resolved it) …
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"), data = Map(slot(Helios, Some(2026)))))
    // … and a stranded yearless, still-unresolved row a race left behind.
    cache.put(cache.keyOf(Title, None),
      MovieRecord(data = Map(slot(Multikino, None))))

    // A fresh Multikino tick (still yearless). With two same-title rows present,
    // the unique-match redirect gives up — `concludedKeyFor` must still land the
    // slot on the resolved row.
    cache.recordCinemaScrape(Multikino, Seq(scrape(Multikino, None)))

    val resolved = cache.get(cache.keyOf(Title, Some(2026))).getOrElse(fail("resolved row vanished"))
    resolved.cinemaData.keySet shouldBe Set(Helios, Multikino)
    // The user sees both cinemas — not just Helios.
    projectedCinemas(cache) shouldBe Set(Helios, Multikino)
  }

  // The "growing Ukrainian duplicate" bug: the film is also listed by a few
  // cinemas under its Ukrainian title ("Denʹ istyny - UA" / Cyrillic "День
  // істини"). That listing is TMDB-resolved to the SAME film, so its row carries
  // the same tmdbId AND the Polish TMDB title as an alias — yet its KEY is the
  // Ukrainian spelling, which sorts BEFORE "Dzień objawienia" ("De" < "Dz") in
  // the canonicalRank tie-break. A plain-Polish scrape must NOT be routed onto
  // that Ukrainian-keyed sibling just because its alias matches and it sorts
  // first: a row whose OWN key is the scraped title always wins over an
  // alias-only match. Otherwise Polish cinemas pile onto the UA row, it starts
  // displaying "Dzień objawienia" too (display = dominant cinema spelling), and
  // the corpus shows two ever-growing "Dzień objawienia" rows that never merge.
  "a plain-Polish scrape" should "land on the Polish-keyed row, not a same-tmdbId Ukrainian sibling whose TMDB alias matches" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)

    val uaTitle = "Denʹ istyny - UA"
    // The Ukrainian-titled sibling: resolved to the same film (tmdbId + the Polish
    // TMDB title as alias), but keyed under the UA spelling.
    cache.put(cache.keyOf(uaTitle, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"),
        data = Map(slot(Helios, Some(2026)), Tmdb -> SourceData(title = Some(Title)))))
    // The real Polish row, keyed by the Polish title.
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"),
        data = Map(slot(Multikino, Some(2026)), Tmdb -> SourceData(title = Some(Title)))))

    // A fresh plain-"Dzień objawienia" scrape from a third cinema.
    cache.recordCinemaScrape(CinemaCityKinepolis, Seq(scrape(CinemaCityKinepolis, Some(2026))))

    // It lands on the Polish-keyed row …
    cache.get(cache.keyOf(Title, Some(2026))).getOrElse(fail("polish row vanished"))
      .cinemaData.keySet should contain(CinemaCityKinepolis)
    // … never on the Ukrainian-keyed sibling.
    cache.get(cache.keyOf(uaTitle, Some(2026))).getOrElse(fail("ua row vanished"))
      .cinemaData.keySet should not contain CinemaCityKinepolis
  }

  // Healing the ALREADY-folded prod state after the Ukrainian-marker rule change:
  // before the fix, "Dzień objawienia ukraiński dubbing" sanitized to the base
  // key, so Cinema City's slot lived INSIDE the base "Dzień objawienia" row. Once
  // the rule no longer strips the marker, that title keys to its own row — and a
  // backfill isn't needed: the next Cinema City scrape writes the slot on the new
  // key AND the prune drops the now-stale slot from the base row. This proves the
  // split + shed happen on one re-scrape (so prod self-heals within a scrape tick).
  "a re-scrape after the Ukrainian-marker rule change" should "split the dub slot onto its own row and shed it from the base row" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository)
    val dubTitle = "Dzień objawienia ukraiński dubbing"

    // Folded prod state: the base row carries Helios (base title) AND Cinema City's
    // dub slot (the old strip put it here, under the base key).
    cache.put(cache.keyOf(Title, Some(2026)), MovieRecord(data = Map(
      slot(Helios, Some(2026)),
      (CinemaCityKinepolis: Source) -> SourceData(title = Some(dubTitle), releaseYear = Some(2026),
        showtimes = Seq(Showtime(When, bookingUrl = None))))))

    // Both cinemas re-scrape under the new rules (the dub title no longer strips).
    cache.recordCinemaScrape(Helios, Seq(scrape(Helios, Some(2026))))
    cache.recordCinemaScrape(CinemaCityKinepolis, Seq(scrapeTitled(CinemaCityKinepolis, dubTitle, Some(2026))))

    // The dub variant is now its OWN row …
    val dubRow = cache.get(cache.keyOf(dubTitle, Some(2026))).getOrElse(fail("dub row never split out"))
    dubRow.cinemaData.keySet shouldBe Set(CinemaCityKinepolis)
    // … and the base row kept Helios but SHED the stale Cinema City dub slot.
    val baseRow = cache.get(cache.keyOf(Title, Some(2026))).getOrElse(fail("base row vanished"))
    baseRow.cinemaData.keySet shouldBe Set(Helios)
  }

  // The "two copies of Kumotry" bug: a cinema reports the film at the PRODUCTION
  // year (2025) while it's TMDB-resolved at the RELEASE year (2026). The scrape's
  // own year differs by one from the concluded row, and with several same-title
  // year-variants present the unique-match redirect gives up — so `concludedKeyFor`
  // is the only thing that can land the slot on the resolved row. It must match
  // within ±1 (the same adjacency `clusterByFilm` uses), else every tick
  // re-spawns a held-back `kumotry|2025` beside the resolved `kumotry|2026`.
  "a later ±1-year scrape" should "fold into the concluded sibling instead of spawning an off-by-one duplicate" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)

    // The resolved row at the TMDB release year 2026 …
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"), data = Map(slot(Helios, Some(2026)))))
    // … plus a stranded yearless sibling, so the unique-match redirect can't fire
    // (two same-title rows present) and `concludedKeyFor` must carry the match.
    cache.put(cache.keyOf(Title, None),
      MovieRecord(data = Map(slot(CinemaCityKinepolis, None))))

    // A cinema reports the film at the production year 2025 — one off the
    // resolved 2026 row.
    cache.recordCinemaScrape(Multikino, Seq(scrape(Multikino, Some(2025))))

    // No off-by-one `kumotry|2025` row was spawned …
    cache.get(cache.keyOf(Title, Some(2025))) shouldBe None
    // … the slot landed on the resolved row instead.
    val resolved = cache.get(cache.keyOf(Title, Some(2026))).getOrElse(fail("resolved row vanished"))
    resolved.cinemaData.keySet should contain(Multikino)
    resolved.tmdbId shouldBe Some(1275779)
  }

  // The "held resolved row" bug: a film is fully TMDB-resolved (tmdbId +
  // ratings + TMDB poster/synopsis) and carries its showtimes from the listing
  // tick, but ONE deferred cinema's detail-page fetch never concludes, so
  // `detailPending` stays true forever. Gating projection on detail completion
  // then hides an otherwise-complete film from EVERY cinema indefinitely — the
  // "Dzień objawienia" disappearance. A TMDB-resolved row must project even
  // while a cinema detail is still outstanding; the detail only adds
  // cinema-specific extras the resolved row doesn't need to be displayable.
  "a TMDB-resolved row with a still-pending cinema detail" should "project anyway, not be held back" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbId = Some(1275779), imdbId = Some("tt15047880"),
        detailPending = true, data = Map(slot(Helios, Some(2026)))))

    projectedCinemas(cache) shouldBe Set(Helios)
  }

  // Negative control: with no TMDB data at all, the cinema detail IS the only
  // source of a poster/synopsis, so a not-yet-concluded detail keeps the row
  // held — and a pre-enrichment orphan (neither tmdbId nor tmdbNoMatch) stays
  // out of the read model. The resolved-row relaxation above must not weaken
  // either case.
  "an unresolved row with a pending detail" should "stay held back until detail concludes" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    cache.put(cache.keyOf(Title, Some(2026)),
      MovieRecord(tmdbNoMatch = true, detailPending = true, data = Map(slot(Helios, Some(2026)))))
    projectedCinemas(cache) shouldBe empty

    cache.put(cache.keyOf(Title, None), MovieRecord(data = Map(slot(Multikino, None))))
    projectedCinemas(cache) shouldBe empty
  }

  // ── PART A: conclusion settles the film's group in one merged write ─────────

  "a TMDB HIT" should "fold a stranded yearless sibling onto the resolved row at conclusion" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    val movieService = service(cache, tmdbHit())

    // Unresolved pair, as a concurrent scrape leaves it: yeared Helios + yearless Multikino.
    cache.put(cache.keyOf(Title, Some(2026)), MovieRecord(data = Map(slot(Helios, Some(2026)))))
    cache.put(cache.keyOf(Title, None),       MovieRecord(data = Map(slot(Multikino, None))))

    movieService.reEnrichSync(Title, Some(2026))

    val rows = cache.snapshot()
    withClue(s"expected ONE row after conclusion, got ${rows.map(r => (r.title, r.year))}\n") {
      rows.size shouldBe 1
    }
    rows.head.record.tmdbId            shouldBe Some(1275779)
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino)
    projectedCinemas(cache)            shouldBe Set(Helios, Multikino)
  }

  "a TMDB MISS" should "still fold a stranded yearless sibling onto the concluded row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    val movieService = service(cache, tmdbMiss())

    cache.put(cache.keyOf(Title, Some(2026)), MovieRecord(data = Map(slot(Helios, Some(2026)))))
    cache.put(cache.keyOf(Title, None),       MovieRecord(data = Map(slot(Multikino, None))))

    // The miss conclusion (tmdbNoMatch) lives in `resolveTmdbOnce`, the
    // production ResolveTmdb handler path — `reEnrichSync` only handles hits.
    movieService.resolveTmdbOnce(Title, Some(2026), None, None, force = true)

    val rows = cache.snapshot()
    withClue(s"expected ONE row after a concluded miss, got ${rows.map(r => (r.title, r.year))}\n") {
      rows.size shouldBe 1
    }
    rows.head.record.tmdbConcluded     shouldBe true
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino)
    projectedCinemas(cache)            shouldBe Set(Helios, Multikino)
  }
}
