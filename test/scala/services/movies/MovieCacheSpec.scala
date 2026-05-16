package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class MovieCacheSpec extends AnyFlatSpec with Matchers {

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating, metascore = None, originalTitle = None)

  "MovieCache" should "hydrate from the repo on construction" in {
    val seed = Seq(("Drzewo Magii", Some(2024), mkEnrichment("tt1")))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo(seed))

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(mkEnrichment("tt1"))
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Drzewo Magii", Some(2024)))
  }

  // rehydrate(): reload from repo. Boot-time hydration goes through the same
  // method, so we cover the on-demand admin-endpoint behaviour here:
  // (a) in-memory rows that aren't in Mongo get dropped, (b) repo-side edits
  // become visible, (c) the negative cache is orthogonal and survives.
  "rehydrate" should "drop in-memory rows that aren't in the repo" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)

    // Land a row in the positive cache without writing to the repo, by using
    // Caffeine directly via the trait's `put`… actually `put` writes through.
    // Instead: put through the cache (repo gets the upsert), then delete from
    // the repo behind the cache's back so they diverge.
    cache.put(cache.keyOf("Ghost", Some(2024)), mkEnrichment("tt1"))
    repo.delete("Ghost", Some(2024))
    cache.get(cache.keyOf("Ghost", Some(2024))) shouldBe defined  // still cached

    val n = cache.rehydrate()

    n shouldBe 0
    cache.get(cache.keyOf("Ghost", Some(2024))) shouldBe None
  }

  it should "make repo-side edits visible" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1", rating = Some(7.0)))

    // Edit Mongo out-of-band: replace the rating.
    repo.upsert("X", Some(2024), mkEnrichment("tt1", rating = Some(9.5)))

    cache.rehydrate() shouldBe 1
    cache.get(cache.keyOf("X", Some(2024))).flatMap(_.imdbRating) shouldBe Some(9.5)
  }

  it should "leave the negative cache alone" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val key   = cache.keyOf("not-a-real-film", Some(2099))
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    cache.rehydrate()

    cache.isNegative(key) shouldBe true
  }

  it should "treat case + diacritics + whitespace differences as the same key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Drzewo Magii", Some(2024)), mkEnrichment("tt9"))

    val expected = mkEnrichment("tt9")
    cache.get(cache.keyOf("drzewo magii",   Some(2024))) shouldBe Some(expected)
    cache.get(cache.keyOf("DRZEWO   MAGII", Some(2024))) shouldBe Some(expected)
    // Different year is a different row.
    cache.get(cache.keyOf("Drzewo Magii",   Some(2025))) shouldBe None
  }

  "put" should "write through to the repo (cache + Mongo stay in lockstep)" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))

    repo.upserts.toList shouldBe List(("X", Some(2024), mkEnrichment("tt1")))
  }

  // ── tmdbId identity gate ───────────────────────────────────────────────────
  //
  // `put` is the single persist point in the codebase (every cache + Mongo
  // write goes through it). The gate is the chokepoint: when a write would
  // create a *new* row carrying a tmdbId that another row already holds,
  // the write is folded onto that canonical row instead — same film at two
  // (title, year) keys can never produce two persisted rows. Both the prod
  // path (MongoMovieRepo behind the cache) and the test path (this
  // InMemoryMovieRepo behind the cache) inherit the gate automatically;
  // there's no duplicate logic to maintain.

  private def mkResolved(tmdbId: Int, scrapes: Set[CinemaScrape] = Set.empty,
                        showings: Map[Cinema, CinemaShowings] = Map.empty): MovieRecord =
    MovieRecord(
      imdbId         = Some("tt-anything"),
      imdbRating     = Some(8.0),
      metascore      = None,
      originalTitle  = Some("Original"),
      tmdbId         = Some(tmdbId),
      cinemaScrapes  = scrapes,
      cinemaShowings = showings
    )

  "put with tmdbId" should "fold onto an existing key carrying the same tmdbId" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497, scrapes = Set(CinemaScrape(KinoPalacowe, "Viridiana", Some(1961)))))
    repo.upserts.clear()
    repo.deletes.clear()

    // Second write at K2 with same tmdbId — should NOT create a row at K2.
    cache.put(k2, mkResolved(4497, scrapes = Set(CinemaScrape(KinoPalacowe, "Viridiana", Some(1962)))))

    cache.get(k1) shouldBe defined
    cache.get(k2) shouldBe None
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Viridiana", Some(1961)))
  }

  it should "union cinemaScrapes from the victim onto the canonical row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497, scrapes = Set(CinemaScrape(KinoPalacowe, "Viridiana", Some(1961)))))
    cache.put(k2, mkResolved(4497, scrapes = Set(CinemaScrape(KinoPalacowe, "Viridiana", Some(1962)))))

    cache.get(k1).get.cinemaScrapes shouldBe Set(
      CinemaScrape(KinoPalacowe, "Viridiana", Some(1961)),
      CinemaScrape(KinoPalacowe, "Viridiana", Some(1962))
    )
  }

  it should "delete the victim from Mongo when the source key already held a row" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497))
    // Source row exists with no tmdbId yet (e.g. fresh scrape).
    cache.put(k2, mkResolved(4497).copy(tmdbId = None))
    repo.upserts.clear()
    repo.deletes.clear()

    // TMDB resolution lands on K2 — gate folds onto K1, K2 must be deleted.
    cache.put(k2, mkResolved(4497))

    repo.deletes.toList should contain (("Viridiana", Some(1962)))
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Viridiana", Some(1961)))
  }

  it should "write through to the repo at the canonical key, not the source" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497))
    repo.upserts.clear()

    cache.put(k2, mkResolved(4497, scrapes = Set(CinemaScrape(KinoPalacowe, "Viridiana", Some(1962)))))

    val titles = repo.upserts.map { case (t, y, _) => (t, y) }
    titles should contain (("Viridiana", Some(1961)))
    titles should not contain (("Viridiana", Some(1962)))
  }

  it should "NOT fold when tmdbId differs (two different films sharing a title)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val k1    = cache.keyOf("Wspinaczka", Some(2017))
    val k2    = cache.keyOf("Wspinaczka", Some(2025))
    cache.put(k1, mkResolved(111))  // film A
    cache.put(k2, mkResolved(222))  // film B — different tmdbId, genuinely different film

    cache.get(k1) shouldBe defined
    cache.get(k2) shouldBe defined
    cache.snapshot().map(r => (r.title, r.year)) should contain allOf (
      ("Wspinaczka", Some(2017)),
      ("Wspinaczka", Some(2025))
    )
  }

  it should "NOT fold when the value has no tmdbId yet (pre-resolution scrape)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497))
    // Fresh scrape at K2, tmdbId not yet known. Gate doesn't apply — the
    // scrape-layer redirect (recordCinemaScrape) is the right tool here,
    // not the tmdbId gate.
    cache.put(k2, mkEnrichment("tt-x").copy(tmdbId = None))

    cache.get(k1) shouldBe defined
    cache.get(k2) shouldBe defined
  }

  it should "be a no-op when the same key is re-written (regular update, not a fold)" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    cache.put(k1, mkResolved(4497))
    repo.deletes.clear()

    // Same key, same tmdbId — sibling check `k != key` excludes itself, no fold.
    cache.put(k1, mkResolved(4497).copy(imdbRating = Some(9.0)))

    cache.get(k1).get.imdbRating shouldBe Some(9.0)
    repo.deletes shouldBe empty
  }

  "invalidate" should "remove from both positive cache and repo" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val key   = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    repo.upserts.clear()

    cache.invalidate(key)

    cache.get(key) shouldBe None
    repo.deletes.toList shouldBe List(("X", Some(2024)))
  }

  "markMissing + isNegative" should "let callers track known-non-films without polluting the positive cache" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val key   = cache.keyOf("not-a-real-film", Some(2099))

    cache.isNegative(key) shouldBe false
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true
    cache.get(key) shouldBe None  // negative cache never returns a positive value
  }

  // ── putIfPresent: no-resurrection writes ───────────────────────────────────

  "putIfPresent" should "update an existing row and return true" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    cache.put(cache.keyOf("Existing", Some(2024)), mkEnrichment("tt1"))

    val landed = cache.putIfPresent(cache.keyOf("Existing", Some(2024)), _.copy(imdbRating = Some(8.5)))

    landed shouldBe true
    cache.get(cache.keyOf("Existing", Some(2024))).flatMap(_.imdbRating) shouldBe Some(8.5)
  }

  it should "be a no-op and return false when the row was deleted" in {
    val repo  = new InMemoryMovieRepo()
    val cache = new CaffeineMovieCache(repo)
    val key   = cache.keyOf("Gone", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    cache.invalidate(key)
    repo.upserts.clear()

    val landed = cache.putIfPresent(key, _.copy(imdbRating = Some(8.5)))

    landed shouldBe false
    cache.get(key) shouldBe None
    repo.upserts shouldBe empty
  }

  it should "operate on the current cached value, not a stale snapshot" in {
    // A rating listener that captured the row at T0, made a slow network
    // call, and now wants to update one field shouldn't clobber concurrent
    // updates to other fields. putIfPresent's updater receives the live row.
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val key   = cache.keyOf("Foo", Some(2024))
    cache.put(key, mkEnrichment("tt1"))

    // Simulate "another listener already wrote metascore" between read and write.
    cache.put(key, mkEnrichment("tt1").copy(metascore = Some(70)))

    cache.putIfPresent(key, current => current.copy(imdbRating = Some(8.5)))

    val row = cache.get(key).get
    row.imdbRating shouldBe Some(8.5)   // our update landed
    row.metascore  shouldBe Some(70)    // concurrent update preserved
  }

  // ── recordCinemaScrape: per-cinema slot management ─────────────────────────
  //
  // Each cinema's scrape tick lands a full snapshot of its current showings.
  // recordCinemaScrape merges that into the unified cache: find-or-create the
  // matching record, replace this cinema's slot in cinemaShowings, and prune
  // the cinema's slot from any record whose film stopped appearing in the
  // fresh batch.

  private def showtime(ymdHm: String): Showtime = {
    val parsed = LocalDateTime.parse(ymdHm)
    Showtime(parsed, bookingUrl = None)
  }

  private def cinemaMovie(title: String, cinema: Cinema, year: Option[Int] = Some(2026),
                          poster: Option[String] = None, showtimes: Seq[Showtime] = Seq.empty,
                          country: Option[String] = None): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year, country = country),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = None,
      synopsis  = None,
      cast      = None,
      director  = None,
      showtimes = showtimes
    )

  // ── Provenance: per-(cinema, title, year) scrape dedup ─────────────────────
  //
  // recordCinemaScrape returns a `(CinemaMovie, CacheKey, isNew)` triple per
  // input. `isNew` is true the first time that exact `(cinema, raw title, raw
  // year)` tuple lands on a row; false on subsequent ticks that report the
  // same combination. ShowtimeCache uses the flag to suppress redundant
  // MovieRecordCreated events — every TMDB / IMDb / rating fetcher's listener
  // re-checks state on each event, so re-publishing for unchanged tuples
  // just churns dispatches. Any change (new cinema, new title spelling, year
  // correction) still flips `isNew` back to true so genuine new context can
  // trigger better enrichment.

  "recordCinemaScrape" should "flag the first scrape of a (cinema, title, year) tuple as new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val touched = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    touched should have size 1
    touched.head._3 shouldBe true
  }

  it should "flag a repeat scrape of the same (cinema, title, year) as not-new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val secondTick = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    secondTick should have size 1
    secondTick.head._3 shouldBe false
  }

  it should "flag the second cinema as new when it scrapes a film already in the cache from another cinema" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val helios = cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Top Gun: Maverick", Helios, Some(2022))
    ))
    helios.head._3 shouldBe true   // Helios hasn't reported this row before
  }

  it should "flag a year correction from the same cinema as new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Bez wyjścia", Multikino, None)))
    // Same cinema now reports the same title with a year — different scrape
    // tuple, even though the redirect routes it onto the same row.
    val corrected = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Bez wyjścia", Multikino, Some(2025))
    ))
    corrected.head._3 shouldBe true
    corrected.head._2 shouldBe cache.keyOf("Bez wyjścia", None)  // canonical key unchanged
  }

  it should "create a new record when no matching row exists yet" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"), Seq(showtime("2026-06-01T18:00")))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaShowings.keySet shouldBe Set(Multikino)
    row.cinemaShowings(Multikino).posterUrl shouldBe Some("multikino.jpg")
    row.cinemaShowings(Multikino).showtimes.size shouldBe 1
  }

  it should "merge two cinemas' slots into the same record when their titles share a docId" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      // Punctuation-only variant — phase 2's docId rule collapses both.
      cinemaMovie("Top Gun Maverick", Helios, Some(2022), Some("helios.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaShowings.keySet shouldBe Set(Multikino, Helios)
    // Multikino's poster wins the merged view (priority rule).
    row.posterUrl shouldBe Some("multikino.jpg")
    // Both raw titles are recorded in cinemaScrapes (and surfaced via the
    // derived `cinemaTitles` view).
    row.cinemaTitles should contain allOf ("Top Gun: Maverick", "Top Gun Maverick")
  }

  // Five of the cinema clients (Helios, Rialto, KinoBulgarska, KinoMuza,
  // KinoPalacowe) parse a production-country string from their sources but
  // until this change CinemaShowings dropped it on the floor — the value was
  // populated on Movie.country, then thrown away when the slot was built.
  // Persisting it lets a downstream view show "USA, Polska" without having to
  // re-scrape, and lets the merged MovieRecord.country surface whichever
  // cinema actually reported one (Multikino-priority, skipping cinemas that
  // never carry the field).
  it should "store production country per cinema slot and expose it on the merged record" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), country = None)
    ))
    cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Top Gun Maverick", Helios, Some(2022), country = Some("USA"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaShowings(Multikino).country shouldBe None
    row.cinemaShowings(Helios).country    shouldBe Some("USA")
    // Multikino has the slot but no country; Helios fills in.
    row.country shouldBe Some("USA")
  }

  it should "prefer Multikino's country over other cinemas' on the merged record" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Hypothetical: a future Multikino enrichment fills country directly.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026), country = Some("Polska"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Foo", Helios, Some(2026), country = Some("USA"))
    ))

    cache.get(cache.keyOf("Foo", Some(2026))).get.country shouldBe Some("Polska")
  }

  it should "preserve enrichment-side fields when only cinema data changes" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Seed with an enriched record (TMDB-resolved, no cinemas yet).
    val seed = MovieRecord(
      imdbId = Some("tt1745960"), imdbRating = Some(8.2), metascore = Some(78),
      originalTitle = Some("Top Gun: Maverick"), tmdbId = Some(361743)
    )
    cache.put(cache.keyOf("Top Gun: Maverick", Some(2022)), seed)

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.imdbId        shouldBe Some("tt1745960")
    row.imdbRating    shouldBe Some(8.2)
    row.metascore     shouldBe Some(78)
    row.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  it should "prune a cinema's slot from records that didn't appear in the fresh scrape" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Tick 1: Multikino reports A + B.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino),
      cinemaMovie("B", Multikino)
    ))
    cache.get(cache.keyOf("A", Some(2026))).get.cinemaShowings should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaShowings should contain key Multikino

    // Tick 2: Multikino only reports A. B's Multikino slot should be pruned.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino)
    ))

    cache.get(cache.keyOf("A", Some(2026))).get.cinemaShowings should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaShowings should not contain key (Multikino)
  }

  // Cross-script protection now comes from `MovieService.normalize` keeping
  // Unicode letters: Cyrillic and Latin titles produce different normalised
  // forms, so the redirect filter can't merge them. No explicit cross-script
  // filter on `put` is needed — and `cinemaTitles` itself is derived from
  // `cinemaScrapes`, so a Polish scrape onto a Polish row never gets a
  // Cyrillic variant in its cinemaTitles to begin with.

  // Stop the flap loop where a year=None cinema title gets re-created every
  // tick. With `recordCinemaScrape`'s redirect, a fresh year=None scrape
  // onto an existing year=Some row gets folded onto that row.
  it should "redirect a fresh year=None scrape onto an existing year=Some row at the same cleanTitle" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val survivor = MovieRecord(
      imdbId = Some("tt1527793"), imdbRating = None, metascore = None,
      originalTitle = Some("어쩔수가없다"), tmdbId = Some(639988)
    )
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)), survivor)
    cache.snapshot().size shouldBe 1

    cache.recordCinemaScrape(KinoBulgarska, Seq(
      cinemaMovie("Bez wyjścia", KinoBulgarska, year = None, showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    cache.snapshot().size shouldBe 1
    val row = cache.get(cache.keyOf("Bez wyjścia", Some(2025))).get
    row.cinemaShowings.keySet shouldBe Set(KinoBulgarska)
    row.cinemaShowings(KinoBulgarska).showtimes.size shouldBe 1
    cache.get(cache.keyOf("Bez wyjścia", None)) shouldBe None
  }

  it should "record the incoming raw cinema title in cinemaScrapes (and the derived cinemaTitles view)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    val survivor = MovieRecord(
      imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
      originalTitle = None, tmdbId = Some(931285)
    )
    cache.put(cache.keyOf("Mortal Kombat II", Some(2026)), survivor)

    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Mortal Kombat 2", KinoBulgarska, Some(2026))))

    // Different year + different cleanTitle normalisation → no redirect; a
    // fresh row was created at ("Mortal Kombat 2", 2026). Its provenance
    // records the raw title.
    val newRow = cache.get(cache.keyOf("Mortal Kombat 2", Some(2026))).get
    newRow.cinemaTitles should contain ("Mortal Kombat 2")
  }

  it should "fold a redirected scrape's slot onto the existing row without creating a duplicate" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None))

    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Bez wyjścia", KinoBulgarska, None)))

    val row = cache.get(cache.keyOf("Bez wyjścia", Some(2025))).get
    row.cinemaTitles should contain ("Bez wyjścia")
    cache.get(cache.keyOf("Bez wyjścia", None)) shouldBe None
  }

  // Regression: the user-visible "Diabeł ubiera się u Prady 2 appears twice"
  // bug. Multikino + CinemaCity report year=None; Helios + Rialto report
  // year=Some(2026). When two threads first-scrape the same brand-new film
  // concurrently, both can see an empty cache in their redirect check and
  // each end up creating its own row. After that, every subsequent tick
  // finds the per-thread row at its primary key and never redirects again,
  // so both rows persist. recordCinemaScrape must serialise the
  // redirect-then-put step per normalised title to keep the race from
  // producing duplicates.
  it should "not create duplicate rows when two cinemas first-scrape the same title with different years concurrently" in {
    val title = "Diabeł ubiera się u Prady 2"
    val iterations = 100
    for (_ <- 1 to iterations) {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
      val latch = new java.util.concurrent.CountDownLatch(1)
      val exec  = java.util.concurrent.Executors.newFixedThreadPool(2)
      val t1 = exec.submit(new Runnable {
        def run(): Unit = {
          latch.await()
          cache.recordCinemaScrape(Multikino, Seq(cinemaMovie(title, Multikino, None)))
        }
      })
      val t2 = exec.submit(new Runnable {
        def run(): Unit = {
          latch.await()
          cache.recordCinemaScrape(Helios, Seq(cinemaMovie(title, Helios, Some(2026))))
        }
      })
      latch.countDown()
      t1.get(); t2.get()
      exec.shutdown()

      withClue(s"snapshot after iteration: ${cache.snapshot().map(r => s"(${r.title}, ${r.year})").mkString(", ")} ") {
        cache.snapshot().size shouldBe 1
      }
    }
  }

  it should "NOT redirect across scripts — Cyrillic and Latin normalise differently" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(931285)))

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Mortal Kombat II", Multikino, Some(2026), showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    // No redirect — a fresh Latin row gets created, distinct from Cyrillic.
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))) shouldBe defined
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
    cache.get(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026))) shouldBe defined
  }

  it should "NOT redirect when two existing rows could both be the target (ambiguous)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Two different films share the cinema-reported title "Wspinaczka" —
    // one row per year, each pinned to a different imdbId. Redirecting a
    // year=None scrape would be a coin-flip → keep them distinct.
    cache.put(cache.keyOf("Wspinaczka", Some(2017)),
              MovieRecord(imdbId = Some("tt5157682"), imdbRating = None, metascore = None,
                         originalTitle = None))
    cache.put(cache.keyOf("Wspinaczka", Some(2025)),
              MovieRecord(imdbId = Some("tt36437006"), imdbRating = None, metascore = None,
                         originalTitle = None))

    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Wspinaczka", KinoBulgarska, year = None)))

    cache.get(cache.keyOf("Wspinaczka", None)) shouldBe defined
    cache.snapshot().size shouldBe 3
  }

  // `hasResolvedSiblingByTitle` is what `scheduleTmdbStage` consults to skip
  // a phantom TMDB call when a sibling row already resolved the same film.
  "hasResolvedSiblingByTitle" should "return true when a resolved row's cleanTitle normalises to the same form" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Milcząca przyjaciółka", Some(2025)),
              MovieRecord(imdbId = Some("tt27811632"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(1168719)))

    cache.hasResolvedSiblingByTitle("Milcząca przyjaciółka") shouldBe true
  }

  it should "return false when the matching row has no tmdbId yet" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Foo", None),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None))

    cache.hasResolvedSiblingByTitle("Foo") shouldBe false
  }

  it should "return false when no row carries the title at all" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Something else", Some(2024)),
              MovieRecord(imdbId = None, imdbRating = None, metascore = None, originalTitle = None,
                         tmdbId = Some(42)))

    cache.hasResolvedSiblingByTitle("Unrelated") shouldBe false
  }

  it should "return false across scripts — Cyrillic cleanTitle doesn't satisfy a Latin lookup" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), imdbRating = None, metascore = None,
                         originalTitle = None, tmdbId = Some(931285)))

    cache.hasResolvedSiblingByTitle("Mortal Kombat II") shouldBe false
  }

  // ── Scrape-failure semantics ────────────────────────────────────────────
  //
  // A cinema's `fetch()` can return Seq.empty for two very different reasons:
  //   (a) The cinema is genuinely showing zero films right now (a holiday, a
  //       closure, or a brand-new site with no schedule yet).
  //   (b) The scraper failed silently — Cloudflare challenge, parser regex
  //       mismatch, ScrapingAnt 503, blank HTML, etc. — and returned [] even
  //       though the cinema is actually screening films normally.
  //
  // In case (b) the prune-on-tick would wipe EVERY slot that cinema holds
  // across the cache, then the next successful tick puts them all back. That
  // produces exactly the "row appears and disappears completely" symptom the
  // user observed: between a failed scrape and the next successful one, the
  // row's cinemaShowings is empty → toSchedules filters it out of the page.
  //
  // Since (a) is vanishingly rare in practice (cinemas don't go dark for 5
  // minutes), we treat Seq.empty as a likely failure and skip the prune.
  // Slots from previous ticks stay until the cinema's NEXT successful scrape
  // (which will prune anything genuinely dropped).
  it should "NOT prune when the scrape returned zero films (likely a scraper failure)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Seed with a film the cinema was previously showing.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, Some(2026))))
    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)

    // Scraper "fails": returns empty list. With the safeguard this is a no-op.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  it should "still prune a slot when the scrape returned other films but not this one" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026)),
      cinemaMovie("Bar", Multikino, Some(2026))
    ))

    // Bar drops out — Foo stays, Bar's slot should be pruned (cinema genuinely
    // stopped showing it, scrape returned a non-empty alternative list).
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026))
    ))

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaShowings.keySet shouldBe Set(Multikino)
    cache.get(cache.keyOf("Bar", Some(2026))).get.cinemaShowings.keySet shouldBe Set.empty
  }

  // Regression for the "Mortal Kombat row appears and disappears" symptom: a
  // merged survivor with slots from multiple cinemas should not lose ALL of
  // one cinema's slot just because that cinema's scrape blanked momentarily.
  it should "preserve all cinemas' slots on a survivor row when one cinema's scrape blanks" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Mortal Kombat", Multikino, Some(2026))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Mortal Kombat", Helios,    Some(2026))))
    val key = cache.keyOf("Mortal Kombat", Some(2026))
    cache.get(key).get.cinemaShowings.keySet shouldBe Set(Multikino, Helios)

    // Multikino's scraper has a hiccup and returns nothing this tick.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    // Helios slot definitely stays. Multikino's slot is preserved by the
    // safeguard so the row keeps rendering until Multikino's next successful
    // tick (which is the "row reappears" the user saw 60s later).
    val row = cache.get(key).get
    row.cinemaShowings.keySet should contain (Helios)
    row.cinemaShowings.keySet should contain (Multikino)
  }

  it should "not prune other cinemas' slots when one cinema's tick lands" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Both cinemas report the same film initially.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Shared", Multikino, Some(2026), Some("mu.jpg"))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Shared", Helios,    Some(2026), Some("he.jpg"))))

    val before = cache.get(cache.keyOf("Shared", Some(2026))).get
    before.cinemaShowings.keySet shouldBe Set(Multikino, Helios)

    // Helios genuinely drops Shared (scrape returns other films, just not
    // Shared) — Helios's slot is pruned, Multikino's slot is untouched.
    cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Different Film", Helios, Some(2026))))

    val after = cache.get(cache.keyOf("Shared", Some(2026))).get
    after.cinemaShowings.keySet shouldBe Set(Multikino)
  }

  "snapshot" should "return rows sorted by title (case-insensitive)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.put(cache.keyOf("Zorro", None),   mkEnrichment("tt3"))
    cache.put(cache.keyOf("alpha", None),   mkEnrichment("tt1"))
    cache.put(cache.keyOf("Beta", None),    mkEnrichment("tt2"))

    cache.snapshot().map(_.title) shouldBe Seq("alpha", "Beta", "Zorro")
  }
}
