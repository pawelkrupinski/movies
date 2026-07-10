package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, LocalDateTime, ZoneOffset}

class MovieCacheSpec extends AnyFlatSpec with Matchers {

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating)

  private def cm(cinema: Cinema, title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title, releaseYear = year),
      cinema    = cinema,
      posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), None))
    )

  // A TMDB-resolved row TMDB knows by `tmdbTitle` (Polish) / `originalTitle`.
  private def resolvedRecord(tmdbId: Int, year: Int, tmdbTitle: String, originalTitle: String, cinema: Cinema): MovieRecord =
    MovieRecord(tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Tmdb              -> SourceData(title = Some(tmdbTitle), originalTitle = Some(originalTitle), releaseYear = Some(year)),
      (cinema: Source)  -> SourceData(title = Some(tmdbTitle), releaseYear = Some(year))))

  it should "carry enriched detail forward when a listing-only cinema re-scrapes (no metadata-wipe flap)" in {
    // A listing-only cinema (arthouse: Muranów, Kinoteka…) scrapes title+showtimes
    // with no director/cast/etc; EnrichDetails fills those later. Pre-fix, the next
    // listing re-scrape rebuilt the slot from the (detail-less) listing and WIPED the
    // enrichment — which EnrichDetails then re-added, flapping the row and doubling
    // its change-stream writes. The re-scrape must now preserve the enriched fields.
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.recordCinemaScrape(KinoMuranow, Seq(cm(KinoMuranow, "Atlantis", Some(1991)))).head._2

    // Detail enrichment lands on the slot (as EnrichDetails' mergeInto would).
    cache.putIfPresent(key, r => r.copy(data = r.data.view.mapValues(_.copy(
      director       = Seq("Andrew Stanton"),
      cast           = Seq("Tom Hanks"),
      runtimeMinutes = Some(102),
      originalTitle  = Some("Atlantis"),
      countries      = Seq("USA"),
      genres         = Seq("Drama"))).toMap))

    // A later listing re-scrape (still no detail), with a new showtime.
    val reScrape = cm(KinoMuranow, "Atlantis", Some(1991))
      .copy(showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 9, 20, 0), None)))
    cache.recordCinemaScrape(KinoMuranow, Seq(reScrape))

    val slot = cache.get(key).get.data.values.head
    slot.director       shouldBe Seq("Andrew Stanton") // preserved, not wiped
    slot.cast           shouldBe Seq("Tom Hanks")
    slot.runtimeMinutes shouldBe Some(102)
    slot.originalTitle  shouldBe Some("Atlantis")
    slot.countries      shouldBe Seq("USA")
    slot.genres         shouldBe Seq("Drama")
    slot.showtimes.map(_.dateTime) should contain (LocalDateTime.of(2026, 6, 9, 20, 0)) // listing still applied
  }

  it should "backfillEmbeddedYears: re-key an existing yearless row onto its delimited title year" in {
    // A row that predates the scrape-boundary persist: stored yearless, its slot
    // title carries the delimited "(1989)".
    val rec   = MovieRecord(data = Map[Source, SourceData](
      KinoMuranow -> SourceData(title = Some("Konwicki: Lawa (1989)"), rawTitle = Some("Konwicki: Lawa (1989)"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Konwicki: Lawa (1989)", None, rec))))

    cache.backfillEmbeddedYears() shouldBe 1
    cache.get(cache.keyOf("Konwicki: Lawa (1989)", None))       shouldBe None       // old yearless key gone
    cache.get(cache.keyOf("Konwicki: Lawa (1989)", Some(1989)))
      .flatMap(_.releaseYear)                                   shouldBe Some(1989) // stamped + re-keyed
  }

  it should "backfillEmbeddedYears: merge into an existing yeared sister row, not clobber it" in {
    // The same film split across two rows: a yeared plain row (already resolved)
    // and a yearless decorated row whose title carries the year. The backfill must
    // fold the decorated slot onto the yeared row, keeping the resolved row's tmdbId.
    val plain     = MovieRecord(tmdbId = Some(377689), imdbId = Some("tt0097721"),
      data = Map[Source, SourceData](KinoMuranow -> SourceData(title = Some("Lawa"), releaseYear = Some(1989))))
    val decorated = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Lawa (1989)"), rawTitle = Some("Lawa (1989)"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Lawa", Some(1989), plain), ("Lawa (1989)", None, decorated))))

    cache.backfillEmbeddedYears() should be >= 1
    val merged = cache.get(cache.keyOf("Lawa", Some(1989)))
    merged.flatMap(_.tmdbId)              shouldBe Some(377689)                 // resolved id preserved
    merged.map(_.data.keySet).getOrElse(Set.empty) should contain allOf (KinoMuranow: Source, Multikino: Source)
    cache.get(cache.keyOf("Lawa (1989)", None)) shouldBe None                  // decorated yearless row folded away
  }

  it should "backfillEmbeddedYears: settle to the SAME rows regardless of insertion order (determinism)" in {
    // Off the async resolve path, over a settled corpus, the migration must be a
    // pure function of the row set — every insertion permutation yields one outcome.
    val rows = Seq(
      // resolved plain row keyed on its TMDB year …
      ("Lawa", Some(1989), MovieRecord(tmdbId = Some(377689),
        data = Map[Source, SourceData](KinoMuranow -> SourceData(title = Some("Lawa"), releaseYear = Some(1989))))),
      // … a yearless decorated sister that must fold onto it …
      ("Lawa (1989)", None, MovieRecord(
        data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Lawa (1989)"), rawTitle = Some("Lawa (1989)"))))),
      // … and an unrelated yearless embedded row that must re-key to its own year.
      ("Following (1998)", None, MovieRecord(
        data = Map[Source, SourceData](Helios -> SourceData(title = Some("Following (1998)"), rawTitle = Some("Following (1998)"))))))
    val outcomes = rows.permutations.map { ordered =>
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository(ordered))
      cache.backfillEmbeddedYears()
      cache.snapshot().map(r => (TitleNormalizer.sanitize(r.title), r.year)).toSet
    }.toSet
    outcomes shouldBe Set(Set(("lawa", Some(1989)), ("following", Some(1998))))
  }

  "MovieCache" should "hydrate from the repository on construction" in {
    // Carry the title in a cinema slot, as a scraped row does: the repository
    // re-derives the display title from the record on read (like Mongo), so a
    // title-less record would surface its sanitized _id prefix, not "Drzewo Magii".
    val record   = mkEnrichment("tt1").copy(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Drzewo Magii"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Drzewo Magii", Some(2024), record))))

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(record)
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Drzewo Magii", Some(2024)))
  }

  // Change-stream DELETE handling: a removed source row (a fold/merge loser, an
  // UnscreenedCleanup removal, a re-key's old id) is dropped from the cache
  // INCREMENTALLY via `applyDelete` — the stream carries only the `_id`, mapped back
  // to its CacheKey by `idFor` — so the periodic backstop rehydrate is no longer the
  // ONLY thing that catches deletes. Pre-fix the cache ignored delete events entirely.
  it should "drop a cached row when its source _id is deleted on the change stream" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("Foo", Some(2024))
    cache.put(key, mkEnrichment("tt-foo"))
    cache.get(key) should not be empty

    cache.applyDelete(StoredMovieRecord.idFor("Foo", Some(2024)))
    cache.get(key) shouldBe None

    // A delete for an unknown id is a harmless no-op — it must not clear other rows.
    val barKey = cache.keyOf("Bar", Some(2024))
    cache.put(barKey, mkEnrichment("tt-bar"))
    cache.applyDelete("does-not-exist|1900")
    cache.get(barKey) should not be empty
  }

  // Redundancy signal for retiring the backstop rehydrate (step 3a): the rehydrate
  // reports how much its full reload caught that the incremental change stream missed —
  // a put whose value DIFFERED (missed upsert) and a key gone from Mongo (missed delete).
  // Once resume-tokens + delete-apply are working this should be ~0 in steady state.
  it should "meter what the backstop rehydrate catches that the change stream missed" in {
    val repo  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt-foo"))))
    val m     = new RecordingCacheMetrics
    val cache = new CaffeineMovieCache(repo, cacheMetrics = m) // boot hydrate counts Foo as changed
    m.reset()
    // The source diverged out-of-band while no stream applied it: Foo removed, Bar added.
    repo.delete("Foo", Some(2024))
    repo.upsert("Bar", Some(2024), mkEnrichment("tt-bar"))
    cache.rehydrate()
    m.changed shouldBe 1 // Bar — a missed upsert
    m.deleted shouldBe 1 // Foo — a missed delete
  }

  private class RecordingCacheMetrics extends CacheSyncMetrics {
    var changed = 0; var deleted = 0
    def recordRehydrate(c: Int, d: Int): Unit = { changed += c; deleted += d }
    def reset(): Unit = { changed = 0; deleted = 0 }
  }

  it should "not write to the repository when putIfPresent produces no change (kills the no-op re-scrape churn)" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Dune", Some(2024))
    cache.put(key, mkEnrichment("tt-dune", rating = Some(8.1)))
    val writesAfterPut = repository.upserts.size

    // An unchanged re-scrape: the updater returns an identical record — the
    // per-tick common case for every already-listed film across ~50 cinemas.
    // Pre-fix it still issued an updateOne (bumping only `updatedAt`), each one
    // an oplog entry + a change-stream `updateLookup` full-document read — the
    // dominant load on the shared-CPU Mongo. It must now be a true no-op.
    cache.putIfPresent(key, (r: MovieRecord) => r) shouldBe true // row present → still reports success
    repository.upserts.size shouldBe writesAfterPut                    // ...but NO new write fired

    // A genuine change still writes through.
    cache.putIfPresent(key, _.copy(imdbRating = Some(9.4))) shouldBe true
    repository.upserts.size shouldBe writesAfterPut + 1
    cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.4)
  }

  // rehydrate(): reload from repository. Boot-time hydration goes through the same
  // method, so we cover the on-demand admin-endpoint behaviour here:
  // (a) in-memory rows that aren't in Mongo get dropped, (b) repository-side edits
  // become visible, (c) the negative cache is orthogonal and survives.
  "rehydrate" should "reap a mis-keyed movies orphan whose _id drifted from its display title" in {
    // Two movies docs for one film: the canonical "zaplatani|2010" plus a stale
    // "tangled|2010" first stored under the English title but now displaying (via
    // its TMDB Polish title) as "Zaplątani". On read both re-derive the same
    // display title → collapse to one CacheKey, so the cross-title settle never
    // sees two rows; the orphan must be reaped on hydrate instead.
    val zaplSlots = Map[Source, SourceData](
      Tmdb      -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled"), releaseYear = Some(2010)),
      Multikino -> SourceData(title = Some("Zaplątani"), releaseYear = Some(2010)))
    val repository = new InMemoryMovieRepository(Seq(
      ("Tangled",   Some(2010), MovieRecord(tmdbId = Some(38757), data = zaplSlots)), // → _id tangled|2010, displays "Zaplątani"
      ("Zaplątani", Some(2010), MovieRecord(tmdbId = Some(38757), data = zaplSlots))  // → _id zaplatani|2010
    ))
    new CaffeineMovieCache(repository) // constructor hydrates → reaps the orphan
    val rows = repository.findAll()
    withClue(s"expected ONE doc, got ${rows.map(r => (r.title, r.persistedId))}\n")(rows.size shouldBe 1)
    rows.head.persistedId shouldBe Some("zaplatani|2010")
    repository.deletes.map(_._1) should contain ("tangled|2010")
  }

  it should "drop in-memory rows that aren't in the repository" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)

    // Two rows in cache + repository; delete one from the repository behind the cache's
    // back so cache + repository diverge by exactly that row. Rehydrate should
    // evict the disappeared one and keep the rest.
    //
    // (The 0-rows-in-repository case is deliberately NOT tested — `rehydrate`
    // skips eviction on an empty `findAll()` because `MovieRepository.findAll`
    // swallows every Mongo error into `Seq.empty`, so an empty result
    // can't reliably be distinguished from a transient TLS/pool race.
    // A real-world empty Mongo is a manual-wipe degenerate case.)
    cache.put(cache.keyOf("Ghost",  Some(2024)), mkEnrichment("tt-ghost"))
    cache.put(cache.keyOf("Keeper", Some(2024)), mkEnrichment("tt-keeper"))
    repository.delete("Ghost", Some(2024))
    cache.get(cache.keyOf("Ghost",  Some(2024))) shouldBe defined  // still cached
    cache.get(cache.keyOf("Keeper", Some(2024))) shouldBe defined

    val n = cache.rehydrate()

    n shouldBe 1
    cache.get(cache.keyOf("Ghost",  Some(2024))) shouldBe None
    cache.get(cache.keyOf("Keeper", Some(2024))) shouldBe defined
  }

  it should "leave the cache intact when findAll() returns empty (treats as transient Mongo failure)" in {
    // The real prod-bug regression: a 30-s rehydrate tick coinciding with a
    // Mongo TLS-selector race (driver retries internally; findAll surfaces
    // it as `Seq.empty` via MovieRepository's swallow-on-error). Pre-fix,
    // every cached row got evicted and the page rendered empty until the
    // next successful tick.
    val repository  = new InMemoryMovieRepository()  // start empty
    val cache = new CaffeineMovieCache(repository)
    cache.put(cache.keyOf("Ghost", Some(2024)), mkEnrichment("tt1"))
    // Mongo "lies" — write straight to the cache, then drop from the repository
    // so the next rehydrate's findAll returns empty.
    repository.delete("Ghost", Some(2024))

    cache.rehydrate() shouldBe 0
    // Cache row survives; the user keeps seeing films through the next
    // (presumably successful) tick.
    cache.get(cache.keyOf("Ghost", Some(2024))) shouldBe defined
  }

  it should "make repository-side edits visible" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1", rating = Some(7.0)))

    // Edit Mongo out-of-band: replace the rating.
    repository.upsert("X", Some(2024), mkEnrichment("tt1", rating = Some(9.5)))

    cache.rehydrate() shouldBe 1
    cache.get(cache.keyOf("X", Some(2024))).flatMap(_.imdbRating) shouldBe Some(9.5)
  }

  it should "leave the negative cache alone" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("not-a-real-film", Some(2099))
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true

    cache.rehydrate()

    cache.isNegative(key) shouldBe true
  }

  // ── incremental change-stream sync (start()) ───────────────────────────────
  // Once started, the cache applies out-of-band Mongo writes the moment they
  // land via `repository.watchUpserts` — no full `findAll()` rehydrate. The
  // InMemoryMovieRepository emulates Mongo's change stream by notifying the watcher
  // on every write.
  "a started MovieCache" should "apply an out-of-band upsert via the change-stream watch, without a rehydrate" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    cache.start()   // establishes the watch (the backstop interval won't fire in-test)
    try {
      // Another process edits Mongo directly. Crucially: no rehydrate() call.
      repository.upsert(key.cleanTitle, key.year, mkEnrichment("tt1", rating = Some(9.5)))
      cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.5)
    } finally cache.stop()
  }

  it should "stop applying changes once the watch is closed by stop()" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    cache.start()
    cache.stop()    // closes the watch handle
    repository.upsert(key.cleanTitle, key.year, mkEnrichment("tt1", rating = Some(9.5)))
    cache.get(key).flatMap(_.imdbRating) shouldBe Some(7.0)  // change not applied — watch is closed
  }

  "InMemoryMovieRepository.watchUpserts" should "notify the watcher on every write until the handle is closed" in {
    val repository   = new InMemoryMovieRepository()
    val seen   = scala.collection.mutable.ListBuffer.empty[(String, Option[Int], Option[Double])]
    val handle = repository.watchUpserts(r => seen.append((r.title, r.year, r.record.imdbRating)))
    handle shouldBe defined
    repository.upsert("A", Some(2024), mkEnrichment("tt-a"))
    repository.updateIfPresent("A", Some(2024), mkEnrichment("tt-a"), mkEnrichment("tt-a", rating = Some(8.0)))
    handle.get.close()
    repository.upsert("B", Some(2025), mkEnrichment("tt-b"))  // after close → not observed
    // The repository re-derives the display title on read, as Mongo does — a record
    // with no title slot collapses to its sanitized _id prefix ("a"), which
    // `displayTitle` then re-cases to "A". What this pins is that the watcher
    // fires on each write until closed; title is incidental.
    seen.toList shouldBe List(("A", Some(2024), None), ("A", Some(2024), Some(8.0)))
  }

  it should "fan out every write to ALL registered watchers, not just the last (prod attaches cache + projector)" in {
    val repository = new InMemoryMovieRepository()
    val a, b = scala.collection.mutable.ListBuffer.empty[String]
    repository.watchUpserts(r => a += r.title)
    repository.watchUpserts(r => b += r.title)  // must NOT clobber the first watcher
    repository.upsert("A", Some(2024), mkEnrichment("tt-a"))
    a.toList shouldBe List("A")
    b.toList shouldBe List("A")
  }

  "InMemoryMovieRepository.updateIfPresent" should "skip the write and the change notification when nothing changed (empty patch)" in {
    val repository = new InMemoryMovieRepository(Seq(("A", Some(2024), mkEnrichment("tt-a"))))
    val seen = scala.collection.mutable.ListBuffer.empty[String]
    repository.watchUpserts(r => seen += r.title)
    repository.upserts.clear()
    val unchanged = mkEnrichment("tt-a")
    repository.updateIfPresent("A", Some(2024), unchanged, unchanged) shouldBe true  // present, already up to date
    repository.upserts shouldBe empty   // no pure-updatedAt no-op write
    seen shouldBe empty                 // and no change-stream event
  }

  "InMemoryMovieRepository.findAll" should "re-derive title/year from the _id + record like Mongo, not return them verbatim" in {
    // The fake must match `MongoMovieRepository`, which persists only `_id` +
    // `sourceData` and re-derives the display title on read. Stored under an
    // English title whose record displays as Polish (a real title `_id` drift):
    // the read-back title MUST be the re-derived "Dziecko z pyłu", not the
    // verbatim "Child of Dust". The verbatim behaviour previously hid title
    // drift and the settle non-determinism from CI.
    val repository = new InMemoryMovieRepository()
    repository.upsert("Child of Dust", Some(2025),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dziecko z pyłu")))))
    repository.findAll().map(r => (r.title, r.year)) shouldBe Seq(("Dziecko z pyłu", Some(2025)))
  }

  it should "treat case + diacritics + whitespace differences as the same key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Drzewo Magii", Some(2024)), mkEnrichment("tt9"))

    val expected = mkEnrichment("tt9")
    cache.get(cache.keyOf("drzewo magii",   Some(2024))) shouldBe Some(expected)
    cache.get(cache.keyOf("DRZEWO   MAGII", Some(2024))) shouldBe Some(expected)
    // Different year is a different row.
    cache.get(cache.keyOf("Drzewo Magii",   Some(2025))) shouldBe None
  }

  // The merge key is derived from the title's OWN form — the same input the
  // display vote (MovieRecord.displayTitle) sanitizes — NOT from the
  // searchTitle/GlobalStructural-stripped form. So a decoration edition
  // ("Top Gun / 40th Anniversary", "Avatar - wersja polska") resolves to a
  // DIFFERENT key than the base film and stays its own card: a record is never
  // merged with something that would resolve to a different title key on its
  // own. Decoration stripping still happens for external lookups (apiQuery),
  // just not for identity.
  it should "key a decoration edition separately from the base film (no searchTitle in the merge key)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.keyOf("Top Gun / 40th Anniversary", Some(2025)) should not be cache.keyOf("Top Gun",  Some(2025))
    cache.keyOf("Avatar - wersja polska",     Some(2025)) should not be cache.keyOf("Avatar",   Some(2025))
  }

  // ...but the GLOBAL canonical folds stay IN the key: Arabic↔Roman numerals
  // and " & "↔" i " still collapse, so the same film spelt differently across
  // cinemas keeps one identity. (`sanitize` applies `normalize` + `canonical`.)
  it should "still collapse global canonical folds (Roman numerals, & → i) into one key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.keyOf("Mortal Kombat 2", Some(2026)) shouldBe cache.keyOf("Mortal Kombat II", Some(2026))
    cache.keyOf("Pizza & Pasta",   Some(2026)) shouldBe cache.keyOf("Pizza i Pasta",    Some(2026))
  }

  "put" should "write through to the repository (cache + Mongo stay in lockstep)" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))

    repository.upserts.toList shouldBe List(("X", Some(2024), mkEnrichment("tt1")))
  }

  // ── tmdbId identity gate ───────────────────────────────────────────────────
  //
  // `put` is the single persist point in the codebase (every cache + Mongo
  // write goes through it). The gate is the chokepoint: when a write would
  // create a *new* row carrying a tmdbId that another row already holds,
  // the write is folded onto that canonical row instead — same film at two
  // (title, year) keys can never produce two persisted rows. Both the prod
  // path (MongoMovieRepository behind the cache) and the test path (this
  // InMemoryMovieRepository behind the cache) inherit the gate automatically;
  // there's no duplicate logic to maintain.

  private def mkResolved(tmdbId: Int,
                         cinemaSlots: Map[Cinema, SourceData] = Map.empty): MovieRecord =
    MovieRecord(
      imdbId         = Some("tt-anything"),
      imdbRating     = Some(8.0),
      tmdbId         = Some(tmdbId),
      data           = cinemaSlots.map { case (c, sd) => (c: Source) -> sd } +
                       (Tmdb -> SourceData(originalTitle = Some("Original")))
    )

  "put with tmdbId" should "fold onto an existing key carrying the same tmdbId" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497, cinemaSlots = Map(KinoPalacowe -> SourceData(title = Some("Viridiana"), releaseYear = Some(1961)))))
    repository.upserts.clear()
    repository.deletes.clear()

    // Second write at K2 with same tmdbId — should NOT create a row at K2.
    cache.put(k2, mkResolved(4497, cinemaSlots = Map(KinoPalacowe -> SourceData(title = Some("Viridiana"), releaseYear = Some(1962)))))

    cache.get(k1) shouldBe defined
    cache.get(k2) shouldBe None
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Viridiana", Some(1961)))
  }

  it should "carry the victim's per-cinema slot onto the canonical row" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497, cinemaSlots = Map(KinoPalacowe -> SourceData(title = Some("Viridiana"), releaseYear = Some(1961)))))
    cache.put(k2, mkResolved(4497, cinemaSlots = Map(Helios       -> SourceData(title = Some("Viridiana"), releaseYear = Some(1962)))))

    val canonical = cache.get(k1).get
    canonical.cinemaData.keySet shouldBe Set(KinoPalacowe, Helios)
    canonical.cinemaData(KinoPalacowe).title       shouldBe Some("Viridiana")
    canonical.cinemaData(KinoPalacowe).releaseYear shouldBe Some(1961)
    canonical.cinemaData(Helios).title             shouldBe Some("Viridiana")
    canonical.cinemaData(Helios).releaseYear       shouldBe Some(1962)
  }

  it should "pick the same canonical row regardless of which tmdbId-sharing key is written first" in {
    // Two cinema-reported variants of one film that resolve to the same tmdbId
    // but key differently — e.g. one cinema reports a release year, another
    // (Ekobilet) reports none. Whichever `put` lands first currently becomes
    // canonical, so the surviving (title, year) depends on enrichment-thread
    // arrival order — non-deterministic across machines (this is the arm64-vs-
    // amd64 whole-corpus snapshot drift). The canonical must be a pure function
    // of the key set, not arrival order.
    def canonicalKeyFor(writeOrder: Seq[Option[Int]]): (String, Option[Int]) = {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
      writeOrder.foreach { year =>
        cache.put(cache.keyOf("Milcząca przyjaciółka", year),
          mkResolved(4497, cinemaSlots = Map(KinoPalacowe ->
            SourceData(title = Some("Milcząca przyjaciółka"), releaseYear = year))))
      }
      val r = cache.snapshot().head
      (r.title, r.year)
    }
    val yearFirst = canonicalKeyFor(Seq(Some(2025), None))
    val noneFirst = canonicalKeyFor(Seq(None, Some(2025)))
    yearFirst shouldBe noneFirst
  }

  it should "delete the victim from Mongo when the source key already held a row" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497))
    // Source row exists with no tmdbId yet (e.g. fresh scrape).
    cache.put(k2, mkResolved(4497).copy(tmdbId = None))
    repository.upserts.clear()
    repository.deletes.clear()

    // TMDB resolution lands on K2 — gate folds onto K1, K2 must be deleted.
    cache.put(k2, mkResolved(4497))

    repository.deletes.toList should contain (("Viridiana", Some(1962)))
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Viridiana", Some(1961)))
  }

  it should "write through to the repository at the canonical key, not the source" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    val k2    = cache.keyOf("Viridiana", Some(1962))
    cache.put(k1, mkResolved(4497))
    repository.upserts.clear()

    cache.put(k2, mkResolved(4497, cinemaSlots = Map(KinoPalacowe -> SourceData(title = Some("Viridiana"), releaseYear = Some(1962)))))

    val titles = repository.upserts.map { case (t, y, _) => (t, y) }
    titles should contain (("Viridiana", Some(1961)))
    titles should not contain (("Viridiana", Some(1962)))
  }

  it should "NOT fold when tmdbId differs (two different films sharing a title)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
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

  // Rows with the same tmdbId are the SAME FILM and fold onto one record,
  // however they're spelled. The "distinct listings for distinct audiences"
  // (Polish vs Cyrillic, original vs dub) are now split back into a CARD per
  // shown title by the read-model projection, not kept as separate storage rows
  // — so no variant's display title is hidden while storage stays one-per-film.
  it should "fold rows with the same tmdbId across different shown titles (cross-script)" in {
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository())
    val latin    = cache.keyOf("Diabeł ubiera się u Prady 2", Some(2026))
    val cyrillic = cache.keyOf("ДИЯВОЛ НОСИТЬ ПРАДА 2",       Some(2026))

    cache.put(latin,    mkResolved(928344, cinemaSlots = Map(Multikino -> SourceData(title = Some("Diabeł ubiera się u Prady 2"), releaseYear = Some(2026)))))
    cache.put(cyrillic, mkResolved(928344, cinemaSlots = Map(Helios    -> SourceData(title = Some("ДИЯВОЛ НОСИТЬ ПРАДА 2"),       releaseYear = Some(2026)))))

    // One record per film; the two cinemas' distinct titles live in separate
    // per-cinema slots, so the read-model projection splits them back into a card
    // each (see ReadModelProjectionSpec.projectAll).
    val snap = cache.snapshot()
    snap should have size 1
    snap.head.record.tmdbId       shouldBe Some(928344)
    snap.head.record.cinemaTitles shouldBe Set("Diabeł ubiera się u Prady 2", "ДИЯВОЛ НОСИТЬ ПРАДА 2")
  }

  it should "keep a same-tmdbId dub + original at the SAME cinema as two distinct slots" in {
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepository())
    val baseTitle = "Diabeł ubiera się u Prady 2"
    val dubTitle  = "Diabeł ubiera się u Prady 2 ukraiński dubbing"
    // Each listing's slot is keyed per shown title (`CinemaShowing`), as the scrape
    // path produces — so the SAME venue holds two title-slots, not one.
    def rec(slotTitle: String, at: LocalDateTime) = MovieRecord(
      imdbId = Some("tt-anything"), tmdbId = Some(928344),
      data = Map[Source, SourceData](
        CinemaShowing.keyFor(CinemaCityPoznanPlaza, slotTitle) ->
          SourceData(title = Some(slotTitle), releaseYear = Some(2026), showtimes = Seq(Showtime(at, None))),
        Tmdb -> SourceData(originalTitle = Some("Original"))))
    cache.put(cache.keyOf(baseTitle, Some(2026)), rec(baseTitle, LocalDateTime.of(2026, 6, 8, 18, 0)))
    cache.put(cache.keyOf(dubTitle,  Some(2026)), rec(dubTitle,  LocalDateTime.of(2026, 6, 9, 20, 0)))

    // Same film → one record, but BOTH shown titles survive as separate slots →
    // the read-model split renders a card each; no showtime is lost or merged.
    val snap = cache.snapshot()
    snap should have size 1
    snap.head.record.cinemaTitles   shouldBe Set(baseTitle, dubTitle)
    snap.head.record.cinemaShowings should have size 2
  }

  it should "be a no-op when the same key is re-written (regular update, not a fold)" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val k1    = cache.keyOf("Viridiana", Some(1961))
    cache.put(k1, mkResolved(4497))
    repository.deletes.clear()

    // Same key, same tmdbId — sibling check `k != key` excludes itself, no fold.
    cache.put(k1, mkResolved(4497).copy(imdbRating = Some(9.0)))

    cache.get(k1).get.imdbRating shouldBe Some(9.0)
    repository.deletes shouldBe empty
  }

  // Rating sources occasionally hand us a literal zero — Metacritic / RT
  // search pages that return 0% for an unrated title, Filmweb's API for
  // a film with no votes yet, IMDb GraphQL for a brand-new entry. Zero
  // isn't a real rating; persisting it would render a misleading "0/10"
  // badge in the UI. Squash to None at the cache write boundary so neither
  // the in-memory positive cache nor Mongo holds a false zero.
  "put" should "squash zero ratings to None on the way into the cache" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Unrated", Some(2026))

    cache.put(key, MovieRecord(
      imdbId         = Some("tt0"),
      imdbRating     = Some(0.0),
      metascore      = Some(0),
      filmwebRating  = Some(0.0),
      rottenTomatoes = Some(0)
    ))

    val cached = cache.get(key).get
    cached.imdbRating     shouldBe None
    cached.metascore      shouldBe None
    cached.filmwebRating  shouldBe None
    cached.rottenTomatoes shouldBe None

    // Same guarantee on the write-through path: Mongo never sees the zero.
    val (_, _, persisted) = repository.upserts.last
    persisted.imdbRating     shouldBe None
    persisted.metascore      shouldBe None
    persisted.filmwebRating  shouldBe None
    persisted.rottenTomatoes shouldBe None
  }

  "putIfPresent" should "squash zero ratings produced by the updater to None" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("Unrated", Some(2026))
    cache.put(key, mkEnrichment("tt0", rating = Some(7.5)))

    // A rating fetcher returns a zero for a row that previously had a real
    // value — the cached row should reset to None, not keep the stale 7.5
    // (the updater explicitly wrote Some(0.0), which is what it observed).
    cache.putIfPresent(key, _.copy(imdbRating = Some(0.0), metascore = Some(0)))

    val row = cache.get(key).get
    row.imdbRating shouldBe None
    row.metascore  shouldBe None
  }

  "invalidate" should "remove from both positive cache and repository" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    repository.upserts.clear()

    cache.invalidate(key)

    cache.get(key) shouldBe None
    repository.deletes.toList shouldBe List(("X", Some(2024)))
  }

  "markMissing + isNegative" should "let callers track known-non-films without polluting the positive cache" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("not-a-real-film", Some(2099))

    cache.isNegative(key) shouldBe false
    cache.markMissing(key)
    cache.isNegative(key) shouldBe true
    cache.get(key) shouldBe None  // negative cache never returns a positive value
  }

  // ── putIfPresent: no-resurrection writes ───────────────────────────────────

  "putIfPresent" should "update an existing row and return true" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    cache.put(cache.keyOf("Existing", Some(2024)), mkEnrichment("tt1"))

    val landed = cache.putIfPresent(cache.keyOf("Existing", Some(2024)), _.copy(imdbRating = Some(8.5)))

    landed shouldBe true
    cache.get(cache.keyOf("Existing", Some(2024))).flatMap(_.imdbRating) shouldBe Some(8.5)
  }

  it should "be a no-op and return false when the row was deleted" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Gone", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    cache.invalidate(key)
    repository.upserts.clear()

    val landed = cache.putIfPresent(key, _.copy(imdbRating = Some(8.5)))

    landed shouldBe false
    cache.get(key) shouldBe None
    repository.upserts shouldBe empty
  }

  it should "operate on the current cached value, not a stale snapshot" in {
    // A rating listener that captured the row at T0, made a slow network
    // call, and now wants to update one field shouldn't clobber concurrent
    // updates to other fields. putIfPresent's updater receives the live row.
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("Foo", Some(2024))
    cache.put(key, mkEnrichment("tt1"))

    // Simulate "another listener already wrote metascore" between read and write.
    cache.put(key, mkEnrichment("tt1").copy(metascore = Some(70)))

    cache.putIfPresent(key, current => current.copy(imdbRating = Some(8.5)))

    val row = cache.get(key).get
    row.imdbRating shouldBe Some(8.5)   // our update landed
    row.metascore  shouldBe Some(70)    // concurrent update preserved
  }

  // The audit-clobber regression: a separate process (FilmwebUrlAudit) writes
  // `filmwebUrl=None` to Mongo. Meanwhile the running app's in-memory cache
  // still has the stale URL — its next hourly rating tick reads the cache,
  // calls `putIfPresent(_.copy(filmwebRating=newRating))`, and the write-
  // through MUST NOT carry the stale `filmwebUrl` along and clobber the
  // audit's None. Only `filmwebRating` changed in this update; only
  // `filmwebRating` should be persisted.
  it should "only persist the fields the updater actually changed, leaving repository-side edits to other fields intact" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Audit Race", Some(2024))

    // Cache state: stale filmwebUrl + old rating. Mongo gets the same on
    // initial put (write-through).
    cache.put(key, mkEnrichment("tt1").copy(
      filmwebUrl    = Some("https://www.filmweb.pl/film/Wrong-2024-99999"),
      filmwebRating = Some(7.0)
    ))

    // Out-of-band Mongo edit (mirrors what FilmwebUrlAudit does in a
    // separate process). Cache doesn't know — its in-memory snapshot
    // still has the stale URL.
    repository.dropFilmwebUrl("Audit Race", Some(2024))

    // Running app's rating tick: cache still has the stale URL, the
    // updater bumps just the rating.
    cache.putIfPresent(key, _.copy(filmwebRating = Some(7.5)))

    // Mongo's audit-applied filmwebUrl=None MUST stay None — the cache
    // write only $set the field that changed (`filmwebRating`).
    val mongoRow = repository.findAll().head.record  // the only row (its title-less record re-derives to "audit race")
    mongoRow.filmwebUrl    shouldBe None
    mongoRow.filmwebRating shouldBe Some(7.5)
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

  // Register a change-stream watcher on the repo and return a counter that ticks
  // for every emission — upsert OR delete — the fan-out fires. Prod attaches the
  // MovieCache AND the ReadModelProjector to this stream, so an emission means a
  // reprojection. Asserting the counter stays 0 across a re-scrape proves the
  // scrape produced no downstream churn, a strictly stronger claim than an empty
  // `upserts` buffer (which counts only one of the two write paths).
  private def changeStreamEmissions(repo: InMemoryMovieRepository): java.util.concurrent.atomic.AtomicInteger = {
    val emitted = new java.util.concurrent.atomic.AtomicInteger(0)
    repo.watchChanges(_ => { emitted.incrementAndGet(); () }, _ => { emitted.incrementAndGet(); () })
    emitted
  }

  private def cinemaMovie(title: String, cinema: Cinema, year: Option[Int] = Some(2026),
                          poster: Option[String] = None, showtimes: Seq[Showtime] = Seq.empty,
                          countries: Seq[String] = Seq.empty, synopsis: Option[String] = None,
                          cast: Seq[String] = Seq.empty, director: Seq[String] = Seq.empty,
                          runtimeMinutes: Option[Int] = None): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year, countries = countries, runtimeMinutes = runtimeMinutes),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = None,
      synopsis  = synopsis,
      cast      = cast,
      director  = director,
      showtimes = showtimes
    )

  // ── Provenance: per-(cinema, title, year) scrape dedup ─────────────────────
  //
  // recordCinemaScrape returns a `(CinemaMovie, CacheKey, isNew)` triple per
  // input. `isNew` is true the first time that exact `(cinema, raw title, raw
  // year)` tuple lands on a row; false on subsequent ticks that report the
  // same combination. CinemaScrapeRunner uses the flag to suppress redundant
  // MovieDetailsComplete events — every TMDB / IMDb / rating fetcher's listener
  // re-checks state on each event, so re-publishing for unchanged tuples
  // just churns dispatches. Any change (new cinema, new title spelling, year
  // correction) still flips `isNew` back to true so genuine new context can
  // trigger better enrichment.

  "recordCinemaScrape" should "flag the first scrape of a (cinema, title, year) tuple as new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val touched = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    touched should have size 1
    touched.head._3 shouldBe true
  }

  it should "flag a repeat scrape of the same (cinema, title, year) as not-new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val secondTick = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))
    ))
    secondTick should have size 1
    secondTick.head._3 shouldBe false
  }

  it should "NOT re-write the row when a re-scrape returns the same showings in a different order" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val a = showtime("2026-06-08T18:00"); val b = showtime("2026-06-08T20:30"); val c = showtime("2026-06-08T22:45")
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, showtimes = Seq(a, b, c))))
    repository.upserts should not be empty   // first scrape established the slot
    repository.upserts.clear()
    val emissions = changeStreamEmissions(repository)
    // Same three showings, different order — the canonical sort makes the stored
    // slot identical, so the write-through equality guard skips it (no write, no
    // change-stream event). Without the ingestion sort this fired a redundant write.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, showtimes = Seq(c, a, b))))
    repository.upserts shouldBe empty
    emissions.get() shouldBe 0               // no write-through → no change-stream emission → no reprojection
  }

  it should "centrally strip format from the title: fold a cinema's format editions into one badged slot, keeping programme + Ukrainian screenings separate" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val t10 = showtime("2026-06-08T10:00"); val t12 = showtime("2026-06-08T12:00")
    val t14 = showtime("2026-06-08T14:00"); val t16 = showtime("2026-06-08T16:00")
    // No per-client strip: the central FormatTags handles every shape at ingest.
    val touched = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Wonka (2D dubbing)",       Multikino, showtimes = Seq(t10)),
      cinemaMovie("Wonka /napisy",            Multikino, showtimes = Seq(t12)),
      cinemaMovie("Kino Dostępne: Wonka",     Multikino, showtimes = Seq(t14)),
      cinemaMovie("Wonka ukraiński dubbing",  Multikino, showtimes = Seq(t16))
    ))
    val slots = touched.map(_._2).distinct.flatMap(k => cache.get(k))
      .flatMap(_.cinemaShowings.collect { case (Multikino, sd) => sd })
    // Three slots: the two format editions folded into one clean "Wonka", plus the
    // programme-prefixed and the Ukrainian screening (kept whole → their own cards).
    slots should have size 3
    val wonka = slots.filter(_.title.exists(_.equalsIgnoreCase("Wonka")))
    wonka should have size 1
    wonka.head.showtimes.map(_.dateTime.getHour).toSet shouldBe Set(10, 12)
    wonka.head.showtimes.flatMap(_.format).toSet       shouldBe Set("2D", "DUB", "NAP")
    slots.flatMap(_.title).exists(_.toLowerCase.contains("dostępne")) shouldBe true
    slots.flatMap(_.title).exists(_.toLowerCase.contains("ukrai"))    shouldBe true
  }

  it should "NOT delete an already-past showing when a re-scrape drops it (retain it → no churn write)" in {
    val now   = LocalDateTime.of(2026, 6, 8, 21, 0)  // 21:00 is "now"
    val repo  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repo, clock = Clock.fixed(now.toInstant(ZoneOffset.UTC), ZoneOffset.UTC))
    val past   = showtime("2026-06-08T18:00")  // before now → past
    val future = showtime("2026-06-08T22:30")  // after now → future
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, showtimes = Seq(past, future))))
    repo.upserts should not be empty      // first scrape established the slot
    repo.upserts.clear()
    val emissions = changeStreamEmissions(repo)
    // Cinema no longer lists the now-past 18:00 showing; only the future one.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, showtimes = Seq(future))))
    repo.upserts shouldBe empty           // past showing retained → slot unchanged → no write
    emissions.get() shouldBe 0            // no write → nothing on the change stream → no reprojection
  }

  it should "fold same-cinema title variants that share a sanitized slot key, so a re-scrape doesn't ping-pong the slot" in {
    val repo  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repo)
    // One cinema lists the SAME film under two spellings that CLEAN differently
    // but SANITIZE identically (canonical maps "&"→"i"), so both target the one
    // slot key CinemaShowing(Multikino, "mandalorianigrogu"). Future showtimes so
    // the past-retention path isn't involved — this is purely the slot collision.
    val amp = cinemaMovie("Mandalorian & Grogu", Multikino, showtimes = Seq(showtime("2027-06-08T18:00")))
    val iii = cinemaMovie("Mandalorian i Grogu", Multikino, showtimes = Seq(showtime("2027-06-08T20:30")))
    cache.recordCinemaScrape(Multikino, Seq(amp, iii))
    repo.upserts should not be empty      // first scrape established the slot
    repo.upserts.clear()
    val emissions = changeStreamEmissions(repo)
    // Identical re-scrape: the two variants fold into ONE slot (deduped at the
    // sanitize granularity), so the stored slot is unchanged → no write. Before
    // the slot-key-aligned dedup they alternately overwrote the single slot, so
    // every tick fired two writes + two change-stream emissions, forever.
    cache.recordCinemaScrape(Multikino, Seq(amp, iii))
    repo.upserts shouldBe empty
    emissions.get() shouldBe 0
  }

  it should "carry a slot's detail fields (director/cast/runtime) forward when a later scrape's listing omits them (no strip, no churn)" in {
    val repo  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repo)
    val show  = Seq(showtime("2027-06-08T18:00"))
    // First scrape carries detail — as a deferred detail-page fetch would have
    // populated the slot (director/cast/runtime live on the venue's film page).
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Nino", Multikino, showtimes = show,
                  cast = Seq("A", "B"), director = Seq("Pauline Loquès"), runtimeMinutes = Some(96))))
    repo.upserts should not be empty
    repo.upserts.clear()
    val emissions = changeStreamEmissions(repo)
    // Next scrape is the bare listing — no cast/director/runtime (they're only on
    // the detail page, not re-parsed). Carry-forward keeps them, so the stored slot
    // is identical → no write. Before the fix the rebuild stripped them → churn,
    // and a TMDB-unresolved film lost its only director.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Nino", Multikino, showtimes = show)))
    repo.upserts shouldBe empty
    emissions.get() shouldBe 0
    val slot = cache.get(cache.keyOf("Nino", Some(2026)))
      .flatMap(_.data.collectFirst { case (_: CinemaShowing, sd) => sd })
    slot.map(_.director)       shouldBe Some(Seq("Pauline Loquès"))
    slot.map(_.runtimeMinutes) shouldBe Some(Some(96))
  }

  it should "flag the second cinema as new when it scrapes a film already in the cache from another cinema" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Top Gun: Maverick", Multikino, Some(2022))))
    val helios = cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Top Gun: Maverick", Helios, Some(2022))
    ))
    helios.head._3 shouldBe true   // Helios hasn't reported this row before
  }

  it should "flag a year correction from the same cinema as new" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Bez wyjścia", Multikino, None)))
    // Same cinema now reports the same title with a year — a different scrape
    // tuple. The redirect routes it onto the same row, then `canonicalRank`
    // promotes the row onto the year-bearing key (a year out-ranks yearless), so
    // the returned canonical key now carries the corrected year.
    val corrected = cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Bez wyjścia", Multikino, Some(2025))
    ))
    corrected.head._3 shouldBe true
    corrected.head._2 shouldBe cache.keyOf("Bez wyjścia", Some(2025))  // promoted to the year-bearing canonical key
  }

  // Helios sources `releaseYear` only from a best-effort REST lookup with no
  // fallback (HeliosClient enriches the NUXT listing, whose year is always
  // None). That lookup is flaky — when the screening's REST body is briefly
  // absent the year comes back None, then reappears next pass. A naive isNew
  // would treat each Some(year)↔None flip as a fresh tuple and refire
  // MovieDetailsComplete every single pass (the recurring "(N new)" on Helios).
  // A tick that DROPS a previously-known year is data loss, not a correction:
  // keep the year and stay quiet. The within-pass dedup (mergeDuplicateFilms)
  // is orthogonal — it can't see a year that flakes only on a later pass.
  it should "not refire as new when a flaky year-source drops the year on a later tick" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Wicked", Helios, Some(2024))))
    // Next pass: REST enrichment flaked, so the scrape carries no year.
    val dropped = cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Wicked", Helios, None)))
    dropped.head._3 shouldBe false
    // The slot keeps the previously-known year for the merged view.
    cache.get(cache.keyOf("Wicked", Some(2024))).get.cinemaData(Helios).releaseYear shouldBe Some(2024)
    // And when the year reappears next pass it's still the same tuple — quiet.
    val back = cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Wicked", Helios, Some(2024))))
    back.head._3 shouldBe false
  }

  // Kino Kultura lists a film both plainly ("Ojczyzna") and as a screening
  // with an associated producer Q&A ("Ojczyzna + spotkanie z producentką Ewą
  // Puszczyńską"). The "+ <event>" suffix marks a distinct screening, so the
  // two are SEPARATE cache rows (each aggregating its own showtimes) that
  // share only their upstream enrichment — exactly the ProgrammePrefix /
  // AccessibilityTag treatment. Before the fix both collapsed onto one slot
  // (searchTitle stripped the suffix): each pass they overwrote each other's
  // showtimes and flipped isNew, the recurring "Refreshed Kino Kultura: …
  // (2 new)" worker log.
  it should "keep a '+ event' screening as its own row and not churn isNew on repeat passes" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    def pass() = cache.recordCinemaScrape(KinoKultura, Seq(
      cinemaMovie("Ojczyzna", KinoKultura, None, showtimes = Seq(showtime("2026-06-08T18:00"))),
      cinemaMovie("Ojczyzna + spotkanie z producentką Ewą Puszczyńską", KinoKultura, None,
                  showtimes = Seq(showtime("2026-06-09T20:00")))
    ))

    val first = pass()
    first should have size 2
    first.map(_._3) shouldBe Seq(true, true)   // both genuinely new on first scrape

    // Two distinct rows, each holding only its own screening's showtimes.
    val plain = cache.get(cache.keyOf("Ojczyzna", None)).get
    val event = cache.get(cache.keyOf("Ojczyzna + spotkanie z producentką Ewą Puszczyńską", None)).get
    plain.cinemaData(KinoKultura).showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.parse("2026-06-08T18:00"))
    event.cinemaData(KinoKultura).showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.parse("2026-06-09T20:00"))

    // Steady state: same two screenings reported again → no "(2 new)" churn, and
    // no write/change-stream emission (an identical re-scrape is a full no-op).
    val emissions = changeStreamEmissions(repository)
    pass().map(_._3) shouldBe Seq(false, false)
    emissions.get() shouldBe 0
  }

  it should "create a new record when no matching row exists yet" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"), Seq(showtime("2026-06-01T18:00")))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaData.keySet shouldBe Set(Multikino)
    row.cinemaData(Multikino).posterUrl shouldBe Some("multikino.jpg")
    row.cinemaData(Multikino).showtimes.size shouldBe 1
  }

  it should "merge two cinemas' slots into the same record when their titles share a documentId" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      // Punctuation-only variant — phase 2's documentId rule collapses both.
      cinemaMovie("Top Gun Maverick", Helios, Some(2022), Some("helios.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaData.keySet shouldBe Set(Multikino, Helios)
    // Multikino's poster wins the merged view (priority rule).
    row.posterUrl shouldBe Some("multikino.jpg")
    // Both raw titles are recorded in their per-cinema slot's `title` (and
    // surfaced via the derived `cinemaTitles` view).
    row.cinemaTitles should contain allOf ("Top Gun: Maverick", "Top Gun Maverick")
  }

  // Most cinema clients parse a production-country list from their sources
  // but until this change CinemaShowings dropped it on the floor — the value
  // was populated on Movie.countries, then thrown away when the slot was
  // built. Persisting it lets a downstream view show "USA, Polska" without
  // re-scraping, and lets the merged MovieRecord.countries surface a union
  // across cinemas in priority order (Multikino first).
  it should "store production countries per cinema slot and union them on the merged record" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), countries = Seq.empty)
    ))
    cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Top Gun Maverick", Helios, Some(2022), countries = Seq("USA"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.cinemaData(Multikino).countries shouldBe Seq.empty
    row.cinemaData(Helios).countries    shouldBe Seq("USA")
    // Multikino has the slot but no countries; Helios fills in.
    row.countries shouldBe Seq("USA")
  }

  it should "union countries across cinemas in priority order on the merged record" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026), countries = Seq("Polska"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      // Helios brings a co-production country Multikino doesn't have; the
      // union picks up both, Multikino-first.
      cinemaMovie("Foo", Helios, Some(2026), countries = Seq("Polska", "USA"))
    ))

    cache.get(cache.keyOf("Foo", Some(2026))).get.countries shouldBe Seq("Polska", "USA")
  }

  // Without a canonical mapping, the same country spelt different ways
  // by different cinemas would surface as both in the merged view ("USA"
  // *and* "Stany Zjednoczone"). The cache folds every alias to a single
  // canonical name per `CountryNames` on the way into storage.
  it should "canonicalise country spellings on the way into the cache" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Bar", Multikino, Some(2026), countries = Seq("Stany Zjednoczone", "UK"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      cinemaMovie("Bar", Helios, Some(2026), countries = Seq("USA", "Wielka Brytania", "francja"))
    ))

    val row = cache.get(cache.keyOf("Bar", Some(2026))).get
    // Per-cinema slot already stores canonical names — `Stany Zjednoczone`
    // folded to `USA`, `UK` folded to `Wielka Brytania`.
    row.cinemaData(Multikino).countries shouldBe Seq("USA", "Wielka Brytania")
    row.cinemaData(Helios).countries    shouldBe Seq("USA", "Wielka Brytania", "Francja")
    // Merged union sees the same canonical strings from both cinemas, so
    // dedup collapses them to one entry each.
    row.countries shouldBe Seq("USA", "Wielka Brytania", "Francja")
  }

  it should "preserve enrichment-side fields when only cinema data changes" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    // Seed with an enriched record (TMDB-resolved, no cinemas yet).
    val seed = MovieRecord(
      imdbId = Some("tt1745960"), imdbRating = Some(8.2), metascore = Some(78),
      tmdbId = Some(361743),
      data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Top Gun: Maverick")))
    )
    cache.put(cache.keyOf("Top Gun: Maverick", Some(2022)), seed)

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))

    val row = cache.get(cache.keyOf("Top Gun: Maverick", Some(2022))).get
    row.imdbId        shouldBe Some("tt1745960")
    row.imdbRating    shouldBe Some(8.2)
    row.metascore     shouldBe Some(78)
    row.cinemaData.keySet shouldBe Set(Multikino)
  }

  // Backfills and `FilmwebUrlAudit` write to the repository directly; the running
  // cache learns about those edits via the 30-s rehydrate tick. Between the
  // write and the tick, `recordCinemaScrape` mustn't clobber the freshly
  // edited field with its own stale-cache view of the record. Concretely:
  // a scrape that only intends to refresh one cinema slot must not undo an
  // out-of-band update to a *different* field of the same row.
  it should "preserve an out-of-band repository edit to a non-cinema field across a concurrent recordCinemaScrape" in {
    val repository  = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Top Gun: Maverick", Some(2022))

    // Initial state — cache + repository agree (rating 7.0, Multikino slot present).
    cache.put(key, MovieRecord(
      imdbRating = Some(7.0),
      data       = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Top Gun: Maverick"), releaseYear = Some(2022), posterUrl = Some("multikino.jpg"))
      )
    ))

    // Out-of-band edit (the FilmwebUrlAudit / one-shot backfill shape) — bumps
    // the rating in the repository without touching the cache. Cache still holds 7.0;
    // repository now holds 8.5.
    val stored = repository.findAll().find(_.title == "Top Gun: Maverick").get
    repository.upsert("Top Gun: Maverick", Some(2022), stored.record.copy(imdbRating = Some(8.5)))

    // Concurrent scrape — repeats the same Multikino slot the cache already has.
    // No cinema-side state actually changes; the only thing the scrape's write
    // could clobber is the out-of-band repository edit.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))

    repository.findAll().find(_.title == "Top Gun: Maverick").get.record.imdbRating shouldBe Some(8.5)
  }

  // Muza is a two-stage scraper: every 5-min listing tick carries None for
  // `posterUrl` / `synopsis` / `trailerUrl` (the listing only knows title +
  // showtimes + director + filmUrl), while the deferred `EnrichDetails` task
  // (`KinoMuzaClient.fetchFilmDetail`) fetches each row's detail page on a
  // slower cadence and writes the portrait poster, synopsis, and trailer back.
  // The next listing tick mustn't undo that upgrade: the merge rule keeps
  // the existing slot's value whenever the cinema reports `None`.
  it should "preserve detail-upgraded synopsis / trailerUrl / posterUrl across listing scrapes" in {
    val repository = new InMemoryMovieRepository()
    val cache = new CaffeineMovieCache(repository)
    val key   = cache.keyOf("Wolność po włosku", Some(2025))

    // Tick 1: listing scrape carries no detail-page fields.
    cache.recordCinemaScrape(KinoMuza, Seq(
      cinemaMovie("Wolność po włosku", KinoMuza, Some(2025), poster = None)
    ))

    // The refresher visits the detail page and upgrades all three fields. The slot
    // is keyed per shown title (`CinemaShowing`), as detail enrichment now targets.
    val muzaSlot = CinemaShowing.keyFor(KinoMuza, "Wolność po włosku")
    cache.putIfPresent(key, current =>
      current.copy(data = current.data + (muzaSlot ->
        current.data(muzaSlot).copy(
          synopsis   = Some("opis filmu"),
          trailerUrl = Some("https://youtube.com/watch?v=abc"),
          posterUrl  = Some("https://www.kinomuza.pl/2026/04/wolnosc-po-w-1-555x800.jpg")
        )
      ))
    )

    // Tick 2: listing scrape with the same None payload. Carry-forward keeps the
    // detail-upgraded fields, so the rebuilt slot is identical → no write, no
    // change-stream emission (the re-scrape is a no-op).
    val emissions = changeStreamEmissions(repository)
    cache.recordCinemaScrape(KinoMuza, Seq(
      cinemaMovie("Wolność po włosku", KinoMuza, Some(2025), poster = None)
    ))
    emissions.get() shouldBe 0

    val slot = cache.get(key).get.cinemaData(KinoMuza)
    slot.synopsis   shouldBe Some("opis filmu")
    slot.trailerUrl shouldBe Some("https://youtube.com/watch?v=abc")
    slot.posterUrl  shouldBe Some("https://www.kinomuza.pl/2026/04/wolnosc-po-w-1-555x800.jpg")
  }

  it should "prune a cinema's slot from records that didn't appear in the fresh scrape" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    // Tick 1: Multikino reports A + B.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino),
      cinemaMovie("B", Multikino)
    ))
    cache.get(cache.keyOf("A", Some(2026))).get.cinemaData should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaData should contain key Multikino

    // Tick 2: Multikino only reports A. B's Multikino slot should be pruned.
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("A", Multikino)
    ))

    cache.get(cache.keyOf("A", Some(2026))).get.cinemaData should contain key Multikino
    cache.get(cache.keyOf("B", Some(2026))).get.cinemaData should not contain key (Multikino)
  }

  // Cross-script protection now comes from `MovieService.normalize` keeping
  // Unicode letters: Cyrillic and Latin titles produce different normalised
  // forms, so the redirect filter can't merge them. No explicit cross-script
  // filter on `put` is needed — and `cinemaTitles` itself is derived from
  // the per-cinema slot's `title`, so a Polish scrape onto a Polish row
  // never gets a Cyrillic variant in its cinemaTitles to begin with.

  // Stop the flap loop where a year=None cinema title gets re-created every
  // tick. With `recordCinemaScrape`'s redirect, a fresh year=None scrape
  // onto an existing year=Some row gets folded onto that row.
  it should "redirect a fresh year=None scrape onto an existing year=Some row at the same cleanTitle" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val survivor = MovieRecord(
      imdbId = Some("tt1527793"),
      tmdbId = Some(639988),
      data   = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("어쩔수가없다")))
    )
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)), survivor)
    cache.snapshot().size shouldBe 1

    cache.recordCinemaScrape(KinoBulgarska, Seq(
      cinemaMovie("Bez wyjścia", KinoBulgarska, year = None, showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    cache.snapshot().size shouldBe 1
    val row = cache.get(cache.keyOf("Bez wyjścia", Some(2025))).get
    row.cinemaData.keySet shouldBe Set(KinoBulgarska)
    row.cinemaData(KinoBulgarska).showtimes.size shouldBe 1
    cache.get(cache.keyOf("Bez wyjścia", None)) shouldBe None
  }

  it should "record the incoming raw cinema title in the per-cinema slot (and the derived cinemaTitles view)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val survivor = MovieRecord(
      imdbId = Some("tt17490712"),
      tmdbId = Some(931285)
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Bez wyjścia", Some(2025)),
              MovieRecord())

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
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
      val latch = new java.util.concurrent.CountDownLatch(1)
      val executionContext    = tools.DaemonExecutors.virtualThreadEC("movie-cache-race")
      val t1 = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(Multikino, Seq(cinemaMovie(title, Multikino, None)))
      }(using executionContext)
      val t2 = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(Helios, Seq(cinemaMovie(title, Helios, Some(2026))))
      }(using executionContext)
      latch.countDown()
      scala.concurrent.Await.ready(scala.concurrent.Future.sequence(Seq(t1, t2))(using implicitly, executionContext), scala.concurrent.duration.Duration.Inf)
      executionContext.shutdown()

      withClue(s"snapshot after iteration: ${cache.snapshot().map(r => s"(${r.title}, ${r.year})").mkString(", ")} ") {
        cache.snapshot().size shouldBe 1
      }
    }
  }

  // Regression: the user-visible "Straszny film appears twice" bug. Helios
  // / Multikino scrape "Straszny film" with year=None; CC reports year=2026.
  // The Helios row gets created at key (None) and the TMDB stage starts
  // running. The TMDB stage's re-key path is two non-atomic steps —
  // `cache.invalidate(oldKey)` then `cache.put(newKey, …)`. If CC's
  // recordCinemaScrape fires AFTER the invalidate but BEFORE the put,
  // CC's `redirectToExistingVariant` sees no sibling row for "Straszny
  // film" and creates a phantom row at (Some(2026)). The TMDB stage
  // then puts at (Some(2000)) and we end up with TWO rows for the same
  // Polish title — one resolved to the 2000 Wayans Bros. film (which no
  // cinema actually screens), one resolved to the 2026 reboot. The fix:
  // the TMDB stage's re-key must hold the same per-title lock that
  // `recordCinemaScrape` holds — exposed here as `cache.rekey`.
  it should "not create a phantom row when a TMDB-stage re-key races a concurrent recordCinemaScrape" in {
    val title = "Straszny film"
    val iterations = 100
    for (_ <- 1 to iterations) {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository())

      // Initial state: Helios scraped no-year. Row at (None).
      cache.recordCinemaScrape(Helios, Seq(cinemaMovie(title, Helios, year = None)))

      val keyNone   = cache.keyOf(title, None)
      val key2000   = cache.keyOf(title, Some(2000))
      val resolved  = MovieRecord(
        tmdbId = Some(4247),
        imdbId = Some("tt0175142"),
        data   = Map[Source, SourceData](Tmdb -> SourceData(title = Some("Scary Movie"), releaseYear = Some(2000)))
      )

      val latch = new java.util.concurrent.CountDownLatch(1)
      val executionContext    = tools.DaemonExecutors.virtualThreadEC("rekey-race")

      // Thread A: the TMDB stage's re-key, mirroring `runTmdbStageSync`'s
      // canonical-key resolution — it addresses the row by its LIVE canonical
      // key (computed under the per-title lock), so a concurrent scrape that
      // already promoted the row onto a year-bearing key can't strand this
      // write at the now-empty (None) key (which is how a phantom used to form).
      val tmdbStage = scala.concurrent.Future {
        latch.await()
        cache.withTitleLock(keyNone.cleanTitle) {
          val live = cache.canonicalKeyFor(keyNone).getOrElse(keyNone)
          cache.rekey(live, key2000, _ => resolved)
        }
      }(using executionContext)
      // Thread B: a concurrent cinema scrape with year=2026. Contends for
      // the same lock — must either see the old (None) row (before the
      // re-key) and redirect onto it, or see the new (Some(2000)) row
      // (after the re-key) and redirect onto that. Either way: one row.
      val ccScrape = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(
          cinemaMovie(title, CinemaCityPoznanPlaza, year = Some(2026))
        ))
      }(using executionContext)

      latch.countDown()
      scala.concurrent.Await.ready(
        scala.concurrent.Future.sequence(Seq(tmdbStage, ccScrape))(using implicitly, executionContext),
        scala.concurrent.duration.Duration.Inf
      )
      executionContext.shutdown()

      withClue(s"snapshot after iteration: ${cache.snapshot().map(r => s"(${r.title}, ${r.year}, tmdb=${r.record.tmdbId.getOrElse("—")})").mkString(", ")} ") {
        cache.snapshot().size shouldBe 1
      }
    }
  }

  // Regression: CI flakily reported "Władcy wszechświata" missing one of
  // its CC slots after a fixture-driven scrape tick. The race:
  //
  //   1. CC Plaza's recordCinemaScrape runs. For Władcy it acquires
  //      lockFor("Władcy"), writes its slot at the row's current key,
  //      releases the lock. The `touched` set captures THAT key.
  //   2. Before CC Plaza's prune loop iterates the cache, an
  //      asynchronously-running TMDB stage (triggered by an earlier
  //      cinema's event) rekeys the row from its current key to a
  //      year-keyed sibling, carrying CC Plaza's slot along.
  //   3. CC Plaza's prune iterates the cache. It finds the row at the
  //      NEW key with a CC Plaza slot, but the NEW key isn't in
  //      `touched` (which holds the OLD key). The prune drops CC Plaza's
  //      slot — a slot that was just written this same tick.
  //
  // Fix: identify "touched" by `SourceData` reference, not by cache key.
  // `cache.rekey` preserves slot references verbatim across the move,
  // so the prune correctly recognises just-written slots even after a
  // concurrent rekey.
  it should "not drop a cinema's slot when a concurrent rekey moves the row between slot-write and prune" in {
    val title      = "Władcy wszechświata"
    val iterations = 200
    for (_ <- 1 to iterations) {
      val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
      // Seed: row at (Władcy, None) from a no-year scrape.
      cache.recordCinemaScrape(Multikino, Seq(cinemaMovie(title, Multikino, None)))

      val latch = new java.util.concurrent.CountDownLatch(1)
      val executionContext    = tools.DaemonExecutors.virtualThreadEC("prune-rekey-race")

      // Thread A: CC Plaza scrape. Lands on (None) via redirect, then
      // enters the prune loop. The buggy path: between the per-movie
      // write and the prune iteration, thread B rekeys the row, so
      // when the prune scans the cache it finds CC Plaza's slot at a
      // key not in its `touched` set and drops it.
      val ccScrape = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(
          cinemaMovie(title, CinemaCityPoznanPlaza, year = None)
        ))
      }(using executionContext)
      // Thread B: rekey (None) → (Some(2026)). The updater reads the
      // row's CURRENT state under the title lock — mirrors
      // `MovieService.runTmdbStageSync`'s carry-forward shape — so CC
      // Plaza's just-written slot is visible if it landed first.
      val rekey = scala.concurrent.Future {
        latch.await()
        cache.rekey(cache.keyOf(title, None), cache.keyOf(title, Some(2026)),
          current => current.copy(tmdbId = Some(454639))
        )
      }(using executionContext)

      latch.countDown()
      scala.concurrent.Await.ready(
        scala.concurrent.Future.sequence(Seq(ccScrape, rekey))(using implicitly, executionContext),
        scala.concurrent.duration.Duration.Inf
      )
      executionContext.shutdown()

      // The CC Plaza slot must end up somewhere in the cache —
      // whichever key the row eventually settles on after the rekey
      // and scrape interleave. Without slot-identity prune, the slot
      // is dropped in races where the rekey lands between CC Plaza's
      // write and its prune.
      val ccPlazaSlots = cache.snapshot().flatMap(_.record.cinemaData.get(CinemaCityPoznanPlaza))
      withClue(s"snapshot: ${cache.snapshot().map(r => s"(${r.year}, data=${r.record.data.keys.map(_.displayName).mkString(",")})").mkString(" | ")} ") {
        ccPlazaSlots should not be empty
      }
    }
  }

  it should "NOT redirect across scripts — Cyrillic and Latin normalise differently" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), tmdbId = Some(931285)))

    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Mortal Kombat II", Multikino, Some(2026), showtimes = Seq(showtime("2026-06-01T18:00")))
    ))

    // No redirect — a fresh Latin row gets created, distinct from Cyrillic.
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))) shouldBe defined
    cache.get(cache.keyOf("Mortal Kombat II", Some(2026))).get.cinemaData.keySet shouldBe Set(Multikino)
    cache.get(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026))) shouldBe defined
  }

  it should "NOT redirect when two existing rows could both be the target (ambiguous)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    // Two different films share the cinema-reported title "Wspinaczka" —
    // one row per year, each pinned to a different imdbId. Redirecting a
    // year=None scrape would be a coin-flip → keep them distinct.
    cache.put(cache.keyOf("Wspinaczka", Some(2017)),
              MovieRecord(imdbId = Some("tt5157682")))
    cache.put(cache.keyOf("Wspinaczka", Some(2025)),
              MovieRecord(imdbId = Some("tt36437006")))

    cache.recordCinemaScrape(KinoBulgarska, Seq(cinemaMovie("Wspinaczka", KinoBulgarska, year = None)))

    cache.get(cache.keyOf("Wspinaczka", None)) shouldBe defined
    cache.snapshot().size shouldBe 3
  }

  // `hasResolvedSiblingByTitle` is what `needsTmdbResolution` consults to skip
  // a phantom TMDB call when a sibling row already resolved the same film.
  "hasResolvedSiblingByTitle" should "return true when a resolved row's cleanTitle normalises to the same form" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Milcząca przyjaciółka", Some(2025)),
              MovieRecord(imdbId = Some("tt27811632"), tmdbId = Some(1168719)))

    cache.hasResolvedSiblingByTitle("Milcząca przyjaciółka") shouldBe true
  }

  it should "return false when the matching row has no tmdbId yet" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Foo", None),
              MovieRecord())

    cache.hasResolvedSiblingByTitle("Foo") shouldBe false
  }

  it should "return false when no row carries the title at all" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Something else", Some(2024)),
              MovieRecord(tmdbId = Some(42)))

    cache.hasResolvedSiblingByTitle("Unrelated") shouldBe false
  }

  it should "return false across scripts — Cyrillic cleanTitle doesn't satisfy a Latin lookup" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("МОРТАЛ КОМБАТ ІІ", Some(2026)),
              MovieRecord(imdbId = Some("tt17490712"), tmdbId = Some(931285)))

    cache.hasResolvedSiblingByTitle("Mortal Kombat II") shouldBe false
  }

  // ── Scrape-failure semantics ────────────────────────────────────────────
  //
  // A cinema's `fetch()` can return Seq.empty for two very different reasons:
  //   (a) The cinema is genuinely showing zero films right now (a holiday, a
  //       closure, or a brand-new site with no schedule yet).
  //   (b) The scraper failed silently — Cloudflare challenge, parser regex
  //       mismatch, proxy 503, blank HTML, etc. — and returned [] even
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    // Seed with a film the cinema was previously showing.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, Some(2026))))
    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaData.keySet shouldBe Set(Multikino)

    // Scraper "fails": returns empty list. With the safeguard this is a no-op.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaData.keySet shouldBe Set(Multikino)
  }

  it should "still prune a slot when the scrape returned other films but not this one" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026)),
      cinemaMovie("Bar", Multikino, Some(2026))
    ))

    // Bar drops out — Foo stays, Bar's slot should be pruned (cinema genuinely
    // stopped showing it, scrape returned a non-empty alternative list).
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026))
    ))

    cache.get(cache.keyOf("Foo", Some(2026))).get.cinemaData.keySet shouldBe Set(Multikino)
    cache.get(cache.keyOf("Bar", Some(2026))).get.cinemaData.keySet shouldBe Set.empty
  }

  // Regression for the "Mortal Kombat row appears and disappears" symptom: a
  // merged survivor with slots from multiple cinemas should not lose ALL of
  // one cinema's slot just because that cinema's scrape blanked momentarily.
  it should "preserve all cinemas' slots on a survivor row when one cinema's scrape blanks" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Mortal Kombat", Multikino, Some(2026))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Mortal Kombat", Helios,    Some(2026))))
    val key = cache.keyOf("Mortal Kombat", Some(2026))
    cache.get(key).get.cinemaData.keySet shouldBe Set(Multikino, Helios)

    // Multikino's scraper has a hiccup and returns nothing this tick.
    cache.recordCinemaScrape(Multikino, Seq.empty)

    // Helios slot definitely stays. Multikino's slot is preserved by the
    // safeguard so the row keeps rendering until Multikino's next successful
    // tick (which is the "row reappears" the user saw 60s later).
    val row = cache.get(key).get
    row.cinemaData.keySet should contain (Helios)
    row.cinemaData.keySet should contain (Multikino)
  }

  it should "not prune other cinemas' slots when one cinema's tick lands" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    // Both cinemas report the same film initially.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Shared", Multikino, Some(2026), Some("mu.jpg"))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Shared", Helios,    Some(2026), Some("he.jpg"))))

    val before = cache.get(cache.keyOf("Shared", Some(2026))).get
    before.cinemaData.keySet shouldBe Set(Multikino, Helios)

    // Helios genuinely drops Shared (scrape returns other films, just not
    // Shared) — Helios's slot is pruned, Multikino's slot is untouched.
    cache.recordCinemaScrape(Helios, Seq(cinemaMovie("Different Film", Helios, Some(2026))))

    val after = cache.get(cache.keyOf("Shared", Some(2026))).get
    after.cinemaData.keySet shouldBe Set(Multikino)
  }

  // ── Synopsis retention: the displayed synopsis stays sticky after a reap ────
  // When the cinema that had the best (longest) synopsis stops listing a film,
  // its slot is pruned — but the synopsis is retained so the displayed value
  // doesn't flip to whatever shorter blurb remains.
  private val longSynopsis  = "Znacznie dłuższy, pełniejszy opis filmu, który ma zostać zachowany po reapie."
  private val shortSynopsis = "Krótki opis."

  it should "retain a pruned cinema's synopsis so the displayed synopsis stays sticky" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, Some(2026), synopsis = Some(longSynopsis))))
    cache.recordCinemaScrape(Helios,    Seq(cinemaMovie("Foo", Helios,    Some(2026), synopsis = Some(shortSynopsis))))
    val key = cache.keyOf("Foo", Some(2026))
    cache.get(key).get.synopsis shouldBe Some(longSynopsis)

    // Multikino (the long-synopsis source) drops Foo: its scrape returns a
    // different film, so Foo's Multikino slot is genuinely pruned.
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Bar", Multikino, Some(2026))))

    val row = cache.get(key).get
    row.cinemaData.keySet           shouldBe Set(Helios)            // live slot gone
    // Retention is per shown-title SLOT (CinemaShowing), so a dub's blurb stays
    // attached to its own title rather than bleeding onto the base.
    row.retainedSynopses.get(CinemaShowing.keyFor(Multikino, "Foo")) shouldBe Some(longSynopsis)
    row.synopsis                    shouldBe Some(longSynopsis)     // still the best one
  }

  it should "collapse a CMS-duplicated blurb at ingestion so the STORED slot is one copy" in {
    // Bilety24's Kino Piast shipped the "Ojczyzna" synopsis 9× glued together in
    // one description field. The root fix collapses it as it's stored, so the raw
    // slot — not just the read-time synopsis — holds a single copy.
    val unit  = "Pełny opis filmu w jednym sensownym zdaniu."
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Foo", Multikino, Some(2026), synopsis = Some(unit * 9))))

    val row = cache.get(cache.keyOf("Foo", Some(2026))).get
    row.cinemaData(Multikino).synopsis shouldBe Some(unit)  // stored slot collapsed
    row.synopsis                       shouldBe Some(unit)
  }

  it should "not keep a movie alive via retained synopses once its last cinema slot is pruned" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Foo", Multikino, Some(2026), synopsis = Some(longSynopsis)),
      cinemaMovie("Bar", Multikino, Some(2026))
    ))
    // Foo drops out entirely (Multikino still scrapes, just not Foo).
    cache.recordCinemaScrape(Multikino, Seq(cinemaMovie("Bar", Multikino, Some(2026))))

    val row = cache.get(cache.keyOf("Foo", Some(2026))).get
    // The retention captured the synopsis, but the row has NO cinema slots — so
    // UnscreenedCleanup (which gates on cinemaData.isEmpty) still deletes it.
    row.retainedSynopses.get(CinemaShowing.keyFor(Multikino, "Foo")) shouldBe Some(longSynopsis)
    row.cinemaData                       shouldBe empty
  }

  "snapshot" should "return rows sorted by title (case-insensitive)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.put(cache.keyOf("Zorro", None),   mkEnrichment("tt3"))
    cache.put(cache.keyOf("alpha", None),   mkEnrichment("tt1"))
    cache.put(cache.keyOf("Beta", None),    mkEnrichment("tt2"))

    cache.snapshot().map(_.title) shouldBe Seq("alpha", "Beta", "Zorro")
  }

  "lastModified" should "advance on put" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val before = cache.lastModified
    Thread.sleep(2)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))
    cache.lastModified.isAfter(before) shouldBe true
  }

  it should "advance on putIfPresent" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    val before = cache.lastModified
    Thread.sleep(2)
    cache.putIfPresent(key, _.copy(imdbRating = Some(9.0)))
    cache.lastModified.isAfter(before) shouldBe true
  }

  it should "advance on rehydrate" in {
    val repository = new InMemoryMovieRepository(Seq(("Film", Some(2024), mkEnrichment("tt1"))))
    val cache = new CaffeineMovieCache(repository)
    val before = cache.lastModified
    Thread.sleep(2)
    cache.rehydrate()
    cache.lastModified.isAfter(before) shouldBe true
  }

  // ── Alias-aware scrape landing (cross-language films) ──────────────────────
  "recordCinemaScrape" should
    "land an original-language scrape on the existing resolved row, not spawn a translation duplicate" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    // An existing resolved row for Tangled, keyed under its Polish title.
    cache.put(cache.keyOf("Zaplątani", Some(2010)),
      resolvedRecord(38757, 2010, tmdbTitle = "Zaplątani", originalTitle = "Tangled", cinema = Helios))
    // Another cinema lists the SAME film under its original English title. Its
    // sanitized title ("tangled") matches the row's TMDB alias, so the slot lands
    // on the existing row instead of creating a second "tangled|2010" doc.
    cache.recordCinemaScrape(Multikino, Seq(cm(Multikino, "Tangled", Some(2010))))
    val rows = cache.snapshot()
    withClue(s"expected ONE row, got ${rows.map(r => (r.title, r.year))}\n")(rows.size shouldBe 1)
    rows.head.record.cinemaData.keySet shouldBe Set(Helios, Multikino)
    TitleNormalizer.sanitize(rows.head.title) shouldBe "zaplatani"
  }

  it should "NOT land a decorated edition on the base film via alias-matching" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository)
    cache.put(cache.keyOf("Zaproszenie", Some(2022)),
      resolvedRecord(9001, 2022, tmdbTitle = "Zaproszenie", originalTitle = "The Invitation", cinema = Helios))
    // A programme edition of the base film: its title adds the banner, so it
    // matches no TMDB alias and must stay its own row.
    cache.recordCinemaScrape(KinoMuza, Seq(cm(KinoMuza, "Zaproszenie | Kinoteka dla rodziców", Some(2022))))
    cache.snapshot().map(r => TitleNormalizer.sanitize(r.title)).toSet shouldBe
      Set("zaproszenie", "zaproszeniekinotekadlarodzicow")
  }
}
