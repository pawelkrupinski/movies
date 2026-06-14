package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class MovieCacheSpec extends AnyFlatSpec with Matchers {

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating)

  "MovieCache" should "hydrate from the repository on construction" in {
    // Carry the title in a cinema slot, as a scraped row does: the repository
    // re-derives the display title from the record on read (like Mongo), so a
    // title-less record would surface its sanitized _id prefix, not "Drzewo Magii".
    val rec   = mkEnrichment("tt1").copy(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Drzewo Magii"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Drzewo Magii", Some(2024), rec))))

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(rec)
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Drzewo Magii", Some(2024)))
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
    // an oplog entry + a change-stream `updateLookup` full-doc read — the
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
  "rehydrate" should "drop in-memory rows that aren't in the repository" in {
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
    // with no title slot collapses to its sanitized _id prefix ("a"). What this
    // pins is that the watcher fires on each write until closed; title is incidental.
    seen.toList shouldBe List(("a", Some(2024), None), ("a", Some(2024), Some(8.0)))
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

  // Rows with the same tmdbId but a different cleanTitle stay separate.
  // TMDB's identity says "same film", but cinemas surface these as distinct
  // listings for distinct audiences:
  //   - Polish/Latin original vs Cyrillic Ukrainian-language listing
  //     ("Diabeł ubiera się u Prady 2" vs "ДИЯВОЛ НОСИТЬ ПРАДА 2").
  //   - Original vs explicitly-labelled dub variant ("Diabeł ubiera się u
  //     Prady 2" vs "Diabeł ubiera się u Prady 2 ukraiński dubbing", both
  //     listed at the same cinema with their own showtimes).
  // Folding them would force one card's display title to be hidden. Each
  // variant gets its own row, its own per-cinema slot, and its own card.
  it should "NOT fold rows with the same tmdbId when their cleanTitles differ (cross-script)" in {
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepository())
    val latin    = cache.keyOf("Diabeł ubiera się u Prady 2", Some(2026))
    val cyrillic = cache.keyOf("ДИЯВОЛ НОСИТЬ ПРАДА 2",       Some(2026))

    cache.put(latin,    mkResolved(928344, cinemaSlots = Map(Multikino -> SourceData(title = Some("Diabeł ubiera się u Prady 2"), releaseYear = Some(2026)))))
    cache.put(cyrillic, mkResolved(928344, cinemaSlots = Map(Helios    -> SourceData(title = Some("ДИЯВОЛ НОСИТЬ ПРАДА 2"),       releaseYear = Some(2026)))))

    cache.get(latin)    shouldBe defined
    cache.get(cyrillic) shouldBe defined
    cache.snapshot().map(_.title).toSet shouldBe Set("Diabeł ubiera się u Prady 2", "ДИЯВОЛ НОСИТЬ ПРАДА 2")
  }

  it should "NOT fold rows with the same tmdbId when their cleanTitles differ (suffix variants)" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository())
    val regular = cache.keyOf("Diabeł ubiera się u Prady 2",                 Some(2026))
    val dub     = cache.keyOf("Diabeł ubiera się u Prady 2 ukraiński dubbing", Some(2026))

    cache.put(regular, mkResolved(928344, cinemaSlots = Map(CinemaCityPoznanPlaza -> SourceData(title = Some("Diabeł ubiera się u Prady 2"),                releaseYear = Some(2026)))))
    cache.put(dub,     mkResolved(928344, cinemaSlots = Map(CinemaCityPoznanPlaza -> SourceData(title = Some("Diabeł ubiera się u Prady 2 ukraiński dubbing"), releaseYear = Some(2026)))))

    cache.get(regular) shouldBe defined
    cache.get(dub)     shouldBe defined
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

  private def cinemaMovie(title: String, cinema: Cinema, year: Option[Int] = Some(2026),
                          poster: Option[String] = None, showtimes: Seq[Showtime] = Seq.empty,
                          countries: Seq[String] = Seq.empty): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year, countries = countries),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = None,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
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

    // Steady state: same two screenings reported again → no "(2 new)" churn.
    pass().map(_._3) shouldBe Seq(false, false)
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

  it should "merge two cinemas' slots into the same record when their titles share a docId" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    cache.recordCinemaScrape(Multikino, Seq(
      cinemaMovie("Top Gun: Maverick", Multikino, Some(2022), Some("multikino.jpg"))
    ))
    cache.recordCinemaScrape(Helios, Seq(
      // Punctuation-only variant — phase 2's docId rule collapses both.
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val key   = cache.keyOf("Wolność po włosku", Some(2025))

    // Tick 1: listing scrape carries no detail-page fields.
    cache.recordCinemaScrape(KinoMuza, Seq(
      cinemaMovie("Wolność po włosku", KinoMuza, Some(2025), poster = None)
    ))

    // The refresher visits the detail page and upgrades all three fields.
    cache.putIfPresent(key, current =>
      current.copy(data = current.data + ((KinoMuza: Source) ->
        current.data(KinoMuza).copy(
          synopsis   = Some("opis filmu"),
          trailerUrl = Some("https://youtube.com/watch?v=abc"),
          posterUrl  = Some("https://www.kinomuza.pl/2026/04/wolnosc-po-w-1-555x800.jpg")
        )
      ))
    )

    // Tick 2: listing scrape with the same None payload.
    cache.recordCinemaScrape(KinoMuza, Seq(
      cinemaMovie("Wolność po włosku", KinoMuza, Some(2025), poster = None)
    ))

    val slot = cache.get(key).get.data(KinoMuza)
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
      val ec    = tools.DaemonExecutors.virtualThreadEC("movie-cache-race")
      val t1 = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(Multikino, Seq(cinemaMovie(title, Multikino, None)))
      }(using ec)
      val t2 = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(Helios, Seq(cinemaMovie(title, Helios, Some(2026))))
      }(using ec)
      latch.countDown()
      scala.concurrent.Await.ready(scala.concurrent.Future.sequence(Seq(t1, t2))(using implicitly, ec), scala.concurrent.duration.Duration.Inf)
      ec.shutdown()

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
      val ec    = tools.DaemonExecutors.virtualThreadEC("rekey-race")

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
      }(using ec)
      // Thread B: a concurrent cinema scrape with year=2026. Contends for
      // the same lock — must either see the old (None) row (before the
      // re-key) and redirect onto it, or see the new (Some(2000)) row
      // (after the re-key) and redirect onto that. Either way: one row.
      val ccScrape = scala.concurrent.Future {
        latch.await()
        cache.recordCinemaScrape(CinemaCityPoznanPlaza, Seq(
          cinemaMovie(title, CinemaCityPoznanPlaza, year = Some(2026))
        ))
      }(using ec)

      latch.countDown()
      scala.concurrent.Await.ready(
        scala.concurrent.Future.sequence(Seq(tmdbStage, ccScrape))(using implicitly, ec),
        scala.concurrent.duration.Duration.Inf
      )
      ec.shutdown()

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
      val ec    = tools.DaemonExecutors.virtualThreadEC("prune-rekey-race")

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
      }(using ec)
      // Thread B: rekey (None) → (Some(2026)). The updater reads the
      // row's CURRENT state under the title lock — mirrors
      // `MovieService.runTmdbStageSync`'s carry-forward shape — so CC
      // Plaza's just-written slot is visible if it landed first.
      val rekey = scala.concurrent.Future {
        latch.await()
        cache.rekey(cache.keyOf(title, None), cache.keyOf(title, Some(2026)),
          current => current.copy(tmdbId = Some(454639))
        )
      }(using ec)

      latch.countDown()
      scala.concurrent.Await.ready(
        scala.concurrent.Future.sequence(Seq(ccScrape, rekey))(using implicitly, ec),
        scala.concurrent.duration.Duration.Inf
      )
      ec.shutdown()

      // The CC Plaza slot must end up somewhere in the cache —
      // whichever key the row eventually settles on after the rekey
      // and scrape interleave. Without slot-identity prune, the slot
      // is dropped in races where the rekey lands between CC Plaza's
      // write and its prune.
      val ccPlazaSlots = cache.snapshot().flatMap(_.record.data.get(CinemaCityPoznanPlaza))
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
}
