package services.movies

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MovieCacheSpec extends AnyFlatSpec with Matchers {

  private def mkEnrichment(imdbId: String, rating: Option[Double] = None): MovieRecord =
    MovieRecord(imdbId = Some(imdbId), imdbRating = rating)

  "MovieCache" should "hydrate from the repository on construction" in {
    // Carry the title in a cinema slot, as a scraped row does: the repository
    // re-derives the display title from the record on read (like Mongo), so a
    // title-less record would surface its sanitized _id prefix, not "Drzewo Magii".
    val record   = mkEnrichment("tt1").copy(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Drzewo Magii"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Drzewo Magii", Some(2024), record)), normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)

    cache.get(cache.keyOf("Drzewo Magii", Some(2024))) shouldBe Some(record)
    cache.snapshot().map(r => (r.title, r.year)) shouldBe Seq(("Drzewo Magii", Some(2024)))
  }

  // Change-stream DELETE handling: a removed source row (an
  // UnscreenedCleanup removal, a retired projection) is dropped from the cache
  // INCREMENTALLY via `applyDelete` — the stream carries only the `_id`, mapped back
  // to its CacheKey by `idFor` — so the periodic backstop rehydrate is no longer the
  // ONLY thing that catches deletes. Pre-fix the cache ignored delete events entirely.
  it should "drop a cached row when its source _id is deleted on the change stream" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key   = cache.keyOf("Foo", Some(2024))
    cache.put(key, mkEnrichment("tt-foo"))
    cache.get(key) should not be empty

    cache.applyDelete(cache.idOf(cache.keyOf("Foo", Some(2024))).getOrElse(fail("row has no id")))
    cache.get(key) shouldBe None

    // A delete for an unknown id is a harmless no-op — it must not clear other rows.
    val barKey = cache.keyOf("Bar", Some(2024))
    cache.put(barKey, mkEnrichment("tt-bar"))
    cache.applyDelete(FilmId("does-not-exist|1900"))
    cache.get(barKey) should not be empty
  }

  // Redundancy signal for retiring the backstop rehydrate (step 3a): the rehydrate
  // reports how much its full reload caught that the incremental change stream missed —
  // a put whose value DIFFERED (missed upsert) and a key gone from Mongo (missed delete).
  // Once resume-tokens + delete-apply are working this should be ~0 in steady state.
  it should "meter what the backstop rehydrate catches that the change stream missed" in {
    val repo  = new InMemoryMovieRepository(Seq(("Foo", Some(2024), mkEnrichment("tt-foo"))), normalizer = titleNormalizer)
    val m     = new RecordingCacheMetrics
    val cache = new CaffeineMovieCache(repo, cacheMetrics = m, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned) // boot hydrate counts Foo as changed
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
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
  "rehydrate" should "keep two documents under their own stored keys, labelled by the display title" in {
    // Two movies docs for one film: "zaplatani|2010" plus a stale "tangled|2010" first
    // stored under the English title but now displaying (via its TMDB Polish title) as
    // "Zaplątani". The stored KEY is the row's lookup identity — not the display title,
    // which is a vote over the slots — so the hydrate keys them apart, exactly as the
    // store does.
    val zaplSlots = Map[Source, SourceData](
      Tmdb      -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled"), releaseYear = Some(2010)),
      Multikino -> SourceData(title = Some("Zaplątani"), releaseYear = Some(2010)))
    val repository = new InMemoryMovieRepository(Seq(
      ("Tangled",   Some(2010), MovieRecord(tmdbId = Some(38757), data = zaplSlots)), // → _id tangled|2010, displays "Zaplątani"
      ("Zaplątani", Some(2010), MovieRecord(tmdbId = Some(38757), data = zaplSlots))  // → _id zaplatani|2010
    ), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.entries.map(_._1.normalized).toSet shouldBe Set("tangled", "zaplatani")     // keyed by the stored keys
    cache.entries.map(_._1.cleanTitle).toSet shouldBe Set("Zaplątani")               // labelled by the display title
  }

  it should "drop in-memory rows that aren't in the repository" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)

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
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)  // start empty
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("Ghost", Some(2024)), mkEnrichment("tt1"))
    // Mongo "lies" — write straight to the cache, then drop from the repository
    // so the next rehydrate's findAll returns empty.
    repository.delete("Ghost", Some(2024))

    cache.rehydrate() shouldBe 0
    // Cache row survives; the user keeps seeing films through the next
    // (presumably successful) tick.
    cache.get(cache.keyOf("Ghost", Some(2024))) shouldBe defined
  }

  it should "not evict a film an INCOMPLETE corpus read left out" in {
    // A page of the corpus scan whose read failed is skipped, not handed on stripped
    // (`MongoMovieRepository.scanStitched`), so an incomplete read is short by whole films.
    // Missing is not deleted: evicting them would drop live films from the cache.
    final class HidingRepository extends InMemoryMovieRepository(normalizer = titleNormalizer) {
      var hiding = false
      override def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] =
        if (hiding) tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new java.io.IOException("unreadable"))) else tools.ReadOutcome.Answered(super.findAll())
      override def findAll(): Seq[StoredMovieRecord] =
        if (hiding) super.findAll().filterNot(_.title == "Ghost") else super.findAll()
    }
    val repository = new HidingRepository
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("Ghost", Some(2024)), mkEnrichment("tt1"))
    cache.put(cache.keyOf("Keeper", Some(2024)), mkEnrichment("tt2"))
    repository.hiding = true

    cache.rehydrate()

    cache.get(cache.keyOf("Ghost", Some(2024))) shouldBe defined
    cache.get(cache.keyOf("Keeper", Some(2024))) shouldBe defined
  }

  it should "make repository-side edits visible" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1", rating = Some(7.0)))

    // Edit Mongo out-of-band: replace the rating.
    repository.upsert("X", Some(2024), mkEnrichment("tt1", rating = Some(9.5)))

    cache.rehydrate() shouldBe 1
    cache.get(cache.keyOf("X", Some(2024))).flatMap(_.imdbRating) shouldBe Some(9.5)
  }

  // ── incremental change-stream sync (start()) ───────────────────────────────
  // Once started, the cache applies out-of-band Mongo writes the moment they
  // land via `repository.watchUpserts` — no full `findAll()` rehydrate. The
  // InMemoryMovieRepository emulates Mongo's change stream by notifying the watcher
  // on every write.
  "a started MovieCache" should "apply an out-of-band upsert via the change-stream watch, without a rehydrate" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key   = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    cache.start()   // establishes the watch (the backstop interval won't fire in-test)
    try {
      // Another process edits Mongo directly. Crucially: no rehydrate() call.
      repository.upsert(key.cleanTitle, key.year, mkEnrichment("tt1", rating = Some(9.5)))
      cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.5)
    } finally cache.stop()
  }

  /** An in-memory store whose change stream the spec drives by hand, the way the real one
   *  delivers: a re-read of the film, with the fence mark taken before that read. */
  private final class HandDeliveredRepository extends InMemoryMovieRepository(normalizer = titleNormalizer) {
    @volatile var deliver: (StoredMovieRecord, Long) => Unit = (_, _) => ()
    override def watchChangesFenced(onUpsert: (StoredMovieRecord, Long) => Unit, onDelete: FilmId => Unit): Option[AutoCloseable] = {
      deliver = onUpsert
      Some(() => deliver = (_, _) => ())
    }
    /** The stream's re-read: the stored film and the mark it was read under. */
    def reread(id: FilmId): (StoredMovieRecord, Long) = {
      val mark = writeFence.mark(id.value)
      (findById(id).get, mark)
    }
  }

  // The 2026-09-25 `RetryResolveServingIntegrationSpec` flake: the stream re-read a film, the
  // cache then wrote it, and the older read — delivered after — rolled the write back until
  // the write's own event re-read the film. Each write path the cache has: patch, slot, put.
  Seq[(String, (CaffeineMovieCache, CacheKey) => Unit)](
    "putIfPresent" -> ((cache, key) => cache.putIfPresent(key, _.copy(imdbRating = Some(9.5)))),
    "put"          -> ((cache, key) => cache.put(key, cache.get(key).get.copy(imdbRating = Some(9.5)))),
  ).foreach { case (path, write) =>
    it should s"not let a change-stream read taken before its own $path roll that write back" in {
      val repository = new HandDeliveredRepository
      val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
      val key        = cache.keyOf("Erupcja", Some(2024))
      cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
      cache.start()
      try {
        val (staleRead, mark) = repository.reread(cache.idOf(key).get)
        write(cache, key)
        repository.deliver(staleRead, mark)
        cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.5)
      } finally cache.stop()
    }
  }

  it should "not let a change-stream read taken before its own slot landing roll that landing back" in {
    val repository = new HandDeliveredRepository
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key        = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1").copy(data = Map(Multikino -> SourceData(title = Some("Erupcja")))))
    cache.start()
    try {
      val (staleRead, mark) = repository.reread(cache.idOf(key).get)
      cache.putSlotIfPresent(key, Helios, SourceData(title = Some("Erupcja")))
      repository.deliver(staleRead, mark)
      cache.get(key).map(_.data.keySet) shouldBe Some(Set(Multikino, Helios))
    } finally cache.stop()
  }

  it should "still apply an out-of-band change-stream read taken after its own write" in {
    val repository = new HandDeliveredRepository
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key        = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    cache.start()
    try {
      cache.putIfPresent(key, _.copy(imdbRating = Some(8.0)))
      repository.upsert(key.cleanTitle, key.year, mkEnrichment("tt1", rating = Some(9.5))) // another writer
      val (freshRead, mark) = repository.reread(cache.idOf(key).get)
      repository.deliver(freshRead, mark)
      cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.5)
    } finally cache.stop()
  }

  /** An in-memory store whose `findAll` runs `afterRead` once the rows are read and before
   *  the caller gets them — the window in which the backstop rehydrate's snapshot goes stale. */
  private final class RacedSnapshotRepository extends InMemoryMovieRepository(normalizer = titleNormalizer) {
    @volatile var afterRead: () => Unit = () => ()
    override def findAll(): Seq[StoredMovieRecord] = {
      val rows = super.findAll()
      val hook = afterRead; afterRead = () => (); hook()
      rows
    }
  }

  // The same rollback as the change stream's, through the backstop: `rehydrate` stored every
  // row of a `findAll` snapshot, so a local write that landed after the snapshot was read
  // was undone — and a row created after it was evicted as "gone from Mongo".
  "a MovieCache rehydrate" should "not roll back a local write that landed after its snapshot was read" in {
    val repository = new RacedSnapshotRepository
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key        = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    repository.afterRead = () => { cache.putIfPresent(key, _.copy(imdbRating = Some(9.5))); () }
    cache.rehydrate()
    cache.get(key).flatMap(_.imdbRating) shouldBe Some(9.5)
  }

  it should "not evict a row created after its snapshot was read" in {
    val repository = new RacedSnapshotRepository
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val old        = cache.keyOf("Erupcja", Some(2024))
    val newcomer   = cache.keyOf("Kumotry", Some(2025))
    cache.put(old, mkEnrichment("tt1"))
    repository.afterRead = () => { cache.put(newcomer, mkEnrichment("tt2")); () }
    cache.rehydrate()
    cache.get(newcomer) shouldBe defined
  }

  it should "still apply a snapshot row no local write overtook, and evict a row Mongo no longer holds" in {
    val repository = new RacedSnapshotRepository
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val kept       = cache.keyOf("Erupcja", Some(2024))
    val gone       = cache.keyOf("Kumotry", Some(2025))
    cache.put(kept, mkEnrichment("tt1", rating = Some(7.0)))
    cache.put(gone, mkEnrichment("tt2"))
    repository.upsert(kept.cleanTitle, kept.year, mkEnrichment("tt1", rating = Some(9.5))) // another writer
    repository.delete(gone.cleanTitle, gone.year)
    cache.rehydrate()
    cache.get(kept).flatMap(_.imdbRating) shouldBe Some(9.5)
    cache.get(gone) shouldBe None
  }

  it should "stop applying changes once the watch is closed by stop()" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key   = cache.keyOf("Erupcja", Some(2024))
    cache.put(key, mkEnrichment("tt1", rating = Some(7.0)))
    cache.start()
    cache.stop()    // closes the watch handle
    repository.upsert(key.cleanTitle, key.year, mkEnrichment("tt1", rating = Some(9.5)))
    cache.get(key).flatMap(_.imdbRating) shouldBe Some(7.0)  // change not applied — watch is closed
  }

  "InMemoryMovieRepository.watchUpserts" should "notify the watcher on every write until the handle is closed" in {
    val repository   = new InMemoryMovieRepository(normalizer = titleNormalizer)
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
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val a, b = scala.collection.mutable.ListBuffer.empty[String]
    repository.watchUpserts(r => a += r.title)
    repository.watchUpserts(r => b += r.title)  // must NOT clobber the first watcher
    repository.upsert("A", Some(2024), mkEnrichment("tt-a"))
    a.toList shouldBe List("A")
    b.toList shouldBe List("A")
  }

  "InMemoryMovieRepository.updateIfPresent" should "skip the write and the change notification when nothing changed (empty patch)" in {
    val repository = new InMemoryMovieRepository(Seq(("A", Some(2024), mkEnrichment("tt-a"))), normalizer = titleNormalizer)
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
    // drift from CI.
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    repository.upsert("Child of Dust", Some(2025),
      MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Dziecko z pyłu")))))
    repository.findAll().map(r => (r.title, r.year)) shouldBe Seq(("Dziecko z pyłu", Some(2025)))
  }

  it should "treat case + diacritics + whitespace differences as the same key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.keyOf("Top Gun / 40th Anniversary", Some(2025)) should not be cache.keyOf("Top Gun",  Some(2025))
    cache.keyOf("Avatar - wersja polska",     Some(2025)) should not be cache.keyOf("Avatar",   Some(2025))
  }

  // ...but the GLOBAL canonical folds stay IN the key: Arabic↔Roman numerals
  // and " & "↔" i " still collapse, so the same film spelt differently across
  // cinemas keeps one identity. (`sanitize` applies `normalize` + `canonical`.)
  it should "still collapse global canonical folds (Roman numerals, & → i) into one key" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.keyOf("Mortal Kombat 2", Some(2026)) shouldBe cache.keyOf("Mortal Kombat II", Some(2026))
    cache.keyOf("Pizza & Pasta",   Some(2026)) shouldBe cache.keyOf("Pizza i Pasta",    Some(2026))
  }

  "put" should "write through to the repository (cache + Mongo stay in lockstep)" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))

    repository.upserts.toList shouldBe List(("X", Some(2024), mkEnrichment("tt1")))
  }

  "put" should "squash zero ratings to None on the way into the cache" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key   = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    repository.upserts.clear()

    cache.invalidate(key)

    cache.get(key) shouldBe None
    repository.deletes.toList shouldBe List(("X", Some(2024)))
  }

  // ── putIfPresent: no-resurrection writes ───────────────────────────────────

  "putIfPresent" should "update an existing row and return true" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("Existing", Some(2024)), mkEnrichment("tt1"))

    val landed = cache.putIfPresent(cache.keyOf("Existing", Some(2024)), _.copy(imdbRating = Some(8.5)))

    landed shouldBe true
    cache.get(cache.keyOf("Existing", Some(2024))).flatMap(_.imdbRating) shouldBe Some(8.5)
  }

  it should "be a no-op and return false when the row was deleted" in {
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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
    val repository  = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
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

  "snapshot" should "return rows sorted by title (case-insensitive)" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    cache.put(cache.keyOf("Zorro", None),   mkEnrichment("tt3"))
    cache.put(cache.keyOf("alpha", None),   mkEnrichment("tt1"))
    cache.put(cache.keyOf("Beta", None),    mkEnrichment("tt2"))

    cache.snapshot().map(_.title) shouldBe Seq("alpha", "Beta", "Zorro")
  }

  "lastModified" should "be stamped from the injected clock, not the wall clock" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val before = cache.lastModified
    before.truncatedTo(java.time.temporal.ChronoUnit.SECONDS) shouldBe _root_.tools.SpecClock.Pinned.instant()
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))
    cache.lastModified shouldBe before.plusNanos(1)  // the clock did not move: the stamp still did
  }

  it should "advance on put" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val before = cache.lastModified
    cache.put(cache.keyOf("X", Some(2024)), mkEnrichment("tt1"))
    cache.lastModified.isAfter(before) shouldBe true
  }

  it should "advance on putIfPresent" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val key = cache.keyOf("X", Some(2024))
    cache.put(key, mkEnrichment("tt1"))
    val before = cache.lastModified
    cache.putIfPresent(key, _.copy(imdbRating = Some(9.0)))
    cache.lastModified.isAfter(before) shouldBe true
  }

  it should "advance on rehydrate" in {
    val repository = new InMemoryMovieRepository(Seq(("Film", Some(2024), mkEnrichment("tt1"))), normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val before = cache.lastModified
    cache.rehydrate()
    cache.lastModified.isAfter(before) shouldBe true
  }

  it should "share one String instance for equal slot text read back from the store" in {
    def fresh(s: String) = new String(s.toCharArray)       // equal, never the same object
    def slot(title: String) = SourceData(title = Some(fresh(title)), cast = Seq(fresh("Emma Thompson")),
      countries = Seq(fresh("GB")), genres = Seq(fresh("Drama")), director = Seq(fresh("Ang Lee")))
    val repo = new InMemoryMovieRepository(Seq(
      ("Sense and Sensibility", Some(1995), MovieRecord(data = Map[Source, SourceData](Helios -> slot("Sense and Sensibility")))),
      ("Rozważna i romantyczna", Some(1995), MovieRecord(data = Map[Source, SourceData](Helios -> slot("Rozważna i romantyczna"))))),
      normalizer = titleNormalizer)
    val cache = new CaffeineMovieCache(repo, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned)
    val slots = cache.snapshot().flatMap(_.record.data.values)
    slots should have size 2
    val casts = slots.flatMap(_.cast)
    withClue("cast names read back from the store: ")(casts.forall(_ eq casts.head) shouldBe true)
    val genres = slots.flatMap(_.genres)
    withClue("genres read back from the store: ")(genres.forall(_ eq genres.head) shouldBe true)
  }
}
