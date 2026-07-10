package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, StoredMovieRecord, TitleNormalizer}

import java.time.LocalDateTime

/**
 * The projector's minimal-write diff: the whole point of the read-model split
 * is that a showtime-only edit moves one screening document and a metadata-only edit
 * moves one movie document. Drives `onMovieUpsert` / `reconcile` directly against the
 * in-memory repos and asserts exactly which documents were written.
 */
class ReadModelProjectorSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private val fid = s"${TitleNormalizer.sanitize("Foo")}|2024"

  private def slot(showtimes: Seq[Showtime]) =
    SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"), showtimes = showtimes)

  // tmdbId set → `readyToProject` (TMDB concluded); the projector only
  // publishes rows whose enrichment has settled.
  private def record(rating: Option[Double], showtimes: Seq[Showtime]): MovieRecord =
    MovieRecord(imdbRating = rating, tmdbId = Some(1), data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  private def stored(record: MovieRecord): StoredMovieRecord = StoredMovieRecord("Foo", Some(2024), record)

  private def fixture(): (ReadModelProjector, InMemoryMovieRepository, InMemoryReadModelRepository) = {
    val repository = new InMemoryMovieRepository()
    val rm   = new InMemoryReadModelRepository()
    (new ReadModelProjector(repository, rm, rm), repository, rm)
  }

  /** Spy sink: tallies the reprojection writes + film prunes + sweep results. */
  private class RecordingMetrics extends ReadModelProjectionMetrics {
    val writes = scala.collection.mutable.Map.empty[(String, String), Int].withDefaultValue(0)
    var prunes = 0
    val sweeps = scala.collection.mutable.Buffer.empty[(String, Boolean)]
    val projectDurations = scala.collection.mutable.Buffer.empty[Double]
    var metadataReused = 0
    var metadataRecomputed = 0
    def projectCalls: Int = projectDurations.size
    def recordWrite(target: String, op: String, count: Int): Unit = writes((target, op)) += count
    def recordFilmPruned(count: Int): Unit                        = prunes += count
    def recordProject(seconds: Double): Unit                      = projectDurations += seconds
    def recordMetadataProjection(reused: Boolean): Unit          = if (reused) metadataReused += 1 else metadataRecomputed += 1
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = sweeps += (kind -> didWork)
  }

  /** Fake scheduler that CAPTURES the fixed-rate tasks `start()` submits instead of
   *  running them on a timer, so a test can assert exactly what was scheduled and run
   *  the tasks deterministically. */
  private class CapturingScheduler extends java.util.concurrent.ScheduledThreadPoolExecutor(1) {
    val scheduled = scala.collection.mutable.Buffer.empty[Runnable]
    override def scheduleAtFixedRate(command: Runnable, initialDelay: Long, period: Long,
                                     unit: java.util.concurrent.TimeUnit): java.util.concurrent.ScheduledFuture[?] = {
      scheduled += command
      null
    }
    def runAll(): Unit = scheduled.foreach(_.run())
  }

  "the first projection of a row" should "write the movie document before its screenings" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))

    rm.movieUpserts should have size 1
    rm.screeningUpserts should have size 1
    rm.writeOrder.head        should startWith("movie:")
    rm.writeOrder(1)          should startWith("screening:")
    rm.screeningUpserts.head._id shouldBe s"$fid|poznan|Multikino Stary Browar"
  }

  // ── Enrichment gate ─────────────────────────────────────────────────────────
  // A row whose TMDB enrichment hasn't concluded (no tmdbId, no tmdbNoMatch) is
  // held back: publishing the pre-enrichment row is exactly what leaks the
  // duplicate `foo|` + `foo|2025` cards.
  private def unresolved(showtimes: Seq[Showtime]): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  "an un-enriched row" should "be held back from the read model" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(StoredMovieRecord("Foo", Some(2024), unresolved(Seq(at("2026-06-12T20:00")))))
    rm.movieUpserts     shouldBe empty
    rm.screeningUpserts shouldBe empty
  }

  "a row that concludes enrichment on a later upsert" should "then be projected" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(StoredMovieRecord("Foo", Some(2024), unresolved(shows)))
    rm.movieUpserts shouldBe empty  // still enriching
    // TMDB concludes as a definitive no-match → `tmdbNoMatch` → ready → projects.
    projector.onMovieUpsert(StoredMovieRecord("Foo", Some(2024), unresolved(shows).copy(tmdbNoMatch = true)))
    rm.movieUpserts     should have size 1
    rm.screeningUpserts should have size 1
  }

  "reconcile" should "skip held-back rows while still projecting ready ones" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))   // ready (tmdbId)
    repository.upsert("Bar", Some(2024), unresolved(Seq(at("2026-06-12T20:00"))).copy(
      data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Bar"), releaseYear = Some(2024), showtimes = Seq(at("2026-06-12T20:00"))))))
    projector.reconcile()
    rm.findAllMovies().map(_._id) should contain only fid  // Bar is held back
  }

  // Memory flatten: the 30-min reconcile holds the read model resident already, so
  // the prune must diff it off id-only projections (`findAllMovieIds` /
  // `findAllScreeningRefs`) — NOT a second full `findAllMovies` / `findAllScreenings`
  // decode of the whole corpus. A second full read on top of the resident copy is the
  // transient that exhausted the worker's 320m heap on the reconcile tick.
  "reconcile" should "prune off id-only projections, never a full findAllMovies/findAllScreenings" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // projects Foo
    repository.delete("Foo", Some(2024))         // source gone → next reconcile prunes
    projector.reconcile()
    rm.movieDeletes should contain(fid)          // the prune actually ran
    rm.findAllMoviesCalls.get()     shouldBe 0   // …off id-only projections, not full decodes
    rm.findAllScreeningsCalls.get() shouldBe 0
  }

  // Prune-safety: `foreachRecord` returns `false` when a Mongo batch read fails
  // mid-scan (a server-selection / socket timeout while the worker is CPU-throttled —
  // the 2026-06-29 served-films flap). `liveIds` is then a TRUNCATED view, so pruning
  // on it would delete the live cards the scan never reached. The reconcile must keep
  // them and skip the prune until a clean tick.
  private class IncompleteScanRepository(seed: Seq[(String, Option[Int], MovieRecord)])
    extends InMemoryMovieRepository(seed) {
    @volatile var failScan = false
    // Mirrors the Mongo impl on a mid-scan read failure: deliver nothing further and
    // report the scan INCOMPLETE (rows before the failure would still have reached `f`;
    // here the very first batch dies, so none do).
    override def foreachRecord(f: StoredMovieRecord => Unit): Boolean =
      if (failScan) false else super.foreachRecord(f)
  }

  "reconcile" should "NOT prune live read-model rows when the source scan failed mid-way (incomplete)" in {
    val repository = new IncompleteScanRepository(Seq(("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))))
    val rm         = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, rm, rm)

    projector.reconcile()                                  // clean scan → Foo is projected
    rm.findAllMovies().map(_._id) should contain(fid)
    val deletesBefore = rm.movieDeletes.size

    repository.failScan = true                             // next scan dies mid-way (incomplete)
    projector.reconcile()

    withClue("a live film was pruned from the read model on an INCOMPLETE source scan — the served-films flap: ") {
      rm.movieDeletes.size          shouldBe deletesBefore // no prune happened
      rm.findAllMovies().map(_._id) should contain(fid)    // Foo still served
    }
  }

  "a showtime-only change" should "move only the one screening document" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))

    rm.movieUpserts     should have size 1  // metadata unchanged → not rewritten
    rm.screeningUpserts should have size 2  // the one changed screening document, again
  }

  // Hash-diff: the resident diff-state keeps only a 32-bit content HASH of each projected
  // document, not the full document (a heap-footprint cut — the read model already lives in
  // Mongo). Re-projecting byte-identical content must still hash-match and skip the write;
  // the change cases below (rating / showtime) guard that a differing field hash-differs and
  // writes. Together they lock the store-hash / compare-hash path.
  "re-projecting identical content" should "write nothing (hash-diff skip)" in {
    val (projector, _, rm) = fixture()
    val rec = record(Some(8.0), Seq(at("2026-06-12T20:00")))
    projector.onMovieUpsert(stored(rec))
    projector.onMovieUpsert(stored(rec))   // identical → hash matches → no rewrite
    rm.movieUpserts     should have size 1
    rm.screeningUpserts should have size 1
  }

  "a rating-only change" should "move only the movie document" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))

    rm.movieUpserts     should have size 2  // rating changed → movie document rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  // ── Optimisation #1: metadata reuse across showtime-only changes ─────────────
  // The projected metadata (ResolvedMovie: title/synopsis/ratings/cast/…) is a pure
  // function of the row's cinema STRUCTURE, not its SHOWTIMES. A showtime-only change at
  // an already-present cinema must REUSE the cached ResolvedMovie (skip resolve/synopsis/
  // ratings) and recompute only the cheap screenings half; a genuine metadata change
  // (rating / synopsis / a NEW cinema→city) must RECOMPUTE and rewrite the movie document.
  "a showtime-only change at an existing cinema" should "REUSE cached metadata, not recompute it" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)

    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.metadataRecomputed shouldBe 1   // first projection → recomputed + cached
    m.metadataReused     shouldBe 0

    // Add a showtime at the SAME cinema — metadata inputs unchanged.
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))
    withClue("metadata should have been reused (resolve/synopsisByCity/ratings NOT recomputed): ") {
      m.metadataReused     shouldBe 1
      m.metadataRecomputed shouldBe 1  // still just the first
    }
    rm.movieUpserts     should have size 1  // movie document NOT rewritten
    rm.screeningUpserts should have size 2  // the one changed screening document, re-written
  }

  "a rating change" should "RECOMPUTE metadata and rewrite the movie document" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    val shows = Seq(at("2026-06-12T20:00"))

    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))  // rating changed, showtimes identical
    withClue("a rating change is a metadata change → must recompute, never reuse: ") {
      m.metadataRecomputed shouldBe 2
      m.metadataReused     shouldBe 0
    }
    rm.movieUpserts     should have size 2  // movie document rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  "a synopsis change at a cinema" should "RECOMPUTE metadata (a showtimes-stripped hash still sees it)" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    val shows = Seq(at("2026-06-12T20:00"))
    def withSynopsis(s: String) = MovieRecord(tmdbId = Some(1),
      data = Map[Source, SourceData](Multikino -> slot(shows).copy(synopsis = Some(s))))

    projector.onMovieUpsert(stored(withSynopsis("A short blurb.")))
    projector.onMovieUpsert(stored(withSynopsis("A different, longer blurb entirely.")))
    m.metadataRecomputed shouldBe 2
    m.metadataReused     shouldBe 0
    rm.movieUpserts should have size 2  // synopsis is metadata → movie rewritten
  }

  "a film gaining a cinema in a NEW city" should "RECOMPUTE metadata (new city → new synopsisByCity)" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    val shows = Seq(at("2026-06-12T20:00"))
    def wroclawSlot = SourceData(title = Some("Foo"), releaseYear = Some(2024),
      filmUrl = Some("https://mk/foo-wro"), showtimes = shows)

    projector.onMovieUpsert(stored(record(Some(8.0), shows)))  // Poznań only
    m.metadataRecomputed shouldBe 1
    // The film now also screens in Wrocław (MultikinoPasazGrunwaldzki) — the city set grows,
    // so the metadata (cities / synopsisByCity) genuinely changes and must NOT be reused.
    val twoCities = record(Some(8.0), shows).copy(
      data = Map[Source, SourceData](Multikino -> slot(shows), MultikinoPasazGrunwaldzki -> wroclawSlot))
    projector.onMovieUpsert(stored(twoCities))
    withClue("a new city is a metadata change → must recompute, never reuse: ") {
      m.metadataReused     shouldBe 0
      m.metadataRecomputed shouldBe 2
    }
  }

  "a film leaving a cinema" should "delete that cinema's screening document" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq.empty)))  // cinema slot now has no showtimes

    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // Boot-cost guard: the full reconcile is a `findAll()` + project-every-row scan.
  // Running it synchronously at `start()` stacked a second full scan onto the cache
  // hydrate + first scrape on a cold JVM (the boot CPU-credit drain). `start()` now
  // seeds state + installs the watch but defers the reconcile to the first scheduled
  // tick — so no source row is projected synchronously. (Before, the boot reconcile
  // projected "Foo" at start(), making movieUpserts size 1 and failing this.)
  "start" should "not reconcile synchronously (defer the full scan off the boot path)" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()
    rm.movieUpserts     shouldBe empty
    rm.screeningUpserts shouldBe empty
    projector.stop()
  }

  "reconcile" should "prune derived documents whose source film vanished" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    rm.movieUpserts should have size 1

    repository.delete("Foo", Some(2024))
    projector.reconcile()
    rm.movieDeletes     should contain(fid)
    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // The change stream drops deletes, so a film re-keyed by `settle` (old `_id`
  // deleted, new one written) leaves the old read-model documents behind. They're
  // pruned only by `reconcile`. The trap: a worker that *wrote* the stale document
  // restarts before pruning it — its successor's in-memory `lastMovie` never
  // knew the document, so a `lastMovie`-based prune can't see it and the duplicate
  // card persists forever. `reconcile` must therefore diff the *actual read
  // model* against the live source, not this process's memory.
  // ── Reprojection / re-key metrics ───────────────────────────────────────────
  // The worker exposes these as kinowo_worker_readmodel_writes_total{target,op}
  // and kinowo_worker_readmodel_films_pruned_total so the rate of reprojection
  // churn — and the link-breaking film-prune events — is visible in Grafana.
  import ReadModelProjectionMetrics.{Op, ReconcileKind, Target}

  // ── Split sweep: cheap prune vs expensive full re-projection ─────────────────
  // The full re-projection (project EVERY row) was the ~1-core corpus burst that filled
  // the heap → GC thrash → credit starvation; it has been retired (the change stream
  // covers missed upserts). The remaining scheduled backstop is a cheap id-only prune
  // that removes deleted/re-keyed rows WITHOUT re-projecting anything.
  "pruneOrphans" should "prune a vanished film WITHOUT re-projecting live rows" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                     // full sweep: projects Foo
    rm.movieUpserts should have size 1
    // A live row's metadata changes: the CHEAP prune must NOT re-project it — that's
    // the whole point (no per-row projection = no CPU burst).
    repository.upsert("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    projector.pruneOrphans()
    rm.movieUpserts should have size 1        // unchanged — prune did not reproject
    // …but a vanished film IS still pruned by the cheap sweep (the load-bearing job).
    repository.delete("Foo", Some(2024))
    projector.pruneOrphans()
    rm.movieDeletes should contain(fid)
  }

  // Only the PRUNE sweep is metered now — the reproject's did_work gate was retired, so
  // reconcile() (the seed/backfill path) records nothing; every sweep row is kind=prune.
  "the reconcile-sweep metric" should "meter only the prune sweep, never the reproject seed" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                                    // seed Foo — NOT metered
    m.sweeps shouldBe empty
    projector.pruneOrphans()                                 // nothing to prune (Foo live) → no-op
    m.sweeps.last shouldBe (ReconcileKind.Prune -> false)
    repository.delete("Foo", Some(2024))
    projector.pruneOrphans()                                 // prunes Foo → did work
    m.sweeps.last shouldBe (ReconcileKind.Prune -> true)
  }

  "the projector" should "meter the movie + screening upserts of a first projection" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    new ReadModelProjector(repository, rm, rm, m)
      .onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.writes((Target.Movie, Op.Upsert))     shouldBe 1
    m.writes((Target.Screening, Op.Upsert)) shouldBe 1
    m.prunes                                shouldBe 0
  }

  "the project-duration metric" should "record one timed projectAll per ready row projected" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)

    // A ready row projected via the change-stream path → one timing recorded.
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    m.projectCalls shouldBe 1
    m.projectDurations.head should be >= 0.0

    // A row still enriching (no tmdbId → !readyToProject) is held back before
    // projectAll runs, so it must NOT be metered — the counter stays put.
    val notReady = MovieRecord(imdbRating = Some(7.0), data = Map[Source, SourceData](Multikino -> slot(Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(StoredMovieRecord("Foo", Some(2024), notReady))
    m.projectCalls shouldBe 1

    // A full reproject sweep projects the live row → one more timing.
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    m.projectCalls shouldBe 2
  }

  "a re-key that prunes the old film in reconcile" should "meter a film prune + its document deletes" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // projects the film
    repository.delete("Foo", Some(2024))         // source row gone (re-keyed/removed)
    projector.reconcile()                       // prunes its derived documents
    m.prunes                            shouldBe 1
    m.writes((Target.Movie, Op.Delete)) shouldBe 1
  }

  "reconcile after a restart" should "prune a stale film a prior process left in the read model" in {
    val repository = new InMemoryMovieRepository()
    val rm   = new InMemoryReadModelRepository()
    def yearKey(y: Int) = s"${TitleNormalizer.sanitize("Foo")}|$y"
    // A film whose reported year was 2025 when an earlier projector ran.
    def recordYear(y: Int) =
      MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Foo"), releaseYear = Some(y),
          filmUrl = Some("https://mk/foo"), showtimes = Seq(at("2026-06-12T20:00")))))

    val p1 = new ReadModelProjector(repository, rm, rm)
    repository.upsert("Foo", Some(2025), recordYear(2025))
    p1.reconcile()
    rm.findAllMovies().map(_._id) should contain(yearKey(2025))
    p1.stop()  // the worker dies, taking its in-memory state with it

    // `settle` re-keys the source row onto the (now resolved) year — old gone,
    // new live.
    repository.delete("Foo", Some(2025))
    repository.upsert("Foo", Some(2026), recordYear(2026))

    // A fresh projector boots with an empty `lastMovie` and reconciles.
    val p2 = new ReadModelProjector(repository, rm, rm)
    rm.movieDeletes.clear(); rm.screeningDeletes.clear()
    p2.reconcile()

    rm.findAllMovies().map(_._id)                  should contain only yearKey(2026)
    rm.findAllScreenings().map(_.filmId).distinct  should contain only yearKey(2026)
    rm.movieDeletes should contain(yearKey(2025))
    p2.stop()
  }

  // The periodic full reproject was retired: the resume-token change stream now catches
  // the upserts it used to, and its ~1-core whole-corpus burst was the CPU-credit drain.
  // `start()` must schedule ONLY the cheap orphan prune — never the reproject. Captured
  // via a fake scheduler: a live source row absent from the read model would be PROJECTED
  // by a scheduled reproject (movieUpserts size 1), but the prune re-projects nothing, so
  // running every scheduled task leaves the read model untouched.
  "start" should "schedule the orphan prune but NOT a periodic reproject" in {
    val fakeScheduler = new CapturingScheduler
    val repository = new InMemoryMovieRepository()
    val rm = new InMemoryReadModelRepository()
    val projector = new ReadModelProjector(repository, rm, rm, scheduler = fakeScheduler)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()

    fakeScheduler.scheduled should have size 1   // only the prune, never the reproject
    fakeScheduler.runAll()                        // a scheduled reproject WOULD project Foo here
    rm.movieUpserts     shouldBe empty            // prune re-projects nothing
    rm.screeningUpserts shouldBe empty
    projector.stop()
  }
}
