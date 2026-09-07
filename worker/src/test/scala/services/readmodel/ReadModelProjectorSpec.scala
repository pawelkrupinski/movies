package services.readmodel

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}

import java.time.LocalDateTime

/**
 * The projector's minimal-write diff: the whole point of the read-model split
 * is that a showtime-only edit moves one screening document and a metadata-only edit
 * moves one movie document. Drives `onMovieUpsert` / `reconcile` directly against the
 * in-memory repos and asserts exactly which documents were written.
 */
class ReadModelProjectorSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private val fid = s"${titleNormalizer.sanitize("Foo")}|2024"

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
    val projectCpuSeconds = scala.collection.mutable.Buffer.empty[Double]
    var metadataReused = 0
    var metadataRecomputed = 0
    def projectCalls: Int = projectDurations.size
    def recordWrite(target: String, op: String, count: Int): Unit = writes((target, op)) += count
    def recordFilmPruned(count: Int): Unit                        = prunes += count
    def recordProject(wallSeconds: Double, cpuSeconds: Double): Unit = {
      projectDurations  += wallSeconds
      projectCpuSeconds += cpuSeconds
    }
    def recordMetadataProjection(reused: Boolean): Unit          = if (reused) metadataReused += 1 else metadataRecomputed += 1
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = sweeps += (kind -> didWork)
  }

  /** CPU clock that advances by a FIXED amount per reading, so a test can assert the
   *  recorded CPU cost exactly instead of racing a real one. Wall-clock keeps running
   *  independently, which is the whole point: the two must not be the same number. */
  private class SteppingCpuClock(stepNanos: Long) extends tools.ThreadCpuClock {
    private var current = 0L
    def nanos(): Long = { val n = current; current += stepNanos; n }
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
    projector.onMovieUpsert(StoredMovieRecord("Foo", Some(2024), unresolved(shows).copy(tmdbAttempt = Some(services.resolution.TmdbAttempt.Legacy))))
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
  "start" should "project only the ready rows that have no card, never the corpus" in {
    // A carded row is left to the diffing change-stream path; a row with no card at
    // all — the 2026-09-07 id-scheme rollout, or a restored database — is projected
    // before the first prune can delete whatever it had under an old id. In steady
    // state nothing is missing and boot projects nothing (the boot CPU-credit drain
    // a whole-corpus reconcile used to be).
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                       // Foo carded
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00"))))
    val before = rm.movieUpserts.size
    projector.start()
    // Bar healed (the helper's slot title is "Foo" for every row, so compare ids — a
    // key-addressed upsert files a new row under its legacy id), Foo untouched.
    rm.movieUpserts.drop(before).map(_._id) shouldBe Seq("bar|2024")
    rm.screeningUpserts.drop(before).map(_.filmId).distinct shouldBe Seq("bar|2024")
    projector.stop()

    val (steady, repository2, rm2) = fixture()
    repository2.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    steady.reconcile()
    val carded = rm2.movieUpserts.size
    steady.start()
    rm2.movieUpserts should have size carded   // nothing missing → nothing projected at boot
    steady.stop()
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

  it should "attribute CPU from the thread CPU clock, not from wall-clock" in {
    // The panel that stacks projection against process CPU is only meaningful if this
    // number is CPU. Wall-clock is not: concurrent projections make it sum past one
    // core-second per second, and steal on a throttled box inflates it further — it
    // read 45.9cc against an 18.0cc process total on kinowo-worker-uk (2026-07-28).
    // Pin the clock to a known step so the recorded cost is exact, and assert it came
    // from THAT clock rather than from however long the projection happened to take.
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m,
      cpuClock = new SteppingCpuClock(stepNanos = 250000000L)) // 0.25s of CPU per reading

    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))

    m.projectCpuSeconds.head shouldBe 0.25
    // …and the wall-clock reading is still its own, genuinely-measured number. An
    // in-memory projection takes far less than the stubbed 0.25s of "CPU", so the two
    // series cannot be the same value — which is exactly the bug this guards.
    m.projectDurations.head should be >= 0.0
    m.projectDurations.head should be < 0.25
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
    def yearKey(y: Int) = s"${titleNormalizer.sanitize("Foo")}|$y"
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
  "start" should "project a split row one of whose variant cards is missing" in {
    // A row screened under two shown titles fans out into two cards. When only one
    // of them exists at boot — a restored database, or the id scheme moving under the
    // variant suffix — the row is healed whole: `filmIds` is asked per row, and a row
    // is healed when ANY of its ids has no card. The first version healed only rows
    // with NO card at all, and on 2026-09-07 that left every decorated listing (a
    // "35 lat po premierze …" banner, a "przedpremiera" screening) whose variant card
    // the prune had removed unserved for hours while the plain card survived:
    // Warszawa was 45 films short four hours after the rollout.
    val (projector, repository, rm) = fixture()
    val twoTitles = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Фу"), showtimes = Seq(at("2026-06-13T20:00")))))
    repository.upsert("Foo", Some(2024), twoTitles)
    val row = repository.findAll().head
    val ids = ReadModelProjection.filmIds(row, titleNormalizer)
    ids should have size 2

    projector.start()                                       // nothing carded → both cards written
    rm.movieUpserts.map(_._id).toSet shouldBe ids.toSet
    projector.stop()

    // One variant card gone: the row is projected whole, and the missing card comes back.
    val (again, _, _) = fixture()
    val partial = new InMemoryReadModelRepository()
    val healer  = new ReadModelProjector(repository, partial, partial)
    partial.upsertMovie(rm.movieUpserts.find(_._id == ids.head).get)
    healer.start()
    partial.findAllMovieIds().toSet shouldBe ids.toSet
    partial.findAllScreenings().map(_.filmId).toSet shouldBe ids.toSet
    healer.stop(); again.stop()
  }

  // The same gap inside the scheduled sweep: the prune that removed a variant card as
  // an orphan under its old id must put it back under its new one in the SAME pass,
  // however healthy the row's plain card is — the change stream only re-creates the
  // rows a scrape happens to rewrite.
  "the orphan prune" should "restore a split row's missing variant card and its screenings" in {
    val (projector, repository, rm) = fixture()
    val twoTitles = MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("35 lat po premierze: Foo"), showtimes = Seq(at("2026-06-13T20:00")))))
    repository.upsert("Foo", Some(2024), twoTitles)
    val row     = repository.findAll().head
    val ids     = ReadModelProjection.filmIds(row, titleNormalizer)
    val variant = ids.find(_.contains("~")).get
    projector.onMovieUpsert(row)
    rm.findAllMovieIds().toSet shouldBe ids.toSet
    rm.deleteMovie(variant)
    rm.findAllScreenings().filter(_.filmId == variant).foreach(s => rm.deleteScreening(s._id))
    rm.findAllMovieIds().toSet shouldBe (ids.toSet - variant)

    projector.pruneOrphans()

    rm.findAllMovieIds().toSet shouldBe ids.toSet
    rm.findAllScreenings().map(_.filmId).toSet shouldBe ids.toSet
    projector.stop()
  }

  // A venue the source lists but the read model lacks: a `movie_slots` row written after
  // the film's last projection (Palace Cinema Kent, 2026-09-07) — the projection needs
  // the slot to emit that venue, and nothing touched the row again. Both heals ask for
  // every screenings row a row's slots project to, not only for its cards.
  private def screenedInTwoCities(): (ReadModelProjector, InMemoryMovieRepository, InMemoryReadModelRepository, StoredMovieRecord, Seq[String]) = {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](
      Multikino   -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-12T20:00"))),
      KinoMuranow -> SourceData(title = Some("Foo"), showtimes = Seq(at("2026-06-13T20:00"))))))
    val row = repository.findAll().head
    val ids = ReadModelProjection.screeningIds(row, titleNormalizer)
    ids should have size 2
    (projector, repository, rm, row, ids)
  }

  "the orphan prune" should "restore a venue's screenings row the read model lacks while the card is healthy" in {
    val (projector, _, rm, row, ids) = screenedInTwoCities()
    projector.onMovieUpsert(row)
    rm.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    rm.deleteScreening(ids.last)                      // the venue's row vanishes; the card stays

    projector.pruneOrphans()

    rm.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    projector.stop()
  }

  "start" should "project a row one of whose venues has no screenings row" in {
    val (projector, repository, rm, row, ids) = screenedInTwoCities()
    projector.onMovieUpsert(row); projector.stop()
    val partial = new InMemoryReadModelRepository()
    rm.findAllMovies().foreach(partial.upsertMovie)
    rm.findAllScreenings().filter(_._id == ids.head).foreach(partial.upsertScreening)   // one venue short
    val healer = new ReadModelProjector(repository, partial, partial)

    healer.start()

    partial.findAllScreenings().map(_._id).toSet shouldBe ids.toSet
    healer.stop()
  }

  // The 2026-09-07 ReadModelFilmPruneBurst, replayed: live rows whose cards sit under ids
  // the source no longer produces (an id scheme change; a restored database), and then
  // the scheduled prune — with no boot heal in between. The prune must leave every live
  // film with a card, whatever id its old card carried.
  "the orphan prune" should "never leave a live film without a card, even when every card it has is stale" in {
    val (projector, repository, rm) = fixture()
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00"))))
    val rows = repository.findAll()
    // Cards under the OLD scheme's ids for both rows, and nothing under the new ones.
    rows.foreach { row =>
      val (card, screenings) = ReadModelProjection.project(row, titleNormalizer)
      val old = s"${row.id.value}-old"
      rm.upsertMovie(card.copy(_id = old))
      screenings.foreach(s => rm.upsertScreening(s.copy(_id = s"$old|${s.city}|${s.cinema}", filmId = old)))
    }
    rm.findAllMovieIds().toSet shouldBe rows.map(r => s"${r.id.value}-old").toSet

    projector.pruneOrphans()                       // straight to the prune: no start(), no boot heal

    rm.findAllMovieIds().toSet shouldBe rows.flatMap(ReadModelProjection.filmIds(_, titleNormalizer)).toSet
    rm.findAllScreenings().map(_.filmId).toSet shouldBe rows.map(_.id.value).toSet
    projector.stop()
  }

  "a card read that did not complete" should "heal nothing and prune nothing" in {
    // "No cards" and "could not read the cards" are different facts: on the second, a
    // heal that trusted the empty answer would re-project every row (a boot burst), and
    // a prune would delete every card. Both stand down.
    val repository = new InMemoryMovieRepository()
    val blind = new InMemoryReadModelRepository() {
      override def findAllMovieIdsChecked(): (Seq[String], Boolean) = (Seq.empty, false)
      override def findAllMovieIds(): Seq[String] = Seq.empty
    }
    val projector = new ReadModelProjector(repository, blind, blind)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    blind.upsertMovie(ReadModelProjection.project(repository.findAll().head, titleNormalizer)._1)
    val before = blind.movieUpserts.size

    projector.start()                                // boot heal: cannot see the cards → nothing
    projector.pruneOrphans()                         // sweep heal + prune: the same
    blind.movieUpserts should have size before
    blind.movieDeletes shouldBe empty
    projector.stop()
  }

  "a screenings write that throws" should "leave the card un-remembered so the next projection retries it" in {
    val repository = new InMemoryMovieRepository()
    val flaky = new InMemoryReadModelRepository() {
      var failOnce = true
      override def upsertScreening(s: CityScreening): Unit =
        if (failOnce) { failOnce = false; throw new RuntimeException("simulated screenings write failure") }
        else super.upsertScreening(s)
    }
    val projector = new ReadModelProjector(repository, flaky, flaky)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    val row = repository.findAll().head

    intercept[RuntimeException](projector.onMovieUpsert(row))
    flaky.screeningUpserts shouldBe empty

    projector.onMovieUpsert(row)                     // the same row again: the card's hash was not remembered
    flaky.screeningUpserts should not be empty
    projector.stop()
  }

  "start" should "schedule the orphan prune but NOT a periodic reproject" in {
    val fakeScheduler = new CapturingScheduler
    val repository = new InMemoryMovieRepository()
    val rm = new InMemoryReadModelRepository()
    val projector = new ReadModelProjector(repository, rm, rm, scheduler = fakeScheduler)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()
    val healedAtBoot = rm.movieUpserts.size       // Foo had no card, so the boot heal wrote it

    fakeScheduler.scheduled should have size 1   // only the prune, never the reproject
    fakeScheduler.runAll()                        // a scheduled reproject WOULD rewrite Foo here
    rm.movieUpserts     should have size healedAtBoot   // prune re-projects nothing
    projector.stop()
  }

}
