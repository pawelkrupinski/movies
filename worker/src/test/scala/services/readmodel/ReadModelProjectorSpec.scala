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
    def recordWrite(target: String, op: String, count: Int): Unit = writes((target, op)) += count
    def recordFilmPruned(count: Int): Unit                        = prunes += count
    def recordReconcileSweep(kind: String, didWork: Boolean): Unit = sweeps += (kind -> didWork)
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
  // The 30-min full re-projection (project EVERY row) was the ~1-core corpus burst
  // that filled the heap → GC thrash → credit starvation. Its only unique job over
  // the change-stream path is catching missed upserts, so it now runs rarely; the
  // FREQUENT backstop is a cheap id-only prune that removes deleted/re-keyed rows
  // WITHOUT re-projecting anything.
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

  "the reconcile-sweep metric" should "record kind + whether the sweep did any work" in {
    val repository = new InMemoryMovieRepository(); val rm = new InMemoryReadModelRepository()
    val m = new RecordingMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()                                    // full reproject that writes Foo
    m.sweeps.last shouldBe (ReconcileKind.Reproject -> true)
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

  // The boot catch-up reproject is GATED on change-stream availability. With an active stream
  // the resume token replays the missed-while-down gap, so a boot reproject would only re-catch
  // what the stream is already delivering — a false-positive did_work on every restart. So when
  // the stream is active the first reproject is deferred to the periodic one (hours out); only a
  // DOWN stream (nothing to replay) runs the boot catch-up soon.
  "the boot reproject" should "be gated on stream availability — deferred to the periodic reproject when the stream is active, run soon only when it's down" in {
    val (projector, _, _) = fixture()
    val active   = projector.reconcileInitialDelaySeconds(streamActive = true)
    val inactive = projector.reconcileInitialDelaySeconds(streamActive = false)
    inactive should be < active     // stream down → boot catch-up runs soon (no replay to wait for)
    active   should be >= 3600L      // stream up → NO boot reproject; deferred to the periodic (hours), not a boot delay
  }
}
