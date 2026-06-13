package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepo, StoredMovieRecord, TitleNormalizer}

import java.time.LocalDateTime

/**
 * The projector's minimal-write diff: the whole point of the read-model split
 * is that a showtime-only edit moves one screening doc and a metadata-only edit
 * moves one movie doc. Drives `onMovieUpsert` / `reconcile` directly against the
 * in-memory repos and asserts exactly which docs were written.
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

  private def stored(rec: MovieRecord): StoredMovieRecord = StoredMovieRecord("Foo", Some(2024), rec)

  private def fixture(): (ReadModelProjector, InMemoryMovieRepo, InMemoryReadModelRepo) = {
    val repo = new InMemoryMovieRepo()
    val rm   = new InMemoryReadModelRepo()
    (new ReadModelProjector(repo, rm, rm), repo, rm)
  }

  "the first projection of a row" should "write the movie doc before its screenings" in {
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
    val (projector, repo, rm) = fixture()
    repo.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))   // ready (tmdbId)
    repo.upsert("Bar", Some(2024), unresolved(Seq(at("2026-06-12T20:00"))).copy(
      data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Bar"), releaseYear = Some(2024), showtimes = Seq(at("2026-06-12T20:00"))))))
    projector.reconcile()
    rm.findAllMovies().map(_._id) should contain only fid  // Bar is held back
  }

  "a showtime-only change" should "move only the one screening doc" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))

    rm.movieUpserts     should have size 1  // metadata unchanged → not rewritten
    rm.screeningUpserts should have size 2  // the one changed screening doc, again
  }

  "a rating-only change" should "move only the movie doc" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))

    rm.movieUpserts     should have size 2  // rating changed → movie doc rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  "a film leaving a cinema" should "delete that cinema's screening doc" in {
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
    val (projector, repo, rm) = fixture()
    repo.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()
    rm.movieUpserts     shouldBe empty
    rm.screeningUpserts shouldBe empty
    projector.stop()
  }

  "reconcile" should "prune derived docs whose source film vanished" in {
    val (projector, repo, rm) = fixture()
    repo.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    rm.movieUpserts should have size 1

    repo.delete("Foo", Some(2024))
    projector.reconcile()
    rm.movieDeletes     should contain(fid)
    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // The change stream drops deletes, so a film re-keyed by `settle` (old `_id`
  // deleted, new one written) leaves the old read-model docs behind. They're
  // pruned only by `reconcile`. The trap: a worker that *wrote* the stale doc
  // restarts before pruning it — its successor's in-memory `lastMovie` never
  // knew the doc, so a `lastMovie`-based prune can't see it and the duplicate
  // card persists forever. `reconcile` must therefore diff the *actual read
  // model* against the live source, not this process's memory.
  "reconcile after a restart" should "prune a stale film a prior process left in the read model" in {
    val repo = new InMemoryMovieRepo()
    val rm   = new InMemoryReadModelRepo()
    def yearKey(y: Int) = s"${TitleNormalizer.sanitize("Foo")}|$y"
    // A film whose reported year was 2025 when an earlier projector ran.
    def recYear(y: Int) =
      MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Multikino ->
        SourceData(title = Some("Foo"), releaseYear = Some(y),
          filmUrl = Some("https://mk/foo"), showtimes = Seq(at("2026-06-12T20:00")))))

    val p1 = new ReadModelProjector(repo, rm, rm)
    repo.upsert("Foo", Some(2025), recYear(2025))
    p1.reconcile()
    rm.findAllMovies().map(_._id) should contain(yearKey(2025))
    p1.stop()  // the worker dies, taking its in-memory state with it

    // `settle` re-keys the source row onto the (now resolved) year — old gone,
    // new live.
    repo.delete("Foo", Some(2025))
    repo.upsert("Foo", Some(2026), recYear(2026))

    // A fresh projector boots with an empty `lastMovie` and reconciles.
    val p2 = new ReadModelProjector(repo, rm, rm)
    rm.movieDeletes.clear(); rm.screeningDeletes.clear()
    p2.reconcile()

    rm.findAllMovies().map(_._id)                  should contain only yearKey(2026)
    rm.findAllScreenings().map(_.filmId).distinct  should contain only yearKey(2026)
    rm.movieDeletes should contain(yearKey(2025))
    p2.stop()
  }
}
