package services.readmodel

import models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, UnreadableByIdMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/**
 * THE DERIVATION PASS. On 2026-09-24 a deploy changed which poster the projection picks for an
 * unchanged row. No row moved, so no card was re-projected except those the rolling content check
 * reached — one slice of 48 per 30-minute sweep — and `ReadModelContentMismatch` held for ~2.5 h
 * until a human set `KINOWO_READMODEL_CONTENT_SLICES=1`. The projector now keeps the version its
 * cards were derived under beside the read model and, when a boot finds another one there,
 * re-projects every row once, a slice per tick, and then records its own.
 *
 * The derivation change is staged as it looks from the store: every source row unchanged, every
 * stored card carrying what the OLD code derived (a poster the new code no longer picks).
 */
class ReadModelDerivationPassSpec extends AnyFlatSpec with Matchers {

  private val OldPoster = "https://old-derivation.example/poster.jpg"
  private val Films     = (1 to 12).map(n => s"Film $n")

  private def record(title: String, tmdbId: Int): MovieRecord =
    MovieRecord(imdbRating = Some(7.0), tmdbId = Some(tmdbId), data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some(title), releaseYear = Some(2024), filmUrl = Some(s"https://mk/$tmdbId"),
        posterUrl = Some(s"https://mk/poster-$tmdbId.jpg"),
        showtimes = Seq(Showtime(LocalDateTime.parse("2026-06-12T20:00"), bookingUrl = Some("https://book"))))))

  /** A corpus projected by the old code: the rows as they are, the cards with the old poster. */
  private def derivedByOldCode(repository: InMemoryMovieRepository): InMemoryReadModelRepository = {
    Films.zipWithIndex.foreach { case (title, i) => repository.upsert(title, Some(2024), record(title, i + 1)) }
    val rm = new InMemoryReadModelRepository()
    repository.findAll().foreach { row =>
      val (card, screenings) = ReadModelProjection.project(row, titleNormalizer)
      rm.upsertMovie(card.copy(posterUrl = Some(OldPoster)))
      screenings.foreach(rm.upsertScreening)
    }
    rm
  }

  private def staleCards(rm: InMemoryReadModelRepository): Int = rm.findAllMovies().count(_.posterUrl.contains(OldPoster))

  private def booted(repository: InMemoryMovieRepository, rm: InMemoryReadModelRepository,
                     marker: ReadModelDerivationMarker,
                     metrics: ReadModelProjectionMetrics = ReadModelProjectionMetrics.noop,
                     history: Seq[Derivation] = ReadModelDerivation.History): ReadModelProjector = {
    val projector = new ReadModelProjector(repository, rm, rm, metrics, derivationMarker = marker, clock = tools.SpecClock.Pinned,
                                           derivationHistory = history)
    projector.start()   // seeds its memo from the store, as a restart does
    projector
  }

  "a boot that finds another derivation recorded" should
    "re-project every row once, one content slice per tick, and then record its own" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"))
    val projector  = booted(repository, rm, marker)

    projector.pruneOrphans()   // the first sweep reads the marker and arms the pass
    (1 to 47).foreach(_ => projector.advanceDerivationPass())
    withClue("the pass is paced — a slice per tick — so it is not done before the last slice: ") {
      marker.current shouldBe Some("an-older-derivation")
    }
    projector.advanceDerivationPass()

    staleCards(rm) shouldBe 0
    marker.current shouldBe Some(ReadModelDerivation.current.value)
    projector.stop()
  }

  // The pass re-projects the whole corpus with no change-stream event behind any of it, so it must
  // not land in the stream's share of readmodel_project_calls: on 2026-09-26 five deploys inside an hour, each
  // carrying a new derivation version, ran it on every worker, and ReadModelProjectionTriggerUnaccounted
  // fired for DE, UK and PL at 1-2 projections/s over their events.
  "the derivation pass" should "meter its projections as its own trigger, never as the change stream's" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = derivedByOldCode(repository)
    val metrics    = new RecordingReadModelProjectionMetrics()
    val projector  = booted(repository, rm, new InMemoryReadModelDerivationMarker(Some("an-older-derivation")), metrics)

    projector.pruneOrphans()
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    metrics.projectCalls(ReadModelProjectionMetrics.ProjectTrigger.Derivation) shouldBe Films.size
    metrics.projectCalls(ReadModelProjectionMetrics.ProjectTrigger.Stream) shouldBe 0
    projector.stop()
  }

  "a store with no derivation recorded" should "be re-projected whole — the first boot after the marker existed" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(None)
    val projector  = booted(repository, rm, marker)

    projector.pruneOrphans()
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    staleCards(rm) shouldBe 0
    marker.current shouldBe Some(ReadModelDerivation.current.value)
    projector.stop()
  }

  "a boot that finds its own derivation recorded" should "leave the corpus to the rolling content check" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = derivedByOldCode(repository)
    val projector  = booted(repository, rm, new InMemoryReadModelDerivationMarker(Some(ReadModelDerivation.current.value)))

    projector.pruneOrphans()   // repairs one slice at most
    val writesAfterSweep = rm.movieUpserts.size
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    rm.movieUpserts.size shouldBe writesAfterSweep
    staleCards(rm) should be > 0
    projector.stop()
  }

  "a row that cannot be read during the pass" should "keep the old marker, so the next sweep runs the pass again" in {
    val repository = new UnreadableByIdMovieRepository(titleNormalizer = titleNormalizer)
    repository.failing = false
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"))
    val projector  = booted(repository, rm, marker)

    projector.pruneOrphans()
    repository.failing = true
    (1 to 48).foreach(_ => projector.advanceDerivationPass())
    withClue("a pass that could not read every row has not re-projected the corpus: ") {
      marker.current shouldBe Some("an-older-derivation")
    }

    repository.failing = false
    projector.pruneOrphans()
    (1 to 48).foreach(_ => projector.advanceDerivationPass())
    staleCards(rm) shouldBe 0
    marker.current shouldBe Some(ReadModelDerivation.current.value)
    projector.stop()
  }

  "a marker that cannot be read" should "start no pass until a later sweep can read it" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"))
    marker.unreadable = true
    val projector  = booted(repository, rm, marker)

    projector.pruneOrphans()
    val writesAfterSweep = rm.movieUpserts.size
    (1 to 48).foreach(_ => projector.advanceDerivationPass())
    withClue("an unreadable marker is not a missing one — no pass on a failed read: ") {
      rm.movieUpserts.size shouldBe writesAfterSweep
    }

    marker.unreadable = false
    projector.pruneOrphans()
    (1 to 48).foreach(_ => projector.advanceDerivationPass())
    staleCards(rm) shouldBe 0
    marker.current shouldBe Some(ReadModelDerivation.current.value)
    projector.stop()
  }

  /** Counts the two by-id reads a pass can make; the slots-only one returns the row without its
   *  showtimes, as `MongoMovieRepository` does when it skips the `screenings` read. */
  private class ReadCountingMovieRepository extends InMemoryMovieRepository(normalizer = titleNormalizer) {
    @volatile var wholeReads = 0
    @volatile var slotsOnlyReads = 0
    override def findByIdChecked(id: services.movies.FilmId) = { wholeReads += 1; super.findByIdChecked(id) }
    override def findByIdWithSlotsChecked(id: services.movies.FilmId) = {
      slotsOnlyReads += 1
      val (row, read) = super.findByIdChecked(id)
      (row.map(r => r.copy(record = r.record.copy(data = r.record.data.view.mapValues(_.copy(showtimes = Nil)).toMap))), read)
    }
    def resetCounts(): Unit = { wholeReads = 0; slotsOnlyReads = 0 }
  }

  private def history(scopeOfLatest: DerivationScope) =
    Seq(Derivation(DerivationVersion("an-older-derivation"), DerivationScope.Full), Derivation(DerivationVersion("this-code"), scopeOfLatest))

  // THE READ THIS SAVES: a derivation that moved only what a card shows (the 2026-09-24 poster
  // change: 58 rows, poster alone) re-projects the cards off the slots-only read. A whole-row read
  // also pulls every showtime from `screenings` — 260 of the US corpus's 341 MB — which no card reads.
  "a pass owed only for card fields" should "read every row slots-only, rewrite the stale cards, and leave the screenings alone" in {
    val repository = new ReadCountingMovieRepository
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"))
    val projector  = booted(repository, rm, marker, history = history(DerivationScope.Cards))
    projector.pruneOrphans()   // arms the pass; its own content slice may repair a row or two
    val screeningsBefore = rm.findAllScreenings().toSet
    val screeningWrites  = rm.screeningUpserts.size
    repository.resetCounts()

    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    repository.slotsOnlyReads shouldBe Films.size
    repository.wholeReads shouldBe 0
    staleCards(rm) shouldBe 0
    rm.findAllScreenings().toSet shouldBe screeningsBefore
    rm.screeningUpserts.size shouldBe screeningWrites
    marker.current shouldBe Some("this-code")
    projector.stop()
  }

  "a pass owed for more than card fields" should "read every row whole" in {
    val repository = new ReadCountingMovieRepository
    val rm         = derivedByOldCode(repository)
    val projector  = booted(repository, rm, new InMemoryReadModelDerivationMarker(Some("an-older-derivation")),
                            history = history(DerivationScope.Full))
    projector.pruneOrphans()
    repository.resetCounts()

    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    repository.wholeReads shouldBe Films.size
    repository.slotsOnlyReads shouldBe 0
    staleCards(rm) shouldBe 0
    projector.stop()
  }

  // A pass takes ~8 minutes and starts ~5 after boot; on 2026-09-26 deploys ten minutes apart
  // restarted one version's pass on every worker three times before one finished.
  "a pass a previous process got part-way through" should "resume at the slice it had reached, not start over" in {
    val repository = new ReadCountingMovieRepository
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"),
                                                           Some(DerivationProgress(DerivationVersion("this-code"), 30)))
    val projector  = booted(repository, rm, marker, history = history(DerivationScope.Full))
    projector.pruneOrphans()
    repository.resetCounts()

    projector.advanceDerivationPass()
    marker.currentProgress shouldBe Some(DerivationProgress(DerivationVersion("this-code"), 31))
    (31 until 48).foreach(_ => projector.advanceDerivationPass())

    marker.current shouldBe Some("this-code")
    val laterRows = repository.findAll().count(row => ReadModelProjector.contentSliceOf(row.id) >= 30)
    repository.wholeReads shouldBe laterRows
    projector.stop()
  }

  it should "start from the first slice when the progress was for another version" in {
    val repository = new ReadCountingMovieRepository
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"),
                                                           Some(DerivationProgress(DerivationVersion("an-abandoned-version"), 30)))
    val projector  = booted(repository, rm, marker, history = history(DerivationScope.Full))
    projector.pruneOrphans()

    (1 to 18).foreach(_ => projector.advanceDerivationPass())
    marker.current shouldBe Some("an-older-derivation")
    marker.currentProgress shouldBe Some(DerivationProgress(DerivationVersion("this-code"), 18))
    projector.stop()
  }

  "a pass that could not read a row" should "stop recording progress, so a restart re-reads from the slice that missed it" in {
    val repository = new UnreadableByIdMovieRepository(titleNormalizer = titleNormalizer)
    repository.failing = false
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(Some("an-older-derivation"))
    val projector  = booted(repository, rm, marker, history = history(DerivationScope.Full))
    projector.pruneOrphans()
    val firstSliceWithARow = repository.findAll().map(row => ReadModelProjector.contentSliceOf(row.id)).min

    repository.failing = true
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    marker.currentProgress.map(_.nextSlice).getOrElse(0) should be <= firstSliceWithARow
    marker.current shouldBe Some("an-older-derivation")
    projector.stop()
  }
}
