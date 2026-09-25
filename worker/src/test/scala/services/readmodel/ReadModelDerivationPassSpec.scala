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
                     marker: ReadModelDerivationMarker): ReadModelProjector = {
    val projector = new ReadModelProjector(repository, rm, rm, derivationMarker = marker)
    projector.start()   // seeds its memo from the store, as a restart does
    projector
  }

  "a boot that finds another derivation recorded" should
    "re-project every row once, one content slice per tick, and then record its own" in {
    val repository = new InMemoryMovieRepository()
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
    marker.current shouldBe Some(ReadModelProjection.DerivationVersion)
    projector.stop()
  }

  "a store with no derivation recorded" should "be re-projected whole — the first boot after the marker existed" in {
    val repository = new InMemoryMovieRepository()
    val rm         = derivedByOldCode(repository)
    val marker     = new InMemoryReadModelDerivationMarker(None)
    val projector  = booted(repository, rm, marker)

    projector.pruneOrphans()
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    staleCards(rm) shouldBe 0
    marker.current shouldBe Some(ReadModelProjection.DerivationVersion)
    projector.stop()
  }

  "a boot that finds its own derivation recorded" should "leave the corpus to the rolling content check" in {
    val repository = new InMemoryMovieRepository()
    val rm         = derivedByOldCode(repository)
    val projector  = booted(repository, rm, new InMemoryReadModelDerivationMarker(Some(ReadModelProjection.DerivationVersion)))

    projector.pruneOrphans()   // repairs one slice at most
    val writesAfterSweep = rm.movieUpserts.size
    (1 to 48).foreach(_ => projector.advanceDerivationPass())

    rm.movieUpserts.size shouldBe writesAfterSweep
    staleCards(rm) should be > 0
    projector.stop()
  }

  "a row that cannot be read during the pass" should "keep the old marker, so the next sweep runs the pass again" in {
    val repository = new UnreadableByIdMovieRepository()
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
    marker.current shouldBe Some(ReadModelProjection.DerivationVersion)
    projector.stop()
  }

  "a marker that cannot be read" should "start no pass until a later sweep can read it" in {
    val repository = new InMemoryMovieRepository()
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
    marker.current shouldBe Some(ReadModelProjection.DerivationVersion)
    projector.stop()
  }
}
