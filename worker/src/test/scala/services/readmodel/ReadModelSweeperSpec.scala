package services.readmodel

import tools.SpecClock.given

import services.movies.ChangeStreamLiveness
import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}

/**
 * The sweeper's passes over the whole corpus: the id-only prune (orphan cards, unlisted venues),
 * the catch-up of rows a silent change stream failed to deliver, and the reconcile-sweep metric.
 * Drives a projector's [[ReadModelSweeper]] against the in-memory repos.
 */
class ReadModelSweeperSpec extends AnyFlatSpec with Matchers with ReadModelProjectorFixtures {
  import ReadModelProjectionMetrics.ReconcileKind

  // ── Split sweep: cheap prune vs expensive full re-projection ─────────────────
  // The full re-projection (project EVERY row) was the ~1-core corpus burst that filled
  // the heap → GC thrash → credit starvation; it has been retired (the change stream
  // covers missed upserts). The remaining scheduled backstop is a cheap id-only prune
  // that removes deleted/re-keyed rows WITHOUT re-projecting anything.
  "pruneOrphans" should "prune a vanished film WITHOUT re-projecting live rows" in {
    val (projector, repository, rm) = fixture()
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    sweeper.reconcile()                     // full sweep: projects Foo
    rm.movieUpserts should have size 1
    // A live row's metadata changes: the CHEAP prune must NOT re-project it — that's
    // the whole point (no per-row projection = no CPU burst).
    repository.upsert("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    sweeper.pruneOrphans()
    rm.movieUpserts should have size 1        // unchanged — prune did not reproject
    // …but a vanished film IS still pruned by the cheap sweep (the load-bearing job).
    repository.delete("Foo", Some(2024))
    sweeper.pruneOrphans()
    rm.movieDeletes should contain(fid)
  }

  // The prune diffs the cards against the rows its scan saw, so it must diff the cards as they were
  // BEFORE that scan: a card another process writes after the scan read its source is one the scan
  // could not have seen the row of, and reading the cards again then pruned it as an orphan.
  it should "not prune a card another process wrote after its scan had read the source" in {
    val rm = new InMemoryReadModelRepository()
    val written = new java.util.concurrent.atomic.AtomicBoolean(false)
    lazy val repository: InMemoryMovieRepository = new InMemoryMovieRepository(normalizer = titleNormalizer) {
      override def foreachRecordWithSlots(f: StoredMovieRecord => Unit): tools.ScanOutcome = {
        val complete = super.foreachRecordWithSlots(f)
        if (written.compareAndSet(false, true)) {
          repository.upsert("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-13T20:00")), tmdbId = 2))
          new ReadModelProjector(repository, rm, rm, clock = specClock)
            .onMovieUpsert(repository.findAll().find(_.record.tmdbId.contains(2)).get)
        }
        complete
      }
    }
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    val projector = new ReadModelProjector(repository, rm, rm, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    sweeper.reconcile()
    val cards = rm.findAllMovieIds().toSet
    sweeper.pruneOrphans()
    written.get shouldBe true
    rm.findAllMovieIds().toSet should have size (cards.size + 1L)
    rm.movieDeletes shouldBe empty
  }

  // A VENUE THE SOURCE DROPPED, UNDER A CARD THAT STAYS. The prune removed screenings only of a
  // card whose row was gone, so a venue row the stream missed the removal of — its film still
  // screening elsewhere — was served until the rolling content check reached the row: Dracula: A
  // Love Tale at the Music Box, pruned at 2026-09-30 08:55:22 while the projector was not yet
  // attached, kept us/chicago a card over its corpus for 22 hours (the check's slice 14, 10-01
  // 07:17). A venue the source no longer lists at all is now pruned by the next sweep.
  it should "prune a venue row the source no longer lists, under a card that stays" in {
    val (projector, repository, rm) = fixture()
    val sweeper = new ReadModelSweeper(projector.projection)
    def foo(venues: (Source, String)*) = MovieRecord(tmdbId = Some(1), data = venues.map { case (cinema, showtime) =>
      cinema -> SourceData(title = Some("Foo"), showtimes = Seq(at(showtime))) }.toMap)
    repository.upsert("Foo", Some(2024), foo(Multikino -> "2026-06-12T20:00", KinoMuranow -> "2026-06-13T18:00"))
    projector.onMovieUpsert(repository.findAll().head)
    rm.findAllScreenings() should have size 2
    repository.upsert("Foo", Some(2024), foo(Multikino -> "2026-06-12T20:00"))   // a removal the stream never delivered

    sweeper.pruneOrphans()

    rm.findAllScreenings().map(_._id) shouldBe ReadModelProjection.screeningIds(repository.findAll().head, titleNormalizer)
    rm.findAllMovies().map(_._id) shouldBe Seq(fid)
    projector.stop()
  }

  // THE BLAST-RADIUS CAP. A missed removal comes a few at a time; a sweep finding many more is
  // likelier a disagreement between the slots-only view and the projection, and pruning on it
  // would blank the served venues of a country. Up to the cap (20 here: 1% of 22 rows is less)
  // every one is pruned; one more, and none is, loudly and metered.
  private def sweepAfterSilentlyDropping(dropped: Int): (InMemoryReadModelRepository, RecordingReadModelProjectionMetrics, Seq[String], Seq[String]) = {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val rm = new InMemoryReadModelRepository()
    val m  = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    val cinemas = Cinema.all.distinct.filter(City.forCinema(_).nonEmpty).take(22)
    cinemas should have size 22
    def foo(screening: Seq[Cinema]) = MovieRecord(tmdbId = Some(1), data = screening.map(c => (c: Source) -> venueSlot("Foo", Seq(at("2026-06-12T20:00")))).toMap)
    repository.upsert("Foo", Some(2024), foo(cinemas))
    projector.onMovieUpsert(repository.findAll().head)
    val before = rm.findAllScreenings().map(_._id)
    before should have size 22
    repository.upsert("Foo", Some(2024), foo(cinemas.drop(dropped)))     // removals the stream never delivered
    val lines = tools.LogCapture.capture(classOf[ReadModelSweeper].getName)(sweeper.pruneOrphans())
      .filter(_.getFormattedMessage.contains("no longer lists the venue")).map(e => s"${e.getLevel} ${e.getFormattedMessage}")
    projector.stop()
    (rm, m, before, lines)
  }

  it should "prune every unlisted venue row up to its cap" in {
    val (rm, m, _, lines) = sweepAfterSilentlyDropping(20)
    rm.findAllScreenings() should have size 2
    m.unlistedVenues.toSeq shouldBe Seq(20 -> false)
    lines.map(_.takeWhile(_ != ' ')) shouldBe Seq("WARN")
  }

  it should "prune none of them past its cap, and say so as an error" in {
    val (rm, m, before, lines) = sweepAfterSilentlyDropping(21)
    rm.findAllScreenings().map(_._id) should contain theSameElementsAs before
    m.unlistedVenues.toSeq shouldBe Seq(21 -> true)
    lines should have size 1
    lines.head should startWith ("ERROR")
    lines.head should include ("pruning NONE")
  }

  "the unlisted-venue cap" should "be 20, or 1% of the venue rows when that is more" in {
    ReadModelSweeper.unlistedPruneCap(0) shouldBe 20
    ReadModelSweeper.unlistedPruneCap(2_000) shouldBe 20
    ReadModelSweeper.unlistedPruneCap(150_000) shouldBe 1_500
  }

  // THE SELF-HEAL FOR A SILENT CHANGE STREAM. A cursor that is open and delivering nothing
  // reopens nothing; the prune sweep healed a MISSING card or venue but never re-projected a
  // CHANGED row, so the site served stale ratings and showtimes until a restart. The sweep
  // now re-projects every row written after the movies cursor's last delivery — bounded to
  // the rows that moved, never the corpus.
  "the prune sweep" should "re-project a row written after the movies cursor's last delivery" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))   // delivered at t0
    sweeper.reconcile()
    rm.movieUpserts should have size 1

    // The store changes after the last delivery and the cursor says nothing — the stall.
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    // Nothing subscribed to the cursor: nothing was promised, the sweep stays id-only.
    sweeper.pruneOrphans()
    rm.movieUpserts should have size 1
    m.caughtUp shouldBe empty                                              // not even metered: no cursor, no catch-up

    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)   // a cursor is open — and silent
    sweeper.pruneOrphans()

    withClue("the changed row must be re-projected by the sweep: ") {
      rm.movieUpserts should have size 2
      rm.movieUpserts.last.ratings.imdb shouldBe Some(9.9)
    }
    m.caughtUp shouldBe Seq(1)
  }

  // A cursor that stays silent does not move the delivery floor, so the sweep used to re-read
  // and re-project the SAME rows every 30 minutes — and on a cursor subscribed after the corpus
  // was written, the whole corpus, every sweep (the convergence legs' fixpoint tick, 2026-09-24).
  it should "catch a silent cursor's missed row up once, then only rows written after that" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)   // open, and never delivers:
    // every write below goes past the cursor, as a stalled one lets it
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    sweeper.pruneOrphans()
    m.caughtUp.last shouldBe 1

    clock.advanceSeconds(60)
    sweeper.pruneOrphans()
    withClue("nothing was written since the last catch-up, so there is nothing to catch up: ") {
      m.caughtUp.last shouldBe 0
    }

    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Bar", Some(2024), record(Some(7.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    sweeper.pruneOrphans()
    m.caughtUp.last shouldBe 1
    rm.movieUpserts should have size 2                       // Foo once, then Bar — never Foo again
  }

  // THE WRITE IN FLIGHT ACROSS A SCAN. `updatedAt` is stamped before the write is sent, so a write
  // stamped just under a catch-up's floor can land after that catch-up's scan — and the next one,
  // reading only rows stamped past the floor, skipped it for good. Here the row is stamped five
  // seconds before the first sweep's read began and becomes visible only after that sweep.
  it should "catch up a row stamped before the last catch-up's floor but written after its scan" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    sweeper.reconcile()
    clock.advanceSeconds(60)
    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)
    clock.advanceSeconds(60)
    sweeper.pruneOrphans()                                 // the scan: nothing written since
    val scannedAt = clock.instant()

    clock.setTo(scannedAt.minusSeconds(5))                   // stamped before the scan began…
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    clock.setTo(scannedAt.plusSeconds(60))                   // …and visible only after it

    sweeper.pruneOrphans()
    withClue("a write that committed after the last scan must be caught up, whatever its stamp: ") {
      rm.movieUpserts.last.ratings.imdb shouldBe Some(9.9)
    }
    m.caughtUp.last shouldBe 1

    clock.advanceSeconds(1)
    sweeper.pruneOrphans()
    withClue("re-reading it inside the overlap writes nothing, so it is not counted again: ") {
      m.caughtUp.last shouldBe 0
      rm.movieUpserts should have size 2
    }
  }

  // A LIVE cursor's backlog is not a silent cursor. The overlap re-reads rows stamped under the
  // delivery floor — which are exactly the rows a live cursor has DELIVERED but whose apply is
  // still queued behind a scrape burst. Projecting one there is harmless (the apply then writes
  // nothing), but counting it books a delivered write as one the cursor failed to deliver: the
  // catch-up metric and its "not delivering" warning, whose healthy reading is zero.
  it should "not count an overlap row whose delivered event is still waiting to be applied" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    val rm         = new InMemoryReadModelRepository()
    val m          = new RecordingReadModelProjectionMetrics()
    val projector  = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    sweeper.reconcile()
    val liveness = repository.changeStreamLiveness
    liveness.watching(ChangeStreamLiveness.Movies)
    clock.advance(services.movies.MovieRepository.CatchUpOverlap.multipliedBy(3))

    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(1)
    liveness.delivered(ChangeStreamLiveness.Movies)           // the cursor delivered it…
    val ticket = liveness.queued(ChangeStreamLiveness.Movies) // …and its apply waits in the queue
    clock.advanceSeconds(1)

    sweeper.pruneOrphans()
    withClue("a delivered write whose apply is merely queued is not one the cursor missed: ") {
      m.caughtUp.last shouldBe 0
    }
    liveness.applied(ChangeStreamLiveness.Movies, ticket)
  }

  it should "re-read a caught-up row whose projection failed on the next sweep" in {
    val clock      = new tools.MutableClock(java.time.Instant.parse("2026-09-07T10:00:00Z"))
    val repository = new InMemoryMovieRepository(clock = clock, normalizer = titleNormalizer)
    var failing    = true
    val rm = new InMemoryReadModelRepository {
      override def upsertMovie(movie: ResolvedMovie): Unit =
        if (failing) throw new RuntimeException("read model unreachable") else super.upsertMovie(movie)
    }
    val m         = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.changeStreamLiveness.watching(ChangeStreamLiveness.Movies)
    clock.advanceSeconds(60)
    repository.putEmbeddedOutOfBand("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    clock.advanceSeconds(60)
    sweeper.pruneOrphans()                                 // the write throws: Foo is still stale

    failing = false
    clock.advanceSeconds(60)
    sweeper.pruneOrphans()
    withClue("a row the catch-up failed to project must be read again: ") {
      rm.movieUpserts.map(_.title) should contain ("Foo")
    }
  }

  it should "not re-project a changed row the cursor did deliver" in {
    val (projector, repository, rm) = fixture()
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    sweeper.reconcile()
    projector.start(sweeper)                                        // subscribed: the next write is DELIVERED
    try {
      repository.upsert("Foo", Some(2024), record(Some(9.9), Seq(at("2026-06-12T20:00"))))
      rm.movieUpserts should have size 2                     // projected off the stream
      val before = rm.movieUpserts.size
      sweeper.pruneOrphans()
      rm.movieUpserts should have size before                // the sweep read nothing to catch up
    } finally projector.stop()
  }

  // Only the PRUNE sweep is metered now — the reproject's did_work gate was retired, so
  // reconcile() (the seed/backfill path) records nothing; every sweep row is kind=prune.
  "the reconcile-sweep metric" should "meter only the prune sweep, never the reproject seed" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer); val rm = new InMemoryReadModelRepository()
    val m = new RecordingReadModelProjectionMetrics()
    val projector = new ReadModelProjector(repository, rm, rm, m, clock = specClock)
    val sweeper = new ReadModelSweeper(projector.projection)
    repository.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    sweeper.reconcile()                                    // seed Foo — NOT metered
    m.sweeps shouldBe empty
    sweeper.pruneOrphans()                                 // nothing to prune (Foo live) → no-op
    m.sweeps.last shouldBe (ReconcileKind.Prune -> false)
    repository.delete("Foo", Some(2024))
    sweeper.pruneOrphans()                                 // prunes Foo → did work
    m.sweeps.last shouldBe (ReconcileKind.Prune -> true)
  }
}
