package services.readmodel

import models.ResolvedMovie

/**
 * A ready row projected to its cards and the screenings rows each would have: the pure half of a projection, timed
 * both ways (wall clock and thread CPU), its card metadata REUSED while the row's metadata inputs stand. What is
 * written of it, and which cards go, is the [[ReadModelProjector]]'s to decide and the [[CardStore]]'s to write.
 *
 * Every method expects the projection lock held: the reuse memo is the projection's ([[ProjectionMemo.lastMetadata]]).
 */
private[readmodel] final class CardProjection(
  memo:          ProjectionMemo,
  screeningRows: ScreeningRowWrites,
  // Confidence-gated ratings (identity phase 3): what each card's ratings are served as.
  ratingGate:    RatingGate,
  metrics:       ReadModelProjectionMetrics,
  cpuClock:      tools.ThreadCpuClock,
  stopwatch:     tools.Stopwatch
)(using services.evidence.LatestTitleYear) {
  import ReadModelProjectionMetrics.ProjectTrigger
  import memo.lastMetadata

  /** The row's cards, each with the venues it screens at and its screenings rows planned against what was last
   *  written. */
  def plan(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[CardProjection.Card] =
    timed(trigger)(reusingMetadata(partition, trigger).map((movie, venues) =>
      CardProjection.Card(movie, venues, screeningRows.plan(movie._id, venues))))

  /** The row's cards and the venues each screens at. */
  def cards(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[(ResolvedMovie, Seq[ReadModelProjection.VenueScreening])] =
    timed(trigger)(reusingMetadata(partition, trigger))

  /** Run a pure projection and measure it BOTH ways: wall-clock answers "how long did it take" (the
   *  latency histogram), thread CPU answers "how much CPU did it burn" (the credit-floor attribution).
   *  They diverge under concurrency and under steal, so the one that may be compared against process
   *  CPU is the CPU one — see `recordProject`. */
  private def timed[A](trigger: ProjectTrigger)(projection: => A): A = {
    val wall     = stopwatch.start()
    val cpuStart = cpuClock.nanos()
    val result   = projection
    metrics.recordProject(trigger, wallSeconds = wall.seconds, cpuSeconds = (cpuClock.nanos() - cpuStart) / 1e9)
    result
  }

  // Project the row, REUSING the cached `ResolvedMovie` metadata when
  // the row's metadata inputs are unchanged (a showtime-only change at an already-present
  // cinema — the overwhelming common case under reproject/enrich showtime churn). Correctness:
  // an unchanged `metadataHash` guarantees an unchanged metadata output AND an unchanged
  // display-title variant partition (the hash covers the whole record bar showtimes, and no
  // metadata accessor reads showtimes), so `venuesAll` — the SAME partition's variants in
  // the SAME order — lines up 1:1 with the cached movie(s). The size guard is a
  // belt-and-suspenders fallback to recomputing: it can never pair a movie with the wrong
  // screenings. Reusing skips the resolve/synopsisByCity/ratings work; the venues' rows are
  // built (or not) afterwards, by `screeningRows.plan`.
  private def reusingMetadata(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[(ResolvedMovie, Seq[ReadModelProjection.VenueScreening])] = {
    val stored = partition.stored
    // Keyed by the SOURCE ROW (`persistedId`, unique per `movies` document), NOT by the
    // projected `ReadModelProjection.filmId`: that id keys on `resolvedYear` and so
    // deliberately COLLAPSES several source rows onto one card (`kumotry|2025` and
    // `kumotry|2026` both project to `kumotry|2026` — see its doc). The cached VALUE is one
    // row's hash + `ResolvedMovie`s, so keying on the shared card id let two rows overwrite
    // each other's entry on every alternating change: neither ever read its own hash back,
    // every projection recomputed, and the miss rate never decayed. Keying per row makes each
    // row's entry its own.
    val rowKey = stored.id.value
    // The gate's version is part of the key: a new gate must not reuse cards gated by the old one.
    val hash   = (ReadModelProjection.metadataHash(stored), ratingGate.version).##
    val venues = partition.venuesAll
    val movies = lastMetadata.get(rowKey) match {
      case Some((cachedHash, cached)) if cachedHash == hash && venues.sizeIs == cached.size =>
        metrics.recordMetadataProjection(trigger, reused = true)
        cached
      case _ =>
        val recomputed = partition.moviesAll.map(ratingGate(stored, _))
        lastMetadata.update(rowKey, hash -> recomputed)
        metrics.recordMetadataProjection(trigger, reused = false)
        recomputed
    }
    movies.zip(venues)
  }
}

private[readmodel] object CardProjection {
  /** One card of a row: its document, the venues it screens at, and the screenings rows to write for them. */
  final case class Card(movie: ResolvedMovie, venues: Seq[ReadModelProjection.VenueScreening], screenings: Seq[PlannedScreening])
}
