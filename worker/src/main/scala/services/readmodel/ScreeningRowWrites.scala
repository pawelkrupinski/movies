package services.readmodel

import models.CityScreening

/**
 * The screenings half of the [[ReadModelProjector]]'s writes: which of a card's venue rows to build — only those whose
 * inputs moved since their row was written — and the diff of the rows built against what [[ScreeningMemo]] remembers
 * writing, so only the rows that changed are upserted and only the rows gone are deleted. Its caller holds the memo's
 * lock.
 */
private[readmodel] final class ScreeningRowWrites(writer: ReadModelWriter, metrics: ReadModelProjectionMetrics, lastScreenings: ScreeningMemo) {
  import ReadModelProjectionMetrics.{Op, Target}

  /** Build the screenings rows of one card — but only for the venues
   *  whose inputs moved since their row was written. A venue whose [[WrittenScreening]]
   *  carries the same input hash already has exactly the row it would build, so it is
   *  carried as `None`: still a live id (not deleted), not rebuilt, not rewritten. This is
   *  what turns a showtime change at one venue of a wide release from thousands of rows
   *  built into one. A memo that cannot vouch — seeded from the read model at boot, or
   *  dropped by a heal — has no input hash, so every such venue is rebuilt. */
  def plan(filmId: String, venues: Seq[ReadModelProjection.VenueScreening]): Seq[PlannedScreening] = {
    val planned  = venues.map { venue =>
      val input = venue.inputHash
      val built = if (lastScreenings.get(filmId, venue._id).exists(_.input.contains(input))) None else Some(venue.screening)
      PlannedScreening(venue._id, input, built)
    }
    val rebuilt = planned.count(_.built.isDefined)
    metrics.recordVenueProjection(rebuilt = rebuilt, reused = planned.size - rebuilt)
    planned
  }

  /** Write the planned rows of one card where they changed, and delete the rows it no longer has: the diff written
   *  (its caller audits the rows deleted). */
  def diff(filmId: String, next: Seq[PlannedScreening]): ScreeningDiff = {
    val previous = lastScreenings.of(filmId)
    val upserts  = Seq.newBuilder[CityScreening]
    val nextById = next.map { planned =>
      val output = planned.built match {
        case Some(s) =>
          val hash = s.##
          if (!previous.get(planned._id).exists(_.output == hash)) upserts += s
          hash
        // Not rebuilt: `plan` only skips a venue whose entry vouches for its row.
        case None => previous(planned._id).output
      }
      planned._id -> WrittenScreening(output, Some(planned.input))
    }.toMap
    val written  = upserts.result()
    writer.upsertScreenings(written)
    val upserted = written.size
    val deletes = previous.keysIterator.filterNot(nextById.contains).toSeq
    deletes.foreach(writer.deleteScreening)
    if (upserted > 0)        metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    if (deletes.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, deletes.size)
    lastScreenings.update(filmId, nextById)
    ScreeningDiff(written.map(_._id), deletes)
  }

  /** Rebuild only the changed venues' rows of `card`: every other row of the
   *  card is carried as written — the whole-film projection would plan it unbuilt from the same
   *  inputs (and a row whose stored content drifted is the content check's, as it is under any
   *  venue apply). Every venue of `changed` has a row already (the venue plan declines one appearing). */
  def applyVenues(card: String, changed: Seq[ReadModelProjection.VenueScreening]): Unit = {
    var upserted = 0
    val updated  = changed.map { venue =>
      val input = venue.inputHash
      val prior = lastScreenings.get(card, venue._id).get
      if (prior.input.contains(input)) venue._id -> prior
      else {
        val screening = venue.screening
        val output    = screening.##
        if (output != prior.output) { writer.upsertScreening(screening); upserted += 1 }
        venue._id -> WrittenScreening(output, Some(input))
      }
    }
    if (upserted > 0) metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    metrics.recordVenueProjection(rebuilt = changed.size, reused = lastScreenings.size(card) - changed.size)
    lastScreenings.updateRows(card, updated)
  }
}
