package services.readmodel

import models.{CityScreening, ResolvedMovie}

// What the projector remembers between projections, and the working state of one of
// [[ReadModelSweeper]]'s passes — the value types both keep under the projector's lock.

/** What the projector remembers about a written screenings row: the hash of the row
 *  itself (`output`, the minimal-write diff) and of the inputs it was built from
 *  (`input`, [[ReadModelProjection.VenueScreening.inputHash]]) — `None` when this process
 *  did not build it, so nothing vouches that the current inputs would build it again. */
private[readmodel] final case class WrittenScreening private (output: Int, private val inputHash: Int, private val built: Boolean) {
  def input: Option[Int] = Option.when(built)(inputHash)
}

private[readmodel] object WrittenScreening {
  // The input hash unboxed and flagged rather than an Option: each of the US's ~100k rows held a Some and a boxed
  // Integer, 32 bytes beside the row's own 24 (dump 2026-10-04).
  def apply(output: Int, input: Option[Int]): WrittenScreening = new WrittenScreening(output, input.getOrElse(0), input.isDefined)
}

/** One venue's screenings row as a projection plans it: rebuilt (`built`), or carried
 *  unbuilt because the row written from the same `input` is still current. */
private[readmodel] final case class PlannedScreening(_id: String, input: Int, built: Option[CityScreening])

/** One row the content check rewrote: the documents by cause, and what moved, for its WARN line. */
private[readmodel] final case class RowDrift(rowId: String, causes: Seq[(String, Int)], detail: String)

/** The screenings rows one card's diff wrote and removed, by id. */
private[readmodel] final case class ScreeningDiff(upserted: Seq[String], deleted: Seq[String]) {
  def documents: Int = upserted.size + deleted.size
}

/** A projected row's display-title groups — the group a slot with no title of its own falls into
 *  (`anchor`), the card each group's venues are filed under, and the venues whose row unioned two
 *  or more of the film's slots (a slot the venue read cannot see — one the `movies` document still
 *  embeds — would be among them). */
private[readmodel] final case class RowGroups(anchor: String, cardByGroup: Map[String, String], multiSlot: Set[String])

/** What [[ReadModelProjector.learn]] takes from a row, without the row: its title groups, and each
 *  card's venue rows as their ids, the content each projects to and the slot inputs it came from.
 *  Kept in place of the row so the boot hydrate's rows can be released before the seed is read. */
private[readmodel] final case class Lesson(anchorKey: String, cardByGroup: Map[String, String],
                                           cards: Seq[(String, Seq[Lesson.Venue])], multiSlot: Set[String])

private[readmodel] object Lesson {
  final case class Venue(id: String, output: Int, input: Int)

  def of(partition: ReadModelProjection.Partition): Lesson = {
    val cards = partition.filmIds.zip(partition.venuesAll)
    Lesson(partition.anchorKey, partition.cardByGroup,
      cards.map { case (card, venues) => card -> venues.map(venue => Venue(venue._id, venue.screening.##, venue.inputHash)) },
      cards.iterator.flatMap(_._2).filter(_.slotCount > 1).map(_._id).toSet)
  }
}

/** One ready row of the boot hydrate's read, as the boot reads use it: what the missing-card check
 *  asks of it, and what [[ReadModelProjector.learn]] would take from it. */
private[readmodel] final case class BootRow(id: services.movies.FilmId, filmIds: Seq[String], screeningIds: Seq[String],
                                            metadataHash: Int, lesson: Lesson)

/** A card held back by the first-publish gate: the source row that projects it, and when its
 *  hold ends (epoch millis). */
private[readmodel] final case class HeldCard(row: String, until: Long)

/** What the projector remembers about a written card: one hash per part, so the next
 *  write can name the parts that moved ([[ReadModelProjectionMetrics.CardPart]]). Equal
 *  when every part is equal, which is exactly "the card did not change". */
private[readmodel] final case class CardHash(parts: Map[String, Int]) {
  def partsDifferingFrom(other: CardHash): Set[String] =
    parts.collect { case (part, h) if !other.parts.get(part).contains(h) => part }.toSet
}

private[readmodel] object CardHash {
  import ReadModelProjectionMetrics.CardPart
  def of(m: ResolvedMovie): CardHash = CardHash(Map(
    CardPart.Title          -> (m.title, m.originalTitle, m.searchTitles, m.provisional).##,
    CardPart.Poster         -> (m.posterUrl, m.fallbackPosterUrls).##,
    CardPart.Facts          -> (m.runtimeMinutes, m.releaseYear, m.genres, m.countries, m.directors, m.cast).##,
    CardPart.Synopsis       -> m.synopsis.##,
    CardPart.SynopsisByCity -> m.synopsisByCity.##,
    CardPart.Ratings        -> (m.ratings, m.weightedRating).##,
    CardPart.Trailers       -> m.trailerUrls.##,
    CardPart.AgeRating      -> m.ageRating.##,
    CardPart.ShareCard      -> (m.shareCard, m.shareCardPending).##))
}


/** What a content check's writes for one row moved, by cause — collected by `project` for the
 *  sweep's drift verdict. */
private[readmodel] final class RowDriftLog {
  private val causes  = Seq.newBuilder[(String, Int)]
  private val drifted = Seq.newBuilder[String]

  def card(id: String, parts: Set[String]): Unit = {
    val cause = ReadModelProjectionMetrics.cardWriteCause(parts)
    causes += cause -> 1
    drifted += s"card $id (${if (parts.isEmpty) cause else parts.toSeq.sorted.mkString("+")})"
  }

  def screenings(diff: ScreeningDiff): Unit = {
    note(ReadModelProjectionMetrics.DriftCause.ScreeningUpsert, diff.upserted)
    note(ReadModelProjectionMetrics.DriftCause.ScreeningDelete, diff.deleted)
  }

  private def note(cause: String, ids: Seq[String]): Unit = if (ids.nonEmpty) {
    causes += cause -> ids.size
    drifted += s"$cause ${ids.size} (${ReadModelSweeper.idsForLog(ids)})"
  }

  /** The row's drift, when it had any. */
  def of(rowId: String): Option[RowDrift] = {
    val drift = drifted.result()
    Option.when(drift.nonEmpty)(RowDrift(rowId, causes.result(), drift.mkString("; ")))
  }
}

/** The read model's cards and screenings rows as a sweep found them before its scan. `cards` is
 *  what the read answered (an unanswered read heals and prunes no card); `screeningRefs` is None
 *  when the rows were not read, or could not be. */
private[readmodel] final case class SweepBaseline(cards: tools.ReadOutcome[Seq[String]], screeningRefs: Option[Seq[ScreeningRef]]) {
  val cardsRead: Boolean                                      = cards.answered.isDefined
  val cardIds: Set[String]                                    = cards.answered.fold(Set.empty[String])(_.toSet)
  val screeningIds: Option[Set[String]]                       = screeningRefs.map(_.map(_._id).toSet)
  val screeningsByCard: Option[Map[String, Seq[ScreeningRef]]] = screeningRefs.map(_.groupBy(_.filmId))
}

/** What one sweep has seen and done so far. */
private[readmodel] final class SweepTally {
  /** The card ids the ready rows project to. */
  val liveIds     = scala.collection.mutable.Set.empty[String]
  /** The source-row keys behind those cards — what `lastMetadata` is keyed by. */
  val liveRowKeys = scala.collection.mutable.Set.empty[String]
  /** The ids of the ready rows this scan saw — what the content check walks its slice of. */
  val liveRowIds  = scala.collection.mutable.ArrayBuffer.empty[services.movies.FilmId]
  /** The venue rows a live card holds that its row no longer lists. */
  val unlisted    = scala.collection.mutable.ArrayBuffer.empty[ScreeningRef]
  /** The rows a heal repaired. */
  val healed      = scala.collection.mutable.ArrayBuffer.empty[String]
  var reprojected = 0
  var prunedFilms      = 0
  var prunedScreenings = 0
  /** A row whose re-projection THREW (a read-model write timed out client-side, often after the
   *  server applied it) leaves that card part-written — its document without its screenings — on a
   *  scan that was itself complete. `reconcile` reports it, so a caller asserting on the read model
   *  reconciles again rather than reading a served film with nothing under it. */
  var projectionFailed = false
  /** A prune that THREW leaves an orphan card or venue served — a film the corpus no longer has — on
   *  a scan that was complete. Reported the same way. */
  var pruneFailed      = false
}

/** A ready row the boot check found short of a card or a venue, and whether every venue it lacks is
 *  one the read model serves under another city (`refile`). */
private[readmodel] final case class MissingAtBoot(id: services.movies.FilmId, metadataHash: Int, absentCards: Seq[String],
                                                  absentVenues: Seq[String], refile: Boolean)

/** The rows the change stream changed while the boot reads ran, held to be projected once they are
 *  done. */
private[readmodel] final class BootHold {
  private var booting = true
  private val held    = scala.collection.mutable.LinkedHashSet.empty[String]

  /** Whether a change to `rowId` is held (and, if so, recorded). */
  def holds(rowId: String): Boolean = synchronized {
    if (booting) held += rowId
    booting
  }

  /** The rows held since the last take; none ends the hold. */
  def take(): Seq[String] = synchronized {
    val rows = held.toList
    held.clear()
    if (rows.isEmpty) booting = false
    rows
  }
}
