package services.readmodel

import scala.collection.mutable

import models.ResolvedMovie

/**
 * What this process knows it has written to the read model, per card, per screenings row and per source row: the
 * [[ReadModelProjector]]'s memo, which its per-row projection diffs against so only what changed is written, which it
 * seeds from the read model at boot and teaches the boot hydrate's lessons, and which every reader and writer
 * of it holds [[lock]] for (the change-stream callback, the sweeper's passes and the boot alike).
 */
private[readmodel] final class ProjectionMemo {
  // Diff state for minimal writes: the CONTENT HASH of the last-projected document per
  // film (and per screening), NOT the full document. The projection is deterministic, so
  // an unchanged row hashes to the same value and its write is skipped; keeping only the
  // hash resident holds the whole read model for diffing at a few bytes per doc instead of
  // the full ResolvedMovie / CityScreening object graph (which also lives in Mongo — this
  // is a pure duplicate we don't need to keep inflated). A 32-bit hash collision
  // (astronomically rare) would skip one genuine write, leaving a stale doc until the
  // row's next real change re-projects it — self-healing, never permanently wrong.
  // Per card: a hash per PART of what was last written, so a rewrite can name what moved.
  val lastMovie      = mutable.Map.empty[String, CardHash]
  // Per SOURCE ROW: the metadata hash at which a heal looked and found nothing to write, and
  // the venues it found to be phantoms.
  // The venue check reads SLOTS ONLY (the sweep must not pull the whole screenings
  // collection), so it cannot tell a slot with no showtimes from a venue whose row is
  // missing, and it asks about both. A slot that is simply spent therefore reads as absent
  // on EVERY sweep: PL re-projected the same ~333 rows every 30 minutes for as long as this
  // ran (2026-09-08). Once a heal has re-projected a row and written nothing, the absence
  // was a phantom of that slots-only view — remember it, and stop asking until the row's
  // metadata moves or a projection WRITES that venue, which is exactly when the answer can
  // change. Not on every projection of the row: a showtime moving at its other venue, or the
  // catch-up re-projecting a row the cursor was late with, leaves a spent slot spent, and
  // wiping the note there re-asked 13 Polish rows on a sweep over an unchanged corpus.
  val healedClean    = mutable.Map.empty[String, (Int, Set[String])]
  // Per SOURCE ROW: the card ids its last projection produced. What lets a re-projection
  // retire the variant card a vanished listing no longer earns, and a delete event retire
  // every card of a row that was merged away — so the prune finds nothing (the rule).
  val lastCardsByRow = mutable.Map.empty[String, Set[String]]
  // Per card: what was last written for each of its screenings rows — see [[WrittenScreening]].
  val lastScreenings = new ScreeningMemo
  // Metadata-reuse cache (optimisation #1): per SOURCE ROW (keyed by the anchor
  // `ReadModelProjection.filmId`, stable across the display-title split), the
  // `metadataHash` of the last projection plus the projected `ResolvedMovie` variant(s)
  // it produced, in `projectAll` order. The projected metadata is a pure function of the
  // row's cinema STRUCTURE, not its SHOWTIMES (see `ReadModelProjection.metadataHash`), so
  // a showtime-only change re-uses these and recomputes only the cheap screenings half —
  // skipping resolve/synopsisByCity/ratings, the expensive projection work the reproject/
  // enrich churn spends its CPU on. Unlike `lastMovie`/`lastScreenings` (which hold a few
  // bytes per doc) this holds the ResolvedMovie object graph resident — but only for rows
  // actually projected, ~corpus-sized (few MB on the 320m heap), and evicted on prune.
  val lastMetadata   = mutable.Map.empty[String, (Int, Seq[ResolvedMovie])]
  // Each projected row's display-title groups and the card each files its venues under, from its last
  // projection: what lets a venue apply place a venue without the rest of the film. A few entries a row.
  val lastGroups     = mutable.Map.empty[String, RowGroups]
  // Whether the boot seed has run: [[learnFrom]] vouches venue rows against it.
  @volatile var seeded = false
  // Held by every reader and writer of this memo.
  val lock           = new AnyRef

  /** Whether the read model serves card `id`: this process wrote it with screenings (or seeded it
   *  so — `lastMovie` and `lastScreenings` are seeded from the read model). Caller holds `lock`. */
  def served(id: String): Boolean = lastMovie.contains(id) && lastScreenings.holdsAny(id)

  /** The venues a heal found to be phantoms of this row, while its metadata is what it was. */
  def phantomsOf(rowId: String, metadataHash: Int): Set[String] =
    healedClean.get(rowId).collect { case (hash, phantoms) if hash == metadataHash => phantoms }.getOrElse(Set.empty)

  /** Keep only the source rows `live` names (the prune's eviction of rows gone). Caller holds `lock`. */
  def retainRows(live: String => Boolean): Unit = {
    lastMetadata.filterInPlace((rowKey, _) => live(rowKey))
    lastCardsByRow.filterInPlace((rowKey, _) => live(rowKey))
    healedClean.filterInPlace((rowKey, _) => live(rowKey))
  }

  /** Learn a row the change stream has not projected since this worker booted, from the boot hydrate's
   *  read of it (every film with its showtimes, see [[BootCorpusStudy]]) and without writing: its title
   *  groups, and the inputs of each venue row whose stored content is exactly what the row projects to.
   *  That is what a venue apply needs to apply a change at a few of its venues from those venues
   *  alone; the boot seed knows only the rows' contents. Without it each film's first change after
   *  every deploy re-read the whole film (`projector_row_unprojected`: 78 of the US's 78 declines in
   *  half an hour, 2026-10-01). A venue row whose content differs is drift and stays unvouched — a
   *  change at its card is re-read whole, and the content check rewrites it. A row the stream has
   *  projected meanwhile is left alone: it knows better. Caller holds `lock`. */
  def learnFrom(rowId: String, lesson: Lesson): Unit = if (!lastGroups.contains(rowId)) {
    lesson.cards.foreach { case (card, venues) =>
      val vouched = venues.flatMap { venue =>
        lastScreenings.get(card, venue.id).filter(row => row.input.isEmpty && row.output == venue.output)
          .map(row => venue.id -> WrittenScreening(row.output, Some(venue.input)))
      }
      lastScreenings.updateRows(card, vouched)
    }
    lastGroups.update(rowId, RowGroups(lesson.anchorKey, lesson.cardByGroup, lesson.multiSlot))
  }
}
