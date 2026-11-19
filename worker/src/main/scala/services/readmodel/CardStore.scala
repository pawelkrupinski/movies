package services.readmodel

import models.ResolvedMovie

/**
 * The read model's cards and their screenings rows as this process writes them. Every card written, rewritten or
 * removed goes to the store through here and into the memo of what was written ([[ProjectionMemo]]), and the share-card
 * gate hears of it; a card is remembered only once its screenings are written too. The [[ReadModelProjector]] decides
 * what a row's cards are and which go; this keeps the store and the memo in step with each decision.
 *
 * Every method expects the projection lock held.
 */
private[readmodel] final class CardStore(
  writer:        ReadModelWriter,
  metrics:       ReadModelProjectionMetrics,
  memo:          ProjectionMemo,
  screeningRows: ScreeningRowWrites,
  shareCardGate: ShareCardGate
) {
  import ReadModelProjectionMetrics.{Op, Target}
  import memo.{lastMovie, lastScreenings}

  /** Write one published card and its screenings rows where they changed: the documents written. A content check's
   *  `drift` hears what moved. */
  def write(movie: ResolvedMovie, screenings: Seq[PlannedScreening], drift: Option[RowDriftLog]): Int = {
    val hash    = CardHash.of(movie)
    val changed = !lastMovie.get(movie._id).contains(hash)
    if (changed) {
      val parts = upsert(movie, hash)
      drift.foreach(_.card(movie._id, parts))
    }
    val screeningDiff = screeningRows.diff(movie._id, screenings)
    // A card kept but some of its screening slots trimmed — routine reprojection
    // churn (DEBUG), but it's how a film can drop out of "served tomorrow" without
    // its whole card being deleted, so the audit covers it too.
    if (screeningDiff.deleted.nonEmpty)
      services.movies.RemovalAudit.screeningsCleared("read-model.diff", movie._id, screeningDiff.deleted.size,
        whole = screenings.isEmpty, reason = "reproject-trim")
    drift.foreach(_.screenings(screeningDiff))
    // Remembered only once the screenings are written too: a throw in the screenings
    // write used to leave the card's hash "current", so the screenings were never
    // retried until the row changed again.
    if (changed) remember(movie, hash, screened = screenings.nonEmpty)
    (if (changed) 1 else 0) + screeningDiff.documents
  }

  /** Rewrite a card whose own document moved, its screenings rows left as they stand: whether it was written. */
  def rewrite(movie: ResolvedMovie): Boolean = {
    val hash    = CardHash.of(movie)
    val changed = !lastMovie.get(movie._id).contains(hash)
    if (changed) {
      upsert(movie, hash)
      remember(movie, hash, screened = lastScreenings.holdsAny(movie._id))
    }
    changed
  }

  /** Rebuild the screenings rows of one venue-only apply, by card ([[VenuePlans]]). */
  def writeVenues(card: String, venues: Seq[ReadModelProjection.VenueScreening]): Unit = screeningRows.applyVenues(card, venues)

  /** Remove a card and the screenings this process remembers for it; `audit` names the path on the removal-audit line. */
  def remove(cardId: String, audit: String): Unit = {
    val screeningIds = lastScreenings.ids(cardId)
    writer.deleteMovie(cardId)
    // The card is gone from here on, so its share-card files go and the memo forgets it even if a
    // screenings delete then throws — remembered, the row coming back unchanged would skip writing
    // the card, and nothing would retire its files again. Screenings left behind belong to no live
    // card; the prune removes them while it stays retired.
    shareCardGate.cardRetired(cardId)
    tools.AnyThrowable.rethrownAfter(screeningIds.foreach(writer.deleteScreening))(_ => forget(cardId))
    // The point a film actually leaves the served site (web_movies + its
    // web_screenings). During a "cards vanish" episode this names every dropped
    // filmId, one INFO line each — the read-model half of the removal audit.
    services.movies.RemovalAudit.cardRemoved(cardId, screeningIds.size, reason = audit)
    metrics.recordWrite(Target.Movie, Op.Delete, 1)
    if (screeningIds.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, screeningIds.size)
    forget(cardId)
    // The card's metadata stays in [[CardProjection]]'s memo, which is keyed by SOURCE ROW id: several rows can
    // collapse onto one card, so there is no card→row mapping to remove by. The prune sweep retains exactly the
    // live row keys instead ([[ProjectionMemo.retainRows]]).
  }

  /** Delete a screenings row a sweep found orphaned, and forget it. */
  def removeScreening(ref: ScreeningRef): Unit = {
    writer.deleteScreening(ref._id)
    metrics.recordWrite(Target.Screening, Op.Delete, 1)
    lastScreenings.forget(ref.filmId, ref._id)
  }

  /** Forget a card as written, so its next projection writes it whatever the memo held. */
  def forget(cardId: String): Unit = {
    lastMovie.remove(cardId)
    shareCardGate.forget(cardId)
    lastScreenings.forgetCard(cardId)
  }

  /** Forget one screenings row as written, by its id — its card is whichever memo entry holds the id. */
  def forgetScreening(screeningId: String): Unit = lastScreenings.forget(screeningId)

  /** A card the read model serves, as the boot seed reads it: remembered as written, and its share card's pending state
   *  told to the gate. */
  def seed(movie: ResolvedMovie): Unit = {
    lastMovie.update(movie._id, CardHash.of(movie))
    if (movie.shareCardPending) shareCardGate.seedPending(movie._id)
  }

  /** Write a card whose hash moved to `hash`: the parts that moved. */
  private def upsert(movie: ResolvedMovie, hash: CardHash): Set[String] = {
    writer.upsertMovie(movie)
    metrics.recordWrite(Target.Movie, Op.Upsert, 1)
    val parts = lastMovie.get(movie._id).fold(Set.empty[String])(_.partsDifferingFrom(hash))
    metrics.recordCardWrite(parts)
    parts
  }

  /** Remember a card as written at `hash`, and tell the share-card gate. */
  private def remember(movie: ResolvedMovie, hash: CardHash, screened: Boolean): Unit = {
    lastMovie.update(movie._id, hash)
    shareCardGate.cardWritten(movie, screened = screened)
  }
}
