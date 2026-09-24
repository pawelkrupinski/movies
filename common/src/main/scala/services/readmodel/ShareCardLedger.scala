package services.readmodel

import models.ResolvedMovie

/**
 * What the read-model projection asks of the share-card store — the seam between the pure
 * projection (common) and the worker's rendered-card files (which only the worker can see).
 *
 * The projection writes `ResolvedMovie.shareCard` from [[current]], so the web learns a card's
 * file name from `web_movies` and never touches the filesystem. [[onProjected]] is where a
 * change to a card's inputs turns into a render; [[readyToPublish]] and [[requestFirstCard]]
 * are the FIRST-PUBLISH GATE: a link-preview scraper (Facebook above all) fetches `og:image`
 * once and caches the answer for about a month, so a film that goes public before its card
 * exists keeps the fallback image wherever it was shared in that window. The projector holds a
 * brand-new card back (bounded, see [[ReadModelProjector]]) until [[readyToPublish]] says yes.
 */
trait ShareCardLedger {

  /** The card file for `movie`, as its `web_movies` document should carry it: the card for its
   *  current inputs when that one exists, else the last card it had while the new one renders,
   *  else none. */
  def current(movie: ResolvedMovie): Option[String]

  /** True when `movie` may go public now: every card it should have exists, or it will never
   *  have one (no share cards in this deployment). */
  def readyToPublish(movie: ResolvedMovie): Boolean

  /** Render `movie`'s cards ahead of everything else, then have the projection publish it —
   *  the first-publish gate's request, made on every projection while the card is held. `until`
   *  is when its hold ends: the projection must be asked again then (see
   *  [[ReadModelProjector.releaseExpiredHolds]]). Must not block. */
  def requestFirstCard(movie: ResolvedMovie, until: java.time.Instant): Unit

  /** A card published by an expired hold (`shareCardPending`) has its share card now — the
   *  preview scrapers that cached the fallback should be asked to look again. */
  def onPendingCardLanded(filmId: String): Unit

  /** A card's document was written with new content. `screened` is whether it has any
   *  screenings (a film with none is not served, so it gets no card). */
  def onProjected(movie: ResolvedMovie, screened: Boolean): Unit
}

object ShareCardLedger {
  /** No share cards: nothing is current, nothing is ever held, nothing is rendered. What the
   *  projection runs with wherever the worker has no card directory (tests, fixtures, dev). */
  val none: ShareCardLedger = new ShareCardLedger {
    def current(movie: ResolvedMovie): Option[String]        = None
    def readyToPublish(movie: ResolvedMovie): Boolean        = true
    def requestFirstCard(movie: ResolvedMovie, until: java.time.Instant): Unit = ()
    def onPendingCardLanded(filmId: String): Unit            = ()
    def onProjected(movie: ResolvedMovie, screened: Boolean): Unit = ()
  }
}
