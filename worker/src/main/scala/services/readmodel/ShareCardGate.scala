package services.readmodel

import models.ResolvedMovie
import play.api.Logging
import settings.ShareCardFirstHold

import java.util.concurrent.TimeUnit

/**
 * THE FIRST-PUBLISH GATE, and the projector's one face of the share-card store ([[ShareCardLedger]]).
 *
 * A link-preview scraper caches `og:image` for about a month on its first fetch, so a card must not
 * go public before its share card exists — but no film waits longer than `firstCardHold` for one.
 * The gate keeps, per card not yet served, its source row and the instant its hold ends ([[HeldCard]]),
 * and the cards published by an expired hold that still have no share card (`shareCardPending` on
 * their documents, seeded from them at boot, so a later projection keeps the mark until the card
 * lands).
 *
 * What the gate does NOT decide is whether a card is served — that is the projector's memo of what it
 * wrote — nor when to re-project a row: it answers which rows' holds ran out ([[expiredRows]]), and the
 * projector projects them. Not thread-safe: its one owner, the projector, calls it under its lock.
 */
final class ShareCardGate(ledger: ShareCardLedger, firstCardHold: ShareCardFirstHold) extends Logging {

  private val held         = scala.collection.mutable.Map.empty[String, HeldCard]
  private val pendingCards = scala.collection.mutable.Set.empty[String]

  /** Whether any card is held. */
  def holdsAny: Boolean = held.nonEmpty

  /** Whether `card` is held. */
  def holds(card: String): Boolean = held.contains(card)

  /** The cards the gate is holding. */
  def heldCards: Set[String] = held.keySet.toSet

  /** The source row behind held card `card`. */
  def heldRow(card: String): Option[String] = held.get(card).map(_.row)

  /** The document to write for `projected` — its share card filled in — or None while the gate
   *  holds it back.
   *
   *  Only a card that is about to be SERVED for the first time — it has screenings, and the read model
   *  does not hold it `served` — is gated: a card whose inputs merely CHANGE keeps its current share
   *  card until the new one exists, which [[ShareCardLedger.current]] already answers. That covers a
   *  film coming BACK on screen: its document outlived its screenings (and the daily prune its card),
   *  and nothing about the document changes when they return. A held card is asked for its share card
   *  on every projection, and published with `shareCardPending` once its hold has run out. */
  def admit(rowId: String, projected: ResolvedMovie, screened: Boolean, served: Boolean, now: Long): Option[ResolvedMovie] = {
    val id       = projected._id
    val firstOne = screened && !served && !ledger.readyToPublish(projected)
    val hold     = Option.when(firstOne)(held.getOrElseUpdate(id, HeldCard(rowId, now + firstCardHold.value.toMillis)))
    if (!firstOne) held.remove(id)
    val expired  = hold.exists(_.until <= now)
    if (firstOne && !expired) {
      ledger.requestFirstCard(projected, java.time.Instant.ofEpochMilli(hold.get.until))
      None
    } else {
      if (expired) {
        held.remove(id)
        pendingCards += id
        logger.warn(s"share card: published $id without its card — none was ready within ${firstCardHold.value.toSeconds}s.")
      }
      Some(withShareCard(projected))
    }
  }

  /** `projected` as the read model serves it: its current share card filled in. A card published
   *  pending (see [[admit]]) whose share card has landed since stops being pending, and the ledger
   *  hears of it. */
  def withShareCard(projected: ResolvedMovie): ResolvedMovie = {
    val id        = projected._id
    val shareCard = ledger.current(projected)
    if (shareCard.nonEmpty && pendingCards.remove(id)) ledger.onPendingCardLanded(id)
    projected.copy(shareCard = shareCard, shareCardPending = pendingCards.contains(id))
  }

  /** Forget the holds on `rowId`'s cards that its projection no longer produces — it lost its
   *  readiness, or its display title moved to another card. Only [[admit]] releases a hold on a card
   *  still produced; a hold nothing produces was otherwise never released, and once expired it cost a
   *  read and a full re-projection of its row on every change event. */
  def dropHoldsNotProducedBy(rowId: String, produced: Set[String]): Unit =
    held.filterInPlace((card, hold) => hold.row != rowId || produced(card))

  /** End the hold on `card` at `now` — its share card can't be made — returning its row to project. */
  def expireNow(card: String, now: Long): Option[String] =
    held.get(card).map { hold =>
      held.update(card, hold.copy(until = now))
      hold.row
    }

  /** The rows of every hold that has run out by `now`. */
  def expiredRows(now: Long): Set[String] =
    held.valuesIterator.filter(_.until <= now).map(_.row).toSet

  /** A card the read model already serves as published pending (`shareCardPending`), read at boot. */
  def seedPending(card: String): Unit = { pendingCards += card; () }

  /** `card` left the read model: no hold, no pending mark. */
  def forget(card: String): Unit = { held.remove(card); pendingCards.remove(card); () }

  /** A card was written to the read model — where a change to its inputs turns into a render. */
  def cardWritten(movie: ResolvedMovie, screened: Boolean): Unit = ledger.onProjected(movie, screened = screened)

  /** A card was retired from the read model: its share-card files go. */
  def cardRetired(card: String): Unit = ledger.onRetired(card)
}

object ShareCardGate {
  /** Two minutes: a render is a poster fetch (up to ~35s against a slow cinema origin) plus a
   *  composite, so this covers a cold one with room to spare while keeping a new film's first
   *  appearance close to its scrape. */
  val DefaultFirstCardHold: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.Duration(120L, TimeUnit.SECONDS)

  /** No share cards: every card is ready to publish, so nothing is ever held. */
  def ungated: ShareCardGate = new ShareCardGate(ShareCardLedger.none, ShareCardFirstHold(DefaultFirstCardHold))
}

/** A card held back by the first-publish gate: the source row that projects it, and when its
 *  hold ends (epoch millis). */
private[readmodel] final case class HeldCard(row: String, until: Long)
