package services.readmodel

import scala.collection.mutable

/**
 * What the projector last wrote for each card's screenings rows ([[WrittenScreening]] by row id),
 * held without a string per row.
 *
 * A row's id is `<card>|<city>|<venue>` (`ReadModelProjection.VenueScreening`): the card is the
 * memo's own key, and `<city>|<venue>` is one of a few thousand venues'. Held whole, the ids were
 * 99k distinct strings — 10.8 MB of the US worker's live heap (dump 2026-09-29). So each row is
 * filed under its `<city>|<venue>` suffix, interned, and its id rebuilt when asked. An id that
 * does not start with its card (none the projector writes, but a stored legacy row could) is
 * kept whole behind [[Whole]], so every id reads back exactly as it was put.
 *
 * Not thread-safe: the projector touches it only under its own lock.
 */
private[readmodel] final class ScreeningMemo {
  import ScreeningMemo.Whole

  private val byCard   = mutable.HashMap.empty[String, Map[String, WrittenScreening]]
  private val suffixes = mutable.HashMap.empty[String, String]

  private def local(card: String, id: String): String =
    if (id.length > card.length + 1 && id.startsWith(card) && id.charAt(card.length) == '|') {
      val suffix = id.substring(card.length + 1)
      suffixes.getOrElseUpdate(suffix, suffix)
    } else s"$Whole$id"

  private def full(card: String, key: String): String =
    if (key.nonEmpty && key.charAt(0) == Whole) key.substring(1) else s"$card|$key"

  /** The card's rows by id, as last written — empty when it has none. */
  def of(card: String): Map[String, WrittenScreening] =
    byCard.get(card).fold(Map.empty[String, WrittenScreening])(_.map { case (key, written) => full(card, key) -> written })

  def ids(card: String): Seq[String] = byCard.get(card).fold(Seq.empty[String])(_.keys.map(full(card, _)).toSeq)

  def holdsAny(card: String): Boolean = byCard.get(card).exists(_.nonEmpty)

  /** Replace what the card's rows were last written as; no rows forgets the card. */
  def update(card: String, byId: Map[String, WrittenScreening]): Unit =
    if (byId.isEmpty) byCard.remove(card) else byCard.update(card, byId.map { case (id, written) => local(card, id) -> written })

  def forgetCard(card: String): Unit = byCard.remove(card)

  /** Forget one row of a known card. */
  def forget(card: String, id: String): Unit = byCard.updateWith(card)(_.map(_ - local(card, id)).filter(_.nonEmpty))

  /** Forget one row, whichever card holds it. */
  def forget(id: String): Unit =
    byCard.keys.toSeq.foreach(card => if (byCard.get(card).exists(_.contains(local(card, id)))) forget(card, id))

  /** Does any card hold this row? */
  def holds(id: String): Boolean = byCard.exists { case (card, rows) => rows.contains(local(card, id)) }
}

private[readmodel] object ScreeningMemo {
  /** Marks a key holding a whole id — a character no card or venue name carries. */
  private val Whole: Char = '\u0000'
}
