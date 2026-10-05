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
  // How many keys hold a whole id, across every card: only while there are any must a lookup by id
  // alone look at every card, rather than just the cards its own `|`-prefixes name.
  private var wholeKeys = 0

  private def isWhole(key: String): Boolean = key.nonEmpty && key.charAt(0) == Whole
  private def wholeCount(rows: Map[String, WrittenScreening]): Int = rows.keysIterator.count(isWhole)

  private def ownsId(card: String, id: String): Boolean =
    id.length > card.length + 1 && id.startsWith(card) && id.charAt(card.length) == '|'

  /** `id`'s key under `card`, its suffix interned — for a row being stored. */
  private def local(card: String, id: String): String =
    if (ownsId(card, id)) {
      val suffix = id.substring(card.length + 1)
      suffixes.getOrElseUpdate(suffix, suffix)
    } else s"$Whole$id"

  /** `id`'s key under `card`, for a lookup — nothing interned. */
  private def lookupKey(card: String, id: String): String =
    if (ownsId(card, id)) id.substring(card.length + 1) else s"$Whole$id"

  private def full(card: String, key: String): String =
    if (isWhole(key)) key.substring(1) else s"$card|$key"

  /** The cards that may hold `id`: each of its `|`-prefixes that is a card (an id the projector
   *  writes is `<card>|<suffix>`), and — only while some key holds a whole id — every card. */
  private def cardsHolding(id: String): Iterator[String] = {
    val owners = Iterator.iterate(id.indexOf('|'))(at => id.indexOf('|', at + 1)).takeWhile(_ > 0)
      .map(id.substring(0, _)).filter(card => byCard.get(card).exists(_.contains(lookupKey(card, id))))
    if (wholeKeys == 0) owners
    else {
      val whole = s"$Whole$id"
      (owners ++ byCard.iterator.collect { case (card, rows) if rows.contains(whole) => card }).distinct
    }
  }

  /** The card's rows by id, as last written — empty when it has none. */
  def of(card: String): Map[String, WrittenScreening] =
    byCard.get(card).fold(Map.empty[String, WrittenScreening])(_.map { case (key, written) => full(card, key) -> written })

  /** One row of the card, as last written — [[of]] without rebuilding every id of the card. */
  def get(card: String, id: String): Option[WrittenScreening] = byCard.get(card).flatMap(_.get(lookupKey(card, id)))

  def contains(card: String, id: String): Boolean = get(card, id).isDefined

  /** How many rows of the card are remembered. */
  def size(card: String): Int = byCard.get(card).fold(0)(_.size)

  /** Replace some of the card's rows, keeping the rest — [[update]] of `of(card) ++ rows` without
   *  rebuilding every id of the card. */
  def updateRows(card: String, rows: Iterable[(String, WrittenScreening)]): Unit =
    if (rows.nonEmpty) {
      val before = byCard.getOrElse(card, Map.empty)
      val added  = rows.iterator.map { case (id, written) => local(card, id) -> written }.toMap
      // Counted over the rows handed in, not the whole card twice: a venue apply touches a few rows of thousands.
      wholeKeys += added.keysIterator.count(key => isWhole(key) && !before.contains(key))
      byCard.update(card, before ++ added)
    }

  def ids(card: String): Seq[String] = byCard.get(card).fold(Seq.empty[String])(_.keys.map(full(card, _)).toSeq)

  def holdsAny(card: String): Boolean = byCard.get(card).exists(_.nonEmpty)

  /** Replace what the card's rows were last written as; no rows forgets the card. A row written as it was is kept as the
   *  object held, and the card's rows are moved only where a row came, went or changed: a whole-film reprojection hands
   *  every row of the card anew (~3.9k on worker-us, 321 an hour), almost all as they were, and the card's rows replaced
   *  whole lived until its next one — minutes, so promoted to the old generation each time, to die there. */
  def update(card: String, byId: Map[String, WrittenScreening]): Unit =
    if (byId.isEmpty) forgetCard(card)
    else byCard.get(card) match {
      case None =>
        val rows = byId.map { case (id, written) => local(card, id) -> written }
        wholeKeys += wholeCount(rows)
        byCard.update(card, rows)
      case Some(before) =>
        val now     = byId.iterator.map { case (id, written) => lookupKey(card, id) -> (id, written) }.toMap
        val gone    = before.keysIterator.filterNot(now.contains).toSeq
        val changed = now.iterator.collect { case (key, (id, written)) if !before.get(key).contains(written) => local(card, id) -> written }.toSeq
        if (gone.nonEmpty || changed.nonEmpty) {
          val rows = before -- gone ++ changed
          wholeKeys += wholeCount(rows) - wholeCount(before)
          byCard.update(card, rows)
        }
    }

  def forgetCard(card: String): Unit = byCard.remove(card).foreach(rows => wholeKeys -= wholeCount(rows))

  /** Forget one row of a known card. */
  def forget(card: String, id: String): Unit = {
    val key = lookupKey(card, id)
    byCard.get(card).filter(_.contains(key)).foreach { rows =>
      if (isWhole(key)) wholeKeys -= 1
      val rest = rows - key
      if (rest.isEmpty) byCard.remove(card) else byCard.update(card, rest)
    }
  }

  /** Forget one row, whichever card holds it. */
  def forget(id: String): Unit = cardsHolding(id).toList.foreach(forget(_, id))

  /** Does any card hold this row? */
  def holds(id: String): Boolean = cardsHolding(id).hasNext
}

private[readmodel] object ScreeningMemo {
  /** Marks a key holding a whole id — a character no card or venue name carries. */
  private val Whole: Char = '\u0000'
}
