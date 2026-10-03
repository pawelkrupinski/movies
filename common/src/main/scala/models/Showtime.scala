package models

import java.nio.charset.StandardCharsets.UTF_8
import java.time.LocalDateTime

/**
 * One screening: when, where to book it, which room, which format.
 *
 * A CASE CLASS IN ALL BUT NAME — `apply`, `copy`, structural equality and the case-class
 * `toString` all behave as one's would — written by hand for how it holds its booking URL.
 * As a scraper or a decode builds it, the URL is whole. Once [[withUrlPrefix]] has split it,
 * it is a prefix shared with the rest of its row plus the UTF-8 bytes after it: a cinema's
 * booking URLs differ only in their last few characters, and web-us holds a corpus of them
 * (New York alone ~50k), so a String and a `Some` each were ~70% of what a showtime cost its
 * heap. Split, New York's cost ~44 bytes instead of ~150 (measured 2026-10-03). Two showtimes
 * are equal by the URL they spell, however each holds it.
 *
 * The persisted shapes are unchanged: `ShowtimeCodec` (BSON) and `ShowtimeJson` write the
 * fields the case class's macros wrote.
 */
final class Showtime private (
  val dateTime: LocalDateTime,
  // Null unless split; then a shared instance.
  private val urlPrefix: String,
  // Null: no URL. A `Some[String]`: the whole URL, as given. An `Array[Byte]`: the UTF-8 of
  // the URL after `urlPrefix`.
  private val urlRest: AnyRef,
  val room: Option[String],
  // Format tokens, e.g. List("2D","NAP","ATMOS") or List("IMAX","2D"). Empty when unknown.
  // Stored token-wise (not as "2D/NAP/ATMOS") so different cinemas' separators
  // (Helios "/", Cinema City " ") don't leak into deduplication logic.
  val format: List[String]
) extends Product with Serializable {

  def bookingUrl: Option[String] = urlRest match {
    case null              => None
    case _: Array[Byte] if urlPrefix eq Showtime.AwaitingPrefix => None
    case rest: Array[Byte] => Some(urlPrefix + new String(rest, UTF_8))
    case whole             => whole.asInstanceOf[Some[String]]
  }

  /** The shared prefix the URL is held split at — `None` while it is held whole. */
  def urlSplitPrefix: Option[String] = Option(urlPrefix)

  /** This showtime with its URL held split at `prefix` — kept by reference, so pass a shared
   *  instance — or itself when it has no URL that starts with it. */
  def withUrlPrefix(prefix: String): Showtime =
    if (urlPrefix eq prefix) this
    else if (urlPrefix != null && urlPrefix == prefix) new Showtime(dateTime, prefix, urlRest, room, format)
    else bookingUrl match {
      case Some(url) if url.startsWith(prefix) =>
        new Showtime(dateTime, prefix, url.substring(prefix.length).getBytes(UTF_8), room, format)
      case _ => this
    }

  /** Decoded from a row whose `bookingUrlPrefix` had not been read yet: its URL is a remainder
   *  with no prefix, which [[withRowPrefix]] completes. Never outlives the decode. */
  def awaitsRowPrefix: Boolean = urlPrefix eq Showtime.AwaitingPrefix

  /** A showtime that [[awaitsRowPrefix]] with its row's prefix put in front of its remainder —
   *  or with no URL at all when the row stored none, rather than a remainder posing as a URL.
   *  Any other showtime as it is. */
  def withRowPrefix(prefix: String): Showtime =
    if (!awaitsRowPrefix) this
    else if (prefix == null) new Showtime(dateTime, null, null, room, format)
    else new Showtime(dateTime, prefix, urlRest, room, format)

  /** Is this showtime still worth showing at `now`? True until [[Showtime.Grace]]
   *  past its start, so a film whose screening just began still lists for a short
   *  window rather than vanishing mid-session. The single rule the web's
   *  `toSchedules` filters list views by and the worker's source-films gauge
   *  counts by — keeping the two apples-to-apples (no drift on the grace edge). */
  def isUpcoming(now: LocalDateTime): Boolean = dateTime.isAfter(now.minus(Showtime.Grace))

  /** As a case class's `copy` — except that leaving `bookingUrl` out keeps the URL as it is
   *  held, split or whole, rather than spelling it out and holding it whole again. */
  def copy(dateTime: LocalDateTime = dateTime, bookingUrl: Option[String] = null,
           room: Option[String] = room, format: List[String] = format): Showtime =
    if (bookingUrl == null) new Showtime(dateTime, urlPrefix, urlRest, room, format)
    else Showtime(dateTime, bookingUrl, room, format)

  override def equals(other: Any): Boolean = other match {
    case that: Showtime =>
      (this eq that) ||
        (dateTime == that.dateTime && sameUrl(that) && room == that.room && format == that.format)
    case _ => false
  }

  private def sameUrl(that: Showtime): Boolean =
    if (urlRest == null || that.urlRest == null) urlRest == null && that.urlRest == null
    else (urlRest, that.urlRest) match {
      case (a: Array[Byte], b: Array[Byte]) if urlPrefix eq that.urlPrefix => java.util.Arrays.equals(a, b)
      case (a: Some[?], b: Some[?])                                        => a == b
      case _                                                               => bookingUrl == that.bookingUrl
    }

  override def hashCode: Int =
    31 * (31 * (31 * dateTime.hashCode + urlHash) + room.hashCode) + format.hashCode

  /** The URL's `String.hashCode`, whichever way it is held — without spelling it out when
   *  its remainder is ASCII, as a booking URL's is. */
  private def urlHash: Int = urlRest match {
    case null => 0
    case rest: Array[Byte] if rest.forall(_ >= 0) =>
      var h = urlPrefix.hashCode
      var i = 0
      while (i < rest.length) { h = 31 * h + rest(i); i += 1 }
      h
    case _ => bookingUrl.get.hashCode
  }

  override def toString: String = scala.runtime.ScalaRunTime._toString(this)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Showtime]
  override def productArity: Int           = 4
  override def productPrefix: String       = "Showtime"
  override def productElement(n: Int): Any = n match {
    case 0 => dateTime
    case 1 => bookingUrl
    case 2 => room
    case 3 => format
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
  override def productElementName(n: Int): String = n match {
    case 0 => "dateTime"
    case 1 => "bookingUrl"
    case 2 => "room"
    case 3 => "format"
    case _ => throw new IndexOutOfBoundsException(n.toString)
  }
}

object Showtime {
  import java.time.Duration

  def apply(dateTime: LocalDateTime, bookingUrl: Option[String], room: Option[String] = None,
            format: List[String] = Nil): Showtime =
    new Showtime(dateTime, null, if (bookingUrl == null || bookingUrl.isEmpty) null else bookingUrl, room, format)

  /** A showtime as a row stores it: its URL split at `prefix`, the row's `bookingUrlPrefix`, with
   *  `rest` after it. A null `prefix` — the row's not read yet — leaves it [[Showtime.awaitsRowPrefix]]. */
  def stored(dateTime: LocalDateTime, prefix: String, rest: String, room: Option[String], format: List[String]): Showtime =
    new Showtime(dateTime, if (prefix == null) AwaitingPrefix else prefix, rest.getBytes(UTF_8), room, format)

  /** Stands in for a row prefix not read yet — compared by identity, never spelled out. */
  private val AwaitingPrefix: String = new String("")

  /** How long after a showtime starts it still counts as "upcoming" — a screening
   *  that began up to 30 min ago is still listed/counted, then drops. */
  val Grace: Duration = Duration.ofMinutes(30)

  /** The prefix every booking URL of `slots` shares — or empty when fewer than two have
   *  one, where a shared prefix would save nothing. */
  def commonUrlPrefix(slots: Iterable[Showtime]): String = {
    var first: String = null
    var length = 0
    var count  = 0
    for (slot <- slots; url <- slot.bookingUrl) {
      if (first == null) { first = url; length = url.length }
      else {
        val limit = math.min(length, url.length)
        var i = 0
        while (i < limit && first.charAt(i) == url.charAt(i)) i += 1
        length = i
      }
      count += 1
    }
    // Never between the two halves of a surrogate pair: each half would be written on its own,
    // and UTF-8 spells a lone half as '?'.
    if (length > 0 && first != null && Character.isHighSurrogate(first.charAt(length - 1))) length -= 1
    if (count < 2) "" else first.substring(0, length)
  }
}
