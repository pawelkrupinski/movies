package services.movies

import models.Showtime
import services.cinemas.CountryNames

import java.net.URI
import java.util.Locale
import scala.util.Try

/**
 * The fields a venue publishes, as a cinema slot may hold them — one rule per field for every path
 * that writes a slot: the listing ([[CinemaSlotBuilder]]) and a detail page landing on it
 * (`EnrichDetailsHandler`). A parser hands the pipeline whatever its source printed; these are
 * where it becomes something a card can serve, so a client cannot leak a spelling, a list or a
 * link the card would show broken.
 *
 * Found by the served-output invariants (`ServedOutputInvariantsSpec`) over every country's corpus:
 * a detail page's "Niderlandy" beside the listing's "Holandia" and its "Canada" beside "Kanada";
 * "Biograficzny/Muzyczny" served as one genre; booking links with a space in the path (AMC), a
 * backslash in the query (every Agile Ticketing venue), `https:\\` for `https://` (Marquee), an
 * unfilled `<perdcode>` template (Roxy Ulverston), `https://-/…` (Act One Acton), a blank string
 * (South Hill Park), a path with no host (Woodstock Community Theatre); a venue printing one
 * screening twice, once with its booking link and once with a link to its home page (Bilety24,
 * Syndicated Bar & Theatre).
 */
object SlotFields {

  /** Production countries in the deployment's language — each folded to the spelling
   *  [[CountryNames]] canonicalises it to, once. */
  def countries(raw: Seq[String], language: Locale): Seq[String] =
    raw.map(_.trim).filter(_.nonEmpty).map(CountryNames.canonical(_, language)).distinct

  private val GenreSeparator = """\s*[/,|;]\s*""".r

  /** Genre labels one each: a source printing "Biograficzny/Muzyczny" or "Dramat, Komedia" as one
   *  label is two genres. Kept in order, each once (case-insensitively). */
  def genres(raw: Seq[String]): Seq[String] = {
    val seen = scala.collection.mutable.HashSet.empty[String]
    raw.flatMap(GenreSeparator.split).map(_.trim).filter(g => g.nonEmpty && seen.add(g.toLowerCase(Locale.ROOT)))
  }

  /** Characters RFC 3986 allows nowhere in a URL, which a browser escapes and `java.net.URI`,
   *  Swift's `URL(string:)` and a crawler refuse: a source pasting a raw space or backslash is
   *  escaped here, so every reader follows the link alike. */
  private val Unescaped: Map[Char, String] =
    Map(' ' -> "%20", '\\' -> "%5C", '|' -> "%7C", '^' -> "%5E", '`' -> "%60", '{' -> "%7B", '}' -> "%7D")

  /** A link a venue published, as one a card may serve: trimmed, a `https:\\` written as the
   *  `https://` it means, the characters a URL may not hold escaped, a path or query relative to
   *  the venue's page (`base`) resolved against it — or nothing, where what is left is still no
   *  link a browser follows (blank, a template placeholder, no host). */
  def url(raw: String, base: Option[String] = None): Option[String] =
    if (plain(raw)) Some(raw) else repaired(raw, base)

  /** The shape nearly every link has — `http(s)://` and a plain host, then only characters a URL
   *  may hold — answered without a parse: this runs on every booking link of every build (the US
   *  corpus holds ~1.5M). */
  private def plain(url: String): Boolean = {
    val start = if (url.startsWith("https://")) 8 else if (url.startsWith("http://")) 7 else -1
    start > 0 && {
      var i = start
      while (i < url.length && { val c = url.charAt(i); (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' || c == '-' || (c >= 'A' && c <= 'Z') }) i += 1
      val hostEnd = i
      hostEnd > start && url.charAt(start) != '-' && url.charAt(start) != '.' &&
        (hostEnd == url.length || { val c = url.charAt(hostEnd); c == '/' || c == '?' || c == '#' }) && {
          while (i < url.length && { val c = url.charAt(i); c > ' ' && c < 0x7f && c != '"' && c != '<' && c != '>' && !Unescaped.contains(c) }) i += 1
          i == url.length
        }
    }
  }

  private def repaired(raw: String, base: Option[String]): Option[String] = {
    val trimmed = raw.trim
    val slashed =
      if (trimmed.regionMatches(true, 0, "https:\\\\", 0, 8) || trimmed.regionMatches(true, 0, "http:\\\\", 0, 7))
        trimmed.replace('\\', '/')
      else trimmed
    val escaped = slashed.flatMap(c => Unescaped.getOrElse(c, c.toString))
    val absolute =
      if (escaped.isEmpty || isHttp(escaped)) escaped
      else if (escaped.startsWith("/") || escaped.startsWith("?"))
        base.filter(followable).flatMap(b => Try(URI.create(b).resolve(escaped).toString).toOption).getOrElse(escaped)
      else escaped
    Option(absolute).filter(followable)
  }

  def url(raw: Option[String], base: Option[String]): Option[String] = raw.flatMap(url(_, base))

  private def isHttp(url: String): Boolean =
    url.regionMatches(true, 0, "https://", 0, 8) || url.regionMatches(true, 0, "http://", 0, 7)

  /** A URL every reader follows: http(s), nothing in it a URL may not hold, and an authority that
   *  parses as a host (`https://-/…` and `https:///…` name none). */
  def followable(url: String): Boolean =
    isHttp(url) && !url.exists(c => c.isWhitespace || c == '"' || c == '<' || c == '>' || Unescaped.contains(c)) &&
      Try(new URI(url)).toOption.exists(u => Option(u.getHost).exists(_.nonEmpty))

  /** A link to ONE page of the venue's site rather than its home page — a booking link names the
   *  screening; `https://www.bilety24.pl#` or `https://www.syndicatedbk.com` names nothing. */
  def specific(url: String): Boolean =
    Try(new URI(url)).toOption.exists(u => Option(u.getRawPath).exists(p => p.nonEmpty && p != "/") || Option(u.getRawQuery).exists(_.nonEmpty))

  /** A listing's showtimes with their booking links as [[url]] makes them, and each screening the
   *  listing printed twice once. Two showtimes of one slot (start, room, format) are one screening
   *  unless each names a DIFFERENT screening by its own specific link — Scott Cinemas and Cinemark
   *  sell parallel screens at one start under their own perfcode / screen id, Helios premieres in
   *  several halls under their own `/screen/<uuid>`. A copy with no link, the same link, or a link
   *  to the home page is the same screening printed again, and the copy carrying the most specific
   *  link stands for it. */
  def showtimes(raw: Seq[Showtime], base: Option[String]): Seq[Showtime] = {
    val linked = raw.map(st => st.bookingUrl match {
      case None    => st
      case Some(u) => val fixed = url(u, base); if (fixed == st.bookingUrl) st else st.copy(bookingUrl = fixed)
    })
    def slotOf(st: Showtime) = (st.dateTime, st.room, st.format)
    val seen = scala.collection.mutable.HashSet.empty[(java.time.LocalDateTime, Option[String], List[String])]
    if (linked.forall(st => seen.add(slotOf(st)))) linked   // the common case: no slot printed twice
    else {
      val bySlot = linked.indices.groupBy(i => slotOf(linked(i)))
      {
        // Per slot, the copies that stand: the first of each specific link, or — where no copy names
        // its own screening — the one with the lowest-ranked link.
        val kept = bySlot.valuesIterator.flatMap { slot =>
          if (slot.sizeIs == 1) slot
          else if (slot.exists(i => namesItsScreening(linked(i)))) slot.filter(i => namesItsScreening(linked(i))).distinctBy(linked(_).bookingUrl)
          else Seq(slot.minBy(i => rank(linked(i))))
        }.toSet
        linked.indices.collect { case i if kept(i) => linked(i) }
      }
    }
  }

  private def namesItsScreening(st: Showtime): Boolean = st.bookingUrl.exists(specific)

  private def rank(st: Showtime): (Boolean, String) = (st.bookingUrl.isEmpty, st.bookingUrl.getOrElse(""))
}
