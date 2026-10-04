package controllers

import java.time.{LocalDate, ZoneId}

import models.{Cinema, Showtime}

/**
 * The repeated parts of `_filmShowings`: one showtime pill, and what its cinema group
 * and day carry so the pill does not have to.
 *
 * WHY IT IS THIS TERSE. A city listing renders every slot of every film for every
 * day it knows — 52,577 pills on New York on 2026-10-02, 19.9 MB of HTML — and each
 * render of that page allocated ~320 MB straight into web-us's old generation. Every
 * byte here is paid tens of thousands of times, so the server sends each fact once
 * and `_showingsHydrate` puts the repeats back in the browser before anything reads
 * them:
 *
 *  - NO `data-time`: the pill's own text starts with the time (`_repertoireView`'s
 *    `slotTime` reads the first text node).
 *  - NO `data-expires` on an ordinary slot: the enclosing `.date-group` carries
 *    [[expiresFrom]], and the slot lapses at that plus its clock time. A slot whose
 *    expiry that sum would get WRONG — a DST changeover day, a past-midnight
 *    screening filed under the previous day, a time with seconds — keeps its own
 *    `data-expires`, so the rule is exact by construction, not approximately right.
 *  - A BOOKING URL AS A SUFFIX: the cinema group carries the URL prefix its slots
 *    share (`Showtime.commonUrlPrefix`, `data-u`), each pill the rest (`data-s`) — and nothing a
 *    link needs, since it is not one until its href exists: the browser adds
 *    `badge-time`, `target` and `nofollow` with it (2.5 MB of New York's 10.7 MB). The
 *    visible time stays server-rendered; only the outbound, `nofollow` link is put
 *    together in the browser, which a crawler renders anyway and which passes us
 *    nothing either way.
 *  - NO `data-cinema`: the name is the group's visible label, which hydration copies
 *    into `data-cinema` for the filters. The label stays server-rendered text, so a
 *    search engine reads every cinema the page lists.
 *  - The cinema's own page for the film ONCE per film: only its first listing
 *    ([[firstListings]]) carries the link; later days' labels are a bare `<a>` that
 *    borrows its `href`, class, target and `nofollow`.
 *  - `rel="nofollow"` WITHOUT `noopener`: every browser the site supports applies
 *    `noopener` to a `target="_blank"` link on its own. `nofollow` stays — it is what
 *    tells a crawler these booking links are plumbing, not endorsements.
 */
object ShowingsMarkup {

  /** The epoch millis at which a slot at 00:00 on `date` stops counting as upcoming
   *  (`Showtime.Grace` after it), in the city's zone — `.date-group[data-expires-from]`. */
  def expiresFrom(date: LocalDate, zone: ZoneId): Long =
    date.atStartOfDay.plus(Showtime.Grace).atZone(zone).toInstant.toEpochMilli

  /** For each cinema of `showings`, the first day it is listed on: the one listing whose
   *  label carries the cinema's page for the film as a real `href`. */
  def firstListings(showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): Map[Cinema, LocalDate] =
    showings.flatMap { case (date, cinemas) => cinemas.map(_.cinema -> date) }
      .groupMapReduce(_._1)(_._2)((a, b) => if (a.isBefore(b)) a else b)

  /** A film's whole showings tree — every day, cinema group and pill `_filmShowings`
   *  lists — streamed: written straight into the response's own buffer as the body
   *  is written (see [[StreamedHtml]]), flushed after each cinema group, so no film's
   *  markup is ever held whole. (A listing keeps whole cards across renders, showings
   *  included: `FilmCardFragments`.)
   *
   *  WHY NOT TWIRL. As a template this was a fragment object per static run of markup
   *  per loop iteration, plus an escaped `Html` per interpolated value: on a
   *  New-York-sized listing (27k cinema groups, 54k pills) 72 MB of the 77 MB its
   *  render allocated, per uncached request. Every interpolated value is escaped with
   *  [[escapeInto]], Twirl's own rule; the page snapshots pin the result.
   *
   *  THE ARROW IS `&#8599;`, NOT `↗`. A Java string holds one byte per char until its
   *  first character outside Latin-1, and then two for all of it: the literal arrow,
   *  in every cinema link, doubled each film's builder mid-render (23 MB of copying on
   *  New York). The entity renders the same glyph, and `textContent` — what the
   *  filters read a cinema's name from — already has it decoded. */
  def days(film: FilmSchedule, city: models.City): play.twirl.api.Html = {
    val zone   = city.zoneId
    val locale = city.country.language
    val clock  = city.country.clockStyle
    val commonToks   = FilmFormat.tokensToStrip(film)
    val firstListing = firstListings(film.showings)
    // Wrapped in a plain `Html`: a template passes a value through untouched only when
    // its class is exactly `Html`, and anything else — a subclass included — it
    // escapes as text (`_display_`), which would print this whole tree as markup.
    new play.twirl.api.Html(List(new StreamedHtml((out, flush) =>
      writeDays(film, commonToks, firstListing, zone, locale, clock, out, flush))))
  }

  private def writeDays(film: FilmSchedule, commonToks: Set[String], firstListing: Map[Cinema, LocalDate],
                        zone: ZoneId, locale: java.util.Locale, clock: models.ClockStyle,
                        out: java.lang.StringBuilder, flush: () => Unit): Unit = {
    val rules = zone.getRules
    // Looked up once per cinema group: the first URL listed for a cinema, as `find` took.
    val filmUrlOf = film.linkableCinemaFilmUrls.groupMapReduce(_._1)(_._2)((first, _) => first)
    for ((date, cinemas) <- film.showings) {
      val day = Day(date, expiresFrom(date, zone), rules.getOffset(date.atStartOfDay.plus(Showtime.Grace)))
      out.append("<div class=\"date-group\" data-date=\"").append(date)
        .append("\" data-expires-from=\"").append(day.expiresFrom).append("\"><div class=\"date-label\">")
      escapeInto(out, CardFormat.date(date, film.asOf, locale))
      out.append("</div>")
      for (cinemaShowtimes <- cinemas) {
        val cinema = cinemaShowtimes.cinema
        val slots  = linkableOnly(cinemaShowtimes.showtimes)
        val prefix = Showtime.commonUrlPrefix(slots)
        out.append("<div class=\"cinema-group\"")
        if (prefix.nonEmpty) { out.append(" data-u=\""); escapeInto(out, prefix); out.append('"') }
        out.append("><div class=\"cinema-label\">")
        filmUrlOf.get(cinema) match {
          case Some(url) if firstListing.get(cinema).contains(date) =>
            out.append("<a href=\""); escapeInto(out, url.url)
            out.append("\" target=\"_blank\" rel=\"nofollow\" class=\"cinema-label-link\">")
            escapeInto(out, cinema.displayName); out.append(" &#8599;</a>")
          case Some(_) =>
            out.append("<a>"); escapeInto(out, cinema.displayName); out.append(" &#8599;</a>")
          case None =>
            escapeInto(out, cinema.displayName)
        }
        out.append("</div><div>")
        for (slot <- slots) badgeInto(out, slot, day, zone, clock, commonToks, prefix)
        out.append("</div></div>")
        flush()
      }
      out.append("</div>")
    }
  }

  /** `slots` with any booking URL that is not a [[WebHref]] dropped — the pill stays, as
   *  a plain time. The same `Seq` when every URL passes, which is every real listing. */
  private def linkableOnly(slots: Seq[Showtime]): Seq[Showtime] =
    if (slots.forall(_.bookingUrl.forall(WebHref.accepts))) slots
    else slots.map(slot => if (slot.bookingUrl.forall(WebHref.accepts)) slot else slot.copy(bookingUrl = None))

  /** One `.date-group`'s day: its date, its [[expiresFrom]], and the zone offset that
   *  base was taken at — what lets a pill on an ordinary day skip the zone arithmetic. */
  private final case class Day(date: LocalDate, expiresFrom: Long, offset: java.time.ZoneOffset)

  /** The instant `slot` lapses, when its day's base plus its clock time would get it
   *  wrong — or `None` when the sum is exact, which is what lets the pill omit it. The
   *  sum is exact whenever the slot is on its day, on a whole minute, and the zone's
   *  offset at its lapse is the offset the base was taken at; only then is the exact
   *  instant (a `ZonedDateTime`, ~3x a pill's own bytes) skipped. */
  private def explicitExpiry(slot: Showtime, day: Day, zone: ZoneId): Option[Long] = {
    val start  = slot.dateTime
    val lapses = start.plus(Showtime.Grace)
    if (start.toLocalDate == day.date && start.getSecond == 0 && start.getNano == 0 &&
        zone.getRules.getOffset(lapses) == day.offset) None
    else {
      val expiresAt = lapses.atZone(zone).toInstant.toEpochMilli
      val derived   = day.expiresFrom + (start.getHour * 60L + start.getMinute) * 60000L
      Option.when(derived != expiresAt)(expiresAt)
    }
  }

  /** The pill for `slot`, filed under `day`, into `out`. `commonToks` are the format
   *  tokens every slot of the film shares, which the pill drops (see `FilmFormat`);
   *  `prefix` is the URL prefix its cinema group's slots share. */
  private def badgeInto(out: java.lang.StringBuilder, slot: Showtime, day: Day, zone: ZoneId,
                        clock: models.ClockStyle, commonToks: Set[String], prefix: String): Unit = {
    val tag = slot.bookingUrl match {
      case Some(url) if prefix.nonEmpty && url.startsWith(prefix) =>
        out.append("<a data-s=\""); escapeInto(out, url, prefix.length); out.append('"'); "a"
      case Some(url) =>
        out.append("<a href=\""); escapeInto(out, url)
        out.append("\" class=\"badge-time\" target=\"_blank\" rel=\"nofollow\""); "a"
      case None =>
        out.append("<span class=\"badge-time\""); "span"
    }
    // Omitted rather than emitted empty: most slots carry neither, and the readers treat
    // a missing attribute and an empty one alike (`badge.dataset.format || ''`).
    slot.room.foreach { room => out.append(" data-room=\""); escapeInto(out, room); out.append('"') }
    if (slot.format.nonEmpty) { out.append(" data-format=\""); escapeInto(out, slot.format.mkString(" ")); out.append('"') }
    explicitExpiry(slot, day, zone).foreach(at => out.append(" data-expires=\"").append(at).append('"'))
    out.append('>')
    appendTime(out, slot.dateTime, clock)
    val tokens = slot.format.filterNot(commonToks.contains)
    if (tokens.nonEmpty) { out.append("<span class=\"badge-fmt\">"); escapeInto(out, tokens.mkString(" ")); out.append("</span>") }
    out.append("</").append(tag).append('>')
  }

  /** The slot's clock time in the country's [[models.ClockStyle]], without allocating it —
   *  or, on a 24-hour clock, `LocalTime.toString`'s full form when it carries seconds. */
  private def appendTime(out: java.lang.StringBuilder, at: java.time.LocalDateTime, clock: models.ClockStyle): Unit =
    if ((at.getSecond != 0 || at.getNano != 0) && clock == models.ClockStyle.TwentyFourHour) out.append(at.toLocalTime.toString)
    else clock.appendTime(out, at.getHour, at.getMinute)

  /** `value` from `from` on, HTML-escaped into `out` exactly as Twirl's
   *  `HtmlFormat.escape` renders it — without the `Html` and `String` it allocates. */
  def escapeInto(out: java.lang.StringBuilder, value: String, from: Int = 0): Unit = {
    var i = from
    while (i < value.length) {
      value.charAt(i) match {
        case '<'  => out.append("&lt;")
        case '>'  => out.append("&gt;")
        case '"'  => out.append("&quot;")
        case '\'' => out.append("&#x27;")
        case '&'  => out.append("&amp;")
        case c    => out.append(c)
      }
      i += 1
    }
  }
}
