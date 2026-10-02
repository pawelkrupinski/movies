package controllers

import java.time.{LocalDate, ZoneId}

import models.{Cinema, Showtime}
import play.twirl.api.HtmlFormat

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
 *    share ([[urlPrefix]], `data-u`), each pill the rest (`data-s`) — and nothing a
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

  /** The prefix every booking URL of `slots` shares — `.cinema-group[data-u]` — or empty
   *  when fewer than two have one, where a shared prefix would save nothing. */
  def urlPrefix(slots: Seq[Showtime]): String = {
    val urls = slots.flatMap(_.bookingUrl)
    if (urls.sizeIs < 2) ""
    else urls.reduce(commonPrefix)
  }

  private def commonPrefix(a: String, b: String): String = {
    val limit = math.min(a.length, b.length)
    var i = 0
    while (i < limit && a.charAt(i) == b.charAt(i)) i += 1
    a.substring(0, i)
  }

  /** For each cinema of `showings`, the first day it is listed on: the one listing whose
   *  label carries the cinema's page for the film as a real `href`. */
  def firstListings(showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): Map[Cinema, LocalDate] =
    showings.flatMap { case (date, cinemas) => cinemas.map(_.cinema -> date) }
      .groupMapReduce(_._1)(_._2)((a, b) => if (a.isBefore(b)) a else b)

  /** The pill for `slot`, filed under `date`. `commonToks` are the format tokens every
   *  slot of the film shares, which the pill drops (see `FilmFormat`); `prefix` is its
   *  cinema group's [[urlPrefix]]. */
  def badge(slot: Showtime, date: LocalDate, zone: ZoneId, commonToks: Set[String], prefix: String): String = {
    val time      = slot.dateTime.toLocalTime.toString
    val tokens    = slot.format.filterNot(commonToks.contains).mkString(" ")
    val fmtBadge  = if (tokens.isEmpty) "" else s"""<span class="badge-fmt">${HtmlFormat.escape(tokens)}</span>"""
    val roomAttr  = slot.room.map(r => s""" data-room="${HtmlFormat.escape(r)}"""").getOrElse("")
    val formats   = slot.format.mkString(" ")
    // Omitted rather than emitted empty: most slots carry no token, and the reader treats a
    // missing attribute and an empty one alike (`badge.dataset.format || ''`).
    val fmtAttr   = if (formats.isEmpty) "" else s""" data-format="${HtmlFormat.escape(formats)}""""
    val expiresAt = slot.dateTime.plus(Showtime.Grace).atZone(zone).toInstant.toEpochMilli
    val derived   = expiresFrom(date, zone) + (slot.dateTime.getHour * 60L + slot.dateTime.getMinute) * 60000L
    val expAttr   = if (derived == expiresAt) "" else s""" data-expires="$expiresAt""""
    val attrs     = s"$roomAttr$fmtAttr$expAttr"
    slot.bookingUrl match {
      case Some(url) if prefix.nonEmpty && url.startsWith(prefix) =>
        s"""<a data-s="${HtmlFormat.escape(url.drop(prefix.length))}"$attrs>$time$fmtBadge</a>"""
      case Some(url) =>
        s"""<a href="${HtmlFormat.escape(url)}" class="badge-time" target="_blank" rel="nofollow"$attrs>$time$fmtBadge</a>"""
      case None =>
        s"""<span class="badge-time"$attrs>$time$fmtBadge</span>"""
    }
  }
}
