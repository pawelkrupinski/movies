package controllers

import java.time.{LocalDate, ZoneId}

import models.Showtime
import play.twirl.api.HtmlFormat

/**
 * One showtime pill's markup, and the per-day expiry base it leans on.
 *
 * WHY IT IS THIS TERSE. A city listing renders every slot of every film for every
 * day it knows — 52,577 pills on New York on 2026-10-02, 19.9 MB of HTML — and each
 * render of that page allocated ~320 MB straight into web-us's old generation. Every
 * byte here is paid 52k times, so the pill carries only what nothing else can give
 * the page:
 *
 *  - NO `data-time`: the pill's own text starts with the time (`_repertoireView`'s
 *    `slotTime` reads the first text node).
 *  - NO `data-expires` on an ordinary slot: the enclosing `.date-group` carries
 *    [[expiresFrom]], and the slot lapses at that plus its clock time. A slot whose
 *    expiry that sum would get WRONG — a DST changeover day, a past-midnight
 *    screening filed under the previous day, a time with seconds — keeps its own
 *    `data-expires`, so the rule is exact by construction, not approximately right.
 *  - `rel="nofollow"` WITHOUT `noopener`: every browser the site supports applies
 *    `noopener` to a `target="_blank"` link on its own. `nofollow` stays — it is what
 *    tells a crawler these booking links are plumbing, not endorsements.
 */
object ShowtimeBadge {

  /** The epoch millis at which a slot at 00:00 on `date` stops counting as upcoming
   *  (`Showtime.Grace` after it), in the city's zone — `.date-group[data-expires-from]`. */
  def expiresFrom(date: LocalDate, zone: ZoneId): Long =
    date.atStartOfDay.plus(Showtime.Grace).atZone(zone).toInstant.toEpochMilli

  /** The pill for `slot`, filed under `date`. `commonToks` are the format tokens every
   *  slot of the film shares, which the pill drops (see `FilmFormat`). */
  def html(slot: Showtime, date: LocalDate, zone: ZoneId, commonToks: Set[String]): String = {
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
      case Some(url) => s"""<a href="${HtmlFormat.escape(url)}" class="badge-time" target="_blank" rel="nofollow"$attrs>$time$fmtBadge</a>"""
      case None      => s"""<span class="badge-time"$attrs>$time$fmtBadge</span>"""
    }
  }
}
