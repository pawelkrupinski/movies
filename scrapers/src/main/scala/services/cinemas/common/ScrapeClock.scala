package services.cinemas.common

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.Locale

import scala.util.Try
import scala.util.matching.Regex

/** Clock times as the cinema pages print them: the shared patterns and their readers. */
private[cinemas] object ScrapeClock {

  /** A clock time as the pages print it, for a client's own pattern to compose with
    * `raw"""…${ScrapeClock.ClockParts}…"""` instead of spelling the digits again
    * (`NoHandRolledClockPatternSpec`). The `Parts` forms capture the hour and the minute,
    * read back by [[clockAt]]; the `Text` forms capture nothing, for a pattern that captures
    * the whole time and reads it with [[parseHHmm]]. `Dotted` also takes the "19.30" a
    * Polish page often prints. */
  val ClockParts: String       = """(\d{1,2}):(\d{2})"""
  val ClockPartsDotted: String = """(\d{1,2})[.:](\d{2})"""
  val ClockText: String        = """\d{1,2}:\d{2}"""
  val ClockTextDotted: String  = """\d{1,2}[.:]\d{2}"""
  /** The `HH:mm` of a machine-written stamp ("2026-09-06 19:30"), always two-digit, read
    * with `LocalDateTime.parse` — where a one-digit hour is not this format at all. */
  val IsoClockText: String     = """\d{2}:\d{2}"""

  private val HourMinute = ClockParts.r

  /** The first `HH:mm` in `s` as a `LocalTime`, or `None` when there's no
    * match or the captured hour/minute is out of range. */
  def parseHHmm(s: String): Option[LocalTime] =
    HourMinute.findFirstMatchIn(s).flatMap(clockAt(_, 1))

  /** An ISO `yyyy-MM-dd` day at the first clock time in `hour`; `None` when either is unreadable. */
  def isoDateAtClock(day: String, hour: String): Option[LocalDateTime] =
    Try(LocalDate.parse(day)).toOption.flatMap(date => parseHHmm(hour).map(date.atTime))

  /** The time a [[ClockParts]] / [[ClockPartsDotted]] match captured, its hour in group
    * `hourGroup` and its minute in the next; `None` for an hour or minute no clock has
    * ("25:00"), never a throw that would take the page's other screenings with it. */
  def clockAt(m: Regex.Match, hourGroup: Int): Option[LocalTime] =
    Option(m.group(hourGroup)).zip(Option(m.group(hourGroup + 1))).flatMap { case (hour, minute) => clock(hour, minute) }

  /** An hour and a minute already split out of a page (an extractor's bindings, a `split(":")`);
    * `None` for one that is no number or no clock ("24", "60"). */
  def clock(hour: String, minute: String): Option[LocalTime] =
    Try(LocalTime.of(hour.trim.toInt, minute.trim.toInt)).toOption

  /** A [[ClockParts]] match whose group `markerGroup` holds an optional "am" / "p.m." marker: a
    * marked hour 1–12 is a 12-hour clock ("12:10 am" → 00:10, "7:05 pm" → 19:05); a marker on an
    * hour no 12-hour clock has ("19:30 pm", "0:15 am") is ignored rather than read twice. */
  def meridiemClockAt(m: Regex.Match, hourGroup: Int, markerGroup: Int): Option[LocalTime] =
    clockAt(m, hourGroup).map { time =>
      val marker = Option(m.group(markerGroup)).map(_.trim.toLowerCase(Locale.ROOT))
      if (time.getHour == 0 || time.getHour > 12) time
      else if (marker.exists(_.startsWith("p"))) time.withHour(time.getHour % 12 + 12)
      else if (marker.exists(_.startsWith("a"))) time.withHour(time.getHour % 12)
      else time
    }

  // The marker must end its word: "12:00 Amazing Grace" is noon, not midnight.
  private val MeridiemClock = raw"""(?i)$ClockPartsDotted\s*([ap]\.?m\.?(?!\p{L}))?""".r

  /** The first clock in `text`, colon or dot, 24-hour or marked as [[meridiemClockAt]] reads
    * it: "16:00", "3:30pm", "7.45 P.M.". */
  def meridiemClockIn(text: String): Option[LocalTime] =
    MeridiemClock.findFirstMatchIn(text).flatMap(meridiemClockAt(_, 1, 3))
}
