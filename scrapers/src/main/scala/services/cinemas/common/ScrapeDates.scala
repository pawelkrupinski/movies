package services.cinemas.common

import java.time.temporal.TemporalAmount
import java.time.{LocalDate, LocalDateTime, LocalTime, MonthDay, Period}
import java.util.Locale

import scala.util.Try

/** Dates as the cinema pages print them: Polish and English month names, numeric and day-month spellings, and the year a yearless date belongs to. */
private[cinemas] object ScrapeDates {

  /** Polish genitive month names as the cinema pages spell dates ("5 maja",
    * "12 grudnia"). Shared so the hand-rolled scrapers don't each carry their
    * own copy of the same 12-entry map. */
  val PolishMonths: Map[String, Int] = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  /** Polish month names in the NOMINATIVE ("Czerwiec", "Styczeń"), as several
    * calendar pages spell their day headers, vs the genitive [[PolishMonths]]
    * ("czerwca"). */
  private val PolishMonthsNominative: Map[String, Int] = Map(
    "styczeń" -> 1, "luty" -> 2, "marzec" -> 3, "kwiecień" -> 4, "maj" -> 5, "czerwiec" -> 6,
    "lipiec" -> 7, "sierpień" -> 8, "wrzesień" -> 9, "październik" -> 10, "listopad" -> 11, "grudzień" -> 12
  )

  /** Genitive and nominative month names folded into one lookup — accepts
    * either spelling (the dok.pl / Iluzjon calendars use the nominative in their
    * day headers, most other pages the genitive). Keyed lower-case; [[polishMonth]]
    * folds the token's case. */
  private val PolishMonthsAnyCase: Map[String, Int] = PolishMonths ++ PolishMonthsNominative

  /** A day header's "<day> <Polish month name>" opening, with [[DayMonthYearPat]]
    * the same shape closed by an explicit 4-digit year ("4 września 2026"). The
    * month group feeds [[polishMonth]] — via [[parseDayMonth]] / [[parseDayMonthYear]]
    * for the common shapes, or directly for a scraper with a composite pattern of
    * its own (a "5-7 września" day range, say).
    *
    * The month is matched with `\p{L}`, NOT `\w`, and that is the whole point of
    * sharing these two: Java's `\w` is ASCII-only unless the pattern is compiled
    * with UNICODE_CHARACTER_CLASS, so it stops dead at the first diacritic. Of the
    * twelve genitive month names exactly two carry one — "września" and
    * "października" — so a `\w`-spelled copy reads every date for ten months of
    * the year and none at all in September and October. Four hand-rolled scrapers
    * had drifted to that spelling and lost their whole autumn programme on
    * 1 September 2026 (see `clients.cinemas.DiacriticMonthNameSpec`); one shared
    * pattern is how the fifth copy doesn't repeat it. */
  val DayMonthPat     = """(\d{1,2})\s+(\p{L}+)""".r
  val DayMonthYearPat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  /** Polish three-letter month abbreviations as several cinema pages spell them
    * ("10 Cze 2026", "5 paź"). Keyed lower-case; [[polishMonthAbbrev]] and
    * [[polishMonth]] fold case so a page can capitalise them ("Cze") or not
    * ("cze"). Shared so the hand-rolled scrapers (the MSI portals, Praha, Art
    * Kino Krosno, and every [[parseDayMonth]] caller) don't each carry their own
    * copy of the same 12-entry map. */
  private val PolishMonthAbbrevs: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  /** The month number for a Polish three-letter abbreviation, case-insensitively
    * ("Cze"/"cze" → 6); `None` for anything not a known abbreviation. */
  def polishMonthAbbrev(token: String): Option[Int] =
    PolishMonthAbbrevs.get(token.trim.toLowerCase(Locale.ROOT))

  private val EnglishMonthAbbrevs: Map[String, Int] =
    (1 to 12).map(m => java.time.Month.of(m).name.toLowerCase(Locale.ROOT).take(3) -> m).toMap

  /** The month number for an English month name or abbreviation by its first three
    * letters, case-insensitively ("Oct", "Sept", "September" → 9 for the last two);
    * `None` for anything that is not one. */
  def englishMonth(token: String): Option[Int] =
    Some(token.trim.toLowerCase(Locale.ROOT)).filter(_.length >= 3).flatMap(t => EnglishMonthAbbrevs.get(t.take(3)))

  /** The month number for a Polish month token in any spelling the cinema
    * pages use — genitive ("września"), nominative ("wrzesień") or the
    * three-letter abbreviation ("wrz") — case-insensitively. */
  def polishMonth(token: String): Option[Int] = {
    val key = token.trim.toLowerCase(Locale.ROOT)
    PolishMonthsAnyCase.get(key).orElse(PolishMonthAbbrevs.get(key))
  }

  /** ISO `2026-09-06` or the day-first `6.09.2026` / `06-09-2026` / `06/09/2026`
    * the Polish cinema pages spell their dates in. Groups 1-3 are the ISO
    * year/month/day, groups 4-6 the day-first day/month/year. */
  private val NumericDate = """(\d{4})-(\d{2})-(\d{2})|(\d{1,2})[./-](\d{1,2})[./-](\d{4})""".r
  private val DayDotMonth = """(\d{1,2})\.(\d{1,2})""".r

  /** The first calendar date in `s` in any [[NumericDate]] spelling; `None`
    * for no match or an impossible date. Finds rather than requires the whole
    * string to be the date — like [[ScrapeClock.parseHHmm]] — so a label such as
    * "Data: 06.09.2026 18:00" reads without a per-client extraction regex, and
    * one helper replaces the `DateTimeFormatter.ofPattern` copy each scraper
    * used to carry for its own separator. */
  def parseDate(s: String): Option[LocalDate] =
    NumericDate.findFirstMatchIn(s).flatMap { m =>
      val (year, month, day) =
        if (m.group(1) != null) (m.group(1), m.group(2), m.group(3))
        else (m.group(6), m.group(5), m.group(4))
      Try(LocalDate.of(year.toInt, month.toInt, day.toInt)).toOption
    }

  /** [[parseDate]] paired with [[ScrapeClock.parseHHmm]] on the same string — "06.09.2026
    * 18:00", "2026-09-06 18:00:00", "6.09.2026, 18:00", in either order.
    * `None` when either half is missing. */
  def parseDateTime(s: String): Option[LocalDateTime] =
    for { date <- parseDate(s); time <- ScrapeClock.parseHHmm(s) } yield date.atTime(time)

  /** The first "<day> <Polish month name>" in `s` ([[DayMonthPat]], the month
    * in any spelling [[polishMonth]] knows) as a `MonthDay` — the yearless day
    * header most calendar pages carry; [[upcomingDate]] places it in a year.
    * `None` when that first "<digits> <word>" is not a date. */
  def parseDayMonth(s: String): Option[MonthDay] =
    DayMonthPat.findFirstMatchIn(s).flatMap { m =>
      polishMonth(m.group(2)).flatMap(month => Try(MonthDay.of(month, m.group(1).toInt)).toOption)
    }

  /** The numeric day header some pages use instead ("5.09") as a `MonthDay`. */
  def parseNumericDayMonth(s: String): Option[MonthDay] =
    DayDotMonth.findFirstMatchIn(s)
      .flatMap(m => Try(MonthDay.of(m.group(2).toInt, m.group(1).toInt)).toOption)

  /** The first "<day> <Polish month name> <yyyy>" in `s` ([[DayMonthYearPat]])
    * as a date; `None` without a year or for an impossible date. */
  def parseDayMonthYear(s: String): Option[LocalDate] =
    DayMonthYearPat.findFirstMatchIn(s).flatMap { m =>
      polishMonth(m.group(2))
        .flatMap(month => Try(LocalDate.of(m.group(3).toInt, month, m.group(1).toInt)).toOption)
    }

  /** The year a yearless page date belongs to: this year's, unless that lies
    * more than `grace` before `today`, in which case next year's — a
    * late-December page listing "13 stycznia" means January of the coming year,
    * while a page still showing last week's screenings means this one. Each
    * scraper picks the grace its page's staleness warrants. The one roll BACK
    * is a leftover row: in early January a "30 grudnia" still on the page is
    * last December's when that lies within [[LeftoverRow]] (and the grace) of
    * `today`, not one 11 months ahead. Nothing else is pulled back, so a date
    * up to a year out keeps its place however long the grace: the horizon is
    * never capped. The year is chosen with a 29 lutego this year lacks placed on
    * the 28th, so one listed in December lands on next year's leap day; `None`
    * when the day doesn't exist in the year chosen. */
  def upcomingDate(dayMonth: MonthDay, today: LocalDate, grace: TemporalAmount = Period.ofDays(60)): Option[LocalDate] = {
    val leftoverFrom = Seq(today.minus(grace), today.minus(LeftoverRow)).maxBy(_.toEpochDay)
    val candidate    = dayMonth.atYear(today.getYear)
    val year =
      if (candidate.isBefore(today.minus(grace))) today.getYear + 1
      else if (candidate.isAfter(today) && candidate.minusYears(1).isBefore(today) && !candidate.minusYears(1).isBefore(leftoverFrom))
        today.getYear - 1
      else today.getYear
    Option.when(dayMonth.isValidYear(year))(dayMonth.atYear(year))
  }

  /** How long a past screening can linger on a page: the only span a yearless
    * date is read as last year's in. */
  private val LeftoverRow: Period = Period.ofDays(31)

  /** [[upcomingDate]] at month granularity: this year for the current month
    * and any later one, next year for an earlier month — for a calendar that
    * only ever lists the current month onwards, where "5 sierpnia" seen in
    * September can only mean next August. */
  def upcomingMonthDate(dayMonth: MonthDay, today: LocalDate): Option[LocalDate] =
    upcomingDate(dayMonth, today, grace = Period.ofDays(today.getDayOfMonth - 1))

  /** A page's numeric day and month as a `MonthDay` for [[upcomingDate]] / [[upcomingMonthDate]],
    * `None` when no year has that day (a "31.04"). Every yearless page date goes through those
    * two — never a year rule of a client's own (`NoHandRolledYearInferenceSpec`). */
  def monthDay(day: Int, month: Int): Option[MonthDay] = Try(MonthDay.of(month, day)).toOption

  /** Every calendar day from `from` to `to` inclusive, at `time` — the
    * "screens daily HH:MM from DD.MM.YYYY to DD.MM.YYYY" shape several small
    * venues (Kino Narew, Kino Parczew) spell a multi-day run in, instead of
    * listing each date separately. `to` before `from` yields empty rather
    * than throwing, so a malformed pair drops the run instead of looping
    * backwards. */
  def dailyRange(from: LocalDate, to: LocalDate, time: LocalTime): Seq[LocalDateTime] =
    if (to.isBefore(from)) Seq.empty
    else Iterator.iterate(from)(_.plusDays(1)).takeWhile(!_.isAfter(to)).map(_.atTime(time)).toSeq

  /** A day-grouped listing read in document order -- a day header, the screenings under it, the next header: an item
    * `dayHeader` takes sets the day the items after it fall on (a header it cannot read sets none), and every other
    * item is read by `row` under the day last set. An item before the first header, or under an unreadable one, reads
    * nothing. */
  def underDayHeaders[E, A](items: Iterable[E])(dayHeader: PartialFunction[E, Option[LocalDate]])(
    row: (E, LocalDate) => IterableOnce[A]
  ): Seq[A] = {
    val out = Seq.newBuilder[A]
    items.foldLeft(Option.empty[LocalDate]) { (day, item) =>
      if (dayHeader.isDefinedAt(item)) dayHeader(item)
      else { day.foreach(d => out ++= row(item, d)); day }
    }
    out.result()
  }
}
