package models

import java.time.{Instant, LocalDate, Month, ZoneOffset}

/**
 * The months a seasonal venue typically screens in, both inclusive: `May..September`.
 *
 * A TYPICAL window, the country's, not the venue's own: no feed says when an open-air
 * screen opens, and the evidence the worker keeps (the latest scrape only) cannot learn
 * it. So each country has one, below, and every seasonal venue in it shares it.
 */
final case class SeasonWindow(opens: Month, closes: Month) {
  /** Is `month` inside the window? A window may wrap the new year (`November..February`). */
  def contains(month: Month): Boolean =
    if (opens.getValue <= closes.getValue) month.getValue >= opens.getValue && month.getValue <= closes.getValue
    else month.getValue >= opens.getValue || month.getValue <= closes.getValue

  /** The first day of the season `date` falls in, or None when `date` is off-season. */
  def openedOn(date: LocalDate): Option[LocalDate] = Option.when(contains(date.getMonth)) {
    val thisYear = LocalDate.of(date.getYear, opens, 1)
    if (thisYear.isAfter(date)) thisYear.minusYears(1) else thisYear
  }

  override def toString: String = s"$opens..$closes"
}

/** Each country's typical season for its open-air, summer and drive-in screens — the ONE place it is written. */
object SeasonWindow {
  val Poland:        SeasonWindow = SeasonWindow(Month.MAY, Month.SEPTEMBER)
  val UnitedKingdom: SeasonWindow = SeasonWindow(Month.MAY, Month.SEPTEMBER)
  val Germany:       SeasonWindow = SeasonWindow(Month.MAY, Month.SEPTEMBER)
  val Spain:         SeasonWindow = SeasonWindow(Month.JUNE, Month.SEPTEMBER)
  val UnitedStates:  SeasonWindow = SeasonWindow(Month.APRIL, Month.OCTOBER)
}

/**
 * When a venue screens at all: all year, or only in a season.
 *
 * An open-air screen, a summer terrace or a drive-in is dark for the rest of the year,
 * and that silence is not a broken scraper or a closed venue. Off its season such a
 * venue is left out of the dead-venue pages, the content census and the closure sweep;
 * it is still scraped as usual, so it reappears the day it lists times. In season it is
 * treated like any other venue — except by the content census, which waits for its first listing
 * of the season before counting its quiet, and past the season's halfway point counts one still
 * unlisted as not reopened instead.
 *
 * [[offSeasonAt]] and the other `Instant` readings judge the UTC calendar: the window is whole
 * months and only a typical one, so the hours a venue's own midnight is away from UTC's change
 * nothing worth a zone lookup. [[offSeasonOn]] takes the caller's own day instead — a page judges
 * its city's — so the two can disagree for those hours either side of a month boundary.
 */
enum Opening {
  case YearRound
  case Seasonal(window: SeasonWindow)

  /** Is the venue outside its season at `now`? Never for a year-round venue. */
  def offSeasonAt(now: Instant): Boolean = offSeasonOn(Opening.day(now))

  /** Might the venue be dark by design at `now`: off its season, or in its window's final
   *  month? The window is a typical one in whole months, so it cannot say which day of that
   *  month a venue's programme ends — a rooftop's ended on 27 September with its window
   *  running to the 30th. Wider than [[offSeasonAt]], so it suits only a reader that would
   *  otherwise call a venue's darkness a fault. Never for a year-round venue. */
  def mayBeDarkAt(now: Instant): Boolean = this match {
    case YearRound        => false
    case Seasonal(window) =>
      val month = Opening.day(now).getMonth
      !window.contains(month) || month == window.closes
  }

  /** Is the venue outside its season on `date` — the city's own day, where a page has one? */
  def offSeasonOn(date: LocalDate): Boolean = this match {
    case YearRound        => false
    case Seasonal(window) => !window.contains(date.getMonth)
  }

  /** When the season `now` falls in began, for a seasonal venue in season: what happened
   *  before it — a winter of 404s — says nothing about this season. None otherwise. */
  def seasonOpenedAt(now: Instant): Option[Instant] = this match {
    case YearRound        => None
    case Seasonal(window) => window.openedOn(Opening.day(now)).map(_.atStartOfDay(ZoneOffset.UTC).toInstant)
  }

  /** Halfway through the season `now` falls in, for a seasonal venue in season: one that has
   *  listed nothing by then is not late to open but dark for a reason. None otherwise. */
  def seasonHalfwayAt(now: Instant): Option[Instant] = this match {
    case YearRound        => None
    case Seasonal(window) => window.openedOn(Opening.day(now)).map { opened =>
      val months = (window.closes.getValue - window.opens.getValue + 12) % 12 + 1
      val closed = opened.plusMonths(months)
      LocalDate.ofEpochDay((opened.toEpochDay + closed.toEpochDay) / 2).atStartOfDay(ZoneOffset.UTC).toInstant
    }
  }

  /** `since`, or when the season `now` falls in began if that is later: how long something has
   *  held is counted within this season only, never across the dark months before it. */
  def sinceThisSeason(since: Instant, now: Instant): Instant =
    seasonOpenedAt(now).filter(_.isAfter(since)).getOrElse(since)
}

object Opening {
  /** A generated roster's venue: seasonal, in its country's `window`, when the generator
   *  named it so (`data/scripts/seasonal_venues.py`); year-round otherwise. */
  def generated(seasonal: Boolean, window: SeasonWindow): Opening =
    if (seasonal) Seasonal(window) else YearRound

  private def day(instant: Instant): LocalDate = LocalDate.ofInstant(instant, ZoneOffset.UTC)
}
