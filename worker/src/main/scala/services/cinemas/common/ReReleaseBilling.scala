package services.cinemas.common

import java.time.LocalDate

/**
 * A re-release booked as a new film: the year a source dates it by is its re-opening, not the film's.
 *
 * Alamo dates a show by its US opening ("Avengers Endgame: Encore" 2026-09-25, "Pan Labyrinth 20th
 * Anniversary"), Flicks gives a re-release its own page dated by its re-run (`/movie/shiva-re-release/`
 * 2025 for Ram Gopal Varma's 1989 film, "Crocodile Dundee: The Encore Cut" 2025 for the 1986 one). Stated
 * as the year, that would deny the film it re-releases by its year distance. Neither source flags them in
 * data; their own billing does — the title, tagline or page slug says encore, anniversary, restored,
 * remastered, re-release or "revisit". So a RECENT year (within [[RecentYears]] of the scrape) billed that
 * way states no year; an older one is the film's own whatever the billing ("lawrence-of-arabia-50th-anniversary-
 * restoration" is dated 1962 by Flicks, and is right).
 */
object ReReleaseBilling {

  /** How far back an opening date can be a re-release's rather than the film's own. */
  val RecentYears = 2L

  /** Billing words that mark a show as a re-release of an older film. Measured on Alamo's 107 shows
   *  (2026-10-06): they marked the four re-releases and one new film whose title happens to contain
   *  "Restoration" (which then merely loses its year). Hyphen-tolerant, so a page slug bills as its title does. */
  private val Billing =
    """(?i)\b(?:encore|anniversary|restor(?:ed|ation)|remaster(?:ed)?|re-?release|revisit|returns? to (?:the big screen|theaters))\b""".r

  /** Does `billing` (a title, tagline or slug) bill the film as a re-release? */
  def bills(billing: String): Boolean = Billing.findFirstIn(billing).isDefined

  /** Is `opened` recent enough, by `today`, to be a re-release's date rather than the film's? */
  def recent(opened: LocalDate, today: LocalDate): Boolean = !opened.isBefore(today.minusYears(RecentYears))

  /** `year` as the film's year, or none when it is recent and `billing` calls the film a re-release. */
  def filmYear(year: Int, billing: String, today: LocalDate): Option[Int] =
    Option.unless(year >= today.minusYears(RecentYears).getYear && bills(billing))(year)
}
