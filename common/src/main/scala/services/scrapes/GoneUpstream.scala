package services.scrapes

import tools.HttpStatusException

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

/**
 * When a cinema's page has stopped existing rather than merely failing.
 *
 * Both aggregators we scrape lists of venues from keep advertising pages that
 * are dead: flicks.us still carries `acme-theatre-riverton` in its own
 * `sitemap-cinemas.xml` with a current `lastmod` while the page 404s, and 21 of
 * the German roster's `filmstarts.de/kinoprogramm/...` pages do the same. Since
 * the roster is harvested FROM those sitemaps, re-harvesting cannot drop them —
 * they come back every time.
 *
 * Left alone, each one is a red row that will never go green and a request per
 * scrape cycle that can never succeed. Worse, they sit in `/uptime`'s Failing
 * section, which exists to be acted on: 23 permanently dead venues across the
 * fleet is 23 rows of noise in front of the one row that means something today.
 *
 * So a "this page is gone" answer — a 404 or a 410, the same durable pair
 * [[HttpStatusException.isDurable]] draws everywhere else — that has STOOD for
 * [[MinimumAge]] is treated as gone: scraped rarely instead of every cycle, and
 * shown in its own section rather than among the failures. Nothing here deletes or disables anything — a venue that comes
 * back clears its own marker on the first successful scrape and leaves the
 * section by itself.
 */
object GoneUpstream {

  /** How long that answer has to have stood before it counts as gone rather than
   *  broken. A day, not an hour: an aggregator that redeploys badly can 404 a
   *  whole roster for minutes, and quarantining the fleet on a blip would hide
   *  exactly the outage worth waking up for. A genuinely dead page pays one more
   *  day of cycles, which costs nothing. */
  val MinimumAge: FiniteDuration = 24.hours

  /** How often a quarantined venue is still re-checked. Rare, never never: the
   *  only way back out of this state is a scrape that succeeds, so a venue whose
   *  page returns has to be given a chance to prove it. */
  val RecheckInterval: FiniteDuration = 24.hours

  /** A status carried in the recorded failure text. The archive persists the
   *  message, not the throwable, so the code has to be read back out of it —
   *  `\b` on both sides so a 404 in a URL or a film title is not mistaken for one. */
  private val StatusInMessage = """(?i)\bHTTP\s+(\d{3})\b""".r

  /** Whether a recorded failure says the PAGE is gone rather than broken.
   *
   *  The set is [[HttpStatusException.isDurable]]'s — 404 and 410 — rather than a
   *  second copy of it, because this is the same line that file already draws:
   *  a status that describes the URL, not the moment. It used to be spelled `404`
   *  here alone, and the drift cost a real venue: SensaCine answers **410** for a
   *  retired cinema page, so `Cine Mota del Cuervo` sat in /uptime's Failing
   *  section indefinitely while the byte-for-byte equivalent 404 venues were
   *  quarantined on day one. A 403, a throttle, a 5xx or a timeout still describes
   *  a page that EXISTS and is misbehaving, and keeps its red row.
   *
   *  A `FallbackHttpFetch` composite ("All N backends failed …") names one status
   *  per leg; a durable one from ANY leg counts, because the leg that got it is the
   *  leg that reached the origin — the others were blocked or timed out and
   *  learned nothing about the resource. */
  def saysPageIsGone(error: String): Boolean =
    StatusInMessage.findAllMatchIn(error).exists(m => HttpStatusException.isDurable(m.group(1).toInt))

  def saysPageIsGone(error: Option[String]): Boolean = error.exists(saysPageIsGone)

  /** Has this cinema's page been answering gone, unbroken, for at least [[MinimumAge]]?
   *  Reads only the barren marker: any successful scrape clears it, so a row
   *  with content newer than the run cannot be gone. */
  def isGone(row: ArchivedScrape, now: Instant): Boolean = goneSince(row, now).isDefined

  /** When this cinema started answering gone, if it qualifies — the value the
   *  `/uptime` row shows ("gone since 29 Aug"). */
  def goneSince(row: ArchivedScrape, now: Instant): Option[Instant] =
    row.lastBarren
      .filter(_.outcome == ScrapeOutcome.Failed)
      .filter(barren => saysPageIsGone(barren.error))
      .map(_.runStartedAt)
      .filter(since => JDuration.between(since, now).toMillis >= MinimumAge.toMillis)

  /** Should this cinema be skipped on this tick? Only a gone venue is ever
   *  skipped, and only until its next re-check falls due — measured from the last
   *  ATTEMPT, so the interval is between probes rather than between failures. */
  def skipScrape(row: ArchivedScrape, now: Instant): Boolean =
    isGone(row, now) &&
      row.lastBarren.exists(barren =>
        JDuration.between(barren.at, now).toMillis < RecheckInterval.toMillis)
}
