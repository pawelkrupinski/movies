package services.scrapes

import models.Cinema
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/**
 * The line between "this venue is broken" and "this venue does not exist".
 *
 * Both aggregators advertise dead venues: flicks.us lists
 * `acme-theatre-riverton` in its own sitemap with a current lastmod while the
 * page 404s, and 21 German `filmstarts.de` venue pages do the same. Getting this
 * line wrong in either direction is expensive — call a real outage "gone" and it
 * disappears from the section someone reads, call a dead venue "failing" and it
 * crowds out the outage.
 */
class GoneUpstreamSpec extends AnyFlatSpec with Matchers {

  private val now    = Instant.parse("2026-08-31T09:00:00Z")
  private val cinema = Cinema.all.head

  private def rowFailing(error: String, since: Instant, lastAttempt: Instant = now) =
    ArchivedScrape(cinema, None, lastSuccess = None,
      lastBarren = Some(BarrenAttempt(lastAttempt, ScrapeOutcome.Failed, Some(error), Some(since))))

  private val NotFound = "HttpStatusException: HTTP 404 for GET https://www.flicks.us/cinema/acme-theatre-riverton/"

  "gone upstream" should "recognise a 404 that has stood for over a day" in {
    GoneUpstream.isGone(rowFailing(NotFound, since = now.minusSeconds(30 * 3600)), now) shouldBe true
  }

  // The whole reason for the delay: an aggregator that redeploys badly 404s its
  // WHOLE roster for minutes, and quarantining the fleet on that would hide the
  // one outage worth waking up for.
  it should "not call a fresh 404 gone" in {
    GoneUpstream.isGone(rowFailing(NotFound, since = now.minusSeconds(3600)), now) shouldBe false
  }

  // SensaCine answers 410 for a retired cinema page, and `Cine Mota del Cuervo`
  // therefore sat in /uptime's Failing section indefinitely while the byte-for-byte
  // equivalent 404 venues were quarantined on day one. 410 is a STRONGER statement
  // than 404 ("gone, and not coming back"), and every other place in the codebase
  // that draws this line already counts it — see HttpStatusException.isDurable.
  it should "recognise a 410 the same way, since that is the stronger 'gone'" in {
    val gone410 = "HttpStatusException: HTTP 410 for GET https://www.sensacine.com/cines/cine/E1013/"
    GoneUpstream.isGone(rowFailing(gone410, since = now.minusSeconds(30 * 3600)), now) shouldBe true
    GoneUpstream.isGone(rowFailing(gone410, since = now.minusSeconds(3600)), now) shouldBe false
  }

  // A proxied chain venue never gets a bare status: the residential leg answers and
  // the direct leg is Cloudflare-blocked, so the archive records both. The leg that
  // REACHED the origin is the one whose answer this is; the blocked leg learned
  // nothing, and letting it veto would make the quarantine unreachable for every
  // venue we fetch through the proxy.
  it should "read the durable leg out of a multi-backend failure" in {
    val composite = "All 2 backends failed for get https://vwc.example/venue/1:\n" +
      "  proxy: HttpStatusException: HTTP 404 for GET https://vwc.example/venue/1\n" +
      "  fallback: HttpStatusException: HTTP 403 for GET https://vwc.example/venue/1"
    GoneUpstream.isGone(rowFailing(composite, since = now.minusSeconds(30 * 3600)), now) shouldBe true
  }

  it should "leave every other failure a plain failure" in {
    val day = now.minusSeconds(30 * 3600)
    GoneUpstream.isGone(rowFailing("HttpStatusException: HTTP 500 for GET https://x/", day), now) shouldBe false
    GoneUpstream.isGone(rowFailing("HttpStatusException: HTTP 403 for GET https://x/", day), now) shouldBe false
    GoneUpstream.isGone(rowFailing("HttpTimeoutException: request timed out", day), now) shouldBe false
    // 400 is NOT gone, even though a retired venue id produces one (Odeon answers
    // `400 Invalid site identifier` for a site it has dropped). A 400 is equally
    // what a malformed request of OURS looks like, and quarantining that would hide
    // a client bug in a collapsed section. The uncovered-venue page is what surfaces
    // this shape — see SourceFallbackScraper.
    GoneUpstream.isGone(rowFailing("HttpStatusException: HTTP 400 for GET https://x/", day), now) shouldBe false
    // A 404 in a URL or a film title is not an HTTP status.
    GoneUpstream.isGone(rowFailing("RuntimeException: no films at https://x/404-cinema", day), now) shouldBe false
  }

  // A blank page is a parser problem, not a missing one — the "white" rows have
  // their own section and their own diagnosis.
  it should "ignore an empty scrape, whatever its age" in {
    val empty = ArchivedScrape(cinema, None, None,
      Some(BarrenAttempt(now, ScrapeOutcome.Empty, None, Some(now.minusSeconds(90 * 3600)))))
    GoneUpstream.isGone(empty, now) shouldBe false
  }

  it should "consider a cinema with no barren marker at all healthy" in {
    GoneUpstream.isGone(ArchivedScrape(cinema, None, None, None), now) shouldBe false
  }

  "the re-check window" should "skip a gone venue until its next daily probe is due" in {
    val since = now.minusSeconds(40 * 3600)
    GoneUpstream.skipScrape(rowFailing(NotFound, since, lastAttempt = now.minusSeconds(3600)), now) shouldBe true
  }

  // The only way out of quarantine is a scrape that works, so one has to happen.
  it should "let a gone venue be probed again once the interval has passed" in {
    val since = now.minusSeconds(200 * 3600)
    val stale = now.minusSeconds(GoneUpstream.RecheckInterval.toSeconds + 60)
    GoneUpstream.skipScrape(rowFailing(NotFound, since, lastAttempt = stale), now) shouldBe false
  }

  it should "never skip a venue that is merely failing" in {
    val row = rowFailing("HttpStatusException: HTTP 500 for GET https://x/", now.minusSeconds(200 * 3600))
    GoneUpstream.skipScrape(row, now) shouldBe false
  }

  "the barren run" should "carry its start forward across attempts, and reset on a success" in {
    val first  = BarrenAttempt(now.minusSeconds(7200), ScrapeOutcome.Failed, Some(NotFound))
    val run    = BarrenAttempt.continuing(None, first)
    run.runStartedAt shouldBe first.at

    val second = BarrenAttempt.continuing(Some(run), BarrenAttempt(now, ScrapeOutcome.Failed, Some(NotFound)))
    second.at           shouldBe now          // the newest attempt is what is stored
    second.runStartedAt shouldBe first.at     // …but the run still began two hours ago

    // A row written before the field existed reads as "since its own attempt" —
    // never as a run reaching back to the epoch, which would quarantine it at once.
    BarrenAttempt(now, ScrapeOutcome.Failed, Some(NotFound)).runStartedAt shouldBe now
  }

  "the 24h delay" should "be measured from the run's start, not the last attempt" in {
    // THE case a per-attempt stamp gets wrong: a venue 404ing for days, scraped a
    // minute ago. Reading `at` would call it a fresh failure forever.
    val row = rowFailing(NotFound, since = now.minusSeconds(96 * 3600), lastAttempt = now.minusSeconds(60))
    GoneUpstream.isGone(row, now) shouldBe true
  }
}
