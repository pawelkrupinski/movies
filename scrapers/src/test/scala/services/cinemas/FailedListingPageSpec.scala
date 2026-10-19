package services.cinemas

import clients.tools.FakeHttpFetch
import models.{KinoMuzeumGdansk, KinoSfinks}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.{KinoJednoscClient, KinoKulturaClient, KinoMuzeumGdanskClient, KinoSfinksClient, UjazdowskiClient}
import tools.HttpFetch

import java.time.LocalDate
import java.util.concurrent.atomic.AtomicBoolean

/**
 * A later page of a multi-page listing that fails used to be dropped by the client in a
 * `Try(...).toOption`, and the listing still reached the cache as COMPLETE — so the films only
 * that page listed were pruned on an upstream blip. Each of these now reads its later pages
 * through `ListingPages`, which keeps the films that did answer and marks the listing
 * INCOMPLETE. Recorded fixtures; one later page made to fail (a 503).
 */
class FailedListingPageSpec extends AnyFlatSpec with Matchers {

  /** The fixture fetch, with the FIRST request whose URL contains `page` answering 503. */
  private def failingOnce(fixtures: String, page: String): HttpFetch = new FakeHttpFetch(fixtures) {
    private val failed = new AtomicBoolean(false)
    override def get(url: String): String =
      if (url.contains(page) && failed.compareAndSet(false, true)) throw new tools.HttpStatusException(503, "GET", url, None)
      else super.get(url)
  }

  private def check(name: String, healthy: => CinemaScraper, pageDown: => CinemaScraper): Unit = {
    s"$name with every page answering" should "land a complete listing" in {
      healthy.fetchWithSource().complete shouldBe true
    }
    it should "land the pages that answered as an INCOMPLETE listing when one later page fails" in {
      val scraped = pageDown.fetchWithSource()
      scraped.complete shouldBe false
      scraped.movies should not be empty
    }
  }

  check("KinoKulturaClient",
    new KinoKulturaClient(new FakeHttpFetch("kino-kultura")),
    new KinoKulturaClient(failingOnce("kino-kultura", "rep_date=")))

  check("KinoMuzeumGdanskClient",
    new KinoMuzeumGdanskClient(new FakeHttpFetch("kino-muzeum"), KinoMuzeumGdansk),
    new KinoMuzeumGdanskClient(failingOnce("kino-muzeum", "repertuar,ts:"), KinoMuzeumGdansk))

  // Ujazdowski walks forward until days stop answering; past its programme a day 404s, which
  // the replay has to say, since nobody recorded the days that do not exist.
  private val ujazdowskiDay = LocalDate.of(2026, 6, 13)
  private class UjazdowskiSite extends FakeHttpFetch("ujazdowski") {
    override def get(url: String): String =
      try super.get(url)
      catch { case _: java.io.FileNotFoundException if url.contains("week.ajax") => tools.UpstreamNotFound(url) }
  }
  check("UjazdowskiClient",
    new UjazdowskiClient(new UjazdowskiSite, ujazdowskiDay),
    new UjazdowskiClient(new UjazdowskiSite {
      private val failed = new AtomicBoolean(false)
      override def get(url: String): String =
        if (url.contains("week.ajax") && failed.compareAndSet(false, true)) throw new tools.HttpStatusException(503, "GET", url, None)
        else super.get(url)
    }, ujazdowskiDay))

  // Per-film pages that CARRY the showtimes: a failed one used to drop its film silently from a
  // complete listing, which then pruned it.
  check("KinoJednoscClient",
    new KinoJednoscClient(new FakeHttpFetch("kino-jednosc")),
    new KinoJednoscClient(failingOnce("kino-jednosc", "/repertuar/")))

  // Only Sfinks's first page was ever recorded, so its next pages fail in the replay: the
  // listing it lands is honestly incomplete — exactly the case that was landing as complete.
  "KinoSfinksClient over its recorded first page only" should "land an INCOMPLETE listing" in {
    val scraped = new KinoSfinksClient(new FakeHttpFetch("kino-sfinks"), KinoSfinks).fetchWithSource()
    scraped.complete shouldBe false
    scraped.movies should not be empty
  }

  // The listing page itself goes through HttpRead.page: a challenge interstitial served with a
  // 200 used to be parsed as a page with no dates and land as an empty, successful scrape. The
  // body is the transport shape HttpRead's own check matches (see HttpReadSpec) — the client
  // never gets to parse it.
  "a cinema whose listing page is a challenge interstitial" should "fail the scrape, not land an empty listing" in {
    val challenge = new tools.GetOnlyHttpFetch {
      def get(url: String): String =
        "<!DOCTYPE html><html><head><title>Just a moment...</title></head><body>" +
          "<script>(function(){window._cf_chl_opt={cType: 'managed'};}());</script></body></html>"
    }
    a[tools.UnexpectedBodyException] should be thrownBy new KinoKulturaClient(challenge).fetchWithSource()
  }
}
