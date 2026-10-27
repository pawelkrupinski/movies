package services.scrapes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.VenueClosure.VenueSignal
import tools.HttpStatusException

import java.net.UnknownHostException
import scala.util.{Failure, Success}

/**
 * What a venue's own website says about the venue. The one mistake that matters is
 * reading a live venue as closed, so every closure needs the site itself to say so, and
 * a page that merely lists nothing is a live site.
 */
class OwnSiteEvidenceSpec extends AnyFlatSpec with Matchers {

  private val url = "https://kino.example/"
  private def page(body: String) = OwnSiteEvidence.read(url, Success(s"<html><body>$body</body></html>"))

  "A venue's own site" should "show closure when it answers 404 or 410, naming the status and the URL" in {
    OwnSiteEvidence.read(url, Failure(new HttpStatusException(404, "GET", url, None))) shouldBe
      VenueSignal.Closure(s"own site $url answered HTTP 404")
    OwnSiteEvidence.read(url, Failure(new HttpStatusException(410, "GET", url, None))) shouldBe a[VenueSignal.Closure]
  }

  it should "show closure when its host no longer resolves" in {
    OwnSiteEvidence.read(url, Failure(new UnknownHostException("kino.example"))).observed should
      (include ("does not resolve") and include ("kino.example"))
  }

  it should "show closure when it announces a permanent closure, in each roster's language" in {
    page("<h1>Das Kino ist dauerhaft geschlossen</h1>").observed should include ("dauerhaft geschlossen")
    page("<p>We have   PERMANENTLY\nclosed. Thank you.</p>") shouldBe a[VenueSignal.Closure]
    page("<p>El cine ha cerrado definitivamente</p>") shouldBe a[VenueSignal.Closure]
    page("<p>Kino zostało zamknięte na stałe</p>") shouldBe a[VenueSignal.Closure]
  }

  // A notice that names no permanence is a pause: renovation, "until further notice".
  it should "show life on a temporary closure notice" in {
    page("<p>Kino zostało zamknięte z powodu remontu</p>") shouldBe a[VenueSignal.Life]
    page("<p>Kino zostało zamknięte do odwołania</p>") shouldBe a[VenueSignal.Life]
  }

  it should "show closure when the domain is parked or for sale, even when only a script names the parking service" in {
    page("<h1>This domain is for sale!</h1>").observed should include ("parked or for-sale")
    OwnSiteEvidence.read(url, Success("""<html><script src="https://www.sedoparking.com/frmpark/x.js"></script></html>""")) shouldBe
      a[VenueSignal.Closure]
  }

  // Every live cinema's opening hours read "Montag geschlossen"; that is not a closure.
  it should "show life on a day-of-week closure notice" in {
    page("Montag geschlossen. Dienstag bis Sonntag ab 17 Uhr.") shouldBe a[VenueSignal.Life]
    page("Closed on Mondays") shouldBe a[VenueSignal.Life]
  }

  it should "show life on a page that answers with no programme at all — lacking showtimes is not a closure" in {
    page("<h1>Kino Example</h1><p>Programm folgt.</p>") shouldBe VenueSignal.Life(s"own site $url answered with no closure notice")
  }

  // Blocked, throttled, broken or slow describes a site that EXISTS.
  it should "say nothing when it cannot be read" in {
    OwnSiteEvidence.read(url, Failure(new HttpStatusException(403, "GET", url, None))) shouldBe a[VenueSignal.Silent]
    OwnSiteEvidence.read(url, Failure(new HttpStatusException(503, "GET", url, None))) shouldBe a[VenueSignal.Silent]
    OwnSiteEvidence.read(url, Failure(new java.net.http.HttpTimeoutException("timed out"))) shouldBe a[VenueSignal.Silent]
  }

  "The checked-in websites" should "name each venue's own site, and skip the comment" in {
    VenueWebsites.checkedIn.of("ACME Screening Room Lambertville") shouldBe Some("https://www.acmescreeningroom.org/")
    VenueWebsites.checkedIn.of("_comment") shouldBe None
    VenueWebsites.checkedIn.of("Acme Theatre Riverton") shouldBe Some("https://barrycinemas.com/acme-theater-riverton")
    VenueWebsites.checkedIn.of("Kino im Badehaus Masserberg") shouldBe Some("http://kino.masserberg.de")
  }

  // The list is keyed by display name, an exact lookup: a misspelt or retired name
  // never matches, and that venue is silently judged without its own site.
  it should "key every site by the display name of a venue on a roster" in {
    val keys = scala.util.Using.resource(getClass.getResourceAsStream("/venue-websites.json")) { in =>
      play.api.libs.json.Json.parse(in).as[play.api.libs.json.JsObject].value.toMap
    }.collect { case (venue, play.api.libs.json.JsString(url)) if !venue.startsWith("_") => venue -> url }
    val rostered = models.City.all.flatMap(_.cinemas).map(_.displayName).toSet
    keys.keySet -- rostered shouldBe empty
    all(keys.values) should startWith regex "https?://"
  }
}
