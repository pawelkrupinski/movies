package services.scrapes

import play.api.libs.json.Json
import services.scrapes.VenueClosure.VenueSignal
import tools.HttpStatusException

import java.net.UnknownHostException
import java.util.Locale
import scala.util.{Failure, Success, Try, Using}

/**
 * What a venue's OWN website says about the venue — the evidence [[VenueClosure]]
 * needs before it calls a venue an aggregator dropped "closed".
 *
 * Closure: the site's host no longer resolves; the page answers 404/410; the domain
 * is parked or for sale; or the page announces a permanent closure. Life: the page
 * answers and announces nothing of the kind — a page that merely lists no showtimes is
 * NOT a closure, it is a venue we have no programme for. Anything else (blocked,
 * throttled, 5xx, timed out) is silent.
 *
 * The phrases live in `closure-notices.json`, which the retirement PR's live re-check
 * (`data/scripts/retire_venues.py`) reads too, so both sides judge a page alike.
 */
object OwnSiteEvidence {

  final case class Notices(closure: Seq[String], parked: Seq[String])

  lazy val notices: Notices = Using.resource(getClass.getResourceAsStream("/closure-notices.json")) { in =>
    val json = Json.parse(in)
    Notices((json \ "closure").as[Seq[String]], (json \ "parked").as[Seq[String]])
  }

  /** The signal `url`'s answer gives: its body, or why there is none. */
  def read(url: String, answer: Try[String]): VenueSignal = answer match {
    case Success(body) =>
      val html = body.toLowerCase(Locale.ROOT)
      lazy val text = org.jsoup.Jsoup.parse(body).text().toLowerCase(Locale.ROOT).replaceAll("\\s+", " ")
      notices.parked.find(html.contains).map(marker => VenueSignal.Closure(s"own site $url is a parked or for-sale domain (\"$marker\")"))
        .orElse(notices.closure.find(text.contains).map(phrase => VenueSignal.Closure(s"own site $url announces the closure (\"$phrase\")")))
        .getOrElse(VenueSignal.Life(s"own site $url answered with no closure notice"))
    case Failure(e: HttpStatusException) if HttpStatusException.isDurable(e.code) =>
      VenueSignal.Closure(s"own site $url answered HTTP ${e.code}")
    case Failure(e: UnknownHostException) =>
      VenueSignal.Closure(s"own site $url does not resolve (no DNS record for ${Option(e.getMessage).getOrElse("its host")})")
    case Failure(e) =>
      VenueSignal.Silent(s"own site $url could not be read (${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")})".take(300))
  }
}

/** Each venue's own website, where we know it — keyed by display name, the wire key. */
trait VenueWebsites {
  def of(venue: String): Option[String]
}

object VenueWebsites {
  def apply(byVenue: Map[String, String]): VenueWebsites = byVenue.get(_)

  /** The checked-in list, `venue-websites.json`. */
  lazy val checkedIn: VenueWebsites = Using.resource(getClass.getResourceAsStream("/venue-websites.json")) { in =>
    VenueWebsites(Json.parse(in).as[Map[String, play.api.libs.json.JsValue]].collect {
      case (venue, play.api.libs.json.JsString(url)) if !venue.startsWith("_") => venue -> url
    })
  }
}
