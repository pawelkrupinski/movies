package views

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The /debug per-source breakdown makes each cinema NAME the link — there's no
 * separate "page ↗" chip beside it. The name links to the venue's public source
 * page (the same href /uptime shows, looked up by `cinema.displayName` from the
 * `url:` UptimeMonitor tag, passed in as `cinemaUrls`) when known, else to this
 * slot's own scraped page (`filmUrl`); plain text only when neither exists.
 * Reuses `_serviceNameLink`, the same partial /uptime uses.
 *
 * The breakdown is the `debugDetails` partial, now fetched lazily per row on
 * expand (it was inlined into every /debug row until it OOM'd the view on the
 * full corpus), so these render it directly rather than through the page dump.
 */
class DebugViewCinemaLinkSpec extends AnyFlatSpec with Matchers {

  private val cinema = CinemaCityWroclavia

  private def detailsWith(slot: SourceData, cinemaUrls: Map[String, String]) =
    views.html.debugDetails("Belle", Some(2021), MovieRecord(data = Map(cinema -> slot)), cinemaUrls).body

  "debug details" should "link the cinema name to its source page when a url is known" in {
    val url  = "https://www.cinema-city.pl/kina/cinema-city/1090"
    val html = detailsWith(SourceData(title = Some("Belle")), Map(cinema.displayName -> url))

    // The link wraps the cinema NAME (the slot's heading) itself.
    html should include regex
      s"""<a class="cinema-name-link" href="${java.util.regex.Pattern.quote(url)}"[^>]*>${cinema.displayName}"""
    // And there is no separate "page ↗" chip next to the name.
    html should not include ("page ↗")
  }

  it should "fall back to the slot's own scraped page when no source url is known" in {
    val filmUrl = "https://www.cinema-city.pl/kina/cinema-city/1090/film/belle"
    val html = detailsWith(SourceData(title = Some("Belle"), filmUrl = Some(filmUrl)), Map.empty[String, String])

    // The name carries the film-url link; no standalone "page ↗".
    html should include regex
      s"""<a class="cinema-name-link" href="${java.util.regex.Pattern.quote(filmUrl)}"[^>]*>${cinema.displayName}"""
    html should not include ("page ↗")
  }

  it should "render the cinema name as plain text when neither url is known" in {
    val html = detailsWith(SourceData(title = Some("Belle")), Map.empty[String, String])

    html should include (s"""<span class="cinema-name-link">${cinema.displayName}""")
    html should not include ("<a class=\"cinema-name-link\"")
  }
}
