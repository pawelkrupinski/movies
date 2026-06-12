package views

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

/**
 * The /debug per-source breakdown links each cinema name to its public source
 * page — the same href /uptime shows, looked up by `cinema.displayName` from the
 * `url:` UptimeMonitor tag (passed in as `cinemaUrls`). A cinema with no known
 * URL stays plain text. Reuses `_serviceNameLink`, the same partial /uptime uses.
 */
class DebugViewCinemaLinkSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  // A row whose only source slot is Cinema City Wroclavia, so exactly one cinema
  // slot renders in the expanded details.
  private val row = StoredMovieRecord(
    title = "Belle",
    year = Some(2021),
    record = MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle")))),
  )

  "debug view" should "link the cinema name to its source page when a url is known" in {
    val url = "https://www.cinema-city.pl/kina/cinema-city/1090"
    val html = views.html.debug(Seq(row), Map(CinemaCityWroclavia.displayName -> url)).body

    html should include (s"""href="$url"""")
    html should include ("cinema-name-link")
    // The link wraps the cinema name (the slot's heading), not just the trailing
    // "page ↗" film-url chip.
    html should include regex s"""<a class="cinema-name-link" href="${java.util.regex.Pattern.quote(url)}"[^>]*>${CinemaCityWroclavia.displayName}"""
  }

  it should "render the cinema name as plain text when no url is known" in {
    val html = views.html.debug(Seq(row), Map.empty[String, String]).body

    html should not include ("cinema-city/1090")
    // Plain span, no link, for the cinema heading.
    html should include (s"""<span class="cinema-name-link">${CinemaCityWroclavia.displayName}""")
  }
}
