package views

import controllers.ServiceRow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The uptime page groups cinema rows under a per-city `<h3>` subheader rather
 * than listing every venue in one flat block. These lock that: each city gets
 * its own header, and each venue's row renders under it.
 */
class UptimeViewSpec extends AnyFlatSpec with Matchers {

  private def cinemaRow(name: String) = ServiceRow(name, Seq.empty)

  private val cinemasByCity = Seq(
    "Poznań"   -> Seq(cinemaRow("Kino Apollo"), cinemaRow("Kino Muza")),
    "Warszawa" -> Seq(cinemaRow("Kino Muranów")),
  )

  private val html = views.html.uptime(Seq.empty, Seq.empty, cinemasByCity, Seq.empty, Seq.empty).body

  "the uptime page" should "render a subheader for each city group" in {
    html should include ("<h3>Poznań</h3>")
    html should include ("<h3>Warszawa</h3>")
  }

  it should "render each venue's row under its city" in {
    val poznanIdx   = html.indexOf("<h3>Poznań</h3>")
    val warszawaIdx = html.indexOf("<h3>Warszawa</h3>")

    val apolloIdx   = html.indexOf("data-service=\"Kino Apollo\"")
    val muzaIdx     = html.indexOf("data-service=\"Kino Muza\"")
    val muranowIdx  = html.indexOf("data-service=\"Kino Muranów\"")

    // Poznań venues sit between the Poznań and Warszawa headers.
    apolloIdx should (be > poznanIdx and be < warszawaIdx)
    muzaIdx should (be > poznanIdx and be < warszawaIdx)
    // The Warszawa venue sits after the Warszawa header.
    muranowIdx should be > warszawaIdx
  }

  it should "still show the single Cinemas section header above the city groups" in {
    val cinemasHeaderIdx = html.indexOf("<h2>Cinemas</h2>")
    cinemasHeaderIdx should be >= 0
    cinemasHeaderIdx should be < html.indexOf("<h3>Poznań</h3>")
  }

  it should "link the cinema name to its source page when a url: tag is present" in {
    val row = ServiceRow("DKF Rumcajs", Seq.empty,
      tags = Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/showtimes/-1714"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq("Częstochowa" -> Seq(row)), Seq.empty, Seq.empty).body
    // The name becomes an anchor to the scraped source, opening in a new tab …
    out should include ("""<a class="name-label" href="https://www.filmweb.pl/showtimes/-1714" target="_blank" rel="noopener noreferrer">DKF Rumcajs</a>""")
    // … and the url: tag is consumed for the link, never rendered as a chip.
    out should not include ("url:https://www.filmweb.pl/showtimes/-1714")
    out should include ("tag-shared")
  }

  it should "render the name as plain text when no url: tag is present" in {
    val row = ServiceRow("Multikino Stary Browar", Seq.empty, tags = Set("shared:MultikinoClient"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq("Poznań" -> Seq(row)), Seq.empty, Seq.empty).body
    out should include ("""<span class="name-label">Multikino Stary Browar</span>""")
    out should not include ("<a class=\"name-label\"")
  }

  it should "render a gold FtFW chip for a cinema currently in Filmweb fallback" in {
    val row = ServiceRow("Kino Iluzjon", Seq.empty, tags = Set("custom:IluzjonClient", "fallback:FtFW"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq("Warszawa" -> Seq(row)), Seq.empty, Seq.empty).body
    // The fallback tag gets its own dedicated chip class + "FtFW" label …
    out should include ("tag-fallback")
    out should include (">FtFW<")
    // … rendered alongside, not instead of, the client marker.
    out should include ("tag-custom")
  }
}
