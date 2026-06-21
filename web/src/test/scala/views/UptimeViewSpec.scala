package views

import controllers.{FallbackRow, ServiceRow}
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

  private val html = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, cinemasByCity, Seq.empty, Seq.empty).body

  "the uptime page" should "render a subheader for each city group" in {
    html should include ("<h3>Poznań</h3>")
    html should include ("<h3>Warszawa</h3>")
  }

  it should "render each venue's row under its city" in {
    val poznanIndex   = html.indexOf("<h3>Poznań</h3>")
    val warszawaIndex = html.indexOf("<h3>Warszawa</h3>")

    val apolloIndex   = html.indexOf("data-service=\"Kino Apollo\"")
    val muzaIndex     = html.indexOf("data-service=\"Kino Muza\"")
    val muranowIndex  = html.indexOf("data-service=\"Kino Muranów\"")

    // Poznań venues sit between the Poznań and Warszawa headers.
    apolloIndex should (be > poznanIndex and be < warszawaIndex)
    muzaIndex should (be > poznanIndex and be < warszawaIndex)
    // The Warszawa venue sits after the Warszawa header.
    muranowIndex should be > warszawaIndex
  }

  it should "still show the single Cinemas section header above the city groups" in {
    val cinemasHeaderIndex = html.indexOf("<h2>Cinemas</h2>")
    cinemasHeaderIndex should be >= 0
    cinemasHeaderIndex should be < html.indexOf("<h3>Poznań</h3>")
  }

  it should "link the cinema name to its source page when a url: tag is present" in {
    val row = ServiceRow("DKF Rumcajs", Seq.empty,
      tags = Set("shared:FilmwebShowtimesClient", "url:https://www.filmweb.pl/cinema/-1714"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq("Częstochowa" -> Seq(row)), Seq.empty, Seq.empty).body
    // The name becomes an anchor to the scraped source, opening in a new tab …
    out should include ("""<a class="name-label" href="https://www.filmweb.pl/cinema/-1714" target="_blank" rel="noopener noreferrer">DKF Rumcajs</a>""")
    // … and the url: tag is consumed for the link, never rendered as a chip.
    out should not include ("url:https://www.filmweb.pl/cinema/-1714")
    out should include ("tag-shared")
  }

  it should "render the name as plain text when no url: tag is present" in {
    val row = ServiceRow("Multikino Stary Browar", Seq.empty, tags = Set("shared:MultikinoClient"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq("Poznań" -> Seq(row)), Seq.empty, Seq.empty).body
    out should include ("""<span class="name-label">Multikino Stary Browar</span>""")
    out should not include ("<a class=\"name-label\"")
  }

  it should "render a gold FtFW chip for a cinema currently in Filmweb fallback" in {
    val row = ServiceRow("Kino Iluzjon", Seq.empty, tags = Set("custom:IluzjonClient", "fallback:FtFW"))
    val out = views.html.uptime(Seq.empty, Seq.empty, Seq.empty, Seq("Warszawa" -> Seq(row)), Seq.empty, Seq.empty).body
    // The fallback tag gets its own dedicated chip class + "FtFW" label …
    out should include ("tag-fallback")
    out should include (">FtFW<")
    // … rendered alongside, not instead of, the client marker.
    out should include ("tag-custom")
  }

  it should "fold the Filmweb-fallback section onto the page, below the triage and above Cinemas" in {
    val fallback = FallbackRow("Kino Praha", "2180", "1 Jan 12:00", "RuntimeException: down",
      2, "1 Jan 13:00", Seq("evt-recent", "evt-older"))
    val out = views.html.uptime(
      Seq.empty, Seq.empty, Seq(fallback), cinemasByCity, Seq.empty, Seq.empty).body

    out should include ("""id="filmweb-fallback"""")
    out should include ("Filmweb fallback — currently on fallback (1)")
    out should include ("Kino Praha")            // the active fallback row
    // The fallback block sits between the triage sections and the Cinemas section.
    out.indexOf("""id="filmweb-fallback"""") should be < out.indexOf("<h2>Cinemas</h2>")
  }

  it should "render the Filmweb-fallback section collapsed by default, expandable on click" in {
    val fallback = FallbackRow("Kino Praha", "2180", "1 Jan 12:00", "down",
      2, "1 Jan 13:00", Seq("evt-recent"))
    val out = views.html.uptime(
      Seq.empty, Seq.empty, Seq(fallback), Nil, Nil, Nil).body

    // A <details> with NO `open` attribute → collapsed until the <summary> is clicked.
    out should include ("""<details class="leading-fallback" id="filmweb-fallback">""")
    out should include ("<summary>Filmweb fallback — currently on fallback (1)</summary>")
    out should not include ("leading-fallback\" id=\"filmweb-fallback\" open")
  }

  it should "show only the most recent history event, with the full history in an instant hover tooltip" in {
    val fallback = FallbackRow("Kino Praha", "2180", "1 Jan 12:00", "down",
      2, "1 Jan 13:00", Seq("evt-recent", "evt-older"))
    val out = views.html.uptime(
      Seq.empty, Seq.empty, Seq(fallback), Nil, Nil, Nil).body

    out should include (">evt-recent</span>")  // only the newest event is the visible cell text
    // The full history rides a data-full CSS tooltip (instant on :hover), not the
    // slow native title attribute.
    out should include ("data-full=")
    out should not include ("title=\"evt-recent")
    out should include ("evt-older")            // the older event survives — in the hover tooltip …
    out should not include (">evt-older")       // … never as visible cell text
  }
}
