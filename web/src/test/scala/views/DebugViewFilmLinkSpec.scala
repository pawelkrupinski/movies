package views

import models.{CinemaCityWroclavia, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

/**
 * The debug page lists the *global* corpus, but the /film page it links to is
 * city-scoped (`film(city, title)` only resolves a film with future showtimes
 * in that city). A row whose film plays only in Wrocław must therefore link
 * into `/wroclaw/film…` — NOT into whatever city the debug page itself happens
 * to be served under (here, Poznań), which would 404 "Film not found".
 */
class DebugViewFilmLinkSpec extends AnyFlatSpec with Matchers {

  // The debug page under test is served under Poznań.
  private implicit val city: models.City = models.Poznan

  private val wroclawOnly = StoredMovieRecord(
    title = "Belle",
    year = Some(2021),
    record = MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))
  )

  "debug view" should "link a Wrocław-only film into /wroclaw/film, not the page's own city" in {
    val html = views.html.debug(Seq(wroclawOnly)).body
    html should include ("""href="/wroclaw/film?title=Belle"""")
    html should not include """href="/poznan/film?title=Belle""""
  }

  it should "render a per-row re-enrich button posting the row's ResolveTmdb enqueue" in {
    val html = views.html.debug(Seq(wroclawOnly)).body
    html should include ("class=\"reenrich\"")
    // The button targets the dev-only reenrich endpoint with this row's identity.
    // (The reverse route's `&` is HTML-escaped to `&amp;` in the attribute; the
    // browser un-escapes it when the JS reads dataset.url, so assert the path +
    // first param only.)
    html should include ("/debug/reenrich?title=Belle")
  }

  it should "key each row by its Mongo _id so the live change stream can patch it" in {
    val html = views.html.debug(Seq(wroclawOnly)).body
    html should include ("""data-id="belle|2021"""")
  }
}
