package views

import tools.SpecClock.given

import models.{CinemaCityWroclavia, Multikino, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Each /debug row shows how many distinct cinemas currently screen the title —
 * `MovieRecord.cinemaData.size`. The count rides a sortable numeric column and a
 * `data-cinemas` attribute (the same shape the Year/TMDB-id columns use), so the
 * table can be ordered "most-screened first". TMDB/IMDb slots are not cinemas and
 * must not be counted.
 */
class DebugViewCinemaCountSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def rowWith(data: Map[models.Source, SourceData]) =
    StoredMovieRecord.synthesised(title = "Belle", year = Some(2021), record = MovieRecord(data = data), services.movies.SingleCountryNormalizer.titleNormalizer)

  "debug view" should "render a header for the cinema-count column" in {
    val html = views.html.debug(controllers.DebugCorpusTable.of(Seq.empty, titleNormalizer), titleNormalizer, current = models.Country.Poland).body
    html should include ("""data-key="cinemas"""")
    html should include ("Cinemas")
  }

  it should "show the number of distinct cinemas screening a title" in {
    val slot = SourceData(title = Some("Belle"))
    val html = views.html.debug(
      controllers.DebugCorpusTable.of(Seq(rowWith(Map(CinemaCityWroclavia -> slot, Multikino -> slot))), titleNormalizer), titleNormalizer, current = models.Country.Poland).body

    html should include ("""data-cinemas="2"""")
    html should include ("""<td class="cinemas">2</td>""")
  }

  it should "count only cinemas, not TMDB/IMDb slots" in {
    val slot = SourceData(title = Some("Belle"))
    val html = views.html.debug(
      controllers.DebugCorpusTable.of(Seq(rowWith(Map(
        CinemaCityWroclavia -> slot,
        models.Tmdb -> SourceData(title = Some("Belle")),
        models.Imdb -> SourceData(title = Some("Belle")),
      ))), titleNormalizer), titleNormalizer, current = models.Country.Poland).body

    html should include ("""data-cinemas="1"""")
    html should include ("""<td class="cinemas">1</td>""")
  }

  // ── Film links ────────────────────────────────────────────────────────────
  //
  // The debug page lists the *global* corpus, but the /movie page it links to is
  // city-scoped (`film(city, title)` only resolves a film with future showtimes
  // in that city). A row whose film plays only in Wrocław must therefore link
  // into `/wroclaw/movie…` — NOT into whatever city the debug page itself happens
  // to be served under (here, Poznań), which would 404 "Film not found".
  //

  private val wroclawOnly = StoredMovieRecord.synthesised(
    title = "Belle",
    year = Some(2021),
    record = MovieRecord(data = Map(CinemaCityWroclavia -> SourceData(title = Some("Belle"))))
  , services.movies.SingleCountryNormalizer.titleNormalizer)

  "debug view" should "link a Wrocław-only film into /wroclaw/movie, not the page's own city" in {
    val html = views.html.debug(controllers.DebugCorpusTable.of(Seq(wroclawOnly), titleNormalizer), titleNormalizer, current = models.Country.Poland).body
    html should include ("""href="/wroclaw/movie/belle"""")
    html should not include """href="/poznan/movie/belle""""
  }

  it should "key each row by its Mongo _id so the live change stream can patch it" in {
    val html = views.html.debug(controllers.DebugCorpusTable.of(Seq(wroclawOnly), titleNormalizer), titleNormalizer, current = models.Country.Poland).body
    html should include ("""data-id="belle|2021"""")
  }
}
