package views

import models.{CinemaCityWroclavia, Multikino, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieRecord

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
    StoredMovieRecord(title = "Belle", year = Some(2021), record = MovieRecord(data = data))

  "debug view" should "render a header for the cinema-count column" in {
    val html = views.html.debug(Seq.empty).body
    html should include ("""data-key="cinemas"""")
    html should include ("Cinemas")
  }

  it should "show the number of distinct cinemas screening a title" in {
    val slot = SourceData(title = Some("Belle"))
    val html = views.html.debug(
      Seq(rowWith(Map(CinemaCityWroclavia -> slot, Multikino -> slot)))).body

    html should include ("""data-cinemas="2"""")
    html should include ("""<td class="cinemas">2</td>""")
  }

  it should "count only cinemas, not TMDB/IMDb slots" in {
    val slot = SourceData(title = Some("Belle"))
    val html = views.html.debug(
      Seq(rowWith(Map(
        CinemaCityWroclavia -> slot,
        models.Tmdb -> SourceData(title = Some("Belle")),
        models.Imdb -> SourceData(title = Some("Belle")),
      )))).body

    html should include ("""data-cinemas="1"""")
    html should include ("""<td class="cinemas">1</td>""")
  }
}
