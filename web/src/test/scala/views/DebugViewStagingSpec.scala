package views

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.StagingRecord

/**
 * /debug renders a "Pending enrichment (staging)" section ABOVE the corpus table
 * listing the per-cinema newcomer rows incubating in `pending_movies`, so a dev
 * can watch a film resolve then graduate. The section is absent when nothing is
 * incubating.
 */
class DebugViewStagingSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def stagingRow(title: String, year: Option[Int], tmdbId: Option[Int]): StagingRecord =
    StagingRecord(Helios, title, year, MovieRecord(
      tmdbId = tmdbId,
      data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), releaseYear = year))))

  "debug view" should "not render the staging section when nothing is incubating" in {
    val html = views.html.debug(Seq.empty, Map.empty[String, String]).body
    html should not include ("Pending enrichment")
  }

  it should "list incubating staging rows above the corpus, before TMDB concludes" in {
    val html = views.html.debug(
      Seq.empty, Map.empty[String, String],
      staging = Seq(stagingRow("Brand New Film", Some(2026), tmdbId = None))).body

    html should include ("Pending enrichment (staging)")
    html should include (Helios.displayName)
    html should include ("Brand New Film")
    // The staging section appears before the main corpus heading.
    html.indexOf("Pending enrichment") should be < html.indexOf("<h1>Debug</h1>")
  }

  it should "show the resolved tmdbId once a staging row has concluded" in {
    val html = views.html.debug(
      Seq.empty, Map.empty[String, String],
      staging = Seq(stagingRow("Kumotry", Some(2026), tmdbId = Some(1454157)))).body
    html should include ("1454157")
  }
}
