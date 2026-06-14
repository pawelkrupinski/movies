package views

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.StagingRecord

/**
 * /debug renders a "Pending enrichment (staging)" section ABOVE the corpus table
 * listing the per-cinema newcomer rows incubating in `pending_movies`, so a dev
 * can watch a film resolve then graduate. The section's header (incl. the queue
 * columns) always renders — an empty `pending_movies` shows as an empty table,
 * not a missing section; only the data rows are conditional on incubating films.
 */
class DebugViewStagingSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def stagingRow(title: String, year: Option[Int], tmdbId: Option[Int]): StagingRecord =
    StagingRecord(Helios, title, year, MovieRecord(
      tmdbId = tmdbId,
      data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), releaseYear = year))))

  "debug view" should "render the empty staging table header when nothing is incubating" in {
    val html = views.html.debug(Seq.empty, Map.empty[String, String]).body
    // The section + its queue columns always render, so an empty pending queue is
    // a visible empty table rather than a vanished section.
    html should include ("Pending enrichment (staging)")
    html should include ("<th>Enrich q#</th>")
    html should include ("<th>TMDB q#</th>")
    // ...with no incubating rows — a staging data row carries data-queue-title.
    html should not include ("data-queue-title")
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
