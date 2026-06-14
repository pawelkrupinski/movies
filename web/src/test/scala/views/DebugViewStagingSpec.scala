package views

import controllers.MovieController
import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.StagingRecord

/**
 * /debug renders a "Pending enrichment (staging)" section ABOVE the corpus table
 * listing the per-cinema newcomer rows incubating in `pending_movies`, so a dev
 * can watch a film walk Detail → TMDB → IMDb → fold then graduate. The section's
 * header (incl. the per-stage + queue columns) always renders — an empty
 * `pending_movies` shows as an empty table, not a missing section; only the data
 * rows are conditional on incubating films. Each stage cell is a server-rendered
 * green ✓ once that step has concluded, empty otherwise.
 */
class DebugViewStagingSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def stagingRow(
    title:         String,
    year:          Option[Int],
    tmdbId:        Option[Int]    = None,
    detailPending: Boolean        = false,
    imdbId:        Option[String] = None,
  ): StagingRecord =
    StagingRecord(Helios, title, year, MovieRecord(
      tmdbId = tmdbId,
      imdbId = imdbId,
      detailPending = detailPending,
      data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), releaseYear = year))))

  "debug view" should "render the empty staging table header when nothing is incubating" in {
    val html = views.html.debug(Seq.empty, Map.empty[String, String]).body
    // The section + its per-stage + queue columns always render, so an empty
    // pending queue is a visible empty table rather than a vanished section.
    html should include ("Pending enrichment (staging)")
    html should include ("<th>Detail</th>")
    html should include ("<th>TMDB</th>")
    html should include ("<th>IMDb</th>")
    html should include ("<th>Queue #</th>")
    // ...with no incubating rows — a staging data row carries a data-anchor attr.
    html should not include ("data-anchor=")
  }

  it should "list incubating staging rows above the corpus, before TMDB concludes" in {
    val html = views.html.debug(
      Seq.empty, Map.empty[String, String],
      staging = Seq(stagingRow("Brand New Film", Some(2026), tmdbId = None))).body

    html should include ("Pending enrichment (staging)")
    html should include (Helios.displayName)
    html should include ("Brand New Film")
    // The anchor (sanitize(title)) the queue-place JS matches on rides along.
    html should include ("""data-anchor="brandnewfilm"""")
    // The staging section appears before the main corpus heading.
    html.indexOf("Pending enrichment") should be < html.indexOf("<h1>Debug</h1>")
  }

  it should "mark a stage done with a ✓ once it has concluded, and leave pending stages empty" in {
    // A row that has fetched detail + resolved TMDB but not yet recovered IMDb.
    val html = views.html.debug(
      Seq.empty, Map.empty[String, String],
      staging = Seq(stagingRow("Kumotry", Some(2026),
        tmdbId = Some(1454157), detailPending = false, imdbId = None))).body
    // Detail + TMDB concluded → their cells carry the ✓; IMDb is still empty.
    html should include ("""<td class="stage-detail"><span class="stage-done">✓</span></td>""")
    html should include ("""<td class="stage-tmdb"><span class="stage-done">✓</span></td>""")
    html should include ("""<td class="stage-imdb"></td>""")
    // The raw tmdb id is no longer printed — the cell is a stage marker, not a value.
    html should not include ("1454157")
  }

  it should "count a no-match TMDB conclusion as a done stage" in {
    val html = views.html.debug(
      Seq.empty, Map.empty[String, String],
      staging = Seq(StagingRecord(Helios, "Obscure One", Some(2026),
        MovieRecord(tmdbNoMatch = true,
          data = Map[Source, SourceData](Helios -> SourceData(title = Some("Obscure One"))))))).body
    html should include ("""<td class="stage-tmdb"><span class="stage-done">✓</span></td>""")
  }

  it should "render every row (header shows the full count) but pre-hide those past the cap" in {
    val extra = 5
    val rows = (1 to MovieController.StagingRowLimit + extra).map(i => stagingRow(s"Film $i", Some(2026)))
    val html = views.html.debug(Seq.empty, Map.empty[String, String], staging = rows).body
    // Header carries the true total, and EVERY row is in the DOM (so the client
    // can re-sort and surface any of them live)…
    html should include (s"""<span id="staging-count">${rows.size}</span>""")
    """data-anchor="""".r.findAllMatchIn(html).size shouldBe rows.size
    // …but rows past the cap start hidden, so the first paint shows only the cap.
    """<tr class="data hidden"""".r.findAllMatchIn(html).size shouldBe extra
    html should include (s"Showing the first ${MovieController.StagingRowLimit}")
  }
}
