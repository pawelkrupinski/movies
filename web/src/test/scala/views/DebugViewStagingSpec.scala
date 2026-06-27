package views

import models.{Helios, Multikino, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.StagingRecord

/**
 * /debug renders a "Pending enrichment (staging)" section ABOVE the corpus table.
 * It FOLDS the per-cinema `pending_movies` rows by film: the server emits one
 * hidden per-cinema SOURCE row (`_stagingRow`, a data carrier) into `#staging-src`,
 * and the page's JS folds same-`data-anchor` rows into the visible `#staging-folded`
 * table (Cinemas = the count, stage ✓ aggregated). These unit tests cover the
 * server side — the header, the columns, and the source-row data model; the folded
 * rendering itself is JS, exercised in PageJsBehaviourSpec.
 */
class DebugViewStagingSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def stagingRow(
    title:         String,
    year:          Option[Int],
    cinema:        Source         = Helios,
    tmdbId:        Option[Int]    = None,
    detailPending: Boolean        = false,
    imdbId:        Option[String] = None,
  ): StagingRecord =
    StagingRecord(cinema, title, year, MovieRecord(
      tmdbId = tmdbId,
      imdbId = imdbId,
      detailPending = detailPending,
      data = Map[Source, SourceData](cinema -> SourceData(title = Some(title), releaseYear = year))))

  "debug view" should "render the empty staging table (header + both tbodies) when nothing is incubating" in {
    val html = views.html.debug(Seq.empty).body
    html should include ("Pending enrichment (staging)")
    html should include ("<th>Cinemas</th>")
    html should include ("""<th class="tick">Detail</th>""")
    html should include ("""<th class="tick">TMDB</th>""")
    html should include ("""<th class="tick">IMDb</th>""")
    html should include ("<th>Queue #</th>")
    // The hidden source tbody + the visible folded tbody both render…
    html should include ("""id="staging-src"""")
    html should include ("""id="staging-folded"""")
    // …with no incubating rows — a source row carries a data-anchor attr.
    html should not include ("data-anchor=")
  }

  it should "emit a hidden per-cinema source row carrying the fold data model" in {
    val html = views.html.debug(
      Seq.empty,
      staging = Seq(stagingRow("Brand New Film", Some(2026), detailPending = true))).body
    // The section appears before the main corpus heading.
    html.indexOf("Pending enrichment") should be < html.indexOf("<h1>Debug</h1>")
    // The source row is hidden and carries the anchor + the per-cinema fold inputs.
    html should include ("""data-anchor="brandnewfilm"""")
    html should include (s"""data-cinema="${Helios.displayName}"""")
    html should include ("""data-title="Brand New Film"""")
    html should include ("""data-year="2026"""")
    html should include ("""data-detail-done="false"""") // detail still pending
    html should include ("""data-tmdb-done="false"""")
    html should include ("""data-imdb-done="false"""")
    """<tr class="data" hidden""".r.findAllMatchIn(html).size shouldBe 1
  }

  it should "carry the concluded-stage flags (detail done, tmdb resolved) on the source row" in {
    val html = views.html.debug(
      Seq.empty,
      staging = Seq(stagingRow("Kumotry", Some(2026),
        tmdbId = Some(1454157), detailPending = false, imdbId = None))).body
    html should include ("""data-detail-done="true"""")
    html should include ("""data-tmdb-done="true"""")
    html should include ("""data-imdb-done="false"""")
    // The source row is a data carrier — no raw tmdb id is printed.
    html should not include ("1454157")
  }

  it should "flag a no-match TMDB conclusion as a done stage" in {
    val html = views.html.debug(
      Seq.empty,
      staging = Seq(StagingRecord(Helios, "Obscure One", Some(2026),
        MovieRecord(tmdbNoMatch = true,
          data = Map[Source, SourceData](Helios -> SourceData(title = Some("Obscure One"))))))).body
    html should include ("""data-tmdb-done="true"""")
  }

  it should "emit one source row per cinema (same anchor) and count distinct films in the header" in {
    // The same film at two cinemas → two source rows sharing an anchor → 1 film.
    val rows = Seq(
      stagingRow("Shared Film", Some(2026), cinema = Helios),
      stagingRow("Shared Film", Some(2026), cinema = Multikino),
      stagingRow("Solo Film",   Some(2026), cinema = Helios))
    val html = views.html.debug(Seq.empty, staging = rows).body
    // Every cinema gets its own (hidden) source row…
    """<tr class="data" hidden""".r.findAllMatchIn(html).size shouldBe 3
    """data-anchor="sharedfilm"""".r.findAllMatchIn(html).size shouldBe 2
    // …but the header counts FILMS (distinct anchors), not cinemas.
    html should include ("""<span id="staging-count">2</span> films""")
    html should include (s"Showing the first ${controllers.MovieController.StagingRowLimit} films")
  }
}
