package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.synopsis` — longest non-empty blurb across sources, with URL
 * tokens stripped. A cinema editor can paste a "watch on YouTube" link into
 * the synopsis block of their detail page; the scraper's `.text` read then
 * folds the bare URL into the blurb (confirmed in prod for Orły Republiki,
 * whose Stacja Falenica synopsis — the longest of 19 sources — led with the
 * distributor's YouTube link). Surfacing that raw URL reads badly AND — being
 * one unbreakable token — overflows the flex column on the film-detail page,
 * shoving the synopsis below the poster. Cleaning at this single read boundary
 * fixes the web detail page, the `/api/details` mobile payload and the social
 * card preview at once, without re-scraping the stored value.
 */
class MovieRecordSynopsisSpec extends AnyFlatSpec with Matchers {

  "synopsis" should "strip a leading YouTube URL folded into the blurb" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        StacjaFalenica -> SourceData(synopsis = Some(
          "https://www.youtube.com/watch?v=ERysio3sHjw&embeds_referring_euri=https%3A%2F%2Faurorafilms.pl%2F " +
            "Nowy film laureata Złotej Palmy George Fahmy to największa produkcja."))
      )
    )
    record.synopsis shouldBe Some("Nowy film laureata Złotej Palmy George Fahmy to największa produkcja.")
  }

  it should "pick the longest blurb after URLs are stripped, not before" in {
    // The Falenica blurb is the longest only because of the prepended URL; once
    // cleaned, the genuine TMDB synopsis is longer and should win.
    val record = MovieRecord(
      data = Map[Source, SourceData](
        StacjaFalenica -> SourceData(synopsis = Some("https://www.youtube.com/watch?v=ERysio3sHjw Krótki opis.")),
        Tmdb           -> SourceData(synopsis = Some("Znacznie dłuższy i pełniejszy opis filmu bez żadnego linku."))
      )
    )
    record.synopsis shouldBe Some("Znacznie dłuższy i pełniejszy opis filmu bez żadnego linku.")
  }

  it should "drop a source whose synopsis is nothing but a URL" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        StacjaFalenica -> SourceData(synopsis = Some("https://www.youtube.com/watch?v=ERysio3sHjw")),
        Tmdb           -> SourceData(synopsis = Some("Prawdziwy opis."))
      )
    )
    record.synopsis shouldBe Some("Prawdziwy opis.")
  }

  it should "leave a URL-free synopsis unchanged" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](Tmdb -> SourceData(synopsis = Some("Zwykły opis filmu.")))
    )
    record.synopsis shouldBe Some("Zwykły opis filmu.")
  }

  it should "be None when no source carries a synopsis" in {
    MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData(title = Some("X")))).synopsis shouldBe None
  }
}
