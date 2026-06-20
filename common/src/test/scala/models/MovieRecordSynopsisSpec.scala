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

  // ── Retained synopses (sticky after a cinema drops the film) ───────────────
  // When a cinema stops listing a film its live slot is pruned, but its synopsis
  // is kept in `retainedSynopses` so the displayed (longest-wins) synopsis stays
  // sticky — the best blurb we ever had keeps showing.

  it should "still surface a retained synopsis once its live slot is gone" in {
    // Multikino (which had the longest blurb) dropped the film: no live slot,
    // only the retained copy survives. Helios is still live with a short one.
    val record = MovieRecord(
      data             = Map[Source, SourceData](Helios -> SourceData(synopsis = Some("Krótki opis."))),
      retainedSynopses = Map[Source, String](Multikino -> "Znacznie dłuższy, pełniejszy opis filmu zachowany po reapie.")
    )
    record.synopsis shouldBe Some("Znacznie dłuższy, pełniejszy opis filmu zachowany po reapie.")
  }

  it should "prefer a longer live synopsis over a shorter retained one" in {
    val record = MovieRecord(
      data             = Map[Source, SourceData](Tmdb -> SourceData(synopsis = Some("Aktualny, dłuższy opis filmu z TMDB."))),
      retainedSynopses = Map[Source, String](Helios -> "Stary, krótszy.")
    )
    record.synopsis shouldBe Some("Aktualny, dłuższy opis filmu z TMDB.")
  }

  it should "strip URLs from retained synopses too" in {
    val record = MovieRecord(
      data             = Map[Source, SourceData](),
      retainedSynopses = Map[Source, String](StacjaFalenica -> "https://youtu.be/x Zachowany opis bez linku.")
    )
    record.synopsis shouldBe Some("Zachowany opis bez linku.")
  }

  // ── CMS-duplicated blurbs (a cinema pastes the synopsis N× into one field) ──
  // Confirmed in prod for "Ojczyzna": Kino Piast (Bilety24) shipped the blurb 9×
  // glued together, which — being the longest "source" — won longest-wins and
  // rendered nine times on the detail page.

  it should "collapse a synopsis a single source duplicated N times" in {
    val unit = "Pełny opis filmu w jednym sensownym zdaniu."
    val record = MovieRecord(
      data = Map[Source, SourceData](Helios -> SourceData(synopsis = Some(unit * 9)))
    )
    record.synopsis shouldBe Some(unit)
  }

  it should "not let a repetition-inflated synopsis beat a genuinely longer one" in {
    // The repeated blurb is RAW-longer (23×8 = 184) than the genuine one (~84),
    // so before collapsing-before-ranking it would win and render eight times.
    val short = "Krótki, lecz powtórzony. "
    val long  = "Znacznie dłuższy, prawdziwy i pełny opis filmu bez żadnych powtórzeń, który powinien wygrać."
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Helios -> SourceData(synopsis = Some(short * 8)),
        Tmdb   -> SourceData(synopsis = Some(long))
      )
    )
    record.synopsis shouldBe Some(long)
  }
}
