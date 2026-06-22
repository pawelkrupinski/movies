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

  // ── Paragraphed blurbs beat single-block ones ──────────────────────────────
  // A synopsis split into paragraphs (cinema pages emit `\n`/`\n\n` breaks)
  // reads far better than an unbroken wall of text, so prefer it even when a
  // single-block source is marginally longer. Length only decides among
  // candidates that tie on whether they carry a paragraph break.

  it should "prefer a paragraphed synopsis over a longer single-block one" in {
    val paragraphed = "Pierwszy akapit opisu filmu.\n\nDrugi akapit z dalszą częścią fabuły."
    val singleBlock = "Jeden długi, nieprzerwany blok tekstu bez żadnego podziału na akapity, a do tego jeszcze trochę więcej słów."
    singleBlock.length should be > paragraphed.length
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Tmdb   -> SourceData(synopsis = Some(singleBlock)),
        Helios -> SourceData(synopsis = Some(paragraphed))
      )
    )
    record.synopsis shouldBe Some(paragraphed)
  }

  it should "pick the longest among several paragraphed synopses" in {
    val shorter = "Akapit jeden.\n\nAkapit dwa."
    val longer  = "Akapit pierwszy z opisem.\n\nAkapit drugi z dalszym ciągiem fabuły filmu."
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Helios -> SourceData(synopsis = Some(shorter)),
        Tmdb   -> SourceData(synopsis = Some(longer))
      )
    )
    record.synopsis shouldBe Some(longer)
  }

  // ── City-scoped synopsis (`synopsisForCity`) ───────────────────────────────
  // The displayed blurb is chosen among only the cinemas screening the film IN
  // the city being viewed, plus the city-independent TMDB/IMDb. A cinema in
  // another city never contributes — even when its blurb is the global longest.

  // `Helios` is a Poznań venue; `HeliosMagnolia` a Wrocław one (see Cinema.byCity).
  it should "prefer an in-city cinema blurb over a longer one from another city" in {
    val poznan   = "Poznański krótki opis."
    val wroclaw  = "Wrocławski znacznie dłuższy i pełniejszy opis filmu, który globalnie by wygrał."
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Helios         -> SourceData(synopsis = Some(poznan)),
        HeliosMagnolia -> SourceData(synopsis = Some(wroclaw))
      )
    )
    record.synopsis                     shouldBe Some(wroclaw) // global still picks the longest
    record.synopsisForCity(Poznan)      shouldBe Some(poznan)  // …but Poznań sees its own
    record.synopsisForCity(Wroclaw)     shouldBe Some(wroclaw)
  }

  it should "fall back to TMDB in a city whose cinemas carry no blurb, never another city's" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        HeliosMagnolia -> SourceData(synopsis = Some("Wrocławski opis kina.")),
        Tmdb           -> SourceData(synopsis = Some("Opis z TMDB."))
      )
    )
    // Poznań has no cinema blurb of its own here → TMDB, NOT the Wrocław cinema.
    record.synopsisForCity(Poznan) shouldBe Some("Opis z TMDB.")
  }

  it should "expose the city-independent (TMDB/IMDb-only) fallback via synopsisNonCinema" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        HeliosMagnolia -> SourceData(synopsis = Some("Dłuższy wrocławski opis kina, dłuższy niż TMDB.")),
        Tmdb           -> SourceData(synopsis = Some("Opis z TMDB."))
      )
    )
    record.synopsis           shouldBe Some("Dłuższy wrocławski opis kina, dłuższy niż TMDB.") // global incl. cinema
    record.synopsisNonCinema  shouldBe Some("Opis z TMDB.")                                     // cinema excluded
  }

  // Cinema City fetches a film's detail ONCE chain-wide into the synthetic
  // `CinemaCityChain` slot (which belongs to no city); a per-city merge admits
  // it only where a Cinema City venue is actually screening the film.
  it should "admit the Cinema City chain blurb only in cities its venues screen in" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        CinemaCityChain       -> SourceData(synopsis = Some("Opis z sieci Cinema City.")),
        CinemaCityPoznanPlaza -> SourceData(), // the Poznań venue screening this film
        Tmdb                  -> SourceData(synopsis = Some("Krótki TMDB."))
      )
    )
    record.synopsisForCity(Poznan)  shouldBe Some("Opis z sieci Cinema City.") // CC venue here → chain applies
    record.synopsisForCity(Wroclaw) shouldBe Some("Krótki TMDB.")              // no CC venue here → chain excluded
  }
}
