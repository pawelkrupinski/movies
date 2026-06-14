package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.displayTitle` — deterministic, scrape-order-independent choice
 * of the user-visible title across the spellings several cinemas (and TMDB)
 * report for one merged film. Three steps:
 *   A. dominant clean form (most cinema votes) settles identity;
 *   B. TMDB's Polish title wins when it shares that clean key (canonical casing);
 *   C. otherwise the cinema spelling ladder picks.
 *
 * Each case fails before the order-independent rewrite (the old tiebreak keyed
 * off lexicographic order of the canonical, which picked the over-capitalised
 * "Drzewo Magii", the de-accented "Diabel", and a minority typo).
 */
class MovieRecordDisplayTitleSpec extends AnyFlatSpec with Matchers {

  "displayTitle" should "prefer TMDB's Polish title when it agrees on identity with the cinemas" in {
    // Two cinemas disagree only on casing of a common noun; TMDB carries the
    // correct lowercase form. The old lexicographic tiebreak picked the
    // over-capitalised "Drzewo Magii".
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Drzewo Magii")),
      KinoApollo -> SourceData(title = Some("Drzewo magii")),
      Tmdb       -> SourceData(title = Some("Drzewo magii"))
    ))
    record.displayTitle("Drzewo magii") shouldBe "Drzewo magii"
  }

  it should "ignore a TMDB title that resolved to a DIFFERENT clean key (mis-resolution)" in {
    // TMDB picked the wrong film — its clean key doesn't match what the cinemas
    // advertise — so we must keep the cinema title, not overwrite it.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Drzewo magii")),
      Tmdb      -> SourceData(title = Some("Some Other Movie"))
    ))
    record.displayTitle("Drzewo magii") shouldBe "Drzewo magii"
  }

  it should "name the row by the dominant clean form, dropping a minority misspelling" in {
    // Two cinemas spell it right, one ships a typo with a different clean key;
    // the typo must not win. The old ladder sorted lexicographically and picked
    // the typo "Wokna" ('k' < 'n').
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino           -> SourceData(title = Some("Wonka")),
      KinoApollo          -> SourceData(title = Some("Wonka")),
      CinemaCityWroclavia -> SourceData(title = Some("Wokna"))
    ))
    record.displayTitle("Wonka") shouldBe "Wonka"
  }

  it should "keep the diacritic spelling in the cinema fallback (no TMDB)" in {
    // Same clean key (ł folds to l), no TMDB to canonicalise — the ladder's
    // diacritic axis must keep "Diabeł" over a scraper-flattened "Diabel".
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Diabeł")),
      KinoApollo -> SourceData(title = Some("Diabel"))
    ))
    record.displayTitle("Diabel") shouldBe "Diabeł"
  }

  it should "reject a malformed ALL-CAPS TMDB title and keep the cinema spelling" in {
    // TMDB's Polish title is ALL-CAPS ("ALL YOU NEED IS KILL") — worse than the
    // mixed-case form every cinema advertises. The defect guard must reject it.
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("All You Need Is Kill")),
      KinoApollo -> SourceData(title = Some("All You Need Is Kill")),
      Tmdb       -> SourceData(title = Some("ALL YOU NEED IS KILL"))
    ))
    record.displayTitle("All You Need Is Kill") shouldBe "All You Need Is Kill"
  }

  it should "reject a TMDB title carrying edge junk and keep the cinema spelling" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Zaproszenie")),
      Tmdb      -> SourceData(title = Some("Zaproszenie."))
    ))
    record.displayTitle("Zaproszenie") shouldBe "Zaproszenie"
  }

  it should "reject a double-spaced TMDB title and keep the cinema spelling" in {
    val record = MovieRecord(data = Map[Source, SourceData](
      Multikino  -> SourceData(title = Some("Super Mario Galaxy Film")),
      KinoApollo -> SourceData(title = Some("Super mario galaxy film")),
      Tmdb       -> SourceData(title = Some("Super Mario  Galaxy Film"))
    ))
    record.displayTitle("Super Mario Galaxy Film") shouldBe "Super Mario Galaxy Film"
  }

  it should "fall back to cleanTitle when no cinema is scraping yet" in {
    val record = MovieRecord(data = Map[Source, SourceData](Imdb -> SourceData()))
    record.displayTitle("Anchor Title") shouldBe "Anchor Title"
  }
}
