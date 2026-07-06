package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.anyOriginalTitle` — best original (production-language) title
 * across sources, TMDB → IMDb → cinema → Filmweb. `distinctOriginalTitle` then
 * drops it when it's only a case/whitespace re-spelling of the title already
 * shown, so frontends can render the result unconditionally.
 */
class MovieRecordOriginalTitleSpec extends AnyFlatSpec with Matchers {

  "anyOriginalTitle" should "prefer the TMDB slot over IMDb and any cinema" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(originalTitle = Some("Cinema Original")),
        Imdb      -> SourceData(originalTitle = Some("IMDb Original")),
        Tmdb      -> SourceData(originalTitle = Some("TMDB Original"))
      )
    )
    record.anyOriginalTitle shouldBe Some("TMDB Original")
  }

  it should "fall back to the IMDb slot when TMDB has none" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(originalTitle = Some("Cinema Original")),
        Imdb      -> SourceData(originalTitle = Some("IMDb Original")),
        Tmdb      -> SourceData(synopsis = Some("no original title here"))
      )
    )
    record.anyOriginalTitle shouldBe Some("IMDb Original")
  }

  it should "fall back to the cinema-reported title when only a cinema has one" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](Multikino -> SourceData(originalTitle = Some("Cinema Original")))
    )
    record.anyOriginalTitle shouldBe Some("Cinema Original")
  }

  it should "fall back to the Filmweb slot when only Filmweb has one (Filmweb-only row)" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Ostatni konsjerż")),
        Filmweb   -> SourceData(originalTitle = Some("Der letzte Concierge"))
      )
    )
    record.anyOriginalTitle shouldBe Some("Der letzte Concierge")
  }

  it should "prefer a cinema original title over Filmweb's" in {
    val record = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(originalTitle = Some("Cinema Original")),
        Filmweb   -> SourceData(originalTitle = Some("Filmweb Original"))
      )
    )
    record.anyOriginalTitle shouldBe Some("Cinema Original")
  }

  it should "be None when no source reported one" in {
    MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData(synopsis = Some("x")))).anyOriginalTitle shouldBe None
  }

  "distinctOriginalTitle" should "surface a title that differs from the displayed one" in {
    val record = MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("The Substance"))))
    record.distinctOriginalTitle("Substancja") shouldBe Some("The Substance")
  }

  it should "hide a title that only differs by case or surrounding whitespace" in {
    val record = MovieRecord(data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("  mickey 17 "))))
    record.distinctOriginalTitle("Mickey 17") shouldBe None
  }

  it should "be None when there is no original title at all" in {
    MovieRecord().distinctOriginalTitle("Anything") shouldBe None
  }
}
