package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.genres` — first non-empty genre list across sources in a
 * dedicated priority order: TMDB → Filmweb → cinemas (in their normal
 * priority order). Different from `countries` (union across sources) because
 * each source has its own taxonomy and spelling — surfacing them all would
 * mean showing "Sci-Fi" and "science fiction" side by side for the same film.
 */
class MovieRecordGenresSpec extends AnyFlatSpec with Matchers {

  "genres" should "prefer the TMDB slot over Filmweb and any cinema" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(genres = Seq("Action")),
        Filmweb   -> SourceData(genres = Seq("Akcja", "Sci-Fi")),
        Tmdb      -> SourceData(genres = Seq("Dramat", "Historyczny"))
      )
    )
    rec.genres shouldBe Seq("Dramat", "Historyczny")
  }

  it should "fall back to Filmweb when TMDB has no genres" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(genres = Seq("Komedia")),
        Filmweb   -> SourceData(genres = Seq("Akcja", "Sci-Fi"))
      )
    )
    rec.genres shouldBe Seq("Akcja", "Sci-Fi")
  }

  it should "fall back to Filmweb when TMDB slot exists but its genres are empty" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Tmdb    -> SourceData(synopsis = Some("Polish synopsis"), genres = Seq.empty),
        Filmweb -> SourceData(genres = Seq("Dramat"))
      )
    )
    rec.genres shouldBe Seq("Dramat")
  }

  it should "fall back to cinema slots when neither TMDB nor Filmweb have genres" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino           -> SourceData(genres = Seq("Komedia")),
        CinemaCityKinepolis -> SourceData(genres = Seq("Akcja"))
      )
    )
    // Multikino wins among cinemas via the standard priority.
    rec.genres shouldBe Seq("Komedia")
  }

  it should "use the next cinema when Multikino's genre list is empty" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Multikino -> SourceData(genres = Seq.empty),
        Helios    -> SourceData(genres = Seq("Dramat", "Romans"))
      )
    )
    rec.genres shouldBe Seq("Dramat", "Romans")
  }

  it should "return an empty list when no source has genres" in {
    val rec = MovieRecord(
      data = Map[Source, SourceData](
        Tmdb      -> SourceData(synopsis = Some("…")),
        Multikino -> SourceData(title = Some("X"))
      )
    )
    rec.genres shouldBe empty
  }

  it should "return an empty list when data is empty" in {
    MovieRecord().genres shouldBe empty
  }
}
