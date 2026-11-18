package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A slot's equality: every metadata field as it is, the cast as the names it holds, the showtimes and their
 *  cache-only digests not at all (SourceData's own comment says why). */
class SourceDataEqualitySpec extends AnyFlatSpec with Matchers {

  private val full = SourceData(
    title = Some("Dune"), rawTitle = Some("DUNE"), originalTitle = Some("Dune: Part One"), englishTitle = Some("Dune"),
    synopsis = Some("Sand."), cast = Seq("Timothée Chalamet", "Zendaya"), director = Seq("Denis Villeneuve"),
    runtimeMinutes = Some(155), releaseYear = Some(2021), countries = Seq("US"), genres = Seq("Sci-Fi"),
    posterUrl = Some("https://p"), filmUrl = Some("https://f"), trailerUrl = Some("https://t"), language = Some("en-US"),
    ageRating = Some("12"), titleSearches = Seq(TitleSearch("dune", Some(1), 0)))

  "two slots" should "be equal, with equal hashes, whatever their cast's order or padding" in {
    val reordered = full.copy(cast = Seq(" Zendaya", "Timothée Chalamet "))
    reordered shouldBe full
    reordered.hashCode shouldBe full.hashCode
  }

  they should "be equal whatever their showtimes and digests" in {
    full.copy(showtimesDigest = Some(7), showtimeStartMinutes = Some(IArray(600))) shouldBe full
  }

  they should "differ when any compared field differs" in {
    val changed = Seq(
      full.copy(title = None), full.copy(rawTitle = None), full.copy(originalTitle = None), full.copy(englishTitle = None),
      full.copy(synopsis = None), full.copy(cast = Seq("Zendaya")), full.copy(director = Nil),
      full.copy(runtimeMinutes = None), full.copy(releaseYear = None), full.copy(countries = Nil), full.copy(genres = Nil),
      full.copy(posterUrl = None), full.copy(filmUrl = None), full.copy(trailerUrl = None), full.copy(language = None),
      full.copy(ageRating = None), full.copy(titleSearches = Nil))
    changed.foreach(other => withClue(other)(other should not be full))
  }

  "a slot" should "not equal anything that is not a slot" in {
    full should not equal "Dune"
  }
}
