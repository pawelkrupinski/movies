package services.enrichment

import clients.TmdbClient
import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}

/** The title ladder MC and RT probe a film under — pure, so pinned on its own
 *  rather than through the two clients' slug fixtures. */
class RatingSiteTitlesSpec extends AnyFlatSpec with Matchers {

  private def keyFor(title: String, year: Option[Int]) =
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq.empty), normalizer = titleNormalizer).keyOf(title, year)

  private def withOriginal(title: String) =
    MovieRecord(tmdbId = Some(1), data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some(title))))

  "RatingSiteTitles.derive" should "ask under the stripped cinema title alone when there is nothing else" in {
    val key = keyFor("Wydma Qzx", Some(2021))
    val titles = RatingSiteTitles.derive(key, MovieRecord(tmdbId = Some(1)), details = None, titleNormalizer)

    titles.linkTitle  shouldBe titleNormalizer.searchQuery(key.cleanTitle)
    titles.fallback   shouldBe None
    titles.year       shouldBe None
    titles.candidates shouldBe Seq(titles.linkTitle)
  }

  it should "lead with TMDB's original title and keep the cinema title as the fallback, with TMDB's year" in {
    val key = keyFor("Wydma Qzx", Some(2021))
    val titles = RatingSiteTitles.derive(key, withOriginal("Qzx Dune"),
      Some(TmdbClient.Details(englishTitle = None, releaseYear = Some(2021))), titleNormalizer)

    titles.linkTitle shouldBe "Qzx Dune"
    titles.fallback  shouldBe Some(titleNormalizer.searchQuery(key.cleanTitle))
    titles.year      shouldBe Some(2021)
  }

  it should "climb primary → English → US, dropping any rung that only repeats an earlier one" in {
    val details = Some(TmdbClient.Details(
      englishTitle = Some("Harry Potter and the Philosopher's Stone"),
      releaseYear  = Some(2001),
      usTitle      = Some("Harry Potter and the Sorcerer's Stone")))
    val titles = RatingSiteTitles.derive(keyFor("Harry Potter i Kamień filozoficzny", Some(2001)),
      withOriginal("Harry Potter and the Philosopher's Stone"), details, titleNormalizer)

    // The English title repeats the primary, so the ladder is primary then US.
    titles.candidates shouldBe Seq("Harry Potter and the Philosopher's Stone", "Harry Potter and the Sorcerer's Stone")
  }

  it should "compare rungs case-insensitively" in {
    val details = Some(TmdbClient.Details(englishTitle = Some("QZX DUNE"), releaseYear = None, usTitle = Some("qzx dune")))
    val titles = RatingSiteTitles.derive(keyFor("Wydma Qzx", None), withOriginal("Qzx Dune"), details, titleNormalizer)

    titles.candidates shouldBe Seq("Qzx Dune")
  }
}
