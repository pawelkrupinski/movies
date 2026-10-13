package services.movies

import models.{Country, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * What two replays of one corpus are compared on. Run 38251516862's United Kingdom order leg held
 * the same films on every pass — its screenings and rendered rows matched — yet failed on its films:
 * nine "Royal Ballet & Opera: Swan Lake" records with no year, two "Secret Screening"s, each pass
 * listing them in its own arrival order, because the records were ordered by title and year alone.
 */
class OrderIndependentIdsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.UnitedKingdom)

  private def film(title: String, key: String, mintedAs: String) =
    StoredMovieRecord(title, None, MovieRecord(), FilmId(mintedAs), storedKey = Some(key))

  "two replays holding the same same-titled films" should "list them identically whichever arrived first" in {
    val aberdeen = film("Secret Screening", "secretscreening|cineworld1000053600", mintedAs = "a")
    val swindon  = film("Secret Screening", "secretscreening|cineworld1000053008", mintedAs = "b")

    val first  = OrderIndependentIds(Seq(aberdeen, swindon), normalizer).stableRecords
    val second = OrderIndependentIds(Seq(swindon.copy(id = FilmId("a")), aberdeen.copy(id = FilmId("b"))), normalizer).stableRecords

    second shouldBe first
  }

  "the films diff" should "pair each of several same-titled films with itself" in {
    val manon = (venue: String) => StoredMovieRecord("Royal Ballet & Opera: Manon", None,
      MovieRecord(rottenTomatoesUrl = Some(s"https://www.rottentomatoes.com/m/$venue")), FilmId(s"manon|$venue"))
    val films = Seq(manon("hexham"), manon("lancaster"))

    CorpusDiff.records(films, films.reverse, "pass0", "pass1") shouldBe ""
  }
}
