package services.identity

import models.{CinemaMovie, KinoMuza, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** The people a listing credits as its directors, as the identity model reads them: one per person, though venues
 *  join two in one credit. PL corpus of recording 37071880312. */
class ListingDirectorCreditsSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private def directors(credits: String*) =
    Listing.of(KinoMuza, CinemaMovie(Movie("Film"), KinoMuza, None, None, None, Nil, credits, Nil), normalizer).directors

  "A director credit" should "name each person a venue joins in it" in {
    // "W imię matki, córki i innych dziewczyn" searched "Arash T. Riahi & Verena Soltiz" as one person, finding nothing
    directors("Arash T. Riahi & Verena Soltiz") shouldBe Seq("Arash T. Riahi", "Verena Soltiz")
    directors("Joel Crawford i Januel Mercado") shouldBe Seq("Januel Mercado", "Joel Crawford")
    directors("Natasha Merkulova, Aleksey Chupov") shouldBe Seq("Aleksey Chupov", "Natasha Merkulova")
  }

  it should "name each person its detail page joins, for a listing crediting nobody itself" in {
    // Kino Muza's "W imię matki, córki i innych dziewczyn" publishes its directors only on the film's page
    val bare = Listing.of(KinoMuza, CinemaMovie(Movie("W imię matki, córki i innych dziewczyn"), KinoMuza, None, None, None, Nil, Nil, Nil), normalizer)
    Evidence.of(bare, Some(DetailFacts(None, Seq("Arash T. Riahi & Verena Soltiz"), None, None))).directors shouldBe
      Seq("Arash T. Riahi", "Verena Soltiz")
  }

  it should "stay whole when a part is no name: a credit line, or a single word" in {
    directors("Karina Grabowska scenografia i animacje: Agata Kurzak") shouldBe Seq("Karina Grabowska scenografia i animacje: Agata Kurzak")
    directors("Ida i Zofia Kowalska") shouldBe Seq("Ida i Zofia Kowalska")
    directors("Andrzej Wajda") shouldBe Seq("Andrzej Wajda")
  }
}
