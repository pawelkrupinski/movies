package services.identity

import models.{CinemaMovie, KinoApollo, KinoMuranow, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime
import scala.util.Random

/** A listing's total order is its fields in turn — the order of the `\u0000`-joined sort key, which
 *  a listing no longer carries: one such string per listing held was ~30 MB on the US worker. */
class ListingOrderSpec extends AnyFlatSpec with Matchers {

  private val show = Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None)
  private def listing(cinema: models.Cinema, title: String, raw: Option[String] = None, year: Option[Int] = None,
                      directors: Seq[String] = Nil, runtime: Option[Int] = None, page: Option[String] = None,
                      original: Option[String] = None): Listing =
    Listing.of(cinema, CinemaMovie(Movie(title, releaseYear = year, runtimeMinutes = runtime, rawTitle = raw, originalTitle = original),
      cinema, None, page, None, Nil, directors, Seq(show)), SingleCountryNormalizer.titleNormalizer)

  "a listing's order" should "be its sort key's order, whatever the fields hold" in {
    val random = new Random(11)
    def pick[A](xs: Seq[A]): A = xs(random.nextInt(xs.size))
    val listings = (1 to 400).map { _ =>
      listing(pick(Seq(KinoApollo, KinoMuranow)), pick(Seq("Belle", "Bell", "Belle 2", "", "Ä")), pick(Seq(None, Some("Belle"), Some("Belle - 2D"))),
        pick(Seq(None, Some(999), Some(2013), Some(20130))), pick(Seq(Nil, Seq("Ana"), Seq("Ana", "Bo"), Seq("Ana,Bo"))),
        pick(Seq(None, Some(9), Some(95), Some(110))), pick(Seq(None, Some("https://a"), Some("https://a/b"))),
        pick(Seq(None, Some("Belle"), Some("Bel"))))
    }
    listings.sorted shouldBe listings.sortBy(_.sortKey)
    for { a <- listings.take(60); b <- listings.take(60) } withClue(s"$a vs $b: ")(
      Integer.signum(Listing.ordering.compare(a, b)) shouldBe Integer.signum(a.sortKey.compareTo(b.sortKey)))
  }

  it should "keep the same listing of a key published twice, countries apart, whichever arrives first" in {
    val plain   = listing(KinoApollo, "Belle")
    val country = plain.copy(countries = Seq("FR"))
    Listing.distinct(Seq(plain, country)) shouldBe Listing.distinct(Seq(country, plain))
  }

  it should "not be carried by every listing as a string" in {
    val held = classOf[Listing].getDeclaredFields.map(_.getName).filter(_.toLowerCase.contains("sortkey"))
    withClue("a field holding the joined sort key: ")(held shouldBe empty)
  }
}
