package services.identity

import models.{CinemaMovie, Helios, KinoApollo, Movie}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import services.movies.SingleCountryNormalizer

import java.time.LocalDate

/**
 * A listing's equality is what the model holds a re-published listing to: an equal one is not resolved again, and its
 * hash keys every map of listings. So equality must read every field the model reads and none it does not (the poster,
 * the names and words of its synopsis, whether its page's facts are merged in), equal listings must hash equally, and the
 * readings of its days ([[Listing.screenedSeason]], [[Listing.airDays]], [[Listing.anniversaryScreeningYear]]) count,
 * the days themselves not.
 */
class ListingEqualitySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks {
  import ListingEqualitySpec.*

  "Listing equality" should "be exactly the fields the model reads, whichever differ" in {
    forAll(pairs, minSuccessful(3000)) { case (a, b) => (a == b) shouldBe (modelReads(a) == modelReads(b)) }
  }

  it should "agree with the hash: equal listings hash equally" in {
    forAll(pairs, minSuccessful(3000)) { case (a, b) => if (a == b) a.## shouldBe b.## }
  }

  it should "be reflexive and symmetric, and never equal another kind of value" in {
    forAll(pairs, minSuccessful(500)) { case (a, b) =>
      a shouldBe a.copy()
      (a == b) shouldBe (b == a)
      a.equals(modelReads(a)) shouldBe false
    }
  }

  it should "read no field outside the model's: the poster, the synopsis' names and words, the page's facts merged" in {
    forAll(listings, minSuccessful(300)) { a =>
      val b = a.copy(poster = Some("https://example.com/p.jpg"), names = VenueNames.of(Seq("Wojciech Has")),
        words = ProseWords.ofWords(Seq("Lalka")), pageFacts = !a.pageFacts)
      a shouldBe b
      a.## shouldBe b.##
    }
  }
}

object ListingEqualitySpec {

  /** Every field the model reads, as [[Listing.equals]] compared them when its equality was first written. */
  def modelReads(l: Listing): Any =
    (l.key, l.rawTitle, l.title, l.cleanTitle, l.year, l.directors, l.runtime, l.page, l.originalTitle, l.countries, l.catalogueIds,
      l.searchTitle, l.properTitle, l.cinema, l.screenedSeason, l.listedCast, l.certificate, l.yearUpperBound, l.performanceYear,
      l.anniversaryScreeningYear, l.airDays, l.kept)

  // Stage works with and without a season or year, an anniversary, a film: what the readings of the days tell apart.
  private val Titles = Seq("Samson i Dalila", "Met Opera 2024-25: Manon", "Royal Ballet & Opera 2024-25: Romeo and Juliet",
    "The Host: 20th Anniversary", "MetOpera: Carmen (2009)", "Lalka")
  private val Days = Seq(Nil, Seq(LocalDate.of(2026, 12, 5)), Seq(LocalDate.of(2027, 5, 25)),
    Seq(LocalDate.of(2026, 12, 5), LocalDate.of(2026, 12, 26)))

  private def published(title: String): Listing =
    Listing.of(KinoApollo, CinemaMovie(Movie(title), KinoApollo, None, None, None, Nil, Nil, Nil), SingleCountryNormalizer.titleNormalizer)
  private val Published = Titles.map(t => t -> published(t)).toMap

  private def years: Gen[Option[Int]] = Gen.option(Gen.oneOf(2009, 2024, 2025, 2026))

  /** One field redrawn, from a few values each, so two listings often agree on it. */
  private val changes: Seq[Gen[Listing => Listing]] = Seq(
    Gen.oneOf(Titles).map(t => (l: Listing) => l.copy(key = Published(t).key)),
    Gen.oneOf(Titles).map(t => (l: Listing) => l.copy(rawTitle = t)),
    Gen.oneOf(Titles).map(t => (l: Listing) => l.copy(title = t)),
    Gen.oneOf(Titles).map(t => (l: Listing) => l.copy(cleanTitle = t)),
    Gen.oneOf(KinoApollo, Helios).map(c => (l: Listing) => l.copy(cinema = c)),
    years.map(y => (l: Listing) => l.copy(year = y)),
    Gen.oneOf(Nil, Seq("Wojciech Has")).map(d => (l: Listing) => l.copy(directors = d)),
    Gen.option(Gen.oneOf(90, 190)).map(r => (l: Listing) => l.copy(runtime = r)),
    Gen.option(Gen.const("https://kino/1")).map(p => (l: Listing) => l.copy(page = p)),
    Gen.option(Gen.oneOf(Titles)).map(t => (l: Listing) => l.copy(originalTitle = t)),
    Gen.oneOf(Nil, Seq("PL")).map(c => (l: Listing) => l.copy(countries = c)),
    Gen.oneOf(Nil, Seq(CatalogueId("filmweb", "1"))).map(c => (l: Listing) => l.copy(catalogueIds = c)),
    Gen.option(Gen.oneOf(Titles)).map(t => (l: Listing) => l.copy(searchTitle = t)),
    Gen.option(Gen.oneOf(Titles)).map(t => (l: Listing) => l.copy(properTitle = t)),
    Gen.oneOf(Days).map(d => (l: Listing) => l.copy(screenings = ScreeningDays.of(d))),
    Gen.oneOf(ListedCast.None, ListedCast.of(Seq("Wojciech Has"))).map(c => (l: Listing) => l.copy(listedCast = c)),
    Gen.option(Gen.oneOf("12", "18")).map(c => (l: Listing) => l.copy(certificate = c)),
    years.map(y => (l: Listing) => l.copy(yearUpperBound = y)),
    years.map(y => (l: Listing) => l.copy(performanceYear = y)),
    Gen.oneOf(true, false).map(k => (l: Listing) => l.copy(kept = k)),
    Gen.option(Gen.const("https://kino/p.jpg")).map(p => (l: Listing) => l.copy(poster = p)),
    Gen.oneOf(true, false).map(p => (l: Listing) => l.copy(pageFacts = p)))

  private def changed(from: Listing, most: Int): Gen[Listing] =
    Gen.choose(0, most).flatMap(n => Gen.listOfN(n, Gen.oneOf(changes).flatMap(identity))).map(_.foldLeft(from)((l, f) => f(l)))

  val listings: Gen[Listing] = Gen.oneOf(Titles).flatMap(t => changed(Published(t), 8))

  /** A listing and another of a few fields redrawn: equal often enough, and unequal in any one field. */
  val pairs: Gen[(Listing, Listing)] = listings.flatMap(a => Gen.frequency(1 -> Gen.const(a.copy()), 4 -> changed(a, 2)).map(a -> _))
}
