package services.identity

import models.{Cinema, CinemaMovie, City, Country, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.Json

import java.nio.file.{Files, Paths}

class CertificatesSpec extends AnyFlatSpec with Matchers {

  // Curry Barker's "Obsession" (2026), recorded in UK recording 38713274617 from
  // /3/movie/1339713?language=en-GB&append_to_response=credits,release_dates: GB 18 (three releases), US R, DE 16, ES 18.
  private lazy val obsession =
    TmdbFilmRecord.parse(Seq(Json.parse(Files.readString(Paths.get("test/resources/fixtures/tmdb/movie_1339713_obsession_gb.json"))))).get._1

  private def venueIn(country: Country) = Cinema.all.find(c => City.forCinema(c).exists(_.country == country)).get
  private def listing(country: Country, certificate: Option[String]) = {
    val cinema = venueIn(country)
    Listing(cinema, services.movies.ListingKey.Published(cinema.displayName, "Obsession", None, Nil), "Obsession", "Obsession", "Obsession",
      None, Nil, Some(96), None, None, certificate = certificate)
  }

  "The weighted certificate measure" should "weigh a certificate only beside a film the billing names as a whole title" in {
    val tiger = IdentityMeasures.Film("The Tiger Who Came to Tea", year = Some(2019), certificates = Some("GB=U"))
    def certificate(title: String) =
      IdentityMeasures.listingFilm(IdentityMeasures.Listing(title, certificate = Some("U")), tiger, Some(1), 0, 0, country = Some("GB"))("certificate")
    certificate("The Tiger Who Came to Tea") shouldBe IdentityMeasures.Category("match")
    // UK Hyde Park Picture House bills two shorts under one U: the certificate rates the programme, not a film it names by a piece
    certificate("The Tiger Who Came for Tea and We're Going on a Bear Hunt") shouldBe IdentityMeasures.Missing("piece")
  }

  it should "re-resolve a listing whose certificate moved: the model reads it" in {
    listing(Country.UnitedKingdom, Some("PG")) should not equal listing(Country.UnitedKingdom, Some("18"))
    listing(Country.UnitedKingdom, Some("PG")).## should not equal listing(Country.UnitedKingdom, Some("18")).##
  }

  "A film's record" should "keep the least restrictive certificate TMDB records in each country a rating scale is read for" in {
    obsession.certificates shouldBe Some("DE=16,ES=18,GB=18,US=R")
    Certificates.in(obsession.certificates, "GB") shouldBe Some("18")
    Certificates.in(obsession.certificates, "PL") shouldBe None
  }

  "A venue's certificate" should "deny a film rated three steps or more above it in the venue's country" in {
    // UK Quad Derby's "Obsession (PG)" at 96 minutes: PG is three steps below the 2026 film's BBFC 18
    Certificates.below(listing(Country.UnitedKingdom, Some("PG")), obsession) shouldBe
      Some("the venue rates it PG, 3 steps below 'Obsession' (2026)'s GB certificate 18")
    Certificates.below(listing(Country.UnitedKingdom, Some("U")), obsession) shouldBe defined
    // the scale's own spellings, as German venues bill them
    Certificates.below(listing(Country.Germany, Some("FSK 0")), obsession) shouldBe defined
  }

  it should "deny nothing two steps or fewer below, nor a certificate its country's scale does not read" in {
    Certificates.below(listing(Country.UnitedKingdom, Some("12A")), obsession) shouldBe None
    Certificates.below(listing(Country.UnitedKingdom, Some("15")), obsession) shouldBe None
    Certificates.below(listing(Country.Germany, Some("FSK 6")), obsession) shouldBe None
    // a US rating at a UK venue, a placeholder, or no certificate at all say nothing
    Certificates.below(listing(Country.UnitedKingdom, Some("G")), obsession) shouldBe None
    Certificates.below(listing(Country.UnitedKingdom, Some("CERT-TBC")), obsession) shouldBe None
    Certificates.below(listing(Country.UnitedKingdom, None), obsession) shouldBe None
    // nor does a record with no certificate in the venue's country
    Certificates.below(listing(Country.UnitedKingdom, Some("PG")), obsession.copy(certificates = Some("US=R"))) shouldBe None
  }

  "A listing" should "carry the certificate its venue bills it with" in {
    val cinema = venueIn(Country.UnitedKingdom)
    val scraped = CinemaMovie(Movie("Obsession"), cinema, None, None, None, Nil, Nil, Nil, ageRating = Some("PG"))
    Listing.of(cinema, scraped, services.movies.TitleNormalizer.forCountry(Country.UnitedKingdom)).certificate shouldBe Some("PG")
  }
}
