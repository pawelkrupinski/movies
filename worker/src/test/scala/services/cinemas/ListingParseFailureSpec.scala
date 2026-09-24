package services.cinemas

import clients.tools.UrlFragmentHttpFetch
import models.{CinemaCityPoznanPlaza, KinoDiana, KinoJOK}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{Cinema1Client, CinemaCityClient, FilmwebShowtimesClient, KinoCentrumCswClient, KinoJOKClient}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDate

/**
 * A listing body that doesn't parse at all (a maintenance page, a WordPress
 * error object, a proxy's HTML answering 200) is a FAILED read, not a venue
 * with nothing on: it has to fail the scrape — red, with its cause — rather
 * than fold into an empty list that reads white. A well-formed EMPTY listing
 * still means "nothing scheduled" and must stay a clean empty result.
 */
class ListingParseFailureSpec extends AnyFlatSpec with Matchers {

  private val ErrorPage = "<!doctype html><html><body>Przerwa techniczna</body></html>"

  "KinoCentrumCswClient.parseRepertoire" should "throw on an unparseable body but read an empty array as no screenings" in {
    an[Exception] should be thrownBy KinoCentrumCswClient.parseRepertoire(ErrorPage)
    an[Exception] should be thrownBy KinoCentrumCswClient.parseRepertoire("""{"code":"rest_no_route","message":"No route"}""")
    KinoCentrumCswClient.parseRepertoire("[]") shouldBe empty
  }

  "Cinema1Client.parseScreenings" should "throw on an unparseable body but read an empty array as no screenings" in {
    an[Exception] should be thrownBy Cinema1Client.parseScreenings(ErrorPage)
    Cinema1Client.parseScreenings("[]") shouldBe empty
  }

  private val day = LocalDate.of(2026, 9, 23)
  private def cinemaCity(dates: String, events: String) = new CinemaCityClient(
    new UrlFragmentHttpFetch(Seq("/dates/in-cinema/" -> dates, "/film-events/in-cinema/" -> events)),
    titles = titleNormalizer)

  "CinemaCityClient.fetchDay" should "throw on an unparseable day body so that day's chunk is retried" in {
    an[Exception] should be thrownBy cinemaCity("", ErrorPage).fetchDay("1078", CinemaCityPoznanPlaza, day)
  }

  it should "read a well-formed day with no films as empty" in {
    cinemaCity("", """{"body":{"films":[],"events":[]}}""").fetchDay("1078", CinemaCityPoznanPlaza, day) shouldBe empty
  }

  "CinemaCityClient.dates" should "throw on an unparseable dates body so the plan fails red" in {
    an[Exception] should be thrownBy cinemaCity(ErrorPage, "").dates("1078")
  }

  it should "read a well-formed body with no dates as an empty plan" in {
    cinemaCity("""{"body":{"dates":[]}}""", "").dates("1078") shouldBe empty
  }

  "KinoJOKClient.parse" should "throw on an unparseable body but read an empty event list as no screenings" in {
    an[Exception] should be thrownBy KinoJOKClient.parse(ErrorPage, KinoJOK)
    KinoJOKClient.parse("""{"events":[]}""", KinoJOK) shouldBe empty
  }

  // Filmweb answering EVERY day with an error page is a Filmweb that is down; each day used
  // to parse as a quiet one, so the whole scrape read as a dormant venue.
  "FilmwebShowtimesClient" should "fail the scrape when every day's page is unparseable" in {
    val errorPages = new tools.HttpFetch {
      def get(url: String): String = ErrorPage
      def post(url: String, body: String, contentType: String): String = ErrorPage
    }
    an[Exception] should be thrownBy
      new FilmwebShowtimesClient(errorPages, 2352, KinoDiana, daysAhead = 2, today = day).fetch()
  }
}
