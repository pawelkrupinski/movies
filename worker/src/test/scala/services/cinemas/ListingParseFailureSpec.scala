package services.cinemas

import clients.tools.UrlFragmentHttpFetch
import models.{CinemaCityPoznanPlaza, KinoDiana, KinoFarys, KinoJOK, KinoLen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{BiletynaClient, Cinema1Client, CinemaCityClient, FilmwebShowtimesClient, KinoCentrumCswClient, KinoJOKClient, SystemBiletowyClient}
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

  // A VisualSoft instance answers a feed it can't serve with {"error": …}. The
  // advanced feed's missing-template error is the one that means "ask the plain
  // feed"; any other error body, or one from the plain feed too, is a failed read.
  "SystemBiletowyClient.parse" should "throw on an error or unparseable body but read an empty programme as no screenings" in {
    an[Exception] should be thrownBy SystemBiletowyClient.parse(ErrorPage, KinoFarys, "https://kfb.example", titleNormalizer)
    an[Exception] should be thrownBy
      SystemBiletowyClient.parse("""{"error":"Internal error","code":500}""", KinoFarys, "https://kfb.example", titleNormalizer)
    SystemBiletowyClient.parse("""{"meta":{"nbResults":0},"repertoires":[]}""", KinoFarys, "https://kfb.example", titleNormalizer) shouldBe empty
  }

  "SystemBiletowyClient" should "fail the scrape when the plain feed errors too" in {
    val missingTemplate = """{"error":"The template \"listAdvancedSuccess.json.php\" does not exist or is unreadable in \"\".","code":200}"""
    val http = new UrlFragmentHttpFetch(Seq("advanced=1" -> missingTemplate, "list.json" -> """{"error":"Internal error","code":500}"""))
    an[Exception] should be thrownBy new SystemBiletowyClient(http, "https://kfb.example", KinoFarys, titleNormalizer).fetch()
  }

  // A full biletyna place page (50 events) is topped up from the hall's event
  // feed. A feed that answers with an error, or with records none of which
  // parse, would otherwise cut the venue at 50 again without a trace.
  private val fullPlacePage = new clients.tools.FakeHttpFetch("biletyna-filmweb-desynced").get("https://biletyna.pl/Zyrardow/Kino-Len")
  private def lenWithFeed(feed: String) =
    new BiletynaClient(new UrlFragmentHttpFetch(Seq("/ajax/events" -> feed, "Kino-Len" -> fullPlacePage)),
      "https://biletyna.pl/Zyrardow/Kino-Len", KinoLen)

  "BiletynaClient" should "fail the scrape when a full page's event feed answers with an error" in {
    an[Exception] should be thrownBy lenWithFeed("""{"status":false,"message":"error"}""").fetch()
  }

  it should "fail the scrape when a full page's event feed holds records none of which parse" in {
    an[Exception] should be thrownBy lenWithFeed("""{"status":true,"events":[{"event_id":1,"artist_name":"X","event_date":"soon"}]}""").fetch()
  }
}
