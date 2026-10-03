package clients.biletyna

import clients.tools.FakeHttpFetch
import com.github.benmanes.caffeine.cache.Ticker
import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.{BiletynaClient, BiletynaNationalFeed, BiletynaPlacePage}
import tools.HttpFetch

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.concurrent.duration._

/** Replays biletyna's national event feed and three venues' place pages, all
 *  captured live 2026-09-27 within the same minute, so the two reads can be held
 *  to each other screening for screening. */
class BiletynaNationalFeedSpec extends AnyFlatSpec with Matchers {

  /** Records every URL asked for, so a test can say which requests a read cost. */
  private class Recorded(underlying: HttpFetch) extends HttpFetch {
    val asked: mutable.Buffer[String] = mutable.Buffer.empty
    def get(url: String): String = { asked.synchronized(asked += url); underlying.get(url) }
    def post(url: String, body: String, contentType: String): String = underlying.post(url, body, contentType)
  }

  private val venues = Seq(
    KinoUciechaCzluchow -> BiletynaPlacePage("https://biletyna.pl/Czluchow/Kino-Uciecha"),
    KinoMuzaWloszczowa  -> BiletynaPlacePage("https://biletyna.pl/Wloszczowa/Dom-Kultury-we-Wloszczowie"),
    KinoMorskieOko      -> BiletynaPlacePage("https://biletyna.pl/Krasnystaw/Kino-Morskie-Oko"),
  )

  private def slots(movies: Seq[CinemaMovie]) =
    movies.flatMap(m => m.showtimes.map(s => (m.movie.title, s.dateTime, s.bookingUrl))).toSet

  private def feedOver(http: HttpFetch, ticker: Ticker = Ticker.systemTicker()) =
    new BiletynaNationalFeed(http, venues.map(_._2).toSet, ticker = ticker)

  "a venue read off the national feed" should "list exactly the screenings its own place page does" in {
    val http = new FakeHttpFetch("biletyna-national")
    val feed = feedOver(http)
    for ((cinema, page) <- venues) {
      val central = new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch()
      val own     = new BiletynaClient(http, page, cinema).fetch()
      central should not be empty
      slots(central) shouldBe slots(own)
    }
  }

  it should "read the feed once for every venue scraped inside its TTL, and never a place page" in {
    val http = new Recorded(new FakeHttpFetch("biletyna-national"))
    val feed = feedOver(http)
    for ((cinema, page) <- venues) new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch()
    http.asked.toSeq shouldBe (1 to 7).map(BiletynaNationalFeed.pageUrl)
  }

  it should "read the feed again once its TTL has passed" in {
    val http  = new Recorded(new FakeHttpFetch("biletyna-national"))
    val now   = new AtomicLong(0L)
    val feed  = feedOver(http, () => now.get)
    val (cinema, page) = venues.head
    new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch()
    now.addAndGet((BiletynaNationalFeed.DefaultTtl + 1.second).toNanos)
    new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch()
    http.asked.count(_ == BiletynaNationalFeed.pageUrl(1)) shouldBe 2
  }

  "a venue whose feed read fails" should "fall back to its own place page, not read empty" in {
    // The place pages are recorded; the feed pages are not, so every feed read throws.
    val http = new FakeHttpFetch("filmweb-only-switch")
    val (cinema, page) = venues.head
    val movies = new BiletynaClient(http, page, cinema, nationalFeed = Some(feedOver(http))).fetch()
    slots(movies) shouldBe slots(new BiletynaClient(http, page, cinema).fetch())
    movies should not be empty
  }

  it should "not cost every other venue in the window the same failed feed read" in {
    val http = new Recorded(new FakeHttpFetch("filmweb-only-switch"))
    val now  = new AtomicLong(0L)
    val feed = feedOver(http, () => now.get)
    for ((cinema, page) <- venues) new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch()
    http.asked.count(_ == BiletynaNationalFeed.pageUrl(1)) shouldBe 1
    now.addAndGet((BiletynaNationalFeed.FailureHold + 1.second).toNanos)
    new BiletynaClient(http, venues.head._2, venues.head._1, nationalFeed = Some(feed)).fetch()
    http.asked.count(_ == BiletynaNationalFeed.pageUrl(1)) shouldBe 2
  }

  // Were the server to serve fewer records a page than `ipp` asks, page 1 would already be short,
  // and a walk that stops on a short page would keep a slice of the country as all of it.
  it should "read past a short first page when the server clamps its page size" in {
    val replay  = new FakeHttpFetch("biletyna-national")
    val page    = venues.head._2
    val path    = page.url.stripPrefix("https://biletyna.pl")
    import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
    val records: Seq[JsValue] = (1 to 7).flatMap(n => (Json.parse(replay.get(BiletynaNationalFeed.pageUrl(n))) \ "events").get match {
      case o: JsObject => o.values.toSeq
      case a: JsArray  => a.value.toSeq
      case _           => Nil
    }).filter(r => (r \ "v2_hall_seo_url").asOpt[String].contains(path))
    records.size should be > 4
    val clamp   = records.size / 3   // three short pages and a remainder
    val clamped = new HttpFetch {
      def get(url: String): String = {
        val n = url.split("page=").last.toInt
        Json.obj("status" -> true, "events" -> JsArray(records.slice((n - 1) * clamp, n * clamp))).toString
      }
      def post(url: String, body: String, contentType: String): String = replay.post(url, body, contentType)
    }
    feedOver(clamped).eventsAt(page).map(_.size) shouldBe Some(records.size)
  }

  "a venue the feed files nothing under" should "be read off its own place page" in {
    val http = new Recorded(new FakeHttpFetch("biletyna-national"))
    val feed = new BiletynaNationalFeed(http, Set.empty)   // knows none of our halls
    val (cinema, page) = venues.head
    new BiletynaClient(http, page, cinema, nationalFeed = Some(feed)).fetch() should not be empty
    http.asked should contain (page.url)
  }
}
