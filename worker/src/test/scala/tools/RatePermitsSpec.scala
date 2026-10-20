package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/** The CI-only decorator that holds every parallel recording leg on ci-runner's one egress address to one rate
 *  budget per service, and its client for the runner's permit service. */
class RatePermitsSpec extends AnyFlatSpec with Matchers {

  private val TmdbUrl = "https://api.themoviedb.org/3/movie/550?api_key=k"

  /** One timeline of permits asked and requests made, so a test can say which came first. */
  private final class Timeline {
    private val events = new ConcurrentLinkedQueue[String]()
    def add(event: String): Unit = { events.add(event); () }
    def seen: Seq[String] = events.asScala.toSeq
    val permits: RatePermits = service => add(s"permit $service")
  }

  /** A wire that answers 429 to its first `refusals` requests, then the URL back. */
  private final class Wire(timeline: Timeline, refusals: Int = 0) extends GetOnlyHttpFetch {
    private val left = new java.util.concurrent.atomic.AtomicInteger(refusals)
    override def get(url: String): String = {
      timeline.add(s"get $url")
      if (left.getAndDecrement() > 0) throw new HttpStatusException(429, "GET", url, None)
      url
    }
  }

  "a rate-permitted wire" should "ask a permit for the request's service before the request goes out" in {
    val timeline = new Timeline
    new RatePermittedHttpFetch(new Wire(timeline), timeline.permits).get(TmdbUrl) shouldBe TmdbUrl
    timeline.seen shouldBe Seq("permit tmdb", s"get $TmdbUrl")
  }

  it should "take a fresh permit for every retry, under the 429 gate" in {
    val timeline = new Timeline
    val chain = new ThrottledHttpFetch(new RatePermittedHttpFetch(new Wire(timeline, refusals = 2), timeline.permits),
      jitterMillis = () => 0, clock = new MutableClock(Instant.EPOCH), sleep = _ => ())
    chain.get(TmdbUrl) shouldBe TmdbUrl
    timeline.seen shouldBe Seq.fill(3)(Seq("permit tmdb", s"get $TmdbUrl")).flatten
  }

  it should "ask each API under its own service, through every member" in {
    val timeline = new Timeline
    val fetch = new RatePermittedHttpFetch(new Wire(timeline), timeline.permits)
    fetch.get("https://caching.graphql.imdb.com/?q=1", Map("x" -> "y"))
    fetch.getBytes("https://www.wikidata.org/w/api.php")
    fetch.getAsync("https://www.omdbapi.com/?i=tt1").join()
    timeline.seen.filter(_.startsWith("permit")) shouldBe Seq("permit imdb", "permit wikidata", "permit omdb")
  }

  it should "ask no permit for a cinema site or an unnamed host" in {
    val timeline = new Timeline
    val fetch = new RatePermittedHttpFetch(new Wire(timeline), timeline.permits)
    fetch.get("https://www.kinoteka.pl/repertuar")
    fetch.get("https://api.telegram.org/bot/send")
    timeline.seen.filter(_.startsWith("permit")) shouldBe empty
  }

  "RatePermits.over" should "hand the wire back unchanged where no permit service is named (github.com, production)" in {
    val wire = new Wire(new Timeline)
    RatePermits.over(wire, RatePermits.from(new settings.ProcessConfiguration(Env.of()).ratePermitService)) should be theSameInstanceAs wire
  }

  it should "ask the service FORGEJO_RATE_PERMITS names" in {
    val service = new FakeRatePermitService
    try {
      val timeline = new Timeline
      RatePermits.over(new Wire(timeline), RatePermits.from(new settings.ProcessConfiguration(Env.of("FORGEJO_RATE_PERMITS" -> service.url)).ratePermitService)).get(TmdbUrl)
      service.services shouldBe Seq("tmdb")
      timeline.seen shouldBe Seq(s"get $TmdbUrl")
    } finally service.close()
  }

  "the permit service's client" should "ask again while the service says the next permit is too far off" in {
    val service = new FakeRatePermitService(429, 429)
    try {
      val paused = new ConcurrentLinkedQueue[Long]()
      new HttpRatePermits(service.url, sleep = ms => { paused.add(ms); () }).acquire("rottentomatoes")
      service.services shouldBe Seq.fill(3)("rottentomatoes")
      paused.asScala.toSeq shouldBe Seq(1000L, 1000L)
    } finally service.close()
  }

  it should "let the request go out when the service answers something unexpected" in {
    val service = new FakeRatePermitService(500)
    try {
      new HttpRatePermits(service.url, sleep = _ => fail("a 500 is not a busy service")).acquire("tmdb")
      service.services shouldBe Seq("tmdb")
    } finally service.close()
  }

  it should "let the request go out when the service is down" in {
    val service = new FakeRatePermitService
    val url = service.url
    service.close()
    new HttpRatePermits(url, sleep = _ => fail("a refused connection is not a busy service")).acquire("tmdb")
  }
}
