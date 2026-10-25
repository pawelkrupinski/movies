package services.identity
package adapters

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{ListingKey, SingleCountryNormalizer}
import settings.{IdentityLookupFillInterval, IdentityLookupFillRate}
import tools.{DaemonExecutors, HttpFetch, HttpStatusException, PacedHttpFetch, TestWiring, TickRecorder}

import java.time.{Clock, Instant, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

/** The shadow run's paced live lookup fill: asks exactly the resolver's unanswered questions, once
 *  each, into the model's normalized TMDB store only; never more than its rate allows; stops at the
 *  first sign of overload; and the next shadow resolve reads what it filed. */
class LookupFillSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private def listing(venue: Cinema, title: String, year: Option[Int], director: Seq[String]): Listing =
    Listing(venue, ListingKey.Published(venue.displayName, title, year, director), title, title, title, year, director, None, None, None)
  // Forty films, each a title search and a director walk the store has never seen.
  private val listings = (1 to 40).map(i => listing(if (i % 2 == 0) Multikino else Helios, s"Film number $i", Some(2026), Seq(s"Director $i")))

  /** The service: every request counted; `failWith` answers the given request numbers with an
   *  error. The body is an empty result set — this spec is about which questions are asked and
   *  where the answers go, not what TMDB says (`ShadowIdentityReaperIntegrationSpec` replays real
   *  answers). */
  private final class Service(failWith: Map[Int, Throwable] = Map.empty, failHost: Option[String] = None) extends HttpFetch {
    val requests = mutable.ArrayBuffer.empty[String]
    private def answer(url: String): String = {
      requests += url
      failWith.get(requests.size).foreach(e => throw e)
      // The host fails its FIRST request only: an overload, then a service that has recovered.
      failHost.filter(h => url.contains(h) && requests.count(_.contains(h)) == 1)
        .foreach(h => throw new HttpStatusException(503, "GET", s"https://$h/", None))
      // IMDb's suggestions as IMDb answers a title it knows nothing by; TMDB's empty result set for the rest
      if (url.startsWith(services.enrichment.ImdbClient.SuggestionBase)) """{"d":[],"q":"x","v":1}"""
      else """{"results":[],"crew":[],"cast":[]}"""
    }
    override def get(url: String): String                                    = answer(url)
    override def get(url: String, headers: Map[String, String]): String      = answer(url)
    override def post(url: String, body: String, contentType: String): String = answer(url)
  }

  private def fill(store: TmdbStore, service: HttpFetch, rate: Int = 600, sleeps: mutable.Buffer[Long] = mutable.Buffer.empty,
                   rounds: mutable.Buffer[LookupFillRound] = mutable.Buffer.empty,
                   renewals: () => Seq[Question] = () => Nil,
                   gaps: TmdbStore => AnswersChanged = modelGaps, memory: Option[QuestionMemory] = None,
                   fillClock: java.time.Clock = Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC),
                   ticks: TickRecorder = TickRecorder.Unrecorded,
                   executor: java.util.concurrent.ExecutorService = DaemonExecutors.directExecutor()) =
    new LookupFill(() => gaps(store), new clients.TmdbClient(_, apiKey = Some(settings.TmdbApiKey("k"))),
      service, new TmdbNormalizer(store), IdentityLookupFillRate(rate), IdentityLookupFillInterval(30.minutes), rounds += _,
      executor, sleeps += _, renewals,
      // a memory of its own unless the test shares one: nothing remembered, every gap due
      memory.getOrElse(new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", fillClock)), fillClock, ticks)

  /** What the identity model over the store's answers finds unanswered — what a round asks. */
  private def modelGaps(s: TmdbStore): AnswersChanged = {
    val model = new IncrementalResolver(new StoredTmdbLookups(s, "pl-PL", UnansweredTmdbLookups, new ObservationReads), normalizer,
      IdentityCalibration.resolver)
    model.batch(seen = listings)
    model.gaps
  }

  private def store() = new TmdbStore(new InMemoryTmdbDocuments, Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC))
  private def gapsOf(s: TmdbStore): Int = { val g = modelGaps(s); g.queries.size + g.films.size }

  "a fill round beside a normalized store" should "ask aged questions live, within what its allowance has left" in {
    val tmdb    = store()
    val service = new Service
    val aged    = (1 to 1000).map(i => CandidateQuery.Title(s"Aged film $i"))
    val round = fill(tmdb, service, rate = 60,
      renewals = () => aged.map(Question.Candidate(_)),
      gaps = _ => AnswersChanged.Empty).round()
    service.requests.map(u => java.net.URLDecoder.decode(u, "UTF-8")).count(_.contains("query=Aged film")) shouldBe round.asked
    round.asked should (be > 0 and be <= IdentityLookupFillRate(60).allowanceOver(IdentityLookupFillInterval(30.minutes).value))
  }

  it should "fetch again, last, the records and people the model reads that TMDB last gave a month ago" in {
    val service = new Service
    val clock   = Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC)
    val held    = new InMemoryTmdbDocuments
    val stamp   = new org.bson.BsonDocument(TmdbStore.FetchedAt, org.bson.BsonInt64(clock.millis() - 40.days.toMillis))
    held.put(TmdbKind.Film, Seq("11" -> stamp))
    held.put(TmdbKind.Person, Seq("22" -> stamp))
    val read = Set(TmdbStore.keyOf(TmdbKind.Film, "11"), TmdbStore.keyOf(TmdbKind.Person, "22"))
    val renewals = new QuestionRenewals(store(), held, "pl-PL", () => Nil, () => Some(read), clock)
    fill(store(), service, gaps = _ => AnswersChanged.Empty, renewals = () => renewals.due()).round()
    service.requests.count(_.contains("/movie/11?")) shouldBe 2   // the record's two responses
    service.requests.count(_.contains("/person/22/")) shouldBe 1
  }

  "a fill round" should "ask every unanswered question once, into the model's store, so the next shadow resolve has no gap" in {
    val tmdb         = store()
    val service      = new Service
    gapsOf(tmdb) should be > 0
    val first = fill(tmdb, service).round()
    first.asked shouldBe service.requests.size
    first.asked should be > 0
    service.requests.distinct.size shouldBe service.requests.size   // one ask per question
    first.deferred shouldBe 0
    gapsOf(tmdb) shouldBe 0

    // Everything is answered now: the next round asks nothing.
    fill(tmdb, service).round().asked shouldBe 0
    service.requests.size shouldBe first.asked
  }

  it should "ask no more than its rate allows over the round's window, at its pace, and defer the rest" in {
    val service = new Service
    val sleeps  = mutable.Buffer.empty[Long]
    // 1 a minute over a 30-minute window: 30 asks, one a minute.
    val round = fill(store(), service, rate = 1, sleeps = sleeps).round()
    val unlimited = fill(store(), new Service).round().asked
    unlimited should be > 30
    round.asked shouldBe 30
    service.requests.size shouldBe 30
    round.deferred should be > 0
    sleeps.toSeq shouldBe Seq.fill(29)(60000L)
  }

  it should "ask, of a round the model's gaps set off soon after the last, only what the rate allows since that one" in {
    // Rounds run as a drain finds gaps (`EventTrigger`), not only once a window: each may ask the rate over the time
    // since the last round, never a whole window again.
    val clock   = new _root_.tools.MutableClock(TestWiring.FixedInstant)
    val service = new Service
    val f       = fill(store(), service, rate = 1, fillClock = clock)
    f.round().asked shouldBe 30                                  // the first: a whole window
    clock.advance(java.time.Duration.ofMinutes(5))
    f.round().asked shouldBe 5                                   // 5 minutes on: 5 at 1 a minute
    clock.advance(java.time.Duration.ofHours(2))
    f.round().asked shouldBe 30                                  // never more than a window
  }

  it should "stop asking the paced service at its first overload, and run the next round at half the rate, doubling back once clean" in {
    val tmdb    = "api.themoviedb.org"
    val service = new Service(failHost = Some(tmdb))
    val rounds  = mutable.Buffer.empty[LookupFillRound]
    val f       = fill(store(), service, rate = 8, rounds = rounds)
    val first   = f.round()
    first.backedOff shouldBe true
    service.requests.count(_.contains(tmdb)) shouldBe 1
    f.effectiveRate shouldBe IdentityLookupFillRate(4)
    f.round().rate shouldBe IdentityLookupFillRate(4)
    f.effectiveRate shouldBe IdentityLookupFillRate(8)
    rounds.map(_.rate.perMinute) shouldBe Seq(8, 4)
  }

  it should "stop asking only the host that overloaded, and keep the rate when that host was not the one it paces" in {
    // Production, 2026-09-28: every round in four countries asked a few hundred lookups, hit ONE
    // failure and stopped all asks, so a 300/min fill ran at ~12/min. The rate paces the round's
    // main service; one host's timeout says nothing about another's.
    val other   = "v3.sg.media-imdb.com"
    val service = new Service(failHost = Some(other))
    val f       = fill(store(), service, rate = 600)
    val round   = f.round()
    val (failing, rest) = service.requests.partition(_.contains(other))
    withClue(service.requests.take(12).mkString("\n")) {
      failing.size shouldBe 1                                  // the failed host: once, then deferred
      val clean = new Service
      fill(store(), clean).round()
      rest.size shouldBe clean.requests.count(!_.contains(other)) // every other host's question still asked
      rest.size should be > 1
    }
    round.backedOff shouldBe true
    f.effectiveRate shouldBe IdentityLookupFillRate(600)
  }

  // A read that FAILED taught nothing about the film. One that failed without overloading (a 403, an
  // error body) was remembered like TMDB's "nothing here" and held back a day; one that overloaded
  // (a 503) was not remembered at all and asked again every round. Both now wait a short backoff.
  it should "remember a question whose read failed for minutes, and one TMDB answered 404 for a day" in {
    val clock   = new tools.MutableClock(TestWiring.FixedInstant)
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val Seq(absent, refused, failing) = Seq("Absent", "Refused", "Failing").map(CandidateQuery.Title(_))
    val service = new HttpFetch {
      private def answer(url: String): String =
        if (url.contains("Failing")) throw new HttpStatusException(503, "GET", url, None)
        else if (url.contains("Refused")) throw new HttpStatusException(403, "GET", url, None)
        else if (url.contains("Absent")) throw new HttpStatusException(404, "GET", url, None)
        else """{"results":[],"crew":[],"cast":[]}"""
      override def get(url: String): String                                    = answer(url)
      override def get(url: String, headers: Map[String, String]): String      = answer(url)
      override def post(url: String, body: String, contentType: String): String = answer(url)
    }
    // Two rounds: the 503 backs the host off, which would defer anything asked after it.
    fill(store(), service, gaps = _ => AnswersChanged(Set(absent, refused), Set.empty), memory = Some(memory)).round()
    fill(store(), service, gaps = _ => AnswersChanged(Set(failing), Set.empty), memory = Some(memory)).round()
    val asked = AnswersChanged(Set(absent, refused, failing), Set.empty)

    clock.advance(java.time.Duration.ofMinutes(1))
    memory.due(asked) shouldBe AnswersChanged.Empty                   // each remembered: not asked every round
    clock.advance(java.time.Duration.ofMinutes(5))
    memory.due(asked) shouldBe AnswersChanged(Set(refused, failing), Set.empty)   // the failures, minutes later
  }

  // The model's failures are marked against the host their read went to, and each host's round is its own: IMDb down
  // while TMDB answers is IMDb's outage, and IMDb answering again releases the IMDb-backed questions that failed in it.
  it should "mark a failed read against its host, and release that host's failures when its own reads recover" in {
    val clock   = new tools.MutableClock(TestWiring.FixedInstant)
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val imdb    = "v3.sg.media-imdb.com"
    @volatile var imdbDown = true
    val service = new HttpFetch {
      private def answer(url: String): String =
        if (url.contains(imdb) && imdbDown) throw new java.net.http.HttpTimeoutException("request timed out")
        else if (url.contains(imdb)) """{"d":[],"q":"x","v":1}"""
        else """{"results":[],"crew":[],"cast":[]}"""
      override def get(url: String): String                                    = answer(url)
      override def get(url: String, headers: Map[String, String]): String      = answer(url)
      override def post(url: String, body: String, contentType: String): String = answer(url)
    }
    val down = (1 to 3).map(i => CandidateQuery.Imdb(s"Down $i"))
    fill(store(), service, gaps = _ => AnswersChanged(down.toSet + CandidateQuery.Title("Answered"), Set.empty), memory = Some(memory)).round()
    memory.outcome(Question.Candidate(down.head)).collect { case f: QuestionMemory.Outcome.Failed => f.host } shouldBe Some(Some(imdb))
    // the first failed (the rest deferred behind IMDb's back-off: IMDb's outage) and is held, though TMDB answered
    memory.due(AnswersChanged(Set(down.head), Set.empty)) shouldBe AnswersChanged.Empty
    clock.advance(java.time.Duration.ofMinutes(1))
    imdbDown = false
    fill(store(), service, gaps = _ => AnswersChanged(Set(CandidateQuery.Imdb("Up")), Set.empty), memory = Some(memory)).round()
    memory.due(AnswersChanged(Set(down.head), Set.empty)) shouldBe AnswersChanged(Set(down.head), Set.empty)  // IMDb is back
  }

  "a background round" should "end quietly when the worker's shutdown interrupts it, leaving the interrupt set, not escape as uncaught" in {
    var interrupt = true
    val rounds    = mutable.Buffer.empty[LookupFillRound]
    val shadow    = fill(store(), new Service, rounds = rounds,
      gaps = s => if (interrupt) throw new InterruptedException("sleep interrupted") else modelGaps(s))
    noException should be thrownBy shadow.start()
    Thread.interrupted() shouldBe true // re-asserted for the executor, and cleared for this test's thread
    rounds shouldBe empty
    interrupt = false
    shadow.start() // the next start runs: the interrupted round let go of `running`
    rounds should have size 1
  }

  it should "log a failed round WITH its stack: the catch-all's cause is unknown, and its message alone rarely names it" in {
    val warned = (shadow: LookupFill) => tools.LogCapture.thisThread(classOf[LookupFill].getName)(shadow.start())
      .filter(_.getLevel == ch.qos.logback.classic.Level.WARN).map(e => Option(e.getThrowableProxy).map(_.getMessage))
    warned(fill(store(), new Service, gaps = _ => throw new IllegalStateException("store down"))) shouldBe Seq(Some("store down"))
  }

  "the pipeline's own requests" should "wait on a shared paced host no longer than one interval per shadow ask, the shadow capped by its rate" in {
    // A virtual clock: sleeping advances it. The pipeline asks the paced host every interval — at
    // the host's full capacity — and the shadow at its capped rate, both through ONE shared pacer.
    val interval = 1000L
    val clock    = new tools.MutableClock(Instant.EPOCH)
    def at       = clock.millis()
    val pacer    = PacedHttpFetch.perProcess(new HttpFetch {
      override def get(url: String): String                                    = "{}"
      override def post(url: String, body: String, contentType: String): String = "{}"
    }, _ => Some(interval.millis), clock = clock, sleep = ms => clock.advanceMillis(ms))
    val rate     = IdentityLookupFillRate(6)   // one every 10 s
    val budget   = new LookupFillBudget(rate.allowanceOver(10.minutes), rate.pace, _ => ())
    val live     = new LookupFillLiveFetch(pacer, budget)
    val horizon  = 10.minutes.toMillis
    val events   = ((0L until horizon by interval).map(_ -> "pipeline") ++ (0L until horizon by rate.pace.toMillis).map(_ -> "shadow"))
      .sortBy { case (t, who) => (t, who) }
    var pipelineWait = 0L   // the longest any one pipeline request waited
    var shadowAsks   = 0
    events.foreach { case (t, who) =>
      if (t > at) clock.setTo(Instant.ofEpochMilli(t))
      if (who == "shadow") { if (scala.util.Try(live.get("https://paced.example/x")).isSuccess) shadowAsks += 1 }
      else { val before = at; pacer.get("https://paced.example/y"); pipelineWait = pipelineWait.max(at - before) }
    }
    shadowAsks shouldBe rate.allowanceOver(10.minutes)
    pipelineWait should be <= shadowAsks * interval
  }

  // What ends a host's asks for the round is the host overloading — never a permanent answer. An open breaker and a
  // network failure (a timeout is an IOException) are overload; a replayed fixture's miss and a parse error are not.
  "LookupFillLiveFetch.isOverload" should "read a breaker, a network failure and a 429/5xx as overload, and nothing else" in {
    def status(code: Int) = new HttpStatusException(code, "GET", "https://api.themoviedb.org/3/x", None)
    LookupFillLiveFetch.isOverload(status(429)) shouldBe true
    LookupFillLiveFetch.isOverload(status(503)) shouldBe true
    LookupFillLiveFetch.isOverload(status(403)) shouldBe false
    LookupFillLiveFetch.isOverload(new tools.CircuitOpenException("api.themoviedb.org", 30000L)) shouldBe true
    LookupFillLiveFetch.isOverload(new java.net.http.HttpTimeoutException("request timed out")) shouldBe true
    LookupFillLiveFetch.isOverload(new java.net.ConnectException("refused")) shouldBe true
    LookupFillLiveFetch.isOverload(new java.io.FileNotFoundException("no fixture")) shouldBe false
    LookupFillLiveFetch.isOverload(new IllegalArgumentException("bad json")) shouldBe false
  }

  "the fill's schedule" should "complete its tick only for a round that ran to its end, never at the hand-off" in {
    // The schedule only hands a round to the fill's executor: completed there, a round throwing or hanging every time
    // kept identity-lookup-refresh fresh, and ScheduledTickStale could never fire for it.
    val ticks = new tools.RecordingTickRecorder
    def scheduled(fill: LookupFill)(during: => Unit): Unit = {
      val clock     = new tools.MutableClock(TestWiring.FixedInstant)
      val scheduler = new tools.ManualScheduler(clock)
      val schedule  = fill.schedule(30.minutes, 1.milli, new services.schedule.InMemoryScheduledRunStore, _ => scheduler)
      schedule.start()
      try { scheduler.advance(java.time.Duration.ofMillis(1)); during } finally schedule.stop()
    }

    scheduled(fill(store(), new Service, gaps = _ => throw new RuntimeException("model unreadable"), ticks = ticks))(())
    ticks.completedRuns shouldBe empty                                    // a round that threw

    val entered = new java.util.concurrent.CountDownLatch(1)
    val release = new java.util.concurrent.CountDownLatch(1)
    val pool    = java.util.concurrent.Executors.newSingleThreadExecutor()
    val hanging = fill(store(), new Service, ticks = ticks, executor = pool,
      gaps = s => { entered.countDown(); release.await(); modelGaps(s) })
    try scheduled(hanging) {
      entered.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
      ticks.completedRuns shouldBe empty                                  // a round still running
      release.countDown()
      pool.shutdown()
      pool.awaitTermination(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
    } finally { release.countDown(); pool.shutdownNow(); () }
    ticks.completedRuns shouldBe Seq(LookupFill.Tick -> 30.minutes)        // and once it ran to its end
  }
}
