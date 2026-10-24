package services.tasks

import clients.TmdbClient
import io.prometheus.metrics.model.registry.PrometheusRegistry
import io.prometheus.metrics.model.snapshots.CounterSnapshot.CounterDataPointSnapshot
import models.{KinoApollo, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.enrichment.{ImdbClient, ImdbIdResolver}
import services.events.InProcessEventBus
import services.freshness.InMemoryFreshnessStore
import services.identity.{AnswersChanged, CandidateQuery, Question, UnansweredTmdbLookups}
import services.identity.adapters.{AgreementAskers, AgreementQuestions, FamilyAnswerStore, InMemoryTmdbDocuments, LookupFill, QuestionMemory, StoredFirstLookups, TmdbChangesSweep, TmdbIdentityLookups, TmdbNormalizer, TmdbStore}
import services.metrics.WorkerHttpMetrics
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository, MovieService}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.venuepages.{InMemoryVenuePageStore, VenuePageReader}
import settings.{IdentityLookupFillInterval, IdentityLookupFillRate}
import tools.{CountingHttpFetch, DaemonExecutors, HttpFetch, HttpPath, TestWiring}
import tools.SpecClock.latestTitleYear

import java.time.{Clock, ZoneOffset}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/** WHO ASKED, on `kinowo_worker_http_total`'s `path` label: each part of the worker that sends requests, driven as it
 *  runs, over a wire counted by the worker's own counter innermost (`CountingHttpFetch` into `WorkerHttpMetrics`) —
 *  and every one of them names itself, none falling into `other`. */
class WorkerHttpPathSpec extends AnyFlatSpec with Matchers {

  private val clock = Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC)

  /** The counted wire: every request answered with an empty TMDB / IMDb body, and counted as the worker counts it. */
  private final class Wire {
    private val metrics = new WorkerHttpMetrics(Seq("pl"), new PrometheusRegistry())
    private val leaf = new HttpFetch {
      def get(url: String): String = """{"results":[],"crew":[],"cast":[],"total_pages":1,"d":[],"q":"x","v":1}"""
      override def get(url: String, headers: Map[String, String]): String = get(url)
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val fetch: HttpFetch = new CountingHttpFetch(leaf, metrics.recorderFor("pl", WorkerHttpMetrics.Phase.Enrich))
    val tmdb: TmdbClient = tmdbOver(fetch)
    def tmdbOver(http: HttpFetch): TmdbClient =
      new TmdbClient(http, apiKey = Some(settings.TmdbApiKey("k")), retrySleep = (_: Long) => ())

    /** The paths the requests made so far were counted under. */
    def paths: Set[String] = metrics.snapshot().getDataPoints.toArray.toSeq.collect {
      case p: CounterDataPointSnapshot if p.getValue > 0 => p.getLabels.get("path")
    }.toSet
  }

  private def lookupFill(wire: Wire, gaps: AnswersChanged = AnswersChanged.Empty, refreshes: Seq[CandidateQuery] = Nil) = {
    val store = new TmdbStore(new InMemoryTmdbDocuments, clock)
    new LookupFill(() => gaps, wire.tmdbOver, wire.fetch, new TmdbNormalizer(store), IdentityLookupFillRate(600),
      IdentityLookupFillInterval(30.minutes), _ => (), DaemonExecutors.directExecutor(), _ => (),
      () => refreshes.map(services.identity.Question.Candidate(_)), new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock), clock)
  }

  private def sweep(wire: Wire) = {
    val docs = new InMemoryTmdbDocuments
    new TmdbChangesSweep(new TmdbStore(docs, clock), docs, wire.fetch, wire.tmdbOver, "pl-PL", TmdbChangesSweep.Schedule(0.hours), clock)
  }

  /** `handler`'s task claimed and run by the task worker, as the worker's pool runs it. */
  private def runTask(kind: TaskType, payload: Map[String, String] = Map.empty)(handler: Task => HandlerOutcome): Unit = {
    val queue = new InMemoryTaskQueue
    queue.enqueue(kind, s"${kind.name}|x", payload, submittedAt = clock.instant().minusSeconds(3600))
    val handlers = Seq(new TaskHandler {
      val taskType: TaskType                 = kind
      def handle(task: Task): HandlerOutcome = handler(task)
    })
    new TaskWorker(queue, handlers, processingTimeout = TaskWorker.ProcessingTimeout(5.minutes),
      poolSize = settings.WorkerPoolSize(1), clock = clock).claimAndRun("w0") shouldBe TaskWorker.PollResult.Completed
  }

  "The identity model's lookup fill" should "ask its gaps on the `fill` path" in {
    val wire = new Wire
    lookupFill(wire, gaps = AnswersChanged(Set(CandidateQuery.Title("Gap film")), Set(42))).round()
    wire.paths shouldBe Set(HttpPath.Fill)
  }

  it should "ask its aged questions again on the `refresh` path" in {
    val wire = new Wire
    lookupFill(wire, refreshes = Seq(CandidateQuery.Title("Aged film"))).round()
    wire.paths shouldBe Set(HttpPath.Refresh)
  }

  "TMDB's changes sweep" should "fetch on the `sweep` path, on its own minute tick" in {
    val wire = new Wire
    sweep(wire).tick().asked should be > 0
    wire.paths shouldBe Set(HttpPath.Sweep)
  }

  "The identity model's stored-first lookups" should "ask what the store lacks on the `live` path" in {
    val wire = new Wire
    val live = new TmdbIdentityLookups(wire.tmdb, new ImdbClient(wire.fetch), Nil)
    new StoredFirstLookups(UnansweredTmdbLookups, live).candidates(CandidateQuery.Title("Take-up film"))
    wire.paths shouldBe Set(HttpPath.Live)
  }

  "An agreement question" should "be asked on the `agreement` path" in {
    val wire = new Wire
    val asker = AgreementAskers.over(new FamilyAnswerStore(new InMemoryTmdbDocuments, clock), clock)(find = id => { wire.tmdb.findByImdbId(id); () })
    runTask(TaskType.AskQuestion, AgreementQuestions.payloadOf(Seq(Question.Find("tt0000001"))))(asker.handle)
    wire.paths shouldBe Set(HttpPath.Agreement)
  }

  "A venue page read" should "be on the `venue_page` path, a film row's detail enrichment reading it too" in {
    val wire = new Wire
    val enricher = new DetailEnricher {
      val cinema                                       = KinoApollo
      val detailGroup                                  = "kino-apollo"
      def fetchFilmDetail(ref: String): Option[FilmDetail] = { wire.fetch.get(ref); None }
    }
    val reader = new VenuePageReader(new InMemoryVenuePageStore, new InMemoryFreshnessStore, _ => (), clock)
    reader.read(enricher, "https://kinoapollo.pl/film/a")
    runTask(TaskType.ReadVenuePage)(_ => { reader.read(enricher, "https://kinoapollo.pl/film/b"); HandlerOutcome.Done })
    runTask(TaskType.EnrichDetails)(_ => { reader.read(enricher, "https://kinoapollo.pl/film/c"); HandlerOutcome.Done })
    wire.paths shouldBe Set(HttpPath.VenuePage)
  }

  "The scraping pipeline" should "scrape on the `scrape` path, its detail fetches handed to a pool included" in {
    val wire = new Wire
    val pool = DaemonExecutors.boundedEC("detail-fetch", 2)
    try runTask(TaskType.ScrapeCinema) { _ =>
      wire.fetch.get("https://kinoapollo.pl/repertuar")
      val details = Future.traverse(Seq("a", "b"))(page => Future(wire.fetch.get(s"https://kinoapollo.pl/film/$page"))(using pool))(
        using implicitly, pool)
      Await.result(details, tools.SpecTimeouts.Io)
      HandlerOutcome.Done
    }
    finally pool.shutdown()
    wire.paths shouldBe Set(HttpPath.Scrape)
  }

  "The pipeline's enrichment" should "rate, resolve IMDb ids and fill a projected film's details on the `enrich` path" in {
    val wire = new Wire
    runTask(TaskType.ImdbRating)(_ => { wire.fetch.get("https://www.imdb.com/title/tt0000001/"); HandlerOutcome.Done })
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(
      Seq(("Film bez IMDb", Some(2026), MovieRecord(tmdbId = Some(7), data = Map[Source, SourceData](Tmdb -> SourceData())))),
      normalizer = titleNormalizer), normalizer = titleNormalizer, clock = tools.SpecClock.Pinned)
    new ImdbIdResolver(cache, new ImdbClient(wire.fetch), stopwatch = tools.Stopwatch.System)
      .resolveSync("Film bez IMDb", Some(2026), "Film bez IMDb")
    scala.util.Try(new MovieService(cache, new InProcessEventBus(), wire.tmdb, clock = tools.SpecClock.Pinned)
      .withFilmDetails(MovieRecord(), 7))
    wire.paths shouldBe Set(HttpPath.Enrich)
  }

  "A request no caller named" should "count as `other`" in {
    val wire = new Wire
    wire.fetch.get("https://api.themoviedb.org/3/movie/1")
    wire.paths shouldBe Set(HttpPath.Other)
  }

  "Every task that sends requests" should "name a path of its own, never `other`" in {
    val silent = Set[TaskType](TaskType.SettleNow, TaskType.RenderShareCard, TaskType.PruneShareCards,
      TaskType.ReleaseShareCardHold, TaskType.RenderCityCard, TaskType.AuditReadModelContent, TaskType.AuditShareCards)
    for (taskType <- TaskType.all.filterNot(silent)) withClue(taskType.name)(TaskWorker.httpPathOf(taskType) should not be HttpPath.Other)
    TaskType.all.map(TaskWorker.httpPathOf).toSet.subsetOf(HttpPath.all.toSet) shouldBe true
  }
}
