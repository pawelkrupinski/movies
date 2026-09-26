package integration

import clients.TmdbClient
import com.mongodb.{ConnectionString, MongoClientSettings}
import models.Source
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.TtlIndexMismatches
import services.cinemas.common.{DetailEnricher, FilmDetail}
import services.identity._
import services.observations.{ObservationStore, ObservingDetailEnricher, ObservingHttpFetch}
import tools._

import java.lang.management.ManagementFactory
import java.time.{Clock, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The PRODUCTION shadow run (`ShadowIdentityReaper`) against the offline harness, on the recorded
 * corpora (docs/design/identity-resolver.md §8). Per corpus:
 *
 *  1. the offline resolve `IdentityShadowIntegrationSpec` makes — `TmdbIdentityLookups` over the
 *     corpus's recorded answers — with every answer it received filed by the PRODUCTION capture
 *     (`ObservingHttpFetch`, `ObservingDetailEnricher`) into an observation store. A request the
 *     recording could not answer is filed as the failed read it is, so it stays a gap;
 *  2. one reaper tick over that store alone, the pipeline booted beside it for the diff, its run
 *     persisted in Mongo (`identity_shadow_decisions` / `identity_shadow_diff`).
 *
 * It requires the persisted decisions to EQUAL the offline resolver's, the corpus's fetch to see
 * ZERO requests during the tick, and no family crossing; and it reports what the tick cost (CPU,
 * allocation, wall seconds) and what its run weighs in Mongo — the numbers the rollout reads.
 *
 * The hard clusters always run (itAll); the full corpora with `KINOWO_IDENTITY_FULL`, as for
 * `IdentityShadowIntegrationSpec`.
 */
class ShadowIdentityReaperIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  import IdentityShadow._

  private val fixtureRoot = configuration.fixtureRoot
  private val storages    = mutable.ListBuffer.empty[ConvergenceStorage]
  private val corpora: Seq[Corpus] =
    hardClusters(configuration.hardClusterCountries.map(_.value.map(_.code))) ++
      configuration.identityCorpusDirectory.toSeq.flatMap(d => IdentityShadow.full(configuration.identityFullCorpora.value, d.value, fixtureRoot))

  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "shadow-reaper"))
  private val clock = Clock.fixed(TestWiring.FixedInstant, ZoneOffset.UTC)

  override def afterAll(): Unit =
    try {
      storages.synchronized(storages.foreach(s => Try(s.close())))
      Await.ready(database.drop().toFuture(), 60.seconds)
    } finally { client.close(); super.afterAll() }

  /** The recorded chain, with a request it could not answer thrown as the failed read it is — the
   *  replays answer a gap with a 404 or an empty body, which the capture would file as an answer. */
  private def gapsFail[A](misses: () => Long)(call: => A): A = {
    val before  = misses()
    val outcome = Try(call)
    if (misses() != before) throw new IllegalStateException("not recorded") else outcome.get
  }

  private final class GapsFail(inner: HttpFetch, misses: () => Long) extends HttpFetch {
    override def get(url: String): String                               = gapsFail(misses)(inner.get(url))
    override def get(url: String, headers: Map[String, String]): String = gapsFail(misses)(inner.get(url, headers))
    override def getBytes(url: String): Array[Byte]                      = gapsFail(misses)(inner.getBytes(url))
    override def post(url: String, body: String, contentType: String): String = gapsFail(misses)(inner.post(url, body, contentType))
  }

  private final class DetailGapsFail(inner: DetailEnricher, misses: () => Long) extends DetailEnricher {
    override def cinema: models.Cinema                      = inner.cinema
    override def detailGroup: String                        = inner.detailGroup
    override def detailTarget: Source                       = inner.detailTarget
    override def enrichmentServiceOverride: Option[String] = inner.enrichmentServiceOverride
    override def defersTmdbResolution: Boolean              = inner.defersTmdbResolution
    override def fetchFilmDetail(ref: String): Option[FilmDetail] = gapsFail(misses)(inner.fetchFilmDetail(ref))
  }

  private final class Recorded extends ShadowIdentityMetrics {
    @volatile var films: Map[ShadowRelation, Int] = Map.empty
    @volatile var crossingCount: Option[Int]      = None
    @volatile var seconds: Double                 = Double.NaN
    def resolved(counts: Map[ShadowRelation, Int], resolveSeconds: Double): Unit = { films = counts; seconds = resolveSeconds }
    def crossings(count: Int): Unit                                              = crossingCount = Some(count)
  }

  private def sizeOf(collection: String): (Long, Long) = {
    val stats = Await.result(database.runCommand(Document("collStats" -> collection)).toFuture(), 30.seconds)
    (stats.get("size").map(_.asNumber.longValue).getOrElse(0L), stats.get("storageSize").map(_.asNumber.longValue).getOrElse(0L))
  }

  corpora.foreach { c =>
    "the production shadow run" should s"decide exactly as the offline resolver, from observations alone and with no request, on ${c.label}" in {
      val w = wiring(mongoTarget, c, storages, fixtureRoot, configuration.env)
      bootPipeline(w)
      val listings = listingsOf(w, c.normalizer)

      // 1. the offline resolve, its answers filed by the production capture
      val observations = ObservationStore.inMemory(clock)
      val offlineSource = new TmdbIdentityLookups(
        new TmdbClient(new ObservingHttpFetch(new GapsFail(c.fetch, c.misses), observations),
          apiKey = Some(settings.TmdbApiKey(StubTmdbKey)), language = c.country.language, retrySleep = (_: Long) => ()),
        w.detailEnrichers.map(e => new ObservingDetailEnricher(new DetailGapsFail(e, c.misses), observations)), c.misses)
      val offline = IdentityResolver.resolve(listings, offlineSource, c.normalizer, IdentityCalibration.default)

      // 2. one production tick over the store alone
      Seq(ShadowRunStore.DecisionsCollection, ShadowRunStore.DiffCollection)
        .foreach(n => Await.result(database.getCollection(n).drop().toFuture(), 30.seconds))
      val runs    = new ShadowRunStore(MongoShadowRunBackend.writer(database, new TtlIndexMismatches), clock)
      val metrics = new Recorded
      val reaper  = new ShadowIdentityReaper(
        // Production's own listing read: the worker's scrape archive, streamed a page at a time.
        listings      = () => w.shadowListings(),
        pipelineFilms = () => w.movieCache.snapshot(),
        // Another key than the offline client's: the observations are keyed with credentials masked.
        lookups       = () => ObservedIdentityLookups.over(observations,
          new TmdbClient(_, apiKey = Some(settings.TmdbApiKey("shadow-key")), language = c.country.language), w.detailEnrichers),
        pins          = new InMemoryPinStore,
        normalizer    = c.normalizer,
        calibration   = IdentityCalibration.default,
        runs          = runs,
        retention     = ShadowRetention(services.observations.ObservationRetention.Window),
        metrics       = metrics,
        clock         = clock)

      val threads      = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
      val thread       = Thread.currentThread().threadId()
      val requests     = c.fetch.requests.get()
      val (cpu0, mem0) = (threads.getThreadCpuTime(thread), threads.getThreadAllocatedBytes(thread))
      val (tick, wall) = timed(reaper.tick())
      val (cpu, alloc) = ((threads.getThreadCpuTime(thread) - cpu0) / 1e9, threads.getThreadAllocatedBytes(thread) - mem0)

      val run = runs.latestRun().getOrElse(fail("no run persisted"))
      val (decisionsSize, decisionsStorage) = sizeOf(ShadowRunStore.DecisionsCollection)
      val (diffSize, diffStorage)           = sizeOf(ShadowRunStore.DiffCollection)
      val lookupBytes = observations.currentLookups().map(o => o.query.key.length.toLong + (o.answer match {
        case services.observations.LookupAnswer.Body(t)  => t.length.toLong
        case b: services.observations.LookupAnswer.Bytes => b.base64.length.toLong
        case f: services.observations.LookupAnswer.Failed => f.message.length.toLong
      })).sum
      info(f"[${c.label}] ${listings.size} listings → ${run.clusters.size} clusters (${tick.films.toSeq.sortBy(_._1.ordinal)
        .map { case (r, n) => s"${r.label} $n" }.mkString(", ")}), ${run.families.size} families differ; tick $wall%.1fs wall, " +
        f"resolve ${metrics.seconds}%.1fs, CPU $cpu%.1fs, allocated ${alloc / 1e6}%.0f MB; ${tick.gaps} unobserved lookups; " +
        f"Mongo: decisions ${decisionsSize / 1e6}%.2f MB (${decisionsStorage / 1e6}%.2f MB on disk), diff ${diffSize / 1e6}%.2f MB " +
        f"(${diffStorage / 1e6}%.2f MB); observations it read: ${observations.currentLookups().size} (${lookupBytes / 1e6}%.1f MB uncompressed)")

      withClue(s"[${c.label}] ") {
        c.fetch.requests.get() shouldBe requests
        w.shadowListings() shouldBe listings
        metrics.crossingCount shouldBe Some(0)
        val persisted = run.clusters.map(_.decision)
        val firstDifference = persisted.zipAll(offline.decisions, null, null).find { case (a, b) => a != b }
        withClue(firstDifference.fold("")(d => s"first difference:\n  shadow:  ${Option(d._1).map(_.render)}\n  offline: ${Option(d._2).map(_.render)}\n")) {
          persisted.size shouldBe offline.decisions.size
          firstDifference shouldBe None
        }
        metrics.films.values.sum shouldBe run.clusters.count(_.relation.isDefined)
      }
    }
  }
}
