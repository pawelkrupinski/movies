package integration

import models.Country
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer
import services.scrapes.ArchivedScrape
import tools._

import java.nio.file.Files
import scala.collection.mutable
import scala.util.Try

/**
 * The identity program's PHASE-1 GATE (docs/design/identity-resolver.md, "Phase 1"): the share of
 * the resolver's query set (`IdentityLookupSweep`) the recorded answers serve, per corpus —
 * `IdentityQueryCoverage` holds the arithmetic.
 *
 * Always: the five checked-in hard-cluster corpora against their checked-in responses. With
 * `KINOWO_IDENTITY_FULL=pl,uk,de,es,us`, `KINOWO_IDENTITY_CORPUS_DIR` (holding
 * `cinema-scrapes-<cc>.json.gz`) and `KINOWO_FIXTURE_ROOT` (holding each `enrichment-<cc>` tree
 * as a real directory): the five full recorded corpora, through a hermetic leg's own replay
 * chain (`ArchiveReplayWiring.recordedChain`) — the tree, then the remembered verdicts beside
 * it, then a refusal that names the request.
 *
 * Each corpus prints one line; with `KINOWO_IDENTITY_GATE=strict` any gap fails the spec. Off by
 * default, because the gate is met by a RECORDING (`gh workflow run "Record scrape fixtures"
 * -f identity-lookups=true`), not by a code change — the hermetic legs that replay that recording
 * with the sweep on are where CI enforces it.
 */
class IdentityQueryCoverageIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with IntegrationMongoSuite {

  private val fixtureRoot = configuration.fixtureRoot
  private val strict      = configuration.identityGateStrict.value
  private val storages = mutable.ListBuffer.empty[ConvergenceStorage]

  override def afterAll(): Unit = {
    storages.synchronized(storages.foreach(s => Try(s.close())))
    super.afterAll()
  }

  private final case class Corpus(label: String, country: Country, rows: Seq[ArchivedScrape],
                                  recording: () => (HttpFetch, () => IdentityQueryCoverage.Request => Boolean))

  private val hardClusters: Seq[Corpus] =
    Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c))).map { c =>
      Corpus(s"hc-${c.code}", c, CorpusFixture.read(HardClusters.corpusKey(c)), () => {
        val recorded = RecordedResponses.replaying(RecordedResponses.pathFor(c.code))
        (recorded, () => { val missed = recorded.missedKeys.toSet; r => missed(r.query) })
      })
    }

  private val full: Seq[Corpus] = {
    val wanted = configuration.identityFullCorpora.value
    val dir    = configuration.identityCorpusDirectory.map(_.value)
    Country.all.filter(wanted).flatMap { c =>
      dir.map(_.resolve(s"cinema-scrapes-${c.code}.json.gz")).filter(Files.exists(_)).map { path =>
        Corpus(s"full-${c.code}", c, CorpusFixture.readFrom(path), () => {
          val missing = new MissingFixtures
          val tree    = s"enrichment-${c.code}"
          // The remembered verdicts are READ, never written: this measures the recording.
          val verdicts = new EnrichmentCacheStore {
            private val files = new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(fixtureRoot, tree), FileEnrichmentCacheStore.NeverExpires)
            override def loadAll(): Map[String, CachedResponse] = files.loadAll()
            override def put(key: String, response: CachedResponse): Unit = ()
          }
          val cache = new EnrichmentCache(verdicts, clock = () => TestWiring.FixedInstant.toEpochMilli, persistSuccesses = false,
            transients = EnrichmentCache.Transients.Replayed)
          cache.preload()
          // The replay chain itself, over a wire that refuses and names each request.
          val fetch = ArchiveReplayWiring.recordedChain(tree, fixtureRoot, Some(cache), new HermeticHttpLeaf(missing), "tree", "verdicts")
          (fetch, () => { val missed = missing.keys.map(_._1).toSet; r => missed(r.fixtureKey) })
        })
      }
    }
  }

  private def measure(corpus: Corpus, suffix: String = ""): IdentityQueryCoverage.Coverage = {
    val storage = ConvergenceStorage.mongo(mongoTarget, s"idq-${corpus.label}$suffix", TitleNormalizer.forCountry(corpus.country))
    storages.synchronized(storages += storage)
    val (fetch, missed)     = corpus.recording()
    val (coverage, summary) = IdentityQueryCoverage.measure(corpus.label, corpus.country, storage, corpus.rows, fetch, missed)
    info(s"[gate] ${coverage.line}  ($summary)")
    println(s"[identity-gate] ${coverage.line}")
    coverage
  }

  "the resolver's query set" should "be answerable from the recorded answers of every corpus (phase-1 gate)" in {
    val results = (hardClusters ++ full).map(measure(_))
    // Never vacuous: every corpus asked something.
    results.filter(_.requests == 0).map(_.label) shouldBe empty
    if (strict) withClue(results.filterNot(_.met).map(_.line).mkString("\n", "\n", "\n")) {
      results.filterNot(_.met).map(_.label) shouldBe empty
    }
  }

  /**
   * That the `identity-lookups` recording fills the gaps and only the gaps: the recording leg's
   * own chain (`ArchiveReplayWiring` in recording mode — the tree, the recorder, the verdict
   * cache, the throttled phase chains) over a full corpus, with the wire replaced by a leaf that
   * notes what reached it and answers 404. Every gap must reach the wire, nothing the recording
   * already served may, and the gate must then be met.
   *
   * It WRITES into the tree (the remembered 404s), so it runs only when
   * `KINOWO_IDENTITY_RECORD_CHECK=<code>` names a country AND `KINOWO_FIXTURE_ROOT` is a scratch
   * copy (`cp -Rc` clones a tree for free on APFS).
   */
  "a recording pass" should "ask the service every request the recording lacks and none it holds, after which the gate is met" in {
    val check = configuration.identityRecordCheck
    assume(check.isDefined, "KINOWO_IDENTITY_RECORD_CHECK not set")
    val corpus = full.find(_.country == check.get.value).getOrElse(fail(s"no full corpus for ${check.get.value.code} (KINOWO_IDENTITY_FULL)"))
    val before = measure(corpus, "-before")

    val asked = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    val wire  = new HttpFetch {
      private def refuse(url: String, body: Option[String]): Nothing = {
        asked.add(clients.tools.RecordingHttpFetch.fixtureKey(url, body, foldYear = false))
        throw new HttpStatusException(404, if (body.isDefined) "POST" else "GET", url, None)
      }
      override def get(url: String): String = refuse(url, None)
      override def get(url: String, headers: Map[String, String]): String = refuse(url, None)
      override def getBytes(url: String): Array[Byte] = refuse(url, None)
      override def post(url: String, body: String, contentType: String): String = refuse(url, Some(body))
    }
    val country = corpus.country
    val storage = ConvergenceStorage.mongo(mongoTarget, s"idq-${corpus.label}-record", TitleNormalizer.forCountry(country))
    storages.synchronized(storages += storage)
    CorpusFixture.seedInto(storage.archive, corpus.rows)
    val cache = new EnrichmentCache(new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(fixtureRoot, s"enrichment-${country.code}")),
      clock = () => TestWiring.FixedInstant.toEpochMilli, persistSuccesses = false,
      transients = EnrichmentCache.Transients.Recorded)
    cache.preload()
    val language = country.language
    val recording = new ArchiveReplayWiring(country, storage.archive, Some(cache), storage, s"enrichment-${country.code}", fixtureRoot) {
      override protected def realHttpLeaf: HttpFetch = wire
      override lazy val clock: java.time.Clock = java.time.Clock.fixed(TestWiring.FixedInstant, java.time.ZoneOffset.UTC)
      override lazy val backgroundBudget: ExecutionBudget = new SameThreadExecutionBudget
      override lazy val uptimeMonitor = new services.UptimeMonitor(None, clock = clock)
      override def tmdbClientOver(http: HttpFetch): clients.TmdbClient = new clients.TmdbClient(http, apiKey = Some(_root_.settings.TmdbApiKey("replay")), language = language)
    }
    info(s"recording pass: ${IdentityLookupSweep.over(recording)}")

    import scala.jdk.CollectionConverters._
    val wired  = asked.asScala.toSet
    val gaps   = before.gaps.map(_.fixtureKey).toSet
    // A hermetic replay stops a lookup at its first gap, so an ANSWERED gap can lead the same
    // lookup on to requests no replay reached yet (a TMDB 404 moves the ladder to its next rung):
    // the recording fills the gaps' closure. It must hold every gap, and nothing already recorded.
    val beyond = wired -- gaps
    info(s"${gaps.size} gap(s); ${wired.size} request(s) reached the wire, ${beyond.size} of them follow-ons: " +
      beyond.toSeq.sorted.take(10).mkString(", "))
    withClue("a gap the recording pass never asked: ")(gaps -- wired shouldBe empty)
    withClue("an already-recorded request re-fetched: ")(wired.intersect(before.served.map(_.fixtureKey).toSet) shouldBe empty)
    measure(corpus, "-after").gaps shouldBe empty
  }
}
