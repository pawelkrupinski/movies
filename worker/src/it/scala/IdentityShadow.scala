package integration

import models.{Country, SourceData, Tmdb}
import services.identity._
import services.movies.{ListingKey, TitleNormalizer}
import services.scrapes.ArchivedScrape
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * The shadow harness around [[IdentityResolver]]: the recorded corpora, today's pipeline booted
 * over them the way the convergence legs boot it, the resolver over the same recorded answers,
 * and everything the phase-2 report compares between the two (docs/design/identity-resolver.md
 * §phase 2). Test code; `IdentityShadowIntegrationSpec` drives it.
 */
object IdentityShadow {

  val StubTmdbKey = "identity-shadow"

  // ── corpora ───────────────────────────────────────────────────────────────────────────

  /** Every request the pipeline or the resolver made, counted — the cost column. */
  final class CountingFetch(inner: HttpFetch) extends HttpFetch {
    val requests = new java.util.concurrent.atomic.AtomicLong()
    override def get(url: String): String = { requests.incrementAndGet(); inner.get(url) }
    override def get(url: String, headers: Map[String, String]): String = { requests.incrementAndGet(); inner.get(url, headers) }
    override def getBytes(url: String): Array[Byte] = { requests.incrementAndGet(); inner.getBytes(url) }
    override def post(url: String, body: String, contentType: String): String = { requests.incrementAndGet(); inner.post(url, body, contentType) }
  }

  /** `misses` counts every request the recording could not answer, each time it is asked — so a
   *  lookup that meets a gap is `Unknown` however often the gap was met before. */
  final class Corpus(val label: String, val country: Country, val rows: Seq[ArchivedScrape], rawFetch: HttpFetch,
                     val misses: () => Long, val missedKeys: () => Seq[String]) {
    val normalizer: TitleNormalizer = TitleNormalizer.forCountry(country)
    val fetch = new CountingFetch(rawFetch)
    def isHardCluster: Boolean = label.startsWith("hc-")
  }

  def hardClusters(only: Option[Set[String]]): Seq[Corpus] =
    Country.all.filter(c => CorpusFixture.exists(HardClusters.corpusKey(c)) && only.forall(_.contains(c.code))).map { c =>
      val r = RecordedResponses.replaying(RecordedResponses.pathFor(c.code))
      new Corpus(s"hc-${c.code}", c, CorpusFixture.read(HardClusters.corpusKey(c)), r, () => r.misses.toLong, () => r.missedKeys)
    }

  /** The full recorded corpora: `cinema-scrapes-<cc>.json.gz` in `corpusDir`, each replayed from
   *  its `enrichment-<cc>` tree (under `KINOWO_FIXTURE_ROOT`) and the remembered verdicts beside
   *  it, read-only — exactly a hermetic leg's chain. A request neither answers is REFUSED and
   *  named, never fetched. */
  def full(countries: Set[Country], corpusDir: Path, root: settings.FixtureRoot): Seq[Corpus] =
    Country.all.filter(countries).flatMap { c =>
      Option(corpusDir.resolve(s"cinema-scrapes-${c.code}.json.gz")).filter(Files.exists(_)).map { path =>
        val missing = new MissingFixtures
        val tree    = s"enrichment-${c.code}"
        val store   = new EnrichmentCacheStore {
          private val files = new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(root, tree), FileEnrichmentCacheStore.NeverExpires)
          override def loadAll(): Map[String, CachedResponse] = files.loadAll()
          override def put(key: String, response: CachedResponse): Unit = ()
        }
        val cache = new EnrichmentCache(store, clock = () => TestWiring.FixedInstant.toEpochMilli)
        val leaf  = new GapLeaf(missing)
        cache.preload()
        val fetch = new FallbackHttpFetch(Seq(
          "tree"  -> new clients.tools.FakeHttpFetch(tree, strict = true, foldYear = false, root = root),
          "cache" -> new CachingEnrichmentFetch(cache, leaf)))
        new Corpus(s"full-${c.code}", c, CorpusFixture.readFrom(path), fetch, () => leaf.met, () => missing.keys.map(_._2))
      }
    }

  /** The end of a full corpus's chain: a request the recorded tree and verdicts cannot answer is
   *  NAMED in `missing` and answered EMPTY, as a remembered 404 would be. A hermetic leaf throws
   *  instead, and on the US corpus one such throw inside a director walk aborted the whole boot;
   *  the shadow comparison wants the pipeline's answer with the gap, not no answer. The resolver
   *  still sees every gap as `Unknown` through `met`, which counts EVERY gap met — `missing`
   *  names each once, so counting it would read a repeated gap as the empty answer. */
  final class GapLeaf(missing: MissingFixtures) extends HttpFetch {
    private val gaps = new java.util.concurrent.atomic.AtomicLong()
    def met: Long = gaps.get()
    private def gap(method: String, url: String): Unit = { gaps.incrementAndGet(); missing.record(s"$method $url", s"$method $url") }
    override def get(url: String): String = { gap("GET", url); "{}" }
    override def get(url: String, headers: Map[String, String]): String = { gap("GET", url); "{}" }
    override def getBytes(url: String): Array[Byte] = { gap("BYTES", url); Array.emptyByteArray }
    override def post(url: String, body: String, contentType: String): String = { gap("POST", url); "{}" }
  }

  // ── today's pipeline ──────────────────────────────────────────────────────────────────

  /** One film the pipeline made: its key, its TMDB id and record, and its cinema slots. */
  final case class PipelineFilm(key: String, tmdbId: Option[Int], film: Option[IdentityMeasures.Film],
                                slots: Seq[(String, String, SourceData)])

  /** The wiring both sides use (`FetchReplayWiring`): a throwaway database seeded with the corpus,
   *  every answer from the corpus's fetch, retries that never sleep on a replayed refusal. */
  def wiring(target: IntegrationMongoTarget, c: Corpus, storages: mutable.ListBuffer[ConvergenceStorage],
             root: settings.FixtureRoot, environment: Env): ArchiveReplayWiring = {
    val storage = ConvergenceStorage.mongo(target, s"idshadow-${c.label}", c.normalizer)
    storages.synchronized(storages += storage)
    FetchReplayWiring(c.country, storage, c.rows, c.fetch, root, retrySleep = (_: Long) => (), environment = environment)
  }

  /** Boot the pipeline the way the convergence legs do (`CountryConvergenceBehaviour.bootSettled`):
   *  the whole-corpus scrape tick (twice) with staging drained, then the periodic settle pair,
   *  staging, the enrichment conclusion and the projection. */
  def bootPipeline(w: ArchiveReplayWiring): Seq[PipelineFilm] = {
    w.bootCorpus()
    w.movieService.settle()
    w.movieCache.canonicalizeBySanitize()
    w.drainStaging()
    w.concludeEnrichment()
    w.readModelProjector.reconcile()
    w.movieRepository.findAll().map { f =>
      val tmdb = f.record.data.get(Tmdb)
      PipelineFilm(f.id.value, f.record.tmdbId,
        f.record.tmdbId.map(_ => IdentityMeasures.Film(tmdb.flatMap(_.title).getOrElse(f.title), tmdb.flatMap(_.originalTitle), Nil,
          tmdb.flatMap(_.releaseYear), tmdb.flatMap(_.runtimeMinutes), tmdb.map(_.director).filter(_.nonEmpty), None)),
        PipelineFilms.slotsOf(f))
    }.sortBy(_.key)
  }

  /** Each listing's pipeline film (`PipelineFilms`, the rule the production shadow diff uses). */
  def pipelineFilmOf(listings: Seq[Listing], films: Seq[PipelineFilm], normalizer: TitleNormalizer): Map[ListingKey, Int] =
    PipelineFilms.assign(listings, films.zipWithIndex.map { case (f, i) => i -> f.slots }, normalizer)

  // ── the resolver ──────────────────────────────────────────────────────────────────────

  /** The corpus's raw listings — never `ScrapeListing.prepare`'s folded rows, which hide the very
   *  listings a venue lists twice. */
  def listingsOf(w: ArchiveReplayWiring, normalizer: TitleNormalizer): Seq[Listing] =
    Listing.corpus(w.archivedListings, normalizer)

  /** [[IdentityLookups]] with every answer memoised, so the permutation runs pay each lookup
   *  once. The memo changes how often the source is reached, never what the resolver ISSUES. */
  final class Memo(inner: IdentityLookups) extends IdentityLookups {
    private val details = new java.util.concurrent.ConcurrentHashMap[(String, String), Answer[Option[DetailFacts]]]()
    private val queries = new java.util.concurrent.ConcurrentHashMap[CandidateQuery, Answer[Seq[Hit]]]()
    private val films   = new java.util.concurrent.ConcurrentHashMap[Int, Answer[Option[IdentityMeasures.Film]]]()
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = details.computeIfAbsent((l.venue, l.page.getOrElse("")), _ => inner.detail(l))
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = queries.computeIfAbsent(q, _ => inner.candidates(q))
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]]      = films.computeIfAbsent(id, _ => inner.film(id))
    def sizes: (Int, Int, Int) = (details.size, queries.size, films.size)
    def unknown: (Int, Int, Int) = (details.values.asScala.count(!_.isKnown), queries.values.asScala.count(!_.isKnown),
      films.values.asScala.count(!_.isKnown))
  }

  /** A simulated TMDB OUTAGE: a deterministic share of candidate queries and film records
   *  withheld as `Unknown`. */
  final class Outage(inner: IdentityLookups, share: Double) extends IdentityLookups {
    private def withheld(key: String) = (key.hashCode.toLong & 0x7fffffffL) % 1000 < (share * 1000).toLong
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = inner.detail(l)
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (withheld(q.sortKey)) Answer.Unknown else inner.candidates(q)
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (withheld(s"film $id")) Answer.Unknown else inner.film(id)
  }

  // ── evidence: labels and contradiction ────────────────────────────────────────────────

  /** Whether a listing's own measurements CONTRADICT a film: two or more of its corroborators
   *  deny it (`IdentityMeasures.ownAgreement`, the calibration's `contradicted` rule). The
   *  benchmark's label-free referee — never an input to the resolver. */
  def contradicts(e: Evidence, f: IdentityMeasures.Film): Boolean =
    IdentityMeasures.ownAgreement(IdentityMeasures.listingFilm(e.measured, f, None, 0, 0))._2.size >= 2

  /** How many of a listing's own corroborators back a film, and how many deny it. */
  def agreement(e: Evidence, f: IdentityMeasures.Film): (Int, Int) = {
    val (agree, deny) = IdentityMeasures.ownAgreement(IdentityMeasures.listingFilm(e.measured, f, None, 0, 0))
    (agree.size, deny.size)
  }

  /** A system's answer for a listing: the film's TMDB id and what TMDB says about it. */
  final case class FilmAnswer(tmdbId: Int, film: IdentityMeasures.Film)

  /** One held-out label of the calibration (`identity-labels.json.gz`, `split == test` only):
   *  `corroborated` names the listing's film; `contradicted` says production's filing of it,
   *  `tmdbId`, is likely WRONG. */
  final case class Label(tmdbId: Int, corroborated: Boolean)

  val LabelsPath: Path = java.nio.file.Paths.get("test", "resources", "fixtures", "identity", "identity-labels.json.gz")

  /** Every test-split label of `country`, by `ListingKey.toString`. */
  def labels(country: Country): Map[String, Label] =
    if (!Files.exists(LabelsPath)) Map.empty
    else {
      val in = new java.util.zip.GZIPInputStream(Files.newInputStream(LabelsPath))
      val js = try play.api.libs.json.Json.parse(in) finally in.close()
      (js \ "listings").as[Seq[play.api.libs.json.JsObject]].iterator
        .filter(l => (l \ "country").as[String] == country.code && (l \ "split").as[String] == "test")
        .flatMap(l => (l \ "tmdbId").asOpt[Int].map(id => (l \ "listingKey").as[String] ->
          Label(id, (l \ "status").as[String] == "corroborated")))
        .toMap
    }

  // ── partitions ────────────────────────────────────────────────────────────────────────

  /** Pairwise agreement of two partitions over the same items, from their contingency table
   *  (never by enumerating pairs: the US corpus holds 100k listings). */
  final case class Pairwise(sameA: Long, sameB: Long, sameBoth: Long) {
    def precision: Double = if (sameB == 0) 1.0 else sameBoth.toDouble / sameB
    def recall: Double    = if (sameA == 0) 1.0 else sameBoth.toDouble / sameA
    def f1: Double        = if (precision + recall == 0) 0.0 else 2 * precision * recall / (precision + recall)
    def wrongMerges: Long = sameB - sameBoth
    def wrongSplits: Long = sameA - sameBoth
  }

  private def pairs(n: Long): Long = n * (n - 1) / 2

  /** `truth` (partition A) against `system` (partition B), over the items both name. */
  def pairwise[K, A, B](truth: Map[K, A], system: Map[K, B]): Pairwise = {
    val items = truth.keySet intersect system.keySet
    val a  = items.toSeq.groupBy(truth).values.map(v => pairs(v.size.toLong)).sum
    val b  = items.toSeq.groupBy(system).values.map(v => pairs(v.size.toLong)).sum
    val ab = items.toSeq.groupBy(k => (truth(k), system(k))).values.map(v => pairs(v.size.toLong)).sum
    Pairwise(a, b, ab)
  }

  // ── reporting ─────────────────────────────────────────────────────────────────────────

  final class Report(path: Path) {
    Files.createDirectories(path.toAbsolutePath.getParent)
    Files.writeString(path, "")
    def line(s: String): Unit = synchronized {
      Files.writeString(path, s + "\n", java.nio.file.StandardOpenOption.APPEND)
      println(s.take(4000))
    }
  }

  def pct(a: Long, b: Long): String = if (b == 0) "—" else f"${100.0 * a / b}%.1f%%"

  def timed[A](body: => A): (A, Double) = { val t0 = System.nanoTime(); val a = body; (a, (System.nanoTime() - t0) / 1e9) }
}
