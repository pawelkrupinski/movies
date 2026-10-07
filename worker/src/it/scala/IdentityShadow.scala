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
                     val misses: () => Long, val missedKeys: () => Seq[String], threadMisses: Option[() => Long] = None) {
    /** How the lookups tell a gap from an answer: by the gaps met on the asking thread when the corpus counts them per
     *  thread (lookups then read side by side), else by the replay's one counter, lookups taking turns. */
    def gaps: TmdbIdentityLookups.Gaps = threadMisses.fold[TmdbIdentityLookups.Gaps](new TmdbIdentityLookups.CountedGaps(misses))(onThread =>
      new TmdbIdentityLookups.Gaps {
        def answered[A](read: => A): Answer[A] = {
          val before = onThread()
          scala.util.Try(read).toOption.filter(_ => onThread() == before).fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
        }
      })
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
  def full(countries: Set[Country], corpusDir: Path, root: settings.FixtureRoot,
           live: Option[settings.IdentityLiveGaps] = None): Seq[Corpus] =
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
        val fetch = replayChain(new clients.tools.FakeHttpFetch(tree, strict = true, foldYear = false, root = root), cache,
          live.fold[HttpFetch](leaf)(key => new LiveGapLeaf(key, leaf)))
        new Corpus(s"full-${c.code}", c, CorpusFixture.readFrom(path), fetch, () => leaf.met, () => missing.keys.map(_._2), Some(() => leaf.metOnThread))
      }
    }

  /** A full corpus's chain: the recorded `tree`, then the remembered verdicts in `cache` ([[RememberedVerdicts]],
   *  read-only), then `leaf` for a request neither answers. */
  def replayChain(tree: HttpFetch, cache: EnrichmentCache, leaf: HttpFetch): HttpFetch =
    new FallbackHttpFetch(Seq("tree" -> tree, "verdicts" -> new RememberedVerdicts(cache, leaf)))

  /** The verdicts a recording remembered beside its tree, READ-ONLY: a remembered answer is answered, a remembered failure
   *  thrown as it was, and anything else asked of `leaf` every time. Never a [[CachingEnrichmentFetch]], which remembers
   *  what `leaf` answered for the rest of the run: the gap leaf's empty stand-in then answered every later ask of the same
   *  request as though the source had — the chain-wide Cineworld detail, a gap for the first venue asking, read
   *  "read, empty" for the other 76, and never read again (UK RBO Macbeth, 2026-10-07). */
  final class RememberedVerdicts(cache: EnrichmentCache, leaf: HttpFetch) extends HttpFetch {
    private def held[A](key: String, url: String)(answer: PartialFunction[CachedResponse, A])(orElse: => A): A =
      cache.lookup(key) match {
        case Some(failed: CachedResponse.Failed) => throw CachingEnrichmentFetch.revive(failed, url)
        case Some(response) if answer.isDefinedAt(response) => answer(response)
        case _ => orElse
      }
    private val body: PartialFunction[CachedResponse, String] = { case CachedResponse.Body(text) => text }
    override def get(url: String): String = held(CachingEnrichmentFetch.keyOf("GET", url), url)(body)(leaf.get(url))
    override def get(url: String, headers: Map[String, String]): String =
      held(CachingEnrichmentFetch.keyOf("GET", url), url)(body)(leaf.get(url, headers))
    override def getBytes(url: String): Array[Byte] =
      held(CachingEnrichmentFetch.keyOf("BYTES", url), url) { case b: CachedResponse.Bytes => b.bytes }(leaf.getBytes(url))
    override def post(url: String, body: String, contentType: String): String =
      held(CachingEnrichmentFetch.keyOf("POST", url, Some(body)), url)(this.body)(leaf.post(url, body, contentType))
  }

  /** The end of a full corpus's chain: a request the recorded tree and verdicts cannot answer is
   *  NAMED in `missing` and answered EMPTY, as a remembered 404 would be. A hermetic leaf throws
   *  instead, and on the US corpus one such throw inside a director walk aborted the whole boot;
   *  the shadow comparison wants the pipeline's answer with the gap, not no answer. The resolver
   *  still sees every gap as `Unknown` through `met`, which counts EVERY gap met — `missing`
   *  names each once, so counting it would read a repeated gap as the empty answer. */
  final class GapLeaf(missing: MissingFixtures) extends HttpFetch {
    private val gaps     = new java.util.concurrent.atomic.AtomicLong()
    private val onThread = ThreadLocal.withInitial[java.lang.Long](() => 0L)
    def met: Long = gaps.get()
    /** The gaps met on the calling thread: what tells one lookup's gap from another's when they read side by side. */
    def metOnThread: Long = onThread.get()
    private def gap(method: String, url: String): Unit = {
      gaps.incrementAndGet(); onThread.set(onThread.get() + 1); missing.record(s"$method $url", s"$method $url")
    }
    override def get(url: String): String = { gap("GET", url); "{}" }
    override def get(url: String, headers: Map[String, String]): String = { gap("GET", url); "{}" }
    override def getBytes(url: String): Array[Byte] = { gap("BYTES", url); Array.emptyByteArray }
    override def post(url: String, body: String, contentType: String): String = { gap("POST", url); "{}" }
  }

  /** A gap TMDB or IMDb can answer, answered LIVE, the stub key swapped for a real one; a venue page the capture read
   *  live before ([[LiveGapLeaf.readPages]]), answered as read; anything else, and a live failure, is still the gap. For
   *  the local resolver-only loop and the capture only (`settings.IdentityLiveGaps`): at most
   *  `perHost` requests at a time per host ([[tools.HostPacing]]: halved on a 429 or 503, retried after a back-off), and
   *  every answer kept on disk ([[LiveGapLeaf.Store]]) so a re-run after a rule change asks nothing it asked before. A
   *  read that FAILED is kept nowhere — the gap again for the rest of this run, asked live again by the next. */
  final class LiveGapLeaf(key: settings.IdentityLiveGaps, gap: GapLeaf,
                          perHost: settings.IdentityLivePerHost = settings.ProcessConfiguration.resolve().identityLivePerHost,
                          real: HttpFetch = new RealHttpFetch()) extends HttpFetch {
    import LiveGapLeaf._
    private val pacing = new tools.HostPacing(perHost.value, Retries, BackOff.toMillis)
    private val failed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    private def answerable(url: String) = url.contains("themoviedb.org") || url.contains("imdb.com")
    private def keyed(url: String) = url.replace(s"api_key=$StubTmdbKey", s"api_key=${key.tmdbKey}")
    private def stored(id: String)(read: => String): String = {
      val file = fileOf(id)
      if (Files.exists(file)) Files.readString(file)
      else { val answer = read; tools.AtomicFiles.writeString(file, answer); answer }
    }
    private def tried(url: String, id: String, read: => String, orGap: => String): String =
      if (failed.contains(id) || (!answerable(url) && !Files.exists(fileOf(id)))) orGap
      else scala.util.Try(stored(id)(pacing(url)(read))).getOrElse { failed.add(id); orGap }
    override def get(url: String): String = tried(url, s"GET $url", real.get(keyed(url)), gap.get(url))
    override def get(url: String, headers: Map[String, String]): String =
      tried(url, s"GET $url", real.get(keyed(url), headers), gap.get(url, headers))
    override def getBytes(url: String): Array[Byte] = gap.getBytes(url)
    override def post(url: String, body: String, contentType: String): String =
      tried(url, s"POST $url $body", real.post(keyed(url), body, contentType), gap.post(url, body, contentType))
  }

  object LiveGapLeaf {
    val Retries = 6
    val BackOff: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.DurationInt(5).seconds
    val Store: Path = java.nio.file.Paths.get("target", "identity-live-gaps")
    private[integration] def fileOf(id: String): Path =
      Store.resolve(java.util.HexFormat.of().formatHex(java.security.MessageDigest.getInstance("SHA-256").digest(id.getBytes("UTF-8"))))

    /** The listings whose venue page a capture reads ahead: those whose detail went `unread`, of a `captured` cluster or
     *  of a cluster the model takes by POOLED evidence alone — where a venue's facts can still veto the take, and an unread
     *  page let it stand (UK "CBeebies Panto 2026: Treasure Island" ×110 taken as the 1950 film; its Flicks page says 2026).
     *  Not a member's own match: its pages cost ~1,600 reads per corpus (UK, 2026-10-07) against 6 for the pooled takes. */
    def readAhead(decisions: Seq[ResolverDecision], captured: Set[ListingKey],
                  listings: Seq[Listing], unread: Listing => Boolean): Seq[Listing] = {
      val pooled = decisions.iterator.filter(_.basis == ResolverDecision.Basis.PooledMatch).flatMap(_.members).toSet
      listings.filter(l => (captured(l.key) || pooled(l.key)) && unread(l))
    }

    /** The gaps met (`missed`, each a requested URL) that hold one of `listings`' venue detail beside its page — a chain
     *  reading its show's detail off its API: those ending in its page's slug (Alamo Drafthouse's `…/presentation/<slug>`),
     *  and those naming one of its catalogue ids as an `ids=` parameter (the Gatsby box-office platform's
     *  `…/movies?…&ids=<id>`, Cineworld's detail). */
    def gapsOf(listings: Seq[Listing], missed: Seq[String]): Seq[String] = {
      val slugs = listings.flatMap(_.page).map(_.trim.stripSuffix("/").split('/').last).filter(_.length >= 4).toSet
      val ids   = listings.flatMap(_.catalogueIds).map(_.id).filter(_.length >= 4).toSet
      def namesAnId(url: String) = Option(java.net.URI.create(url).getRawQuery).toSeq.flatMap(_.split('&'))
        .exists(p => p.startsWith("ids=") && ids(java.net.URLDecoder.decode(p.drop(4), java.nio.charset.StandardCharsets.UTF_8)))
      missed.filter(url => slugs.exists(slug => url.stripSuffix("/").endsWith(s"/$slug")) || scala.util.Try(namesAnId(url)).getOrElse(false))
    }

    /** Reads the venue `pages` the store does not hold yet, live, and keeps each for a later run's [[LiveGapLeaf]] to
     *  answer: a capture's listings whose page the recorded tree lacks, which leave their whole cluster unread (a fill
     *  waits on every listing's page — US "SEVENTEEN World Tour 'NEW_'": one page of 392). Paced like every live read of
     *  the capture (`pacing`: this run's share of `KINOWO_IDENTITY_LIVE_PER_HOST`, a 429 or 503 retried). How many it
     *  read; a page that fails stays a gap. */
    def readPages(pages: Seq[String],
                  pacing: tools.HostPacing = new tools.HostPacing(settings.ProcessConfiguration.resolve().identityLivePerHost.value, Retries, BackOff.toMillis),
                  real: HttpFetch = new RealHttpFetch()): Int = {
      val fresh = pages.distinct.filterNot(url => Files.exists(fileOf(s"GET $url")))
      val read  = new java.util.concurrent.atomic.AtomicInteger()
      val pool  = java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor()
      try {
        fresh.groupBy(url => Option(java.net.URI.create(url).getHost).getOrElse(url)).toSeq.flatMap { case (host, urls) =>
          val queue = new java.util.concurrent.ConcurrentLinkedQueue[String](urls.asJava)
          Seq.fill(pacing.limitOf(host))(java.util.concurrent.CompletableFuture.runAsync({ () =>
            Iterator.continually(Option(queue.poll())).takeWhile(_.isDefined).flatten.foreach { url =>
              scala.util.Try(pacing(url)(real.get(url))).foreach { body =>
                tools.AtomicFiles.writeString(fileOf(s"GET $url"), body); read.incrementAndGet()
              }
            }
          }: Runnable, pool))
        }.foreach(_.join())
      } finally pool.shutdown()
      read.get()
    }
  }

  // ── today's pipeline ──────────────────────────────────────────────────────────────────

  /** One film the pipeline made: its key, its TMDB id and record, and its cinema slots. `basis`: how
   *  the pipeline concluded `tmdbId` (`TmdbBasis`: TitleOnly, YearScoped, DirectorWalk, ExternalId) —
   *  which of its steps an old match rests on. */
  final case class PipelineFilm(key: String, tmdbId: Option[Int], film: Option[IdentityMeasures.Film],
                                slots: Seq[(String, String, SourceData)], basis: Option[String] = None)

  /** The wiring both sides use (`FetchReplayWiring`): a throwaway database seeded with the corpus,
   *  every answer from the corpus's fetch, retries that never sleep on a replayed refusal. */
  def wiring(target: IntegrationMongoTarget, c: Corpus, storages: mutable.ListBuffer[ConvergenceStorage],
             root: settings.FixtureRoot, environment: Env): ArchiveReplayWiring = {
    val storage = ConvergenceStorage.mongo(target, s"idshadow-${c.label}", c.normalizer)
    storages.synchronized(storages += storage)
    FetchReplayWiring(c.country, storage, c.rows, c.fetch, root, retrySleep = (_: Long) => (), environment = environment)
  }

  /** Boot production — the identity projection — the way the convergence legs do: every venue
   *  scraped into the intake, one projection and the enrichment it announces, then the read model. */
  def bootPipeline(w: ArchiveReplayWiring): Seq[PipelineFilm] = {
    w.bootCutover()
    tools.WholeReconcile(w.readModelProjector)
    w.movieRepository.findAll().map { f =>
      val tmdb = f.record.data.get(Tmdb)
      PipelineFilm(f.id.value, f.record.tmdbId,
        f.record.tmdbId.map(_ => IdentityMeasures.Film(tmdb.flatMap(_.title).getOrElse(f.title), tmdb.flatMap(_.originalTitle), Nil,
          tmdb.flatMap(_.releaseYear), tmdb.flatMap(_.runtimeMinutes), tmdb.map(_.director).filter(_.nonEmpty), None)),
        PipelineFilms.slotsOf(f), f.record.tmdbBasis)
    }.sortBy(_.key)
  }

  /** One corpus's booted pipeline, as the resolver side reads it: its films (without their slots),
   *  each listing's film, and what the boot cost. Kept on disk (`KINOWO_IDENTITY_PIPELINE_CACHE`)
   *  so several resolver builds measure against ONE boot of today's pipeline. */
  final case class BootedPipeline(films: Seq[PipelineFilm], filmOf: Map[ListingKey, Int], seconds: Double, requests: Long,
                                  unanswerable: Long)

  object BootedPipeline {
    import play.api.libs.json._
    private def filmJson(f: IdentityMeasures.Film): JsObject = Json.obj("title" -> f.title, "originalTitle" -> f.originalTitle,
      "year" -> f.year, "runtime" -> f.runtime, "directors" -> f.directors)
    private def filmFrom(js: JsValue): IdentityMeasures.Film = IdentityMeasures.Film((js \ "title").as[String],
      (js \ "originalTitle").asOpt[String], Nil, (js \ "year").asOpt[Int], (js \ "runtime").asOpt[Int], (js \ "directors").asOpt[Seq[String]], None)

    /** Keys by their rendered form: the listing keys a corpus produces render uniquely. */
    def write(path: Path, b: BootedPipeline): Unit = {
      val js = Json.obj(
        "films" -> b.films.map(f => Json.obj("key" -> f.key, "tmdbId" -> f.tmdbId, "film" -> f.film.map(filmJson), "basis" -> f.basis)),
        "filmOf" -> JsObject(b.filmOf.toSeq.map { case (k, i) => k.toString -> JsNumber(i) }),
        "seconds" -> b.seconds, "requests" -> b.requests, "unanswerable" -> b.unanswerable)
      Files.createDirectories(path.getParent)
      val out = new java.util.zip.GZIPOutputStream(Files.newOutputStream(path))
      try out.write(Json.stringify(js).getBytes(java.nio.charset.StandardCharsets.UTF_8)) finally out.close()
    }

    def read(path: Path, listings: Seq[Listing]): BootedPipeline = {
      val in = new java.util.zip.GZIPInputStream(Files.newInputStream(path))
      val js = try Json.parse(in) finally in.close()
      val byName = listings.map(l => l.key.toString -> l.key).toMap
      val filmOf = (js \ "filmOf").as[JsObject].value.toMap.map { case (k, i) =>
        byName.getOrElse(k, throw new IllegalStateException(s"$path names a listing the corpus does not list: $k")) -> i.as[Int] }
      BootedPipeline((js \ "films").as[Seq[JsValue]].map(f =>
          PipelineFilm((f \ "key").as[String], (f \ "tmdbId").asOpt[Int], (f \ "film").asOpt[JsValue].filter(_ != JsNull).map(filmFrom), Nil,
            (f \ "basis").asOpt[String])),
        filmOf, (js \ "seconds").as[Double], (js \ "requests").as[Long], (js \ "unanswerable").as[Long])
    }
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
    // What a resolve announces it will ask, answered ahead on a few threads: a live-gap replay
    // (`settings.IdentityLiveGaps`) otherwise asks its live questions one after another.
    override def prefetch(qs: Iterable[CandidateQuery], ids: Iterable[Int], pages: Iterable[Listing]): Unit = {
      inner.prefetch(qs, ids, pages)
      val pool = java.util.concurrent.Executors.newFixedThreadPool(Memo.PrefetchThreads)
      try {
        (qs.toSeq.map(q => () => { candidates(q); () }) ++ ids.toSeq.map(id => () => { film(id); () }))
          .map(task => pool.submit(new java.util.concurrent.Callable[Unit] { def call(): Unit = task() })).foreach(_.get())
      } finally pool.shutdown()
    }
    def sizes: (Int, Int, Int) = (details.size, queries.size, films.size)
    /** Whether `l`'s venue detail was asked through it and had no answer: its page unread. */
    def unreadDetail(l: Listing): Boolean = Option(details.get((l.venue, l.page.getOrElse("")))).exists(!_.isKnown)
    /** Every candidate question and film record asked through it, with the answer it got. */
    def asked: (Map[CandidateQuery, Answer[Seq[Hit]]], Map[Int, Answer[Option[IdentityMeasures.Film]]]) =
      (queries.asScala.toMap, films.asScala.toMap)
    def unknown: (Int, Int, Int) = (details.values.asScala.count(!_.isKnown), queries.values.asScala.count(!_.isKnown),
      films.values.asScala.count(!_.isKnown))
  }

  object Memo { val PrefetchThreads = 8 }

  /** A simulated TMDB OUTAGE: a deterministic share of candidate queries and film records
   *  withheld as `Unknown`. */
  final class Outage(inner: IdentityLookups, share: Double) extends IdentityLookups {
    private def withheld(key: String) = (key.hashCode.toLong & 0x7fffffffL) % 1000 < (share * 1000).toLong
    override def hasDetail(l: Listing): Boolean = inner.hasDetail(l)
    override def detail(l: Listing): Answer[Option[DetailFacts]] = inner.detail(l)
    override def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (withheld(q.sortKey)) Answer.Unknown else inner.candidates(q)
    override def film(id: Int): Answer[Option[IdentityMeasures.Film]] = if (withheld(s"film $id")) Answer.Unknown else inner.film(id)
  }

  /** `f` over `items` on every core, answers in `items`' order — for the robustness measures'
   *  many independent resolves of one corpus (21 presentations, up to 160 perturbed copies), which
   *  ran one after another and made this suite the integration job's critical path (~3 min of
   *  its ~4 on a CI runner). Safe because a resolve is a pure function of its listings and its
   *  lookups, and the lookups beneath it are a [[Memo]] (concurrent maps) over
   *  `TmdbIdentityLookups.CountedGaps`, which answers a fresh question under its own lock so a
   *  recording miss is still attributed to the question that met it.
   *
   *  `threads` bounds how many run at once: a WHOLE-corpus resolve holds a country resident, and
   *  one per core of a full US corpus would not fit the heap a hard-cluster resolve fits many times. */
  def sideBySide[A, B](items: Seq[A], threads: Int = Runtime.getRuntime.availableProcessors)(f: A => B): Seq[B] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, threads))
    try {
      given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.fromExecutor(pool)
      scala.concurrent.Await.result(scala.concurrent.Future.traverse(items)(item => scala.concurrent.Future(f(item))),
        scala.concurrent.duration.Duration.Inf)
    } finally pool.shutdownNow()
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
