package integration

import models.Country
import services.identity._
import services.identity.adapters.TmdbIdentityLookups
import services.movies.{ListingKey, TitleNormalizer}
import services.scrapes.ArchivedScrape
import tools._

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
 * The recorded corpora the whole-corpus [[IdentityResolver]] tools read (`IdentityResolveDump`, the
 * unmatched-cluster capture, the seeding and query-coverage specs): each corpus's replayed answers, the
 * wiring that scrapes it, its raw listings and the memoised lookups over it. Test code.
 */
object IdentityShadow {

  val StubTmdbKey = "identity-shadow"

  // ── corpora ───────────────────────────────────────────────────────────────────────────

  /** Every request the resolver made, counted — the cost column. */
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
                     val misses: () => Long, val missedKeys: () => Seq[String], threadMisses: Option[() => Long] = None,
                     /** The TMDB and IMDb answers read live in place of the recording's gaps ([[LiveGapLeaf.answered]]). */
                     val liveAnswers: () => Map[String, String] = () => Map.empty,
                     /** The recording's capture of the listings prod's model kept for their facts alone ([[tools.ProdRetained]]). */
                     val retained: Seq[Listing] = Nil) {
    /** How the lookups tell a gap from an answer: by the gaps met on the asking thread when the corpus counts them per
     *  thread (lookups then read side by side), else by the replay's one counter, lookups taking turns. */
    def gaps: TmdbIdentityLookups.Gaps = threadMisses.fold[TmdbIdentityLookups.Gaps](new CountedGaps(misses))(onThread =>
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
   *  it, read-only — exactly a hermetic leg's chain ([[RecordedCorpus]]). A request neither answers is REFUSED and
   *  named, never fetched. */
  def full(countries: Set[Country], corpusDir: Path, root: settings.FixtureRoot,
           live: Option[settings.IdentityLiveGaps] = None): Seq[Corpus] =
    Country.all.filter(countries).flatMap { c =>
      Option(corpusDir.resolve(s"cinema-scrapes-${c.code}.json.gz")).filter(Files.exists(_)).map { path =>
        val liveLeaf = new java.util.concurrent.atomic.AtomicReference[Option[LiveGapLeaf]](None)
        val opened = RecordedCorpus.open(c, path, root, leaf => live.fold[HttpFetch](leaf) { key =>
          val l = new LiveGapLeaf(key, leaf); liveLeaf.set(Some(l)); l
        })
        new Corpus(s"full-${c.code}", c, opened.rows, opened.fetch, () => opened.leaf.met, () => opened.missing.keys.map(_._2),
          Some(() => opened.leaf.metOnThread), () => liveLeaf.get.fold(Map.empty[String, String])(_.answered),
          tools.ProdRetained.readIn(corpusDir, c.code))
      }
    }

  /** A gap TMDB or IMDb can answer, answered LIVE, the stub key swapped for a real one; a venue page the capture read
   *  live before ([[LiveGapLeaf.readPages]]), answered as read; anything else, and a live failure, is still the gap. For
   *  the local resolver-only loop and the capture only (`settings.IdentityLiveGaps`): at most
   *  `perHost` requests at a time per host ([[tools.HostPacing]]: halved on a 429 or 503, retried after a back-off), and
   *  every answer kept on disk ([[LiveGapLeaf.Store]]) so a re-run after a rule change asks nothing it asked before. A
   *  read that FAILED is kept nowhere — the gap again for the rest of this run, asked live again by the next. Every TMDB
   *  and IMDb answer it gives is also kept in `answered`, for the capture to hold ([[UnmatchedClusters.Capture.live]]). */
  final class LiveGapLeaf(key: settings.IdentityLiveGaps, gap: RecordedCorpus.GapLeaf,
                          perHost: settings.IdentityLivePerHost = settings.ProcessConfiguration.resolve().identityLivePerHost,
                          real: HttpFetch = new RealHttpFetch()) extends HttpFetch {
    import LiveGapLeaf._
    private val pacing = new tools.HostPacing(perHost.value, Retries, BackOff.toMillis)
    private val failed = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    private val gave   = new java.util.concurrent.ConcurrentHashMap[String, String]()
    /** Every TMDB and IMDb answer given in place of the gap this run, keyed as the replay asks (the stub key). */
    def answered: Map[String, String] = gave.asScala.toMap
    private def answerable(url: String) = url.contains("themoviedb.org") || url.contains("imdb.com")
    private def keyed(url: String) = url.replace(s"api_key=$StubTmdbKey", s"api_key=${key.tmdbKey}")
    private def stored(id: String)(read: => String): String = {
      val file = fileOf(id)
      if (Files.exists(file)) Files.readString(file)
      else { val answer = read; tools.AtomicFiles.writeString(file, answer); answer }
    }
    private def tried(url: String, id: String, read: => String, orGap: => String): String =
      if (failed.contains(id) || (!answerable(url) && !Files.exists(fileOf(id)))) orGap
      else scala.util.Try(stored(id)(pacing(url)(read))).fold(_ => { failed.add(id); orGap }, { body =>
        if (answerable(url)) gave.put(id, body)
        body
      })
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
      // every host at once, `pacing` holding each to its limit
      tools.ConcurrentAsks.each(fresh, atOnce = fresh.size) { url =>
        scala.util.Try(pacing(url)(real.get(url))).foreach { body =>
          tools.AtomicFiles.writeString(fileOf(s"GET $url"), body); read.incrementAndGet()
        }
      }
      read.get()
    }
  }

  // ── the replay wiring ─────────────────────────────────────────────────────────────────

  /** The corpus's wiring (`FetchReplayWiring`): a throwaway database seeded with the corpus,
   *  every answer from the corpus's fetch, retries that never sleep on a replayed refusal. */
  def wiring(target: IntegrationMongoTarget, c: Corpus, storages: mutable.ListBuffer[ConvergenceStorage],
             root: settings.FixtureRoot, environment: Env): ArchiveReplayWiring = {
    val storage = ConvergenceStorage.mongo(target, s"idshadow-${c.label}", c.normalizer)
    storages.synchronized(storages += storage)
    FetchReplayWiring(c.country, storage, c.rows, c.fetch, root, fillSleep = (_: Long) => (), environment = environment, retained = c.retained)
  }

  // ── the resolver ──────────────────────────────────────────────────────────────────────

  /** The corpus's raw listings — never `ScrapeListing.prepare`'s folded rows, which hide the very
   *  listings a venue lists twice — read as prod's model reads them ([[ModelRead]]): lean. With them, the listings prod's
   *  model kept for their facts alone that a billed one still bills the title of ([[ArchiveReplayWiring.retainedListings]]):
   *  the model's whole input, as prod's take-up holds it. */
  def listingsOf(w: ArchiveReplayWiring, normalizer: TitleNormalizer): Seq[Listing] = {
    val billed = ModelRead.corpus(w.archivedListings, normalizer)
    val kept   = w.retainedListings(billed)
    if (w.recordedRetained.nonEmpty)
      System.err.println(s"[identity] ${billed.size} listings billed, ${kept.size} kept for their facts alone (of ${w.recordedRetained.size} recorded)")
    billed ++ kept
  }

  /** [[tools.MemoLookups]], by the name the shadow harness has always used. */
  type Memo = tools.MemoLookups
  val Memo: tools.MemoLookups.type = tools.MemoLookups

  // ── reporting ─────────────────────────────────────────────────────────────────────────

  def pct(a: Long, b: Long): String = if (b == 0) "—" else f"${100.0 * a / b}%.1f%%"

  def timed[A](body: => A): (A, Double) = { val t0 = System.nanoTime(); val a = body; (a, (System.nanoTime() - t0) / 1e9) }
}
