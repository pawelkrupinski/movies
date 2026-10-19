package tools

import scala.annotation.nowarn
import models.Country
import services.scrapes.ArchivedScrape

import java.nio.file.Path

/**
 * A "Record scrape fixtures" recording opened WITHOUT Mongo: one country's archived scrapes
 * (`cinema-scrapes-<cc>.json.gz`) and the fetch a hermetic leg replays its answers through — the recorded
 * `enrichment-<cc>` tree, then the verdicts remembered beside it ([[RememberedVerdicts]], read-only), then a
 * [[GapLeaf]] that names every request neither answers. What `integration.IdentityShadow` and `scripts.IdentitySlice`
 * both read a recording through.
 */
object RecordedCorpus {

  /** One country's recording, opened: its archived scrapes, the replay fetch, its gap leaf and the gaps it named. */
  @nowarn("msg=unused import")
  final class Opened(val country: Country, val rows: Seq[ArchivedScrape], val fetch: HttpFetch, val leaf: GapLeaf, val missing: MissingFixtures) {
    // The identity adapters, in either package a ref keeps them (OlderRefsPackage).
    import services.identity.*, services.identity.adapters.*

    /** The gaps met so far, as `TmdbIdentityLookups` tells a gap from an answer: per thread, lookups reading side by side. */
    def gaps: TmdbIdentityLookups.Gaps = new TmdbIdentityLookups.Gaps {
      def answered[A](read: => A): Answer[A] = {
        val before = leaf.metOnThread
        scala.util.Try(read).toOption.filter(_ => leaf.metOnThread == before)
          .fold[Answer[A]](Answer.Unknown)(Answer.Known(_))
      }
    }
  }

  /** `corpus` (a `cinema-scrapes-<cc>.json.gz`) and the `enrichment-<cc>` tree under `root`; `leafOf` wraps the gap leaf
   *  (a live leaf for the capture; the leaf itself for a hermetic replay). */
  def open(country: Country, corpus: Path, root: settings.FixtureRoot, leafOf: GapLeaf => HttpFetch = identity): Opened = {
    val missing = new MissingFixtures
    val tree    = s"enrichment-${country.code}"
    val store   = new EnrichmentCacheStore {
      private val files = new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(root, tree), FileEnrichmentCacheStore.NeverExpires)
      override def loadAll(): Map[String, CachedResponse] = files.loadAll()
      override def put(key: String, response: CachedResponse): Unit = ()
    }
    val cache = new EnrichmentCache(store)
    val leaf  = new GapLeaf(missing)
    cache.preload()
    val fetch = replayChain(new clients.tools.FakeHttpFetch(tree, strict = true, foldYear = false, root = root), cache, leafOf(leaf))
    new Opened(country, CorpusFixture.readFrom(corpus), fetch, leaf, missing)
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
}
