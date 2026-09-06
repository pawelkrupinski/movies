package controllers

import org.apache.pekko.util.ByteString

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.zip.GZIPOutputStream

import com.aayushatharva.brotli4j.Brotli4jLoader
import com.aayushatharva.brotli4j.encoder.{BrotliOutputStream, Encoder}

/** Caches the COMPRESSED bytes of the responses that are byte-identical
 *  for every client at a given cache version: the anonymous HTML pages
 *  (`/{city}/`, `/{city}/movies`) and the mobile JSON endpoints
 *  (`/{city}/api/repertoire`, `/{city}/api/details`). A hit skips BOTH the body
 *  build (Twirl render / JSON serialize) and the compression pass — measured at
 *  ~16 ms of server CPU per `/api/repertoire`, of which the compression alone is a
 *  large share. Caching the compressed bytes (not just the body string) is what
 *  captures that share, and it is what makes brotli affordable here at all: brotli
 *  costs meaningfully more CPU than gzip per pass, and this turns "per request"
 *  into "once per version".
 *
 *  ⚠️ KEYED BY PATH **AND ENCODING**, because one page now has two compressed
 *  spellings. Sharing one slot between them would serve brotli bytes under
 *  `Content-Encoding: gzip` to the next caller — a body the client cannot inflate,
 *  from a cache that looked like it hit. The two blobs are independent entries and
 *  each is charged to the same byte budget.
 *
 *  The cache is keyed by request path and versioned by
 *  `WebReadModel.lastModifiedFor(city)` — the same per-city validator the
 *  response's ETag carries. A showtime update advances the version for THAT
 *  city and its entries are transparently rebuilt on next read; the other
 *  cities' blobs stay hot. (While the version was model-wide, one Warsaw
 *  showtime discarded every city's compressed body, so the cache was rebuilding
 *  the whole roster every couple of minutes.) Only requests whose output is
 *  client-independent reach it — see `MovieController` (anonymous, no query,
 *  non-swap, gzip-accepting), so one blob per path is valid for everyone.
 *
 *  IT IS BOUNDED BY BYTES, AND THAT IS NOT BOOKKEEPING FOR ITS OWN SAKE. This
 *  used to be a plain `ConcurrentHashMap` with no eviction, on the reasoning that
 *  "the corpus is tiny (a handful of cities × a few paths)". That held until the
 *  US: a city there is a STATE, so the roster is 55 of them × the five cacheable
 *  paths, and California's listing alone is 1.06 MB gzipped. A crawler walking
 *  the sitemap pinned every one of them in a 768m heap that also holds the read
 *  model — permanently, since nothing ever evicted. `web-us` restarted roughly
 *  hourly, and `pekko.jvm-exit-on-fatal-error` means the JVM exits rather than
 *  limps, so each one was a 502 window on the public site.
 *
 *  Eviction is least-recently-USED rather than least-recently-written: the
 *  access pattern that overflows this is a crawler sweeping cold states while
 *  real visitors sit on a few hot ones, and insertion order would evict exactly
 *  the hot ones. */
class EncodedResponseCache(maxBytes: Long = EncodedResponseCache.DefaultMaxBytes) {

  private final case class Entry(version: Instant, bytes: ByteString)

  // Access-ordered, so `get` promotes; guarded by `this` rather than concurrent
  // because access order makes reads mutating anyway. The critical sections are
  // map operations on a few dozen entries — the render and the gzip pass, which
  // are the expensive parts, deliberately happen OUTSIDE the lock so a cold miss
  // on one path never blocks a hit on another.
  private val entries = new java.util.LinkedHashMap[String, Entry](16, 0.75f, true)
  private var bytesHeld = 0L

  /** `encoding`-compressed bytes for `key` at `version`. On a hit with a matching
   *  version the cached bytes are returned and `renderBody` is never evaluated;
   *  otherwise `renderBody` runs, its output is compressed, stored under `version`,
   *  and returned.
   *
   *  NOTE THE MISS IS PER (path, encoding), SO A PAGE FETCHED BOTH WAYS RENDERS
   *  TWICE. That is the deliberate half of the trade: holding one rendered string
   *  and compressing it two ways would save the second render, at the cost of
   *  keeping the uncompressed body — several megabytes of it, in the same heap as
   *  the read model, for a second encoding almost nobody asks for. Virtually every
   *  client takes brotli, so the gzip render happens for the rare client that
   *  cannot, and the brotli one is what stays hot. */
  def encodedBody(key: String, version: Instant, encoding: ContentEncoding)
                 (renderBody: => String): ByteString = {
    val slot = s"${encoding.token}\u001f$key"
    val hit  = synchronized(Option(entries.get(slot)))
    hit match {
      case Some(entry) if entry.version == version => entry.bytes
      case _ =>
        val bytes = EncodedResponseCache.compress(encoding, renderBody)
        store(slot, Entry(version, bytes))
        bytes
    }
  }

  /** Bytes currently held, and how many bodies that is. Asserted by the size
   *  tests and published as `kinowo_web_cache_*` — a cache whose bound is
   *  the point needs its accounting measured, not assumed, and nothing in the
   *  process could previously say how much heap it was holding. */
  def heldBytes: Long = synchronized(bytesHeld)
  def heldEntries: Int = synchronized(entries.size)

  /** What this cache holds against its byte budget, for `kinowo_web_cache_*`.
   *  Built by hand rather than read off Caffeine — this one is an access-ordered
   *  `LinkedHashMap`, and it has no hit counters to report. */
  def occupancy: services.metrics.CacheOccupancy =
    services.metrics.CacheOccupancy(
      entries   = heldEntries.toLong,
      heldBytes = Some(heldBytes),
      maxBytes  = Some(maxBytes))

  private def store(key: String, entry: Entry): Unit = synchronized {
    // An entry larger than the whole budget is never worth holding: storing it
    // would evict everything else and then itself on the next put.
    if (entry.bytes.size <= maxBytes) {
      Option(entries.put(key, entry)).foreach(previous => bytesHeld -= previous.bytes.size)
      bytesHeld += entry.bytes.size
      val stale = entries.entrySet().iterator()
      while (bytesHeld > maxBytes && stale.hasNext) {
        val evicted = stale.next()          // access order: eldest use first
        if (evicted.getKey != key) {
          bytesHeld -= evicted.getValue.bytes.size
          stale.remove()
        }
      }
    } else
      Option(entries.remove(key)).foreach(previous => bytesHeld -= previous.bytes.size)
  }
}

object EncodedResponseCache {
  /** 64 MiB of compressed bodies. Chosen against the two shapes that share this
   *  process: every Polish, German, Spanish and British city's pages fit inside it
   *  several times over (so those deployments never evict), while the US — 55
   *  states, the largest 1.06 MB gzipped apiece — keeps its warm ones and lets the
   *  long tail a crawler touches fall out, instead of holding all of them against
   *  the same heap the read model lives in.
   *
   *  A PAGE CAN NOW TAKE TWO ENTRIES, one per encoding, so that arithmetic is no
   *  longer strictly one blob per path. It holds anyway, and by more than it used
   *  to: brotli is about two thirds the size of the gzip those figures were
   *  measured in, and essentially every client that asks takes brotli, so the gzip
   *  slot is minted only for the rare client that cannot. The worst case is a page
   *  fetched both ways, which is smaller than the two gzip copies this budget was
   *  already sized to survive — and the LRU is what makes the worst case an
   *  eviction rather than a leak. */
  val DefaultMaxBytes: Long = 64L * 1024 * 1024

  /** Brotli quality. NOT the library default of 11.
   *
   *  Measured on a real `/uk/manchester/` body (3,820,489 bytes of HTML), JIT and
   *  native warmed, best of three:
   *
   *      gzip    300,431 B     125 ms
   *      q=1     649,868 B      16 ms   (worse than gzip — brotli's window is tiny here)
   *      q=4     229,805 B      17 ms
   *      q=5     197,131 B      24 ms   ← chosen
   *      q=6     191,109 B      36 ms
   *      q=9     181,875 B     132 ms
   *      q=11    157,323 B  16,506 ms
   *
   *  q=5 is 34% SMALLER THAN GZIP AND FIVE TIMES FASTER TO PRODUCE, so replacing
   *  gzip with it costs nothing on either axis — an unusual position, and the
   *  reason there is no trade to argue about below q=6.
   *
   *  It also beats what we lost. Cloudflare's edge brotli measured 228,940 B on
   *  this page, which is q=4 to within a rounding error; `no-transform` gave that
   *  up to get the ETag through, and this gets back more than it gave.
   *
   *  Above q=5 the curve turns: q=9 buys 8% for 5x the CPU, and q=11 buys 20% for
   *  SIXTEEN SECONDS on the thread that renders — paid by whichever visitor
   *  arrives first after a showtime moves, on a page that moves all day. */
  val BrotliQuality = 5

  def compress(encoding: ContentEncoding, s: String): ByteString = encoding match {
    case ContentEncoding.Gzip   => gzip(s)
    case ContentEncoding.Brotli => brotli(s)
  }

  def gzip(s: String): ByteString = {
    val bos = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(bos)
    try gz.write(s.getBytes(StandardCharsets.UTF_8))
    finally gz.close()
    ByteString(bos.toByteArray)
  }

  /** ⚠️ `ensureAvailability` FIRST, EVERY TIME. It unpacks and links the JNI
   *  native, is idempotent and cheap after the first call, and without it the
   *  first `BrotliOutputStream` on a fresh JVM throws `UnsatisfiedLinkError` —
   *  which surfaces as a 500 on one unlucky request rather than as a boot
   *  failure, because nothing else touches brotli until a page is served. */
  def brotli(s: String): ByteString = {
    Brotli4jLoader.ensureAvailability()
    val bos = new ByteArrayOutputStream()
    val br  = new BrotliOutputStream(bos, new Encoder.Parameters().setQuality(BrotliQuality))
    try br.write(s.getBytes(StandardCharsets.UTF_8))
    finally br.close()
    ByteString(bos.toByteArray)
  }
}
