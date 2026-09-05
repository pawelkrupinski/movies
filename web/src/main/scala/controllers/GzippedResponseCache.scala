package controllers

import org.apache.pekko.util.ByteString

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.zip.GZIPOutputStream

/** Caches the gzip-compressed bytes of the responses that are byte-identical
 *  for every client at a given cache version: the anonymous HTML pages
 *  (`/{city}/`, `/{city}/movies`) and the mobile JSON endpoints
 *  (`/{city}/api/repertoire`, `/{city}/api/details`). A hit skips BOTH the body
 *  build (Twirl render / JSON serialize) and the gzip pass — measured at ~16 ms
 *  of server CPU per `/api/repertoire`, of which gzip alone is a large share.
 *  Caching the
 *  compressed bytes (not just the body string) is what captures that share.
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
class GzippedResponseCache(maxBytes: Long = GzippedResponseCache.DefaultMaxBytes) {

  private final case class Entry(version: Instant, gzipped: ByteString)

  // Access-ordered, so `get` promotes; guarded by `this` rather than concurrent
  // because access order makes reads mutating anyway. The critical sections are
  // map operations on a few dozen entries — the render and the gzip pass, which
  // are the expensive parts, deliberately happen OUTSIDE the lock so a cold miss
  // on one path never blocks a hit on another.
  private val entries = new java.util.LinkedHashMap[String, Entry](16, 0.75f, true)
  private var bytesHeld = 0L

  /** Gzipped bytes for `key` at `version`. On a hit with a matching version the
   *  cached bytes are returned and `renderBody` is never evaluated; otherwise
   *  `renderBody` runs, its output is compressed, stored under `version`, and
   *  returned. */
  def gzippedBody(key: String, version: Instant)(renderBody: => String): ByteString = {
    val hit = synchronized(Option(entries.get(key)))
    hit match {
      case Some(entry) if entry.version == version => entry.gzipped
      case _ =>
        val bytes = GzippedResponseCache.gzip(renderBody)
        store(key, Entry(version, bytes))
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
    if (entry.gzipped.size <= maxBytes) {
      Option(entries.put(key, entry)).foreach(previous => bytesHeld -= previous.gzipped.size)
      bytesHeld += entry.gzipped.size
      val stale = entries.entrySet().iterator()
      while (bytesHeld > maxBytes && stale.hasNext) {
        val evicted = stale.next()          // access order: eldest use first
        if (evicted.getKey != key) {
          bytesHeld -= evicted.getValue.gzipped.size
          stale.remove()
        }
      }
    } else
      Option(entries.remove(key)).foreach(previous => bytesHeld -= previous.gzipped.size)
  }
}

object GzippedResponseCache {
  /** 64 MiB of compressed bodies. Chosen against the two shapes that share this
   *  process: every Polish, German, Spanish and British city's pages fit inside it
   *  several times over (so those deployments never evict), while the US — 55
   *  states, the largest 1.06 MB gzipped apiece — keeps its warm ones and lets the
   *  long tail a crawler touches fall out, instead of holding all of them against
   *  the same heap the read model lives in. */
  val DefaultMaxBytes: Long = 64L * 1024 * 1024

  def gzip(s: String): ByteString = {
    val bos = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(bos)
    try gz.write(s.getBytes(StandardCharsets.UTF_8))
    finally gz.close()
    ByteString(bos.toByteArray)
  }
}
