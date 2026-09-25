package controllers

import org.apache.pekko.util.ByteString
import play.api.Logging

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.{Duration, Instant}
import java.util.concurrent.{ConcurrentHashMap, RejectedExecutionException}
import java.util.zip.GZIPOutputStream
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/** Caches the GZIPPED bytes of the responses that are byte-identical
 *  for every client at a given cache version: the anonymous HTML pages
 *  (`/{city}/`, `/{city}/movies`) and the mobile JSON endpoints
 *  (`/{city}/api/repertoire`, `/{city}/api/details`). A hit skips BOTH the body
 *  build (Twirl render / JSON serialize) and the compression pass — measured at
 *  ~16 ms of server CPU per `/api/repertoire`, of which the compression alone is a
 *  large share. Caching the compressed bytes (not just the body string) is what
 *  captures that share.
 *
 *  The cache is keyed by request path and versioned by
 *  `WebReadModel.lastModifiedFor(city)` — the same per-city validator the
 *  response's ETag carries. A showtime update advances the version for THAT
 *  city, and the next read of its entries keeps being served the previous copy
 *  while ONE background render rebuilds it (see [[gzippedBody]]); the other
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
class EncodedResponseCache(refreshExecutor: ExecutionContext,
                           now: () => Instant,
                           maxBytes: Long = EncodedResponseCache.DefaultMaxBytes) extends Logging {
  import EncodedResponseCache.{MaxStaleAge, Served}

  /** `renderedAt` is when the body's render STARTED — the moment its snapshot of
   *  the read model was taken, and so what its staleness is measured from. */
  private final case class Entry(version: Instant, renderedAt: Instant, bytes: ByteString)

  // Access-ordered, so `get` promotes; guarded by `this` rather than concurrent
  // because access order makes reads mutating anyway. The critical sections are
  // map operations on a few dozen entries — the render and the gzip pass, which
  // are the expensive parts, deliberately happen OUTSIDE the lock so a cold miss
  // on one path never blocks a hit on another.
  private val entries = new java.util.LinkedHashMap[String, Entry](16, 0.75f, true)
  private var bytesHeld = 0L

  /** Keys with a background render scheduled or running — the single-flight
   *  guard. A key is released when its render ends, successfully or not, so a
   *  failed render is retried by the next request that finds the copy superseded. */
  private val refreshing = ConcurrentHashMap.newKeySet[String]()

  /** Gzipped bytes for `key`, and the version they were rendered at — which is
   *  what the caller's validators must describe, because it is NOT always
   *  `version`.
   *
   *  - A copy held at `version` (or later) is returned; `renderBody` is not
   *    evaluated.
   *  - A copy SUPERSEDED by `version` is still returned — stale-while-revalidate —
   *    while it was rendered no earlier than `staleFloor` and at most
   *    [[EncodedResponseCache.MaxStaleAge]] ago. One render at `version` is
   *    scheduled on `refreshExecutor` and replaces the copy when it finishes.
   *    Single-flight per key: a request finding one under way schedules nothing.
   *  - Otherwise — a cold key, or a copy too old to serve — `renderBody` runs on
   *    the calling thread, as every miss used to.
   *
   *  `staleFloor` retires copies at a boundary this cache cannot see: a city page
   *  rendered before that city's midnight names the old midnight as its reload
   *  time, so serving it afterwards would reload it straight into itself.
   *
   *  WHY: `/uk/london/` is 1.28 MB of HTML, its copy was superseded every 1-2
   *  minutes as London's showtimes moved, and the request that found it so
   *  rendered synchronously in 0.7-1.2 s (measured in production 2026-09-25)
   *  against ~5 ms for a hit. */
  def gzippedBody(key: String, version: Instant, staleFloor: Instant = Instant.MIN)(renderBody: => String): Served = {
    val held = synchronized(Option(entries.get(key)))
    held match {
      case Some(entry) if !entry.version.isBefore(version) =>
        Served(entry.version, entry.bytes)
      case Some(entry) if servableWhileRefreshing(entry, staleFloor) =>
        refreshInBackground(key, version, renderBody)
        Served(entry.version, entry.bytes)
      case _ =>
        render(key, version, renderBody)
    }
  }

  private def servableWhileRefreshing(entry: Entry, staleFloor: Instant): Boolean =
    !entry.renderedAt.isBefore(staleFloor) && !entry.renderedAt.isBefore(now().minus(MaxStaleAge))

  private def render(key: String, version: Instant, renderBody: => String): Served = {
    val renderedAt = now()
    val bytes      = EncodedResponseCache.gzip(renderBody)
    store(key, Entry(version, renderedAt, bytes))
    Served(version, bytes)
  }

  private def refreshInBackground(key: String, version: Instant, renderBody: => String): Unit =
    if (refreshing.add(key)) {
      val refresh: Runnable = () =>
        try render(key, version, renderBody)
        catch {
          case NonFatal(e) =>
            logger.warn(s"Background render of $key at $version failed; the next request retries it", e)
        }
        finally refreshing.remove(key)
      try refreshExecutor.execute(refresh)
      catch {
        // Shutting down: nothing will run it, so free the key and keep serving the copy.
        case e: RejectedExecutionException =>
          refreshing.remove(key)
          logger.warn(s"Background render of $key not scheduled: ${e.getMessage}")
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
    // A render finishing after a NEWER one was stored (a background refresh
    // overtaken by a synchronous render) must not put the older version back.
    val newerHeld = Option(entries.get(key)).exists(_.version.isAfter(entry.version))
    // An entry larger than the whole budget is never worth holding: storing it
    // would evict everything else and then itself on the next put.
    if (newerHeld) ()
    else if (entry.bytes.size <= maxBytes) {
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

  /** A body as served: the gzipped bytes and the version they were rendered at. */
  final case class Served(version: Instant, bytes: ByteString)

  /** How old a superseded copy's render may be and still be served while its
   *  replacement is built. Measured from when that render STARTED, so it bounds
   *  the age of the read-model snapshot a visitor is shown.
   *
   *  Ten minutes, against two cadences. On a healthy process it never engages: a
   *  refresh takes about a second (0.7-1.2 s for London) and the busiest city's
   *  copy is superseded every 1-2 minutes, so what is served is seconds to a
   *  couple of minutes old. What it catches is a refresh that keeps failing or
   *  queueing, and a quiet page whose copy is hours old — both render
   *  synchronously instead, as every miss did before. And ten minutes of lag is
   *  noise against the data itself: each cinema is re-scraped once per freshness
   *  window (60 min in Poland, 180 in Germany, 420 in the UK), so the read model
   *  already trails the cinemas' own sites by tens of minutes to hours. */
  val MaxStaleAge: Duration = Duration.ofMinutes(10)

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
