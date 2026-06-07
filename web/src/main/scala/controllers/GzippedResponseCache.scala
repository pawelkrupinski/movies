package controllers

import org.apache.pekko.util.ByteString

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.GZIPOutputStream

/** Caches the gzip-compressed bytes of the responses that are byte-identical
 *  for every client at a given cache version: the anonymous HTML pages
 *  (`/{city}/`, `/{city}/filmy`) and the mobile JSON endpoints
 *  (`/{city}/api/repertoire`, `/{city}/api/details`). A hit skips BOTH the body
 *  build (Twirl render / JSON serialize) and the gzip pass — measured at ~16 ms
 *  of server CPU per `/api/repertoire`, of which gzip alone is a large share.
 *  Caching the
 *  compressed bytes (not just the body string) is what captures that share.
 *
 *  The cache is keyed by request path and versioned by the [[MovieCache]]
 *  mtime: a showtime update advances the version and every stale entry is
 *  transparently rebuilt on next read. Only requests whose output is
 *  client-independent reach it — see `MovieController` (anonymous, no query,
 *  non-swap, gzip-accepting), so one blob per path is valid for everyone.
 *
 *  The corpus is tiny (a handful of cities × a few paths), so a plain
 *  `ConcurrentHashMap` holding one entry per path is enough — no eviction
 *  policy needed; a version bump replaces in place. */
class GzippedResponseCache {

  private final case class Entry(version: Instant, gzipped: ByteString)

  private val entries = new ConcurrentHashMap[String, Entry]()

  /** Gzipped bytes for `key` at `version`. On a hit with a matching version the
   *  cached bytes are returned and `renderBody` is never evaluated; otherwise
   *  `renderBody` runs, its output is compressed, stored under `version`, and
   *  returned. */
  def gzippedBody(key: String, version: Instant)(renderBody: => String): ByteString = {
    val hit = entries.get(key)
    if (hit != null && hit.version == version) hit.gzipped
    else {
      val bytes = GzippedResponseCache.gzip(renderBody)
      entries.put(key, Entry(version, bytes))
      bytes
    }
  }
}

object GzippedResponseCache {
  def gzip(s: String): ByteString = {
    val bos = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(bos)
    try gz.write(s.getBytes(StandardCharsets.UTF_8))
    finally gz.close()
    ByteString(bos.toByteArray)
  }
}
