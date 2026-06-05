package controllers

import org.apache.pekko.util.ByteString

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.zip.GZIPOutputStream

/** Caches the fully-rendered, gzip-compressed bytes of the anonymous,
 *  unparameterised HTML pages (`/{city}/`, `/{city}/filmy`,
 *  `/{city}/kina[/:cinema]`). A hit skips BOTH the Twirl render and the gzip
 *  pass — measured at ~40 ms of server CPU per `/kina` request, of which gzip
 *  alone is ~40 %. Caching the compressed bytes (not just the HTML string) is
 *  what captures that gzip share.
 *
 *  The cache is keyed by request path and versioned by the [[MovieCache]]
 *  mtime: a showtime update advances the version and every stale entry is
 *  transparently re-rendered on next read. Only the plain (anonymous, no query,
 *  non-swap, gzip-accepting) case is ever cached — see `MovieController`; those
 *  responses are byte-identical for every visitor at a given version, so one
 *  blob per path is valid for all of them.
 *
 *  The corpus is tiny (a handful of cities × a few page types), so a plain
 *  `ConcurrentHashMap` holding one entry per path is enough — no eviction
 *  policy needed; a version bump replaces in place. */
class PageResponseCache {

  private final case class Entry(version: Instant, gzipped: ByteString)

  private val entries = new ConcurrentHashMap[String, Entry]()

  /** Gzipped bytes for `key` at `version`. On a hit with a matching version the
   *  cached bytes are returned and `renderHtml` is never evaluated; otherwise
   *  `renderHtml` runs, its output is compressed, stored under `version`, and
   *  returned. */
  def gzippedBody(key: String, version: Instant)(renderHtml: => String): ByteString = {
    val hit = entries.get(key)
    if (hit != null && hit.version == version) hit.gzipped
    else {
      val bytes = PageResponseCache.gzip(renderHtml)
      entries.put(key, Entry(version, bytes))
      bytes
    }
  }
}

object PageResponseCache {
  def gzip(s: String): ByteString = {
    val bos = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(bos)
    try gz.write(s.getBytes(StandardCharsets.UTF_8))
    finally gz.close()
    ByteString(bos.toByteArray)
  }
}
