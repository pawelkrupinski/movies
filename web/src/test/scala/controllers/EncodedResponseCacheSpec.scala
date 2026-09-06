package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.zip.GZIPInputStream

class EncodedResponseCacheSpec extends AnyFlatSpec with Matchers {

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes.toArray))
    new String(in.readAllBytes(), StandardCharsets.UTF_8)
  }

  private val v1 = Instant.parse("2026-06-05T10:00:00Z")
  private val v2 = Instant.parse("2026-06-05T10:05:00Z")

  "encodedBody" should "render once and serve the cached bytes on a second same-version read" in {
    val cache = new EncodedResponseCache
    var renders = 0
    def render(): String = { renders += 1; "<html>hello</html>" }

    val first  = cache.encodedBody("/poznan/movies", v1, ContentEncoding.Gzip)(render())
    val second = cache.encodedBody("/poznan/movies", v1, ContentEncoding.Gzip)(render())

    renders shouldBe 1
    second shouldBe first
    gunzip(first) shouldBe "<html>hello</html>"
  }

  it should "re-render when the version advances (stale entry invalidated)" in {
    val cache = new EncodedResponseCache
    var renders = 0
    def render(): String = { renders += 1; s"<html>v$renders</html>" }

    cache.encodedBody("/poznan/movies", v1, ContentEncoding.Gzip)(render())
    val afterBump = cache.encodedBody("/poznan/movies", v2, ContentEncoding.Gzip)(render())

    renders shouldBe 2
    gunzip(afterBump) shouldBe "<html>v2</html>"
  }

  it should "key independently per path" in {
    val cache = new EncodedResponseCache
    val a = cache.encodedBody("/poznan/movies", v1, ContentEncoding.Gzip)("<html>filmy</html>")
    val b = cache.encodedBody("/poznan/", v1, ContentEncoding.Gzip)("<html>index</html>")

    gunzip(a) shouldBe "<html>filmy</html>"
    gunzip(b) shouldBe "<html>index</html>"
  }

  // ── The byte bound ──────────────────────────────────────────────────────────
  //
  // This cache used to be unbounded, on the assumption of "a handful of cities x a
  // few paths". A US city is a STATE: 55 of them, the largest 1.06 MB gzipped, all
  // pinned forever in the same 768m heap as the read model once a crawler had
  // walked the sitemap. `web-us` restarted roughly hourly.

  /** Incompressible bytes, so a body's gzipped size is ~its source size and the
   *  budget arithmetic below is about what it says it is. */
  private def incompressible(bytes: Int): String = {
    val random = new scala.util.Random(bytes)
    val chars = new Array[Char](bytes)
    var i = 0
    while (i < bytes) { chars(i) = (32 + random.nextInt(95)).toChar; i += 1 }
    new String(chars)
  }

  "a cache at its byte budget" should "evict rather than grow without bound" in {
    val cache = new EncodedResponseCache(maxBytes = 64 * 1024)
    (1 to 40).foreach(state => cache.encodedBody(s"/state-$state/", v1, ContentEncoding.Gzip)(incompressible(16 * 1024)))

    cache.heldBytes should be <= 64L * 1024
  }

  // Least-recently-USED, not least-recently-written: the pattern that overflows
  // this is a crawler sweeping cold states while visitors sit on a few hot ones,
  // and insertion order would evict exactly the pages being read.
  it should "keep the page that is still being read and drop the ones that are not" in {
    val cache = new EncodedResponseCache(maxBytes = 64 * 1024)
    val hot = incompressible(16 * 1024)
    cache.encodedBody("/california/", v1, ContentEncoding.Gzip)(hot)

    (1 to 20).foreach { state =>
      cache.encodedBody(s"/cold-$state/", v1, ContentEncoding.Gzip)(incompressible(16 * 1024))
      cache.encodedBody("/california/", v1, ContentEncoding.Gzip)(fail("the hot page must still be cached"))
    }

    var rerendered = false
    cache.encodedBody("/california/", v1, ContentEncoding.Gzip) { rerendered = true; hot }
    rerendered shouldBe false
  }

  // A body bigger than the whole budget would evict everything else and then
  // itself on the next put — pure churn. Serve it, hold nothing.
  it should "serve but not store a body larger than the entire budget" in {
    val cache = new EncodedResponseCache(maxBytes = 8 * 1024)

    val served = cache.encodedBody("/california/", v1, ContentEncoding.Gzip)(incompressible(64 * 1024))

    gunzip(served) should have length 64 * 1024
    cache.heldBytes shouldBe 0L
  }

  // ── Brotli, and the slot separation it forced ──────────────────────────────
  //
  // Cloudflare used to brotli our gzip at the edge. `Cache-Control: no-transform`
  // stopped it — that is what let the ETag through — so the origin compresses or
  // nobody does.
  "brotli" should "round-trip through the real encoder" in {
    val html  = "<html><body>" + ("Kino Muranów — Poznań " * 500) + "</body></html>"
    val bytes = EncodedResponseCache.brotli(html)

    com.aayushatharva.brotli4j.Brotli4jLoader.ensureAvailability()
    val decoded = new String(
      com.aayushatharva.brotli4j.decoder.Decoder.decompress(bytes.toArray).getDecompressedData,
      java.nio.charset.StandardCharsets.UTF_8)

    decoded shouldBe html                    // the native is loaded and actually works
    bytes.size should be < html.length       // …and it compressed
  }

  it should "beat gzip on the markup this cache actually holds" in {
    val html = "<html><body>" + ("<div class=\"film\"><h2>Dune</h2></div>" * 800) + "</body></html>"
    EncodedResponseCache.brotli(html).size should be < EncodedResponseCache.gzip(html).size
  }

  // ⚠️ THE BUG A SHARED SLOT WOULD CAUSE: brotli bytes served under
  // `Content-Encoding: gzip`, which no client can inflate — from a cache that
  // looked like a hit. One page, one version, two encodings, two entries.
  "the cache" should "keep a page's two encodings apart rather than overwriting one with the other" in {
    val cache = new EncodedResponseCache
    val v1    = Instant.parse("2026-01-01T00:00:00Z")
    val html  = "<html>Poznań</html>"

    val gz = cache.encodedBody("/poznan/", v1, ContentEncoding.Gzip)(html)
    val br = cache.encodedBody("/poznan/", v1, ContentEncoding.Brotli)(html)

    gz should not be br
    cache.heldEntries shouldBe 2
    // and each still serves its OWN bytes back, rather than the other's
    cache.encodedBody("/poznan/", v1, ContentEncoding.Gzip)(fail("gzip must still be cached")) shouldBe gz
    cache.encodedBody("/poznan/", v1, ContentEncoding.Brotli)(fail("brotli must still be cached")) shouldBe br
  }

  it should "rebuild both encodings when the version moves" in {
    val cache = new EncodedResponseCache
    val v1    = Instant.parse("2026-01-01T00:00:00Z")
    val v2    = Instant.parse("2026-01-02T00:00:00Z")

    cache.encodedBody("/poznan/", v1, ContentEncoding.Brotli)("<html>before</html>")
    val after = cache.encodedBody("/poznan/", v2, ContentEncoding.Brotli)("<html>after</html>")

    after shouldBe EncodedResponseCache.brotli("<html>after</html>")
    cache.heldEntries shouldBe 1   // the stale one was replaced in its own slot, not added to
  }
}
