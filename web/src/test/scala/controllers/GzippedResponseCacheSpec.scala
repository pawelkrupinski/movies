package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.zip.GZIPInputStream

class GzippedResponseCacheSpec extends AnyFlatSpec with Matchers {

  private def gunzip(bytes: org.apache.pekko.util.ByteString): String = {
    val in = new GZIPInputStream(new ByteArrayInputStream(bytes.toArray))
    new String(in.readAllBytes(), StandardCharsets.UTF_8)
  }

  private val v1 = Instant.parse("2026-06-05T10:00:00Z")
  private val v2 = Instant.parse("2026-06-05T10:05:00Z")

  "gzippedBody" should "render once and serve the cached bytes on a second same-version read" in {
    val cache = new GzippedResponseCache
    var renders = 0
    def render(): String = { renders += 1; "<html>hello</html>" }

    val first  = cache.gzippedBody("/poznan/movies", v1)(render())
    val second = cache.gzippedBody("/poznan/movies", v1)(render())

    renders shouldBe 1
    second shouldBe first
    gunzip(first) shouldBe "<html>hello</html>"
  }

  it should "re-render when the version advances (stale entry invalidated)" in {
    val cache = new GzippedResponseCache
    var renders = 0
    def render(): String = { renders += 1; s"<html>v$renders</html>" }

    cache.gzippedBody("/poznan/movies", v1)(render())
    val afterBump = cache.gzippedBody("/poznan/movies", v2)(render())

    renders shouldBe 2
    gunzip(afterBump) shouldBe "<html>v2</html>"
  }

  it should "key independently per path" in {
    val cache = new GzippedResponseCache
    val a = cache.gzippedBody("/poznan/movies", v1)("<html>filmy</html>")
    val b = cache.gzippedBody("/poznan/", v1)("<html>index</html>")

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
    val cache = new GzippedResponseCache(maxBytes = 64 * 1024)
    (1 to 40).foreach(state => cache.gzippedBody(s"/state-$state/", v1)(incompressible(16 * 1024)))

    cache.heldBytes should be <= 64L * 1024
  }

  // Least-recently-USED, not least-recently-written: the pattern that overflows
  // this is a crawler sweeping cold states while visitors sit on a few hot ones,
  // and insertion order would evict exactly the pages being read.
  it should "keep the page that is still being read and drop the ones that are not" in {
    val cache = new GzippedResponseCache(maxBytes = 64 * 1024)
    val hot = incompressible(16 * 1024)
    cache.gzippedBody("/california/", v1)(hot)

    (1 to 20).foreach { state =>
      cache.gzippedBody(s"/cold-$state/", v1)(incompressible(16 * 1024))
      cache.gzippedBody("/california/", v1)(fail("the hot page must still be cached"))
    }

    var rerendered = false
    cache.gzippedBody("/california/", v1) { rerendered = true; hot }
    rerendered shouldBe false
  }

  // A body bigger than the whole budget would evict everything else and then
  // itself on the next put — pure churn. Serve it, hold nothing.
  it should "serve but not store a body larger than the entire budget" in {
    val cache = new GzippedResponseCache(maxBytes = 8 * 1024)

    val served = cache.gzippedBody("/california/", v1)(incompressible(64 * 1024))

    gunzip(served) should have length 64 * 1024
    cache.heldBytes shouldBe 0L
  }
}
