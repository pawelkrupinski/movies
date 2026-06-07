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

    val first  = cache.gzippedBody("/poznan/filmy", v1)(render())
    val second = cache.gzippedBody("/poznan/filmy", v1)(render())

    renders shouldBe 1
    second shouldBe first
    gunzip(first) shouldBe "<html>hello</html>"
  }

  it should "re-render when the version advances (stale entry invalidated)" in {
    val cache = new GzippedResponseCache
    var renders = 0
    def render(): String = { renders += 1; s"<html>v$renders</html>" }

    cache.gzippedBody("/poznan/filmy", v1)(render())
    val afterBump = cache.gzippedBody("/poznan/filmy", v2)(render())

    renders shouldBe 2
    gunzip(afterBump) shouldBe "<html>v2</html>"
  }

  it should "key independently per path" in {
    val cache = new GzippedResponseCache
    val a = cache.gzippedBody("/poznan/filmy", v1)("<html>filmy</html>")
    val b = cache.gzippedBody("/poznan/", v1)("<html>index</html>")

    gunzip(a) shouldBe "<html>filmy</html>"
    gunzip(b) shouldBe "<html>index</html>"
  }
}
