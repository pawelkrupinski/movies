package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HostPacing, HttpFetch, HttpStatusException}

import java.nio.file.Files
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/** The capture's read-ahead of the venue pages its recorded tree lacks: paced like every other live read of the capture
 *  — one per-host budget shared between the countries run side by side, a 429 or 503 retried — and kept for the next
 *  run's `LiveGapLeaf` to answer. */
class LiveGapLeafReadPagesSpec extends AnyFlatSpec with Matchers {

  import IdentityShadow.LiveGapLeaf

  private def pages(n: Int) = {
    val host = s"read-pages-${java.util.UUID.randomUUID()}.example"
    (1 to n).map(i => s"https://$host/film/$i")
  }
  /** A GET-only host: the read-ahead posts nothing. */
  private abstract class Pages extends HttpFetch {
    def post(url: String, body: String, contentType: String): String = throw new UnsupportedOperationException(url)
  }
  private def forget(urls: Seq[String]): Unit = urls.foreach(url => Files.deleteIfExists(LiveGapLeaf.fileOf(s"GET $url")))

  "the venue-page read-ahead" should "retry a page the host throttled, and keep it" in {
    val urls    = pages(1)
    val refused = new AtomicInteger
    val fetch = new Pages {
      def get(url: String): String =
        if (refused.getAndIncrement() == 0) throw new HttpStatusException(429, "GET", url, None) else s"<html>$url</html>"
    }
    try {
      LiveGapLeaf.readPages(urls, new HostPacing(budget = 4, sleep = _ => ()), fetch) shouldBe 1
      Files.readString(LiveGapLeaf.fileOf(s"GET ${urls.head}")) shouldBe s"<html>${urls.head}</html>"
    } finally forget(urls)
  }

  it should "read no more pages of a host at once than the run's share of the budget" in {
    val urls   = pages(6)
    val inside = new AtomicInteger
    val peak   = new AtomicInteger
    val seen   = ConcurrentHashMap.newKeySet[String]()
    val fetch = new Pages {
      def get(url: String): String = {
        peak.accumulateAndGet(inside.incrementAndGet(), math.max)
        try { seen.add(url); url } finally inside.decrementAndGet()
      }
    }
    try {
      LiveGapLeaf.readPages(urls, new HostPacing(budget = 1, sleep = _ => ()), fetch) shouldBe 6
      seen.size shouldBe 6
      peak.get shouldBe 1
    } finally forget(urls)
  }
}
