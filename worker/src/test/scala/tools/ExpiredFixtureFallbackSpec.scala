package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/**
 * A recording keeps a site's previous answer when the site fails it.
 *
 * Record scrape fixtures #186 (2026-11-14): Kino Muza answered HTTP 503 to every request, directly and through
 * production's proxy. The expiry had already deleted its detail pages past their lifetime, so their live refetch failed
 * and the published tree held nothing for them. Expired responses are now shelved, not deleted, and a failed refetch
 * puts the shelved one back.
 */
class ExpiredFixtureFallbackSpec extends AnyFlatSpec with Matchers with ScratchDirectories {

  private val url = "https://www.kinomuza.pl/movie/buty-nieboszczyka/"

  private final class Scripted(answer: () => Array[Byte]) extends HttpFetch {
    var asked = 0
    override def get(url: String): String = new String(getBytes(url), "UTF-8")
    override def getBytes(url: String): Array[Byte] = { asked += 1; answer() }
    override def post(url: String, body: String, contentType: String): String = new String(getBytes(url), "UTF-8")
  }

  private val down = () => throw new RuntimeException("All 2 backends failed: direct: HTTP 503, proxy: HTTP 503")

  private def withRecordedTree(ageInDays: Int)(body: (Path, Path) => Unit): Unit = {
    val root = tempDirectory("expired-fixture-fallback-spec")
    val tree = root.resolve("enrichment-pl")
    val page = tree.resolve("www.kinomuza.pl/movie/buty-nieboszczyka/.0")
    Files.createDirectories(page.getParent)
    Files.writeString(page, "<html>recorded</html>")
    Files.setLastModifiedTime(page, FileTime.fromMillis(System.currentTimeMillis() - ageInDays.days.toMillis))
    try body(tree, page)
    finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))
  }

  "an expired response" should "be shelved, not deleted, so a failed refetch still has it" in {
    withRecordedTree(ageInDays = 9) { (tree, page) =>
      EnrichmentFreshness.prune(tree) shouldBe 1
      Files.exists(page) shouldBe false
      Files.exists(EnrichmentFreshness.shelfOf(tree).resolve(tree.relativize(page))) shouldBe true
    }
  }

  it should "come back into the tree, still expired, when the site fails its refetch" in {
    withRecordedTree(ageInDays = 9) { (tree, page) =>
      val recordedAt = Files.getLastModifiedTime(page)
      EnrichmentFreshness.prune(tree)
      val fetch = new ExpiredFixtureFallback(new Scripted(down), EnrichmentFreshness.shelfOf(tree), tree)

      new String(fetch.getBytes(url), "UTF-8") shouldBe "<html>recorded</html>"
      Files.readString(page) shouldBe "<html>recorded</html>"
      // Its old mtime keeps it expired: the next recording asks the site again.
      Files.getLastModifiedTime(page) shouldBe recordedAt
      Files.exists(EnrichmentFreshness.shelfOf(tree).resolve(tree.relativize(page))) shouldBe false
    }
  }

  it should "be what a recording's chain answers with, and then the tree, the site asked once" in {
    withRecordedTree(ageInDays = 9) { (tree, _) =>
      EnrichmentFreshness.prune(tree)
      val site  = new Scripted(down)
      val chain = ArchiveReplayWiring.recordedChain(tree.getFileName.toString, settings.FixtureRoot(tree.getParent), None, site,
        "fixtures", "live", keepsExpiredOnFailure = true)

      chain.get(url) shouldBe "<html>recorded</html>"
      chain.get(url) shouldBe "<html>recorded</html>"
      site.asked shouldBe 1
    }
  }

  it should "stay gone in a chain that does not keep expired answers (a hermetic replay)" in {
    withRecordedTree(ageInDays = 9) { (tree, _) =>
      EnrichmentFreshness.prune(tree)
      val chain = ArchiveReplayWiring.recordedChain(tree.getFileName.toString, settings.FixtureRoot(tree.getParent), None,
        new Scripted(down), "fixtures", "live")
      an[Exception] should be thrownBy chain.get(url)
    }
  }

  it should "give way to the live answer when the site answers" in {
    withRecordedTree(ageInDays = 9) { (tree, page) =>
      EnrichmentFreshness.prune(tree)
      val fetch = new ExpiredFixtureFallback(new Scripted(() => "<html>fresh</html>".getBytes("UTF-8")),
        EnrichmentFreshness.shelfOf(tree), tree)

      new String(fetch.getBytes(url), "UTF-8") shouldBe "<html>fresh</html>"
      Files.exists(page) shouldBe false   // the recorder above it writes the fresh answer, not this
    }
  }

  "a failed fetch with nothing shelved" should "fail the way the site did" in {
    withRecordedTree(ageInDays = 1) { (tree, _) =>
      EnrichmentFreshness.prune(tree) shouldBe 0
      val fetch = new ExpiredFixtureFallback(new Scripted(down), EnrichmentFreshness.shelfOf(tree), tree)
      the[RuntimeException] thrownBy fetch.getBytes("https://www.kinomuza.pl/movie/never-recorded/") should
        have message "All 2 backends failed: direct: HTTP 503, proxy: HTTP 503"
    }
  }

  "the shelf" should "hold only this run's expiries, never a previous run's" in {
    withRecordedTree(ageInDays = 9) { (tree, _) =>
      val leftover = EnrichmentFreshness.shelfOf(tree).resolve("www.kinomuza.pl/movie/gone-since/.0")
      Files.createDirectories(leftover.getParent)
      Files.writeString(leftover, "from an earlier run")
      EnrichmentFreshness.prune(tree) shouldBe 1
      Files.exists(leftover) shouldBe false
    }
  }
}
