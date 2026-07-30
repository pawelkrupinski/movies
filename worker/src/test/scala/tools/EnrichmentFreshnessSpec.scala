package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}
import scala.concurrent.duration._

/**
 * Ageing the RECORDED responses, which nothing used to age at all.
 *
 * `RecordingHttpFetch` writes a file and no one has ever removed one, so a Tomatometer
 * or an IMDb rating captured once was replayed indefinitely — the suite would go on
 * asserting last month's numbers while looking entirely healthy.
 */
class EnrichmentFreshnessSpec extends AnyFlatSpec with Matchers {

  private def withTree(body: Path => Unit): Unit = {
    val root = Files.createTempDirectory("enrichment-freshness-spec")
    try body(root)
    finally Files.walk(root).sorted(java.util.Comparator.reverseOrder())
      .forEach(path => Files.deleteIfExists(path))
  }

  private def write(root: Path, relative: String, ageInDays: Int): Path = {
    val path = root.resolve(relative)
    Files.createDirectories(path.getParent)
    Files.writeString(path, "recorded")
    Files.setLastModifiedTime(path,
      FileTime.fromMillis(System.currentTimeMillis() - ageInDays.days.toMillis))
    path
  }

  "enrichment freshness" should "delete a response older than the TTL and keep a newer one" in {
    withTree { root =>
      val stale = write(root, "www.rottentomatoes.com/m/old.content", ageInDays = 9)
      val fresh = write(root, "www.rottentomatoes.com/m/new.content", ageInDays = 1)

      EnrichmentFreshness.prune(root) shouldBe 1

      Files.exists(stale) shouldBe false
      Files.exists(fresh) shouldBe true
    }
  }

  // The verdict cache carries `fetchedAt` inside each entry and is expired on read by
  // the store, which is more accurate than an mtime — an entry rewritten in place would
  // otherwise look freshly fetched when it wasn't.
  it should "leave the remembered-answer cache to expire itself" in {
    withTree { root =>
      val cached = write(root, s"${FileEnrichmentCacheStore.CacheDirectoryName}/ab/cd/old.entry", ageInDays = 30)

      EnrichmentFreshness.prune(root) shouldBe 0

      Files.exists(cached) shouldBe true
    }
  }

  // Ages deliberately skip the TTL itself. A file written at exactly `now - Ttl` sits on
  // the boundary, and `prune` samples its own clock a few milliseconds later — so under
  // load the floor moves past it and the file expires, which is how this test failed
  // once in a full run and passed on its own.
  it should "expire gradually, so a tree recorded over days doesn't go cold all at once" in {
    withTree { root =>
      val fresh = Seq(1, 2, 3, 4)
      val stale = Seq(6, 7, 8)
      (fresh ++ stale).foreach(day => write(root, s"host/day-$day.json", ageInDays = day))

      EnrichmentFreshness.prune(root) shouldBe stale.size

      fresh.foreach(day => Files.exists(root.resolve(s"host/day-$day.json")) shouldBe true)
      stale.foreach(day => Files.exists(root.resolve(s"host/day-$day.json")) shouldBe false)
    }
  }

  it should "do nothing at all when there is no tree yet" in {
    withTree { root =>
      EnrichmentFreshness.prune(root.resolve("never-recorded")) shouldBe 0
    }
  }

  it should "hold the TTL in one place for the cache and the recorded responses alike" in {
    EnrichmentFreshness.Ttl shouldBe 5.days
    withClue("the verdict cache must not age at a different rate from the responses: ") {
      FileEnrichmentCacheStore.Ttl shouldBe EnrichmentFreshness.Ttl
    }
  }
}
