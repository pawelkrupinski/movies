package tools

import play.api.Logging

import java.nio.file.{Files, Path}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Try, Using}

/**
 * How long a recorded enrichment answer is allowed to stand in for the live service.
 *
 * ONE number for the whole mechanism, because the mechanism has two halves that were
 * ageing at different rates — or not at all. The remembered-verdict cache expired
 * entries after seven days (one, in the Mongo store), while the recorded RESPONSES
 * under the fixture tree expired never: `RecordingHttpFetch` writes a file and nothing
 * has ever removed one. That is fine for a TMDB id, which doesn't change, and wrong for
 * everything the same tree holds beside it — an IMDb rating, a Tomatometer, a
 * Metascore, a Filmweb score all drift, and a corpus recorded once would have gone on
 * asserting last month's numbers indefinitely while looking perfectly healthy.
 *
 * Five days, applied to both halves, so "how fresh is this suite's metadata?" has a
 * single answer rather than three. Long enough that a leg is warm across a weekend and
 * the daily schedule never starts cold; short enough that a rating is at most a working
 * week stale, and that a wrong answer — a rate-limited 429 pinned as a verdict, a slug
 * that has since appeared — ages out on its own rather than needing anyone to notice.
 *
 * Expiry is deliberately gradual rather than a cliff. Files carry their own mtimes, so
 * a corpus recorded over several days expires over several days: roughly a fifth of it
 * refreshes daily once it reaches a steady state, instead of every leg going cold
 * together every fifth day.
 */
object EnrichmentFreshness extends Logging {

  val Ttl: FiniteDuration = 5.days

  /**
   * Delete recorded responses older than [[Ttl]] from a fixture tree, and report how
   * many went.
   *
   * The `.enrichment-cache` directory is left alone: those entries carry their own
   * `fetchedAt` inside the file and are expired on read by
   * [[FileEnrichmentCacheStore.loadAll]], which is both cheaper and more accurate than
   * an mtime — a cache entry rewritten in place would otherwise look freshly fetched
   * when it wasn't.
   *
   * Never fatal. A tree that can't be pruned costs staleness, not correctness, and
   * failing the run over it would trade a small problem for a total one.
   */
  def prune(root: Path, ttl: FiniteDuration = Ttl, now: Long = System.currentTimeMillis()): Int =
    if (!Files.isDirectory(root)) 0
    else {
      val floor = now - ttl.toMillis
      val stale = Using(Files.walk(root)) { walk =>
        walk.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filterNot(_.startsWith(root.resolve(FileEnrichmentCacheStore.CacheDirectoryName)))
          .filter(path => Try(Files.getLastModifiedTime(path).toMillis).getOrElse(Long.MaxValue) < floor)
          .toList
      }.getOrElse {
        logger.warn(s"Could not scan $root for stale enrichment fixtures — leaving it as it is")
        Nil
      }

      val removed = stale.count(path => Try(Files.deleteIfExists(path)).getOrElse(false))
      if (removed > 0)
        logger.info(s"Expired $removed recorded enrichment response(s) older than ${ttl.toDays}d from $root — " +
          "they will be re-fetched and recorded fresh")
      removed
    }
}
