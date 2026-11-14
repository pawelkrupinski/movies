package tools

import play.api.Logging

import java.nio.file.{Files, Path, StandardCopyOption}
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
 * a corpus recorded over several days expires over several days — and one recorded in a
 * SINGLE leg would still share one mtime and expire whole, which is what emptied the US
 * tree on 2026-09-23 (27,359 recorded responses pruned to 3,534 in one run). So each
 * file's lifetime is also spread over the second half of the TTL by its path
 * ([[lifetimeOf]]): a tree recorded at one instant expires across two and a half days,
 * is re-recorded across them, and stays staggered from then on.
 */
object EnrichmentFreshness extends Logging {

  val Ttl: FiniteDuration = 5.days

  /**
   * Move recorded responses older than their [[lifetimeOf]] (at most [[Ttl]]) out of a
   * fixture tree onto its [[shelfOf]], and report how many went. The shelf is emptied
   * first, so it holds only this run's expiries. Shelved, not deleted: the live leg
   * refetches them, and when the site fails that refetch [[ExpiredFixtureFallback]] puts the
   * shelved answer back (Record scrape fixtures #186: Kino Muza answered 503 to everything,
   * and its expired pages, deleted here, left the published tree without them). A shelved
   * answer nothing asks again is gone with the next run's shelf, as it went before.
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
  /** How long the recorded response at `relative` (a path inside the tree) stands:
   *  somewhere in the second half of `ttl`, fixed by the path so every run agrees. Never
   *  longer than `ttl`, so the staleness bound is unchanged; never shorter than half of it,
   *  so a fresh tree is never pruned on its next run. */
  def lifetimeOf(relative: Path, ttl: FiniteDuration = Ttl): FiniteDuration =
    lifetimeOfKey(relative.toString.replace('\\', '/'), ttl)

  /** The same spread for anything named by a key — a remembered answer of the `.enrichment-cache`, by its request
   *  ([[FileEnrichmentCacheStore.loadAll]]): one recording's thousands of remembered 404s otherwise all expire in the
   *  same run, which re-asks every one of them live (Germany, 2026-10-25: 2,207 of ~20,000 preloaded, Metacritic asked
   *  9,523 times against 1,090, the rating phase 4,639 s against 576 — Record scrape fixtures #1542, #1476). */
  def lifetimeOfKey(key: String, ttl: FiniteDuration = Ttl): FiniteDuration = {
    val spread = Math.floorMod(key.hashCode, 1000) / 1000.0
    (ttl.toMillis - (ttl.toMillis / 2 * spread).toLong).millis
  }

  /** Where [[prune]] puts a tree's expired responses: beside the tree, never in it, so nothing packs or replays it. */
  def shelfOf(root: Path): Path = root.toAbsolutePath.resolveSibling(s"${root.getFileName}.expired")

  def prune(root: Path, ttl: FiniteDuration = Ttl): Int =
    if (!Files.isDirectory(root)) 0
    else {
      val shelf = shelfOf(root)
      if (Files.exists(shelf))
        Using(Files.walk(shelf))(_.iterator().asScala.toList.reverse.foreach(path => Try(Files.deleteIfExists(path))))
      val now   = System.currentTimeMillis()   // file mtimes are wall-clock stamps, so they are aged against the wall
      val stale = Using(Files.walk(root)) { walk =>
        walk.iterator().asScala
          .filter(Files.isRegularFile(_))
          .filterNot(_.startsWith(root.resolve(FileEnrichmentCacheStore.CacheDirectoryName)))
          .filter(path => Try(Files.getLastModifiedTime(path).toMillis).getOrElse(Long.MaxValue) <
                          now - lifetimeOf(root.relativize(path), ttl).toMillis)
          .toList
      }.getOrElse {
        logger.warn(s"Could not scan $root for stale enrichment fixtures — leaving it as it is")
        Nil
      }

      val removed = stale.count { path =>
        val shelved = shelf.resolve(root.relativize(path))
        Try { Files.createDirectories(shelved.getParent); Files.move(path, shelved, StandardCopyOption.REPLACE_EXISTING) }.isSuccess
      }
      if (removed > 0)
        logger.info(s"Expired $removed recorded enrichment response(s) past their lifetime (at most ${ttl.toDays}d) from $root — " +
          s"they will be re-fetched and recorded fresh, or kept from $shelf where the site fails the refetch")
      removed
    }
}
