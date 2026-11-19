package services.enrichment

import models.{MovieRecord, SourceData}
import services.enrichment.SiteRatings.{Found, Page, PageRead}
import services.movies.{CacheKey, MovieCache}
import services.tasks.BulkRefreshResult
import tools.{BoundedParallel, Stopwatch}

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try}

/**
 * The enrichment flow every URL-keyed rating site shares, over the site's port ([[SiteRatings.Site]], below): the queue's per-row
 * refresh (`RatingHandler`), the operator's full-corpus walk (`/tasks`) and the re-check of given films
 * (`services.refresh.RatingRefreshSource`). The site finds and reads pages; this decides what is written, through the
 * cache's per-title lock, and what the adaptive cadence is told.
 *
 * Per row: a stored link is read (`read`) -- a denied page drops the link, its score and the site's slot, so the next
 * refresh finds the film afresh; a missing link is found first (`find`) and stored. Returns the badge a moved score
 * became, the signal [[services.cadence.RatingCadence]] backs off on.
 */
class SiteRatings[A](
  movieCache: MovieCache,
  site: SiteRatings.Site[A],
  // Times the walk, read off the wiring's clock source (tools.Stopwatch).
  stopwatch: Stopwatch,
  recordBulkChange: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(movieCache, recordBulkChange) {

  private val columns = site.columns

  override protected def sourceName: String = site.name

  override protected def refreshConcurrency: Int = site.walkConcurrency

  private def label(key: CacheKey): String = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"

  // ── Per row ────────────────────────────────────────────────────────────────

  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { row =>
      columns.url(row) match {
        case Some(url) => scoreFrom(key, row, url)
        case None => site.find(key, row) match {
          case Some(found) => take(key, row, found)
          case None        => logger.info(s"${site.name}: ${label(key)} → no URL match"); None
        }
      }
    }

  /** A found page stored on the row as it stood (`row`), then scored: a link's score is the one its probe read, else
   *  the page's own; a page read whole is stored whole. Returns the badge the score became if it moved. */
  private def take(key: CacheKey, row: MovieRecord, found: Found[A]): Option[String] = found match {
    case Found.Link(url, probed, how) =>
      logger.info(s"${site.name}: ${label(key)} → URL discovered$how $url")
      cache.putIfPresent(key, columns.withUrl(_, url))
      probed match {
        case Some(score) => cache.get(key).flatMap(now => persistIfMoved(key, url, columns.score(now), score))
        case None        => cache.get(key).flatMap(scoreFrom(key, _, url))
      }
    case Found.Whole(url, page) =>
      logger.info(s"${site.name}: ${label(key)} → URL discovered $url ${columns.noun}=${page.score.getOrElse("—")}")
      val moved = storeWhole(key, row, page, Some(url))
      site.pageTaken(key, row)
      moved
  }

  /** The page at the row's stored `url`, read and written; the badge its score became if it moved. */
  private def scoreFrom(key: CacheKey, row: MovieRecord, url: String): Option[String] =
    verdict(key, url, site.read(key, row, url)).flatMap { read =>
      val moved = read match {
        case PageRead.Read(page) =>
          logger.info(s"${site.name}: ${label(key)} $url → page read whole ${columns.noun}=${page.score.getOrElse("—")}")
          storeWhole(key, row, page, None)
        case PageRead.Scored(Some(score)) =>
          persistIfMoved(key, url, columns.score(row), score)
        case _ =>
          logger.info(s"${site.name}: ${label(key)} $url → no ${columns.noun} on page")
          None
      }
      site.pageTaken(key, row)
      moved
    }

  /** A read that denied its page drops what came off it (and answers `None`); any other is handed back. */
  private def verdict(key: CacheKey, url: String, read: PageRead[A]): Option[PageRead[A]] = read match {
    case PageRead.Denied(why) =>
      logger.info(s"${site.name}: ${label(key)} $url → $why, not this film — dropping the URL")
      cache.putIfPresent(key, columns.cleared)
      None
    case other => Some(other)
  }

  /** A page read whole, written in one write with the link it was found under (if newly found), and the row's other
   *  sources re-kicked by what its content changed; the badge its score became if it moved. */
  private def storeWhole(key: CacheKey, row: MovieRecord, page: Page[A], url: Option[String]): Option[String] = {
    cache.putIfPresent(key, current => url.foldLeft(columns.withPage(current, page))(columns.withUrl))
    cache.get(key).foreach(after => cache.retriggerAfterEnrichment(key, row, after))
    Option.when(page.score != columns.score(row))(page.score.map(columns.badge)).flatten
  }

  /** Write a score just read off the row's link when it differs from what the row holds (the LIVE row: the merge
   *  point for a link just written and any concurrent update), and answer the badge it became — the displayed-value
   *  change the cadence keys on — else `None`. */
  private[enrichment] def persistIfMoved(key: CacheKey, url: String, stored: Option[A], fresh: A): Option[String] = {
    val commit = !stored.contains(fresh)
    logger.info(s"${site.name}: ${label(key)} $url → ${columns.noun} $fresh" +
      (if (commit) s" (was ${stored.getOrElse("—")})" else " (unchanged)"))
    if (commit) { cache.putIfPresent(key, columns.withScore(_, Some(fresh))); Some(columns.badge(fresh)) }
    else None
  }

  // ── The walk ───────────────────────────────────────────────────────────────

  private[services] def refreshAll(): BulkRefreshResult = recheck(cache.entries)

  /** The walk over `rows` -- the whole cache for the full-corpus walk, an operator's films for a re-check: ONE pass,
   *  two steps per row — find the row's page afresh, then re-read the score off whatever link the row NOW holds.
   *
   *  It used to be two passes split on whether the row already had a link, which made a stored link permanently
   *  authoritative: the operator's button could only re-scrape the score off whatever was there, so a WRONG link was
   *  never corrected and the run reported "0 changed" while films sat on another film's page.
   *
   *  Both steps run, deliberately. Finding ALONE would be a regression: a row whose search fails (transient, or the
   *  site genuinely has no page) would stop refreshing its score at all.
   *
   *  Step 1 runs only for rows with a TMDB id. Each score write goes through `cache.putIfPresent` (the per-title
   *  lock), and a moved score is reported to the adaptive cadence under the SNAPSHOT row's tmdbId. A failed step — a
   *  search or a read that throws — counts the row as failed and leaves what the row holds alone; a row whose search
   *  failed still refreshes its score off the link it already had. A denied page drops its link and is not counted. */
  override private[services] def recheck(rows: Seq[(CacheKey, MovieRecord)]): BulkRefreshResult = {
    val startedAt  = stopwatch.start()
    val resolvable = rows.count { case (_, row) => row.tmdbId.isDefined }
    logger.info(s"${site.walkLabel}: starting tick over ${rows.size} cached row(s) ($resolvable re-resolving their URL first).")
    val counts = new SiteRatings.WalkCounts
    BoundedParallel.foreach(site.walkLabel.replace(' ', '-'), rows, refreshConcurrency) { case (key, row) =>
      if (row.tmdbId.isDefined) rediscover(key, row, counts)
      rescore(key, row, counts)
    }
    val message = s"tick done in ${startedAt.millis}ms — ${counts.changed.get} ${columns.plural} changed, " +
                  s"${counts.urlDiscovered.get} URL(s) newly discovered, ${counts.failed.get} failed."
    logger.info(s"${site.walkLabel}: $message")
    BulkRefreshResult.counts(walked = rows.size, changed = counts.changed.get, discovered = counts.urlDiscovered.get,
      failed = counts.failed.get, message = message)
  }

  /** Step 1: the row's page found afresh and stored. A link counts as discovered whenever found; a page read whole
   *  when it gave the row a link it lacked, and the score it moved is told to the cadence. A search that THROWS (a
   *  failed read: tools.ReadOutcome) is that row's failure, never the walk's: BoundedParallel loses the whole batch to
   *  one throwing row. */
  private def rediscover(key: CacheKey, row: MovieRecord, counts: SiteRatings.WalkCounts): Unit =
    Try(site.find(key, row).exists(store(key, row, _))) match {
      case Success(discovered) => if (discovered) counts.urlDiscovered.incrementAndGet()
      case Failure(exception) =>
        counts.failed.incrementAndGet()
        logger.debug(s"${site.walkLabel}: ${key.cleanTitle} lookup failed: ${exception.getMessage}")
    }

  /** A page step 1 found, stored; whether it counts as discovered. */
  private def store(key: CacheKey, row: MovieRecord, found: Found[A]): Boolean = found match {
    case Found.Link(url, _, how) =>
      logger.info(s"${site.name}: ${label(key)} → URL discovered$how $url")
      cache.putIfPresent(key, columns.withUrl(_, url))
      true
    case whole: Found.Whole[A] =>
      take(key, row, whole).foreach(badge => recordCadenceChange(key, row.tmdbId, Some(badge)))
      cache.get(key).exists(columns.url(_).isDefined) && columns.url(row).isEmpty
  }

  /** Step 2: the score re-read off whatever link the row NOW holds — possibly the one just found. */
  private def rescore(key: CacheKey, snapshot: MovieRecord, counts: SiteRatings.WalkCounts): Unit = {
    val current = cache.get(key).getOrElse(snapshot)
    columns.url(current).foreach { url =>
      Try(verdict(key, url, site.walkRead(key, current, url))) match {
        case Success(Some(read)) =>
          val fresh = read match {
            case PageRead.Read(page)    => page.score
            case PageRead.Scored(score) => score
            case PageRead.Denied(_)     => None
          }
          if (fresh != columns.score(current)) {
            logger.debug(s"${site.walkLabel}: ${key.cleanTitle} $url ${columns.score(current).getOrElse("—")} → ${fresh.getOrElse("—")}")
            cache.putIfPresent(key, columns.withScore(_, fresh))
            fresh.foreach(score => recordCadenceChange(key, snapshot.tmdbId, Some(columns.badge(score))))
            counts.changed.incrementAndGet()
          }
        case Success(None) => ()
        case Failure(exception) =>
          counts.failed.incrementAndGet()
          logger.debug(s"${site.walkLabel}: $url lookup failed: ${exception.getMessage}")
      }
    }
  }

  // ── The audit ──────────────────────────────────────────────────────────────

  /** Audit-and-fix of the row's STORED link for `(title, year)`: the page found afresh, whether or not a link is
   *  stored, and any change written back. The backfill a tightened search owes the links an older one stored
   *  (`scripts.FilmwebUrlAudit`), and how the fixture harness settles Filmweb against the final row (`TestWiring`):
   *  its search is fuzzy, so a link found against a half-merged row would otherwise stay.
   *
   *  Outcomes:
   *    - Kept      — found the same link; a page read whole BACKFILLS its content (a row enriched before the site
   *                  became a full source carries an empty slot, which the cheap rating-only refresh never fills) and
   *                  its score when it read one, both only when they changed. A first discovery counts as Kept too.
   *    - Corrected — found a DIFFERENT link; link, score and content replaced.
   *    - Dropped   — found none the search's bar now accepts; link, score and content cleared.
   *    - NoUrl     — nothing stored and nothing found, or the row is gone. */
  def auditOneSync(title: String, year: Option[Int]): SiteRatings.AuditOutcome = {
    val key = cache.keyOf(title, year)
    cache.get(key).fold(SiteRatings.NoUrl) { row =>
      (columns.url(row), site.find(key, row)) match {
        case (Some(before), Some(found)) if urlOf(found) == before =>
          pageOf(found).foreach { page =>
            val backfilled = page.keepingScore(columns.score(row))
            if (columns.withPage(row, backfilled) != row) {
              cache.putIfPresent(key, columns.withPage(_, backfilled))
              cache.get(key).foreach(after => cache.retriggerAfterEnrichment(key, row, after))
            }
          }
          SiteRatings.Kept(before)
        case (before, Some(found)) =>
          val url = urlOf(found)
          cache.putIfPresent(key, current => columns.withUrl(pageOf(found).fold(current)(columns.withPage(current, _)), url))
          cache.get(key).foreach(after => cache.retriggerAfterEnrichment(key, row, after))
          before.fold(SiteRatings.Kept(url))(SiteRatings.Corrected(_, url))
        case (Some(before), None) =>
          cache.putIfPresent(key, columns.cleared)
          SiteRatings.Dropped(before)
        case (None, None) => SiteRatings.NoUrl
      }
    }
  }

  private def urlOf(found: Found[A]): String = found match {
    case Found.Link(url, _, _) => url
    case Found.Whole(url, _)   => url
  }

  private def pageOf(found: Found[A]): Option[Page[A]] = found match {
    case Found.Whole(_, page) => Some(page)
    case _: Found.Link[A]     => None
  }
}

object SiteRatings {
  /**
   * THE PORT of a URL-keyed rating site (Metacritic, Rotten Tomatoes, Filmweb): what only the site knows -- how to find
   * a film's page on it (`find`: its search, slug ladder and same-film guards), and how to read a page (`read`: fetch,
   * parse, and whether the page is this film's at all) -- and where its link and score live on the row (`columns`).
   *
   * Everything else, the flow every site shares, is the class's: the per-row refresh (a stored link read, a missing
   * one found first), the full-corpus walk and the operator's re-check, what a denied page drops, what a moved score
   * reports to the adaptive cadence, the summaries. A new rating site is one more implementation of this port, plus its
   * `RatingSource` entry (`services.tasks.RatingSources`).
   *
   * Neither `find` nor `read` writes anything: the flow does, through the cache's per-title lock. A failed read (the site
   * down, a soft-block) THROWS out of either, and the flow counts the row as failed (tools.ReadOutcome); a site that has
   * no page for the film answers `None`.
   */
  trait Site[A] {

    /** The site's name in the per-step enrichment logs (`"Metacritic"`, `"RT"`, `"Filmweb"`). */
    def name: String

    /** The full-corpus walk's log prefix, which also names its pool (`"RT refresh"`). */
    def walkLabel: String

    /** Where the site's link, score and content slot live on the row, and the badge a score shows as. */
    def columns: Columns[A]

    /** How many rows the walk asks the site about at once (tools.BoundedParallel); lower for a site that soft-blocks. */
    def walkConcurrency: Int = 8

    /** The film's page on the site, found afresh: its link (with what the search already read of it), or `None` when
     *  the site has no page the guards accept. Asked for a row with no link, and by the walk for every row with a TMDB id
     *  (a better match replaces a stored link). */
    def find(key: CacheKey, row: MovieRecord): Option[Found[A]]

    /** The page at the row's stored `url`, read: its score, the whole page, or `Denied` when it is another film's. */
    def read(key: CacheKey, row: MovieRecord, url: String): PageRead[A]

    /** The walk's read of a stored `url`: [[read]] unless the site reads more cheaply there. */
    def walkRead(key: CacheKey, row: MovieRecord, url: String): PageRead[A] = read(key, row, url)

    /** Told after a page of the site was taken for the row as it stood before (`row`): found and stored, or read and
     *  not denied. Filmweb asks the IMDb id resolver for a film TMDB found but IMDb did not. */
    def pageTaken(key: CacheKey, row: MovieRecord): Unit = ()
  }


  /** What a search found. */
  enum Found[+A] {
    /** A link alone, and the score the probe that found it already read off it (`None`: not read, the flow reads it).
     *  `how` qualifies the discovery in the log (" without TMDB"). */
    case Link(url: String, score: Option[A] = None, how: String = "")
    /** A page read whole by the search: stored with its score and content in one write. */
    case Whole(url: String, page: Page[A])
  }

  /** A page read whole: its score (`None`: it carries none, and the row's is cleared) and its content slot (`None`:
   *  no content, and the row's slot is dropped). */
  final case class Page[+A](score: Option[A], slot: Option[SourceData]) {
    /** This page, its score kept from `stored` when it read none: an audit backfills content, never clears a score. */
    def keepingScore[B >: A](stored: Option[B]): Page[B] = Page(score.orElse(stored), slot)
  }

  /** What reading a stored link said. */
  enum PageRead[+A] {
    /** The page is another film's (`why`, for the log): the link, its score and the site's slot are dropped. */
    case Denied(why: String)
    /** The page's score, `None` when it carries none (the row's is kept). */
    case Scored(score: Option[A])
    /** The page read whole (a row holding no content slot for its link). */
    case Read(page: Page[A])
  }

  /** The row's columns a site owns.
   *
   *  @param noun     what a score is called in the per-row log ("metascore").
   *  @param plural   what the walk's summary counts ("score(s)").
   *  @param url      the stored link.
   *  @param score    the stored score.
   *  @param withUrl  a link written.
   *  @param withScore a score written.
   *  @param withPage a page read whole written: its score and its content slot (none: the slot dropped).
   *  @param cleared  link, score and slot dropped: a denied page's.
   *  @param badge    the displayed value a score becomes -- what the cadence is told. */
  final case class Columns[A](
    noun:      String,
    plural:    String,
    url:       MovieRecord => Option[String],
    score:     MovieRecord => Option[A],
    withUrl:   (MovieRecord, String) => MovieRecord,
    withScore: (MovieRecord, Option[A]) => MovieRecord,
    withPage:  (MovieRecord, Page[A]) => MovieRecord,
    cleared:   MovieRecord => MovieRecord,
    badge:     A => String)

  /** Outcome of a single `auditOneSync` call; `scripts.FilmwebUrlAudit` counts them for its summary. */
  sealed trait AuditOutcome
  /** The stored link re-resolves to the same page (good). */
  final case class Kept(url: String) extends AuditOutcome
  /** The stored link re-resolves to a DIFFERENT page — fixed in place. */
  final case class Corrected(before: String, after: String) extends AuditOutcome
  /** The stored link no longer passes the search's bar — cleared. */
  final case class Dropped(before: String) extends AuditOutcome
  /** No link stored and none found. */
  case object NoUrl extends AuditOutcome

  /** What a walk counted, row by row and from several threads. */
  private final class WalkCounts {
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)
  }
}
