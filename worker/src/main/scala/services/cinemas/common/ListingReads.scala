package services.cinemas.common

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/**
 * Whether one scrape read its whole listing — decided from the outcome of every page read,
 * never declared by a client.
 *
 * A listing spread over pages (days, months, events, film detail pages that carry the
 * showtimes) used to tolerate a failed page: the other pages still carry most of the
 * programme. But the listing then went to the cache as COMPLETE, and the cache's prune reads
 * a film's absence from a complete listing as "it stopped screening" — so every film that
 * lived only on the page that failed was deleted on an upstream blip. Now every page a shared
 * walk reads reports here: a page that failed makes the listing incomplete, the cache keeps
 * that page's films (`ScrapeHealth.breadth`), and only a scrape whose every page answered can
 * prune. A scrape whose EVERY page failed still throws ([[ListingPages.requireAnyReached]]).
 *
 * The ledger is scoped to one scrape attempt by [[during]] — opened by
 * `CinemaScraper.fetchWithSource`, per attempt by `RetryingCinemaScraper` and
 * `SourceFallbackScraper`, and by the chunk pipeline per plan and per chunk — and carried
 * onto a worker thread with [[carry]] (`AdaptiveTimeoutScraper`). The walks
 * ([[ListingPages]], [[ScrapeHorizon]], [[DayPickerProgramme]]) record into whichever scope is
 * open; outside any scope (a unit test driving a parser) a record is a no-op. There is no way
 * to mark a listing complete: the scope starts complete and can only lose that.
 *
 * How to use, writing a client: walk a paged listing through [[ListingPages]], [[ScrapeHorizon]] or
 * [[DayPickerProgramme]] and it is recorded for you. A page read by hand that fails and is skipped
 * must say so — `ListingReads.pageFailed(e)` (or `ListingPages.reportFailed(attempts)` over a batch
 * of `Try`s) — never `Try(page).toOption` / `getOrElse(Nil)`, which turns the lost page into films
 * that stopped screening. Hand work to another thread with `ListingReads.carry(body)`. The scraper
 * wrappers open the scope; a client never calls [[during]] or [[attempt]] itself.
 */
final class ListingReads private () {
  private val failures = new ConcurrentLinkedQueue[Throwable]()

  /** Every page read so far answered. */
  def complete: Boolean = failures.isEmpty

  /** The page failures, in the order they were recorded — for a log line. */
  def failed: Seq[Throwable] = failures.asScala.toSeq

  private def record(failure: Throwable): Unit = { failures.add(failure); () }
  private def absorb(other: ListingReads): Unit = other.failed.foreach(record)
}

object ListingReads extends play.api.Logging {

  private val open = new ThreadLocal[ListingReads]

  /** Run `body` (one scrape attempt) in a fresh ledger, returning what it read alongside. */
  def during[A](body: => A): (A, ListingReads) = {
    val reads    = new ListingReads
    val enclosing = open.get
    open.set(reads)
    try (body, reads)
    finally open.set(enclosing)
  }

  /** [[during]], for an attempt that may be retried: its page failures reach the enclosing
   *  scope only if it succeeds, so a failed attempt's half-read pages don't taint the retry. */
  def attempt[A](body: => A): A = {
    val (result, reads) = during(body)
    Option(open.get).foreach(_.absorb(reads))
    result
  }

  /** A page of the listing could not be read; the scrape carries on without it. A page the
   *  upstream answered is gone (404/410, `ReadOutcome.isAbsent`) is an answer — nothing is
   *  listed there — and leaves the listing complete. */
  def pageFailed(failure: Throwable): Unit = Option(open.get).foreach { reads =>
    if (tools.ReadOutcome.isAbsent(failure)) logger.debug(s"a listing page is gone upstream: ${failure.getMessage}")
    else {
      logger.info(s"a listing page failed, so the listing is incomplete: ${failure.getClass.getSimpleName}: ${failure.getMessage}")
      reads.record(failure)
    }
  }

  /** `body` as a thunk that, run on another thread, records into the scope open HERE. */
  def carry[A](body: => A): () => A = {
    val here = open.get
    () => {
      val there = open.get
      open.set(here)
      try body finally open.set(there)
    }
  }
}
