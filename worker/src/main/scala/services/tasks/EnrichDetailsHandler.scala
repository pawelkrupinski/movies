package services.tasks

import services.events.EventBus
import models.{CinemaShowing, Source, SourceData}
import services.freshness.{FreshnessKind, FreshnessStore}
import play.api.Logging
import services.movies.{CacheKey, MovieCache}
import services.UptimeMonitor
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome, FilmDetail}
import services.venuepages.VenuePage

import java.time.{Clock, Instant}

/** Builds the dedup key, freshness key, and payload for an `EnrichDetails` task.
 *  The dedup key encodes `(detailGroup, film)` so "enrich film F for group G"
 *  can exist at most once — the queue's unique index rejects a second. */
object EnrichDetailsTasks {
  val GroupKey  = "group"
  val RefKey    = "ref"
  val TitleKey  = "title"
  val YearKey   = "year"

  def dedupKey(group: String, key: CacheKey): String =
    s"detail|$group|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(enricher: DetailEnricher, key: CacheKey, ref: String): Map[String, String] =
    Map(
      GroupKey -> enricher.detailGroup,
      RefKey   -> ref,
      TitleKey -> key.cleanTitle,
      YearKey  -> key.year.map(_.toString).getOrElse("")
    )

  /** The PAGE's own stamps: `page` of `group` was read into venue_pages, or found gone. Keyed by the
   *  page, never by the film row it was on: a page's answer is the page's, wherever its listing goes. */
  def pageRead(group: String, page: String): String = s"${pageDedupKey(group, page)}|read"
  /** The dedup (and due) key of a detail task asked per PAGE (`DetailPages.PerPage`). */
  def pageDedupKey(group: String, page: String): String = s"detail-page|$group|$page"
  def pageGone(group: String, page: String): String = s"detail-page|$group|$page|gone"
  /** `page` of `group` was tried by a `ReadVenuePage` task, whatever it said: all a display-only venue's
   *  waiting listing needs (`VenuePageWait`). */
  def pageAttempted(group: String, page: String): String = s"detail-page|$group|$page|attempted"

  /** Enqueue a detail task only when it's DUE under `dueWindow` — the phase-spread
   *  gate the periodic [[DetailReaper]] enqueues on and [[EnrichDetailsHandler]]
   *  re-gates on (shared instance). The phase offset, hashed from `dk`, scatters a
   *  synchronized cohort (a re-key wave that orphans a whole batch's stamps at
   *  once) across the period instead of dumping it in one tick. Returns true iff
   *  newly enqueued. */
  def enqueueIfDue(queue: TaskQueue, freshness: FreshnessStore, dueWindow: DueWindow,
                   enricher: DetailEnricher, key: CacheKey, ref: String, now: Instant): Boolean =
    enqueueIfDueAs(queue, freshness, dueWindow, enricher, key, ref, dedupKey(enricher.detailGroup, key), now)

  /** [[enqueueIfDue]] under the dedup key `dk` the task is due, deduplicated and stamped by. */
  def enqueueIfDueAs(queue: TaskQueue, freshness: FreshnessStore, dueWindow: DueWindow,
                     enricher: DetailEnricher, key: CacheKey, ref: String, dk: String, now: Instant): Boolean = {
    dueWindow.isDue(dk, freshness.lastFetchedAt(dk), now) &&
      queue.enqueue(TaskType.EnrichDetails, dk, payload(enricher, key, ref)) == EnqueueResult.Added
  }
}

/**
 * Handles an `EnrichDetails` task: fetch one film's detail (once per
 * `(detailGroup, film)` per 6h) and merge it into the enricher's `detailTarget`
 * `SourceData` slot, preserving anything already in that slot (e.g. a cinema
 * slot's showtimes). A 1:1 cinema targets its own slot; a chain targets a
 * shared network source, so one fetch serves every venue.
 *
 * Re-gated at pickup by the SAME [[DueWindow]] the [[DetailReaper]] enqueues on,
 * not a separate rolling TTL: when it didn't, the reaper enqueued a row on its
 * phase boundary while the handler skipped it as still-fresh, so the same row
 * churned the queue every tick without ever refreshing (see [[DueWindow]]). A
 * TRANSIENTLY failed fetch is reported `Done` without marking fresh, so the
 * cinema's next scrape re-enqueues it rather than the worker spinning. A
 * DURABLY gone page (404/410) is stamped instead — there, leaving it stale is
 * what makes the worker spin, because an unstamped key is due on every reaper
 * tick forever. See [[services.cinemas.common.DetailFetchOutcome]].
 */
class EnrichDetailsHandler(
  enrichersByGroup: Map[String, DetailEnricher],
  cache:            MovieCache,
  freshness:        FreshnessStore,
  uptime:           UptimeMonitor,
  bus:              EventBus,
  // SAME instance the DetailReaper enqueues on — see the class doc / [[DueWindow]].
  dueWindow:        DueWindow,
  clock:            Clock,
  // The country's badge vocabulary, for the detail-page `format` merged below.
  // Wired at the composition root beside the cache's own copy; defaulted like the
  // cache's, for the same reason — a wrong value mis-SPELLS a badge rather than
  // mis-keying a row.
  screeningTokens:  services.movies.ScreeningTokens = services.movies.ScreeningTokens.forDefaultCountry(),
  // `venue_pages`, where every page read is written once (`VenuePageReader`): wired to the country's
  // collection at the composition root; in memory where a test does not look at it.
  pages:            services.venuepages.VenuePageStore = new services.venuepages.InMemoryVenuePageStore
) extends TaskHandler with Logging {

  private val reader = new services.venuepages.VenuePageReader(pages, freshness, event => bus.publish(event), clock)

  private val normalizer: services.movies.TitleNormalizer = cache.normalizer
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.EnrichDetails

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!dueWindow.isDue(key, freshness.lastFetchedAt(key), clock.instant())) return Skipped

    enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case None =>
        logger.warn(s"No detail enricher for task $key; dropping.")
        Done
      case Some(enricher) =>
        val label = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, key)
        val ref = task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")
        // The page is read into venue_pages, stamped and announced there, and recorded on /uptime;
        // this handler lands it on the row.
        val prior   = pages.get(services.venuepages.VenuePageKey(enricher.detailGroup, ref)).map(_.outcome)
        val outcome = reader.read(enricher, ref)
        services.venuepages.DetailUptime.record(uptime, enricher, label, outcome)
        outcome match {
          case DetailFetchOutcome.Failed =>
            Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case DetailFetchOutcome.Gone(_) =>
            // The page is gone (404/410), not failing. Leaving it stale is a
            // livelock: with no stamp `DueWindow.isDue` is unconditionally true, so
            // DetailReaper re-enqueues this film every tick — once a minute, forever
            // — and each pass records another /uptime failure. Two such films held
            // the Cinema City chain row at ~90% failures and, since a bucket keeps
            // only 10 error strings, hid every other Cinema City enrichment failure
            // behind them.
            //
            // So stamp it. This is NOT claiming a detail landed — nothing reads
            // DetailEnrich freshness as "we have data", only as "we asked recently"
            // (this handler's own due gate, and DetailReaper's `detailOutstanding`).
            // It costs nothing in recovery either: both detail caches already pin a
            // durable failure for 12h, so a retry inside this 6h window was being
            // answered from cache anyway. If the film comes back under a NEW url the
            // listing scrape rewrites the slot's `filmUrl`, and the next window
            // fetches it.
            //
            // Releasing `detailOutstanding` also unsticks the worse case this hid: a
            // row held `detailPending` on a detail that 404s from the start never
            // cleared, so it stayed out of the read model — invisible on the site —
            // permanently. `reapStuckPending` can now let it through.
            freshness.markFresh(key, FreshnessKind.DetailEnrich, clock.instant())
            Done
          case DetailFetchOutcome.Fetched(detail) =>
            val title  = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year   = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            val rowKey = cache.keyOf(title, year)
            // For a 1:1 venue the listing slot is keyed per shown title
            // (`CinemaShowing`), so target THAT slot — a bare-cinema target would
            // create a separate empty slot and the detail would never merge into the
            // film's showtimes. A chain redirects detail to its shared network source
            // (not per-title), which is left as-is.
            //
            // `title` is the film's BASE (row) title, but a DECORATED edition
            // ("Plenerowe Pałacowe: X", folded onto the base row) has its listing slot
            // keyed by the decorated shown title — so `keyFor(cinema, base)` derives a
            // slot the scrape never wrote, fabricating a phantom bare slot the next
            // scrape prunes (per-tick churn + the detail never reaches the real slot).
            // One cinema can run the same film as SEVERAL programme editions ("Kino bez
            // barier: X", "Pora dla seniora: X", "X przedpremierowo") — each its own
            // card/slot; the detail is the SAME film's, so land it on EVERY one of this
            // cinema's existing slots, never fabricating a bare phantom. Only when NONE
            // exists (a chain's network source, or a not-yet-scraped row) create the
            // derived key. A chain redirects to its shared network source, left as-is.
            //
            // "The SAME film's" detail holds only for an edition that has no page of its
            // own. Kinoteka lists "Rozważna i romantyczna | Kino dla rodzica" and "… |
            // Kino przy herbatce" on one row, each with its OWN page (112 vs 131 minutes,
            // a different synopsis and billing), and this task reads just one of them —
            // the representative slot's. Landing it on the sibling made the re-read, which
            // is authoritative, overwrite that edition with a page that is not its own, so
            // the row changed between two days of identical listings. A slot pointing at a
            // DIFFERENT page is that page's to fill, not this one's.
            val targets: Seq[Source] =
              if (enricher.detailTarget != enricher.cinema) Seq(enricher.detailTarget)
              else {
                val derived     = CinemaShowing.keyFor(enricher.cinema, title, cache.normalizer)
                val venueSlots  = cache.get(rowKey).toList.flatMap(_.data.filter { case (s, _) => Source.cinemaOf(s).contains(enricher.cinema) })
                val cinemaSlots = venueSlots.collect { case (s, sd) if DetailEnricher.nativeRefOf(sd).forall(_ == ref) => s }
                if (venueSlots.isEmpty) Seq(derived)             // chain / not-yet-scraped → create it
                else if (cinemaSlots.contains(derived)) Seq(derived)  // scrape wrote the base title too
                else cinemaSlots                                  // decorated edition(s) → merge into the real slot(s)
              }
            // What did this page say when it was last read (`venue_pages`, as it stood before
            // this read)? The fields it now states DIFFERENTLY are authoritative
            // (`FilmDetail.refreshInto`): a venue that reuses a URL for a different film —
            // Kino Pionier's `/event/lalka`, Has's 1968 picture then the 2026 one — otherwise
            // keeps the first film's year and runtime for ever, because the fill-only merge
            // has nothing to fill. Everything else only fills gaps (`mergeInto`), so the
            // listing keeps out-ranking the page wherever both speak — on a first read (a
            // page never read, or read only as gone: a 404 is not a read), and on every
            // re-read that says what the page said before. Not a freshness marker: the
            // reader stamps the page read before this handler merges, so a marker read here
            // made every read — the first included — authoritative, and a re-read every
            // refresh window rewrote the listing's own countries and genres with the page's.
            val changed = prior match {
              case Some(VenuePage.Read(before)) => detail.changedSince(before)
              case _                            => FilmDetail()
            }
            // Merge into the target slot(s), creating one if absent: a chain's network
            // source has no slot from a listing scrape, so it must be added here;
            // a 1:1 cinema's slot already exists, so this preserves its showtimes.
            // Clearing `detailPending` releases the row to the read model now that its
            // detail (director/originalTitle/year) is in. `putIfPresent` is a no-op on a
            // row that was re-keyed between enqueue and pickup.
            cache.putIfPresent(rowKey, current =>
              current.copy(
                data          = targets.foldLeft(current.data)((d, tgt) =>
                                  d + (tgt -> detail.mergeInto(changed.refreshInto(d.getOrElse(tgt, SourceData()), screeningTokens), screeningTokens))),
                detailPending = false))
            freshness.markFresh(key, FreshnessKind.DetailEnrich, clock.instant())
            Done
        }
    }
  }
}
