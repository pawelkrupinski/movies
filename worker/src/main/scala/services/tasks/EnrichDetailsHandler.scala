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
  /** The row's key AS STORED (`normalized|year`). A cut-over country holds several films of one
   *  title and year apart by key alone (`lalka|` and `lalka~1164|`); re-deriving the key from the
   *  title and year addresses whichever owns the bare one, and lands the page on the wrong film. */
  val RowKey    = "row"

  def dedupKey(group: String, key: CacheKey): String =
    s"detail|$group|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(enricher: DetailEnricher, key: CacheKey, ref: String): Map[String, String] =
    Map(
      GroupKey -> enricher.detailGroup,
      RefKey   -> ref,
      TitleKey -> key.cleanTitle,
      YearKey  -> key.year.map(_.toString).getOrElse(""),
      RowKey   -> services.movies.StoredMovieRecord.keyFor(key)
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
  // The language the country's corpus names countries in (`CountryNames.canonical`): a page's own
  // spelling ("Niderlandy") lands as the one every listing-built slot holds ("Holandia").
  enrichmentLanguage: java.util.Locale,
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

  /** A detail page's fields as a cinema slot holds them (`FilmDetail.landed`). */
  private def landed(detail: FilmDetail): FilmDetail = detail.landed(enrichmentLanguage)
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
        val pageKey = services.venuepages.VenuePageKey(enricher.detailGroup, ref)
        // What venue_pages held of the page before this read replaces it: the last read and what the rows took of it.
        val before  = pages.stored(pageKey)
        val read    = reader.read(enricher, ref)
        services.venuepages.DetailUptime.record(uptime, enricher, label, read.outcome)
        read.outcome match {
          case DetailFetchOutcome.Failed =>
            Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case _ if read.unfiled =>
            // Read and not filed: neither stamped nor announced, so asked again — never landed on the row as done
            // while the identity model, which reads only venue_pages, decides the film without it.
            Reschedule(Some(s"venue_pages did not take ${enricher.detailGroup}|$ref"))
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
            // (this handler's own due gate, and DetailReaper's).
            // It costs nothing in recovery either: both detail caches already pin a
            // durable failure for 12h, so a retry inside this 6h window was being
            // answered from cache anyway. If the film comes back under a NEW url the
            // listing scrape rewrites the slot's `filmUrl`, and the next window
            // fetches it.
            freshness.markFresh(key, FreshnessKind.DetailEnrich, clock.instant())
            Done
          case DetailFetchOutcome.Fetched(read) =>
            val detail = landed(read)
            val title  = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year   = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            // The row the task was asked for, by its stored key; a task queued before the key rode
            // along (one deploy's worth) falls back to the title and year.
            val rowKey = task.payload.get(EnrichDetailsTasks.RowKey).fold(cache.keyOf(title, year))(CacheKey.stored(title, _))
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
              // A market-wide page (`pagesSharedAcrossVenues`): the enricher held is any one of the group's,
              // so the page lands on every slot of the row that names it, whichever venue's — and creates none.
              else if (enricher.pagesSharedAcrossVenues)
                cache.get(rowKey).toList.flatMap(_.data.collect {
                  case (s, sd) if Source.cinemaOf(s).isDefined && DetailEnricher.nativeRefOf(sd).contains(ref) => s
                })
              else {
                val derived     = CinemaShowing.keyFor(enricher.cinema, title, cache.normalizer)
                val venueSlots  = cache.get(rowKey).toList.flatMap(_.data.filter { case (s, _) => Source.cinemaOf(s).contains(enricher.cinema) })
                val cinemaSlots = venueSlots.collect { case (s, sd) if DetailEnricher.nativeRefOf(sd).forall(_ == ref) => s }
                if (venueSlots.isEmpty) Seq(derived)             // chain / not-yet-scraped → create it
                else if (cinemaSlots.contains(derived)) Seq(derived)  // scrape wrote the base title too
                else cinemaSlots                                  // decorated edition(s) → merge into the real slot(s)
              }
            // What did this page say when the rows last took it (`StoredVenuePage.landed`)? The fields
            // it now states DIFFERENTLY are authoritative
            // (`FilmDetail.refreshInto`): a venue that reuses a URL for a different film —
            // Kino Pionier's `/event/lalka`, Has's 1968 picture then the 2026 one — otherwise
            // keeps the first film's year and runtime for ever, because the fill-only merge
            // has nothing to fill. Everything else only fills gaps (`mergeInto`), so the
            // listing keeps out-ranking the page wherever both speak — on a first landing (a
            // page never read, or read only as gone: a 404 is not a read), and on every
            // re-read that says what the rows took before. Not the page as last READ: the reader
            // stores that before this merge, and a merge that missed its row (re-keyed between
            // enqueue and pickup) left the changed read stored and never landed — every later read
            // then matched it, and the row kept the old year and runtime for ever. Not a freshness marker: the
            // reader stamps the page read before this handler merges, so a marker read here
            // made every read — the first included — authoritative, and a re-read every
            // refresh window rewrote the listing's own countries and genres with the page's.
            // Both reads as a slot may hold them (`landed`): compared raw, a page that spells a
            // country or lists its genres the way it always has would differ from what it landed as.
            //
            // A page last read before `landed` was kept has no record of what the rows took. Its last read is what every
            // read then landed — except on a row whose merge had already missed, which holds an older read. Its year and
            // runtime (what such a stuck row shows: the row is keyed by the year) are therefore landed authoritatively
            // ONCE, and the page is then landed, so every later read is measured against what the rows took. Only those
            // two: making the whole read authoritative would rewrite the listing's own countries and genres (above). The
            // cost is that a listing whose own year or runtime differs from its page's is overruled once, on a
            // field the page states.
            val changed = before.landed match {
              case Some(took) => detail.changedSince(landed(took))
              case None       => before.page.map(_.outcome).collect { case VenuePage.Read(last) => last }.fold(FilmDetail())(last =>
                detail.changedSince(landed(last)).copy(releaseYear = detail.releaseYear, runtimeMinutes = detail.runtimeMinutes))
            }
            // Merge into the target slot(s), creating one if absent: a chain's network
            // source has no slot from a listing scrape, so it must be added here;
            // a 1:1 cinema's slot already exists, so this preserves its showtimes.
            // Only those slots are written, each from what it held (`putSlotsIfPresent`): a no-op on a row that was
            // re-keyed between enqueue and pickup — and then the page is not landed, so its next read still overrules.
            val onRow = cache.putSlotsIfPresent(rowKey, targets)((_, held) =>
              detail.mergeInto(changed.refreshInto(held.getOrElse(SourceData()), screeningTokens), screeningTokens))
            // Landed against what the rows took alone: compared with the last read, a page read before `landed` was kept
            // and unchanged since was never landed, and paid this check's find on every task for ever.
            if (onRow && !before.landed.contains(read)) pages.land(pageKey, read)
            freshness.markFresh(key, FreshnessKind.DetailEnrich, clock.instant())
            Done
        }
    }
  }
}
