package services.cinemas.common

import models.CinemaMovie
import services.UptimeMonitor
import services.fallback.{FallbackEvent, FallbackState, FallbackStore}
import services.scrapes.SeparateRuns

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * Decorator that serves showtimes from a SECONDARY source when the cinema's own
 * primary scraper has been failing continuously for [[fallbackAfter]] (default
 * 6h; a venue on a long scrape cadence counts separate failed runs instead, see
 * [[FallbackAfter]]), and records the outcome — including the "served via <fallback>" flag —
 * against the `UptimeMonitor`. It REPLACES `UptimeRecordingScraper` for venues
 * that have a fallback; its `primary` is the retry-only `RetryingCinemaScraper`.
 *
 * Source-neutral by construction: `fallback` supplies the secondary scraper and
 * `fallbackName` names it ("Filmweb" for a Polish own-site venue, "Flicks" for a
 * UK chain venue whose aggregator backstop is flicks.co.uk). The name flows into
 * the persisted state, the /uptime label and the Telegram alert so each reads as
 * "via Filmweb" / "via Flicks" correctly.
 *
 * The 6h grace is the whole point: the fallback feed is sparser than a cinema's
 * own site, so switching to it on the FIRST failed scrape made a block of films
 * flicker out and back in during a site's brief morning outage. Instead we ride
 * out short outages on the corpus's last-good data — a throw skips the tick and
 * an empty scrape is a no-op (`MovieCache` bails on empty rather than pruning),
 * so both keep the last successful showtimes — and only fall back once the primary
 * has been down for `fallbackAfter` without interruption.
 *
 * Per tick (`service` = `cinema.displayName`):
 *
 *   - Primary returns screenings  → record success; end any failing spell (if we
 *                                    were on fallback, mark RECOVERED and release
 *                                    it; if merely in the grace window, just clear
 *                                    the clock).
 *   - Primary threw, or returned empty while the fallback has data, but the failing
 *                                    run is still younger than `fallbackAfter` →
 *                                    GRACE: record the primary's real outcome (a
 *                                    throw re-raises so the tick skips the cinema;
 *                                    an empty is returned as-is, a no-op downstream),
 *                                    start/keep the `failingSince` clock, DON'T
 *                                    serve the fallback.
 *   - The failing run has reached `fallbackAfter` AND the fallback has data → serve
 *                                    it, record a fallback-success (green + "via
 *                                    <fallback>"), ENTER fallback (or PROBE_FAILED
 *                                    if already on it) and back off the next primary
 *                                    re-probe.
 *   - Primary THREW, the run has reached `fallbackAfter`, and the fallback has
 *                                    nothing either → UNCOVERED: nobody is serving
 *                                    this venue. Same state as a grace failure, but
 *                                    it pages once per spell, because this is the one
 *                                    outcome nothing else in the system reports.
 *   - Primary empty AND the fallback also empty/unavailable → the genuine-empty case
 *                                    (e.g. a dark late-night repertoire): record the
 *                                    primary's real outcome and do NOT start a
 *                                    failing spell — only a real failure (a throw, or
 *                                    an empty the fallback could actually cover)
 *                                    counts, so an empty night never trips.
 *
 * Recovery: while on fallback we skip the (often slow/timing-out) primary until
 * `nextPrimaryProbeAt`, serving the fallback directly; at that point we re-probe the
 * primary, recovering immediately if it's back, else extending the backoff
 * (exponential, capped). State + history persist via `FallbackStore` for
 * the /uptime/fallback page; `onEvent` fires on ENTER / PROBE_FAILED / RECOVERED /
 * UNCOVERED for alerting.
 *
 * `listingIsComplete` stays the PRIMARY's answer (via [[DelegatingCinemaScraper]])
 * even on a tick served from the fallback, where what we actually returned is the
 * fallback's always-whole listing. The two errors are not symmetric: reporting a short
 * listing as whole lets the cache prune films it never looked at, while reporting a
 * whole one as short only defers a prune by one tick. Delegating unconditionally takes
 * the recoverable side and needs no per-tick state, which a `fetch()`-then-
 * `listingIsComplete` read would otherwise have to carry between two calls.
 *
 * Which of the two SERVED is reported per tick by [[fetchWithSource]], returned with the
 * listing so no state spans two calls. A fallback tick is NOT a change of source — the
 * venue's source is still its primary, merely down — so the cache lands it additively:
 * no guard judges the thin board against the primary's rows, and no prune retires the
 * primary's films it does not list (`ScrapeLanding.recordCinemaScrape`).
 */
class SourceFallbackScraper(
  primary:         CinemaScraper,
  fallback:        () => Option[CinemaScraper],
  fallbackName:    String,
  fallbackRef:     () => Option[String],
  monitor:         UptimeMonitor,
  store:           FallbackStore,
  now:             () => Instant = () => Instant.now(),
  baseBackoff:     FiniteDuration = SourceFallbackScraper.DefaultBaseBackoff,
  maxBackoff:      FiniteDuration = SourceFallbackScraper.DefaultMaxBackoff,
  fallbackAfter:   FallbackAfter = FallbackAfter.FailingFor(SourceFallbackScraper.DefaultFallbackAfter),
  onEvent:         (FallbackState, FallbackEvent) => Unit = (_, _) => ()
) extends DelegatingCinemaScraper(primary) {
  import SourceFallbackScraper._

  private val service = cinema.displayName

  def fetch(): Seq[CinemaMovie] = fetchWithSource().movies

  /** The tick's listing, and whether the fallback served it (see the class doc). */
  override def fetchWithSource(): CinemaScraper.Scraped = {
    val previous      = store.get(service).map(ownSpell)
    val active        = previous.exists(_.active)
    val nowI          = now()
    val withinBackoff = active && previous.flatMap(_.nextPrimaryProbeAt).exists(nowI.isBefore)

    if (withinBackoff) {
      // On fallback, not yet time to re-probe → skip the broken primary entirely.
      // The fallback is the ONLY source this tick, so its failure is the scrape's: it
      // fails (red) rather than reading as an empty listing. Only a fallback that
      // answered empty is an empty tick.
      val (fwResult, fwMs) = fetchFallback()
      previous.foreach(p => store.put(p.copy(updatedAt = nowI)))
      fwResult match {
        case scala.util.Success(fwMovies) if showtimeCount(fwMovies) > 0 =>
          monitor.recordFallbackSuccess(service, fwMs, thin(fwMovies)); fallbackServed(fwMovies)
        case scala.util.Success(_) =>
          monitor.recordEmpty(service, fwMs); primaryServed(Seq.empty)
        case scala.util.Failure(t) =>
          monitor.recordFailure(service, UptimeRecordingScraper.errorLabel(t)); throw t
      }
    } else {
      runPrimary() match {
        case PrimaryOutcome.Healthy(movies, ms) =>
          endFailingSpell(previous, nowI)
          monitor.recordSuccess(service, ms, thin(movies))
          primaryServed(movies)

        case PrimaryOutcome.Threw(t) =>
          // A throw is unambiguously a failure — no need to consult the fallback to
          // classify it; only reach for it once the grace window has elapsed.
          onFailure(previous, nowI, active, UptimeRecordingScraper.errorLabel(t)) {
            monitor.recordFailure(service, UptimeRecordingScraper.errorLabel(t)); throw t
          }

        case PrimaryOutcome.Empty(movies, ms) =>
          // Empty only counts as a failure if the fallback can actually cover it —
          // otherwise it's a genuine empty repertoire and must never trip.
          val (fwMovies, fwMs, fwServed) = tryFallback()
          if (!fwServed) {
            if (active) markPrimaryDown(previous, nowI, EmptyReason)  // already on fallback: still a failed re-probe
            monitor.recordEmpty(service, ms); primaryServed(movies)
          } else if (active) {
            markPrimaryDown(previous, nowI, EmptyReason); monitor.recordFallbackSuccess(service, fwMs, thin(fwMovies)); fallbackServed(fwMovies)
          } else if (graceElapsed(previous, nowI)) {
            enterFallback(previous, nowI, EmptyReason); monitor.recordFallbackSuccess(service, fwMs, thin(fwMovies)); fallbackServed(fwMovies)
          } else {
            recordGraceFailure(previous, nowI, EmptyReason); monitor.recordEmpty(service, ms); primaryServed(movies)
          }
      }
    }
  }

  /** Failure handling for the "no fallback needed to classify" path (a throw). If
   *  we're already on fallback it's a failed re-probe; otherwise it's a grace
   *  failure until [[fallbackAfter]] elapses, at which point — if the fallback has data
   *  — we enter fallback and serve it. `keepPrimaryOutcome` is evaluated (re-raising
   *  the throw) whenever the fallback can't step in. */
  private def onFailure(previous: Option[FallbackState], nowI: Instant, active: Boolean, reason: String)(
    keepPrimaryOutcome: => CinemaScraper.Scraped
  ): CinemaScraper.Scraped =
    if (active) {
      val (fwMovies, fwMs, fwServed) = tryFallback()
      markPrimaryDown(previous, nowI, reason)
      if (fwServed) { monitor.recordFallbackSuccess(service, fwMs, thin(fwMovies)); fallbackServed(fwMovies) } else keepPrimaryOutcome
    } else if (graceElapsed(previous, nowI)) {
      val (fwMovies, fwMs, fwServed) = tryFallback()
      if (fwServed) { enterFallback(previous, nowI, reason); monitor.recordFallbackSuccess(service, fwMs, thin(fwMovies)); fallbackServed(fwMovies) }
      else { recordUncovered(previous, nowI, reason); keepPrimaryOutcome }
    } else {
      recordGraceFailure(previous, nowI, reason); keepPrimaryOutcome
    }

  /** Whether what this tick served held no near-term screening ([[NearTermProgramme]]),
   *  judged on the same injected clock as the fallback's own state machine. */
  private def thin(movies: Seq[CinemaMovie]): Boolean =
    NearTermProgramme.isThin(cinema, movies, Clock.fixed(now(), ZoneOffset.UTC))

  private def primaryServed(movies: Seq[CinemaMovie]): CinemaScraper.Scraped  = CinemaScraper.Scraped(movies, viaFallback = false)
  private def fallbackServed(movies: Seq[CinemaMovie]): CinemaScraper.Scraped = CinemaScraper.Scraped(movies, viaFallback = true)

  /** The stored state as THIS fallback's: a state another fallback wrote describes
   *  that feed's spell — German venues carried Filmweb-wrapper state before
   *  kinoprogramm.com became their fallback — so its clock, run count and pages
   *  are not ours, and reusing them would, say, treat its UNCOVERED page as ours
   *  and never page for a venue this feed cannot cover either. The spell starts
   *  afresh; the history stays, as the venue's record. */
  private def ownSpell(state: FallbackState): FallbackState =
    if (state.fallbackSource == fallbackName) state
    else state.copy(
      active = false, alerted = false, failingSince = None, failedRuns = 0, since = None,
      consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None)

  /** Has the current failing spell, counting THIS failed run, reached [[fallbackAfter]]?
   *  `failingSince` and `failedRuns` are carried from the persisted state (or start
   *  now), so both survive worker restarts. */
  private def graceElapsed(previous: Option[FallbackState], nowI: Instant): Boolean =
    fallbackAfter.reached(
      failingSince = previous.flatMap(_.failingSince).getOrElse(nowI),
      failedRuns   = failedRunsIncluding(previous.getOrElse(initialState), nowI),
      now          = nowI)

  /** The current spell's separate failed runs, counting a failure at `nowI`: a new
   *  spell is one run, and a failure within [[SeparateRuns.MinGap]] of the last
   *  probe is a retry of that run, not another. */
  private def failedRunsIncluding(base: FallbackState, nowI: Instant): Int =
    if (base.failingSince.isEmpty) 1
    else if (base.lastPrimaryProbeAt.forall(SeparateRuns.isNewRun(_, nowI))) base.failedRuns + 1
    else math.max(base.failedRuns, 1)

  private def runPrimary(): PrimaryOutcome = {
    val t0 = System.currentTimeMillis()
    try {
      val movies = primary.fetch()
      val ms = System.currentTimeMillis() - t0
      if (showtimeCount(movies) > 0) PrimaryOutcome.Healthy(movies, ms) else PrimaryOutcome.Empty(movies, ms)
    } catch {
      case NonFatal(t) => PrimaryOutcome.Threw(t)
    }
  }

  /** One fallback fetch and how long it took; an absent fallback answers empty. */
  private def fetchFallback(): (scala.util.Try[Seq[CinemaMovie]], Long) = fallback() match {
    case Some(fw) =>
      val t0 = System.currentTimeMillis()
      val movies = scala.util.Try(fw.fetch())
      (movies, System.currentTimeMillis() - t0)
    case None => (scala.util.Success(Seq.empty), 0L)
  }

  /** The fallback as cover for a primary that failed or came back empty: whether it can
   *  serve this tick. A fallback that threw cannot, and the primary's own outcome — its
   *  throw, or its empty listing — stands; nothing is reported on the fallback's behalf. */
  private def tryFallback(): (Seq[CinemaMovie], Long, Boolean) = {
    val (movies, ms) = fetchFallback()
    val served       = movies.toOption.filter(showtimeCount(_) > 0)
    (served.getOrElse(Seq.empty), ms, served.isDefined)
  }

  /** Grace-window failure: keep the `failingSince` clock running (starting it if
   *  this is the first failure) without entering fallback. `active=false` so the
   *  /uptime page (which filters on `active`) ignores it, and no history/event is
   *  recorded — a grace failure is not a fallback transition. */
  private def recordGraceFailure(previous: Option[FallbackState], nowI: Instant, reason: String): Unit =
    store.put(stillFailing(previous.getOrElse(initialState), nowI, reason))

  /** The primary has now been failing for the whole grace window AND the fallback
   *  has nothing to serve either, so NOBODY is covering this venue — and nothing
   *  else in the system says so. `/uptime` shows the same red row a ten-minute blip
   *  gets; the country-level scrape-age alert goes quiet as soon as
   *  `ScrapeFreshnessPolicy` parks the venue on the normal freshness window; and
   *  ENTER, the only fallback page there was, by definition needs a fallback WITH
   *  data. That is how ODEON Basingstoke scraped nothing for a week after Odeon
   *  dropped its site id — the cinema had closed, so flicks.co.uk had no showtimes
   *  to fall back to either, and the venue never left the grace window.
   *
   *  So it pages, ONCE per failing spell: repeated every tick the alert would be
   *  worth nothing, and the state is otherwise the grace state — still
   *  `active = false`, because we are not serving a fallback, we are serving
   *  nothing. A primary success clears `failingSince` and so re-arms it. */
  private def recordUncovered(previous: Option[FallbackState], nowI: Instant, reason: String): Unit = {
    val base = previous.getOrElse(initialState)
    if (alreadyPagedUncovered(base)) recordGraceFailure(previous, nowI, reason)
    else {
      val event = FallbackEvent(nowI, FallbackEvent.Uncovered, reason)
      val next  = stillFailing(base, nowI, reason)
        .copy(history = (event :: base.history).take(FallbackState.MaxHistory))
      store.put(next)
      onEvent(next, event)
    }
  }

  /** Have we already paged for THIS failing spell? The newest history entry is the
   *  spell's own UNCOVERED only while the spell lasts: any later transition
   *  (ENTER / PROBE_FAILED / RECOVERED) pushes itself in front, and a success clears
   *  `failingSince`, so the next spell's entry cannot be mistaken for this one's. */
  private def alreadyPagedUncovered(base: FallbackState): Boolean =
    base.history.headOption.exists(entry =>
      entry.event == FallbackEvent.Uncovered &&
        base.failingSince.exists(since => !entry.at.isBefore(since)))

  /** The persisted shape of "the primary is down and we are not on fallback",
   *  shared by the silent grace ticks and the one that pages. */
  private def stillFailing(base: FallbackState, nowI: Instant, reason: String): FallbackState =
    base.copy(
      active              = false,
      fallbackSource = fallbackName, fallbackRef = fallbackRef(),
      failingSince        = base.failingSince.orElse(Some(nowI)),
      failedRuns          = failedRunsIncluding(base, nowI),
      lastReason          = Some(reason),
      consecutiveFailures = 0,             // backoff only matters once we're on fallback
      lastPrimaryProbeAt  = Some(nowI),
      nextPrimaryProbeAt  = None,          // no backoff in the grace window — probe every tick
      updatedAt           = nowI
    )

  /** Cross from the grace window into fallback: the primary has now been failing
   *  for [[fallbackAfter]] and the fallback has data. Pages ENTER immediately — the
   *  grace window already proved this is no brief blip. */
  private def enterFallback(previous: Option[FallbackState], nowI: Instant, reason: String): Unit = {
    val base  = previous.getOrElse(initialState)
    val event = FallbackEvent(nowI, FallbackEvent.Enter, reason)
    val next = base.copy(
      active              = true,
      fallbackSource = fallbackName, fallbackRef = fallbackRef(),
      failingSince        = base.failingSince.orElse(Some(nowI)),
      failedRuns          = failedRunsIncluding(base, nowI),
      since               = Some(nowI),
      lastReason          = Some(reason),
      consecutiveFailures = 1,
      lastPrimaryProbeAt  = Some(nowI),
      nextPrimaryProbeAt  = Some(nowI.plusMillis(backoffFor(1).toMillis)),
      updatedAt           = nowI,
      history             = (event :: base.history).take(FallbackState.MaxHistory),
      alerted             = true
    )
    store.put(next)
    onEvent(next, event)
  }

  /** A re-probe while already on fallback found the primary still down: record a
   *  PROBE_FAILED, bump the failure count and push the next probe out with
   *  exponential backoff. Routine backoff noise — no page (FallbackAlert ignores
   *  PROBE_FAILED). */
  private def markPrimaryDown(previous: Option[FallbackState], nowI: Instant, reason: String): Unit = {
    val base        = previous.getOrElse(initialState)
    val consecutive = base.consecutiveFailures + 1
    val event       = FallbackEvent(nowI, FallbackEvent.ProbeFailed, reason)
    val next = base.copy(
      active              = true,
      fallbackSource = fallbackName, fallbackRef = fallbackRef(),
      failingSince        = base.failingSince.orElse(Some(nowI)),
      since               = base.since.orElse(Some(nowI)),
      lastReason          = Some(reason),
      consecutiveFailures = consecutive,
      lastPrimaryProbeAt  = Some(nowI),
      nextPrimaryProbeAt  = Some(nowI.plusMillis(backoffFor(consecutive).toMillis)),
      updatedAt           = nowI,
      history             = (event :: base.history).take(FallbackState.MaxHistory)
    )
    store.put(next)
    onEvent(next, event)
  }

  /** A healthy primary tick ends the current failing run. If we were on fallback,
   *  mark RECOVERED and page (the entry paged, so the recovery is worth a page); if
   *  we were merely in the grace window, clear the clock silently. */
  private def endFailingSpell(previous: Option[FallbackState], nowI: Instant): Unit = previous.foreach { p =>
    if (p.active) {
      val event = FallbackEvent(nowI, FallbackEvent.Recovered, "primary recovered")
      val next = p.copy(
        active = false, lastReason = Some("primary recovered"), consecutiveFailures = 0,
        failingSince = None, failedRuns = 0, since = None, lastPrimaryProbeAt = Some(nowI), nextPrimaryProbeAt = None,
        updatedAt = nowI, history = (event :: p.history).take(FallbackState.MaxHistory)
      )
      store.put(next)
      onEvent(next, event)
    } else if (p.failingSince.isDefined) {
      store.put(p.copy(failingSince = None, failedRuns = 0, lastPrimaryProbeAt = Some(nowI), updatedAt = nowI))
    }
  }

  private def backoffFor(consecutive: Int): FiniteDuration = {
    val shifted = baseBackoff * (1L << math.min(consecutive - 1, 20))
    if (shifted > maxBackoff) maxBackoff else shifted
  }

  private def initialState = FallbackState(
    cinema = service, active = false, fallbackSource = fallbackName, fallbackRef = fallbackRef(), failingSince = None, since = None,
    lastReason = None, consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
    updatedAt = Instant.EPOCH, history = Nil
  )
}

object SourceFallbackScraper {
  val DefaultBaseBackoff: FiniteDuration = 5.minutes
  val DefaultMaxBackoff:  FiniteDuration = 60.minutes
  /** Ride out a primary outage on last-good data for this long before switching to
   *  the fallback's sparser listing — only a genuinely sustained failure trips fallback,
   *  so a brief blip never flickers a block of films out of the corpus. */
  val DefaultFallbackAfter: FiniteDuration = 6.hours

  private val EmptyReason = "primary returned no screenings"

  private def showtimeCount(movies: Seq[CinemaMovie]): Int = movies.iterator.map(_.showtimes.size).sum

  private sealed trait PrimaryOutcome
  private object PrimaryOutcome {
    case class Healthy(movies: Seq[CinemaMovie], ms: Long) extends PrimaryOutcome
    case class Empty(movies: Seq[CinemaMovie], ms: Long)   extends PrimaryOutcome
    case class Threw(error: Throwable)                     extends PrimaryOutcome
  }
}
