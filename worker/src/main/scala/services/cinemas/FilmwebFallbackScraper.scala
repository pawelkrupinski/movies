package services.cinemas

import models.{Cinema, CinemaMovie}
import services.UptimeMonitor
import services.fallback.{FallbackEvent, FilmwebFallbackState, FilmwebFallbackStore}

import java.time.Instant
import scala.concurrent.duration._
import scala.util.control.NonFatal

/**
 * Decorator (for non-chain, non-Filmweb venues — see [[FallbackEligibility]])
 * that serves showtimes from Filmweb when the cinema's own scraper fails or comes
 * back empty, and records the outcome — including the "served via Filmweb" flag —
 * against the `UptimeMonitor`. It REPLACES `UptimeRecordingScraper` for eligible
 * cinemas (it does its own recording); its `primary` is the retry-only
 * `RetryingCinemaScraper`.
 *
 * Per tick (`service` = `cinema.displayName`):
 *
 *   - Primary returns screenings  → record success; if we were on fallback, mark
 *                                    RECOVERED and release it.
 *   - Primary threw / empty, Filmweb has data → serve Filmweb, record a
 *                                    fallback-success (green + "via Filmweb"),
 *                                    ENTER fallback (or PROBE_FAILED if already on
 *                                    it) and back off the next primary re-probe.
 *   - Primary down AND Filmweb also empty/unavailable → record the primary's real
 *                                    outcome (red on throw, white on empty). When
 *                                    we were NOT already on fallback this is the
 *                                    night-empty / not-on-Filmweb case: we DON'T
 *                                    enter fallback, because only Filmweb actually
 *                                    having data trips it — so a genuinely empty
 *                                    late-night repertoire (Filmweb empty too)
 *                                    never false-trips.
 *
 * Recovery: while on fallback we skip the (often slow/timing-out) primary until
 * `nextPrimaryProbeAt`, serving Filmweb directly; at that point we re-probe the
 * primary, recovering immediately if it's back, else extending the backoff
 * (exponential, capped). State + history persist via `FilmwebFallbackStore` for
 * the /uptime/fallback page; `onEvent` fires on ENTER / PROBE_FAILED / RECOVERED
 * for alerting.
 */
class FilmwebFallbackScraper(
  primary:         CinemaScraper,
  filmweb:         () => Option[CinemaScraper],
  filmwebCinemaId: () => Option[Int],
  monitor:         UptimeMonitor,
  store:           FilmwebFallbackStore,
  now:             () => Instant = () => Instant.now(),
  baseBackoff:     FiniteDuration = FilmwebFallbackScraper.DefaultBaseBackoff,
  maxBackoff:      FiniteDuration = FilmwebFallbackScraper.DefaultMaxBackoff,
  onEvent:         (FilmwebFallbackState, FallbackEvent) => Unit = (_, _) => ()
) extends CinemaScraper {
  import FilmwebFallbackScraper._

  val cinema: Cinema           = primary.cinema
  def scrapeHosts: Set[String] = primary.scrapeHosts
  private val service          = cinema.displayName

  def fetch(): Seq[CinemaMovie] = {
    val prev   = store.get(service)
    val active = prev.exists(_.active)
    val nowI   = now()
    val withinBackoff = active && prev.flatMap(_.nextPrimaryProbeAt).exists(nowI.isBefore)

    if (withinBackoff) {
      // On fallback, not yet time to re-probe → skip the broken primary entirely.
      val (fwMovies, fwMs, fwServed) = tryFilmweb()
      prev.foreach(p => store.put(p.copy(updatedAt = nowI)))
      if (fwServed) { monitor.recordFallbackSuccess(service, fwMs); fwMovies }
      else { monitor.recordEmpty(service, fwMs); Seq.empty }
    } else {
      runPrimary() match {
        case PrimaryOutcome.Healthy(movies, ms) =>
          if (active) recover(prev, nowI)
          monitor.recordSuccess(service, ms)
          movies
        case down =>
          val (fwMovies, fwMs, fwServed) = tryFilmweb()
          if (active) {
            // Re-probed, primary still down → failed probe; extend backoff.
            markPrimaryDown(prev, nowI, reasonOf(down))
            if (fwServed) { monitor.recordFallbackSuccess(service, fwMs); fwMovies }
            else recordPrimaryOutcome(down)
          } else if (fwServed) {
            // First fall: enter fallback only because Filmweb actually has data.
            markPrimaryDown(prev, nowI, reasonOf(down))
            monitor.recordFallbackSuccess(service, fwMs)
            fwMovies
          } else {
            // Primary down and Filmweb can't help → its real outcome (no false trip).
            recordPrimaryOutcome(down)
          }
      }
    }
  }

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

  private def tryFilmweb(): (Seq[CinemaMovie], Long, Boolean) = filmweb() match {
    case Some(fw) =>
      val t0 = System.currentTimeMillis()
      val movies = try fw.fetch() catch { case NonFatal(_) => Seq.empty }
      (movies, System.currentTimeMillis() - t0, showtimeCount(movies) > 0)
    case None => (Seq.empty, 0L, false)
  }

  /** No fallback data to serve, so the cinema's real outcome stands — exactly as
   *  `UptimeRecordingScraper` would have recorded it. A throw is RE-RAISED (not
   *  swallowed to empty) so `ShowtimeCache` / the scrape tick skips the cinema as
   *  before; an empty result is returned as-is. */
  private def recordPrimaryOutcome(down: PrimaryOutcome): Seq[CinemaMovie] = down match {
    case PrimaryOutcome.Threw(t)          => monitor.recordFailure(service, UptimeRecordingScraper.errorLabel(t)); throw t
    case PrimaryOutcome.Empty(movies, ms) => monitor.recordEmpty(service, ms); movies
    case PrimaryOutcome.Healthy(movies, _) => movies // unreachable: handled before
  }

  /** Enter fallback (ENTER) or, if already on it, record a failed re-probe
   *  (PROBE_FAILED) — either way bump the failure count and push the next probe
   *  out with exponential backoff. */
  private def markPrimaryDown(prev: Option[FilmwebFallbackState], nowI: Instant, reason: String): Unit = {
    val wasActive   = prev.exists(_.active)
    val base        = prev.getOrElse(initialState)
    val consecutive = if (wasActive) base.consecutiveFailures + 1 else 1
    val event       = FallbackEvent(nowI, if (wasActive) FallbackEvent.ProbeFailed else FallbackEvent.Enter, reason)
    val next = base.copy(
      active              = true,
      filmwebCinemaId     = filmwebCinemaId(),
      since               = if (wasActive) base.since.orElse(Some(nowI)) else Some(nowI),
      lastReason          = Some(reason),
      consecutiveFailures = consecutive,
      lastPrimaryProbeAt  = Some(nowI),
      nextPrimaryProbeAt  = Some(nowI.plusMillis(backoffFor(consecutive).toMillis)),
      updatedAt           = nowI,
      history             = (event :: base.history).take(FilmwebFallbackState.MaxHistory)
    )
    store.put(next)
    onEvent(next, event)
  }

  private def recover(prev: Option[FilmwebFallbackState], nowI: Instant): Unit = prev.foreach { p =>
    val event = FallbackEvent(nowI, FallbackEvent.Recovered, "primary recovered")
    val next = p.copy(
      active = false, lastReason = Some("primary recovered"), consecutiveFailures = 0,
      since = None, lastPrimaryProbeAt = Some(nowI), nextPrimaryProbeAt = None, updatedAt = nowI,
      history = (event :: p.history).take(FilmwebFallbackState.MaxHistory)
    )
    store.put(next)
    onEvent(next, event)
  }

  private def backoffFor(consecutive: Int): FiniteDuration = {
    val shifted = baseBackoff * (1L << math.min(consecutive - 1, 20))
    if (shifted > maxBackoff) maxBackoff else shifted
  }

  private def initialState = FilmwebFallbackState(
    cinema = service, active = false, filmwebCinemaId = filmwebCinemaId(), since = None, lastReason = None,
    consecutiveFailures = 0, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
    updatedAt = Instant.EPOCH, history = Nil
  )
}

object FilmwebFallbackScraper {
  val DefaultBaseBackoff: FiniteDuration = 5.minutes
  val DefaultMaxBackoff:  FiniteDuration = 60.minutes

  private def showtimeCount(movies: Seq[CinemaMovie]): Int = movies.iterator.map(_.showtimes.size).sum

  private def reasonOf(down: PrimaryOutcome): String = down match {
    case PrimaryOutcome.Threw(t)   => UptimeRecordingScraper.errorLabel(t)
    case _: PrimaryOutcome.Empty   => "primary returned no screenings"
    case _: PrimaryOutcome.Healthy => ""
  }

  private sealed trait PrimaryOutcome
  private object PrimaryOutcome {
    case class Healthy(movies: Seq[CinemaMovie], ms: Long) extends PrimaryOutcome
    case class Empty(movies: Seq[CinemaMovie], ms: Long)   extends PrimaryOutcome
    case class Threw(error: Throwable)                     extends PrimaryOutcome
  }
}
