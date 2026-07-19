package services.attempts

import java.time.Instant

/**
 * The outcome of ONE per-row enrichment fetch — what the /debug expand section
 * shows per (source, film).
 *
 * The three cases are deliberately distinct, because the pair that used to be
 * conflated is exactly what made a stuck film un-diagnosable: a source that
 * ERRORS every time and a source that fetched fine but reported no change both
 * came back as `None` from `refreshOneSync`, so the logs, the freshness stamp and
 * the adaptive cadence all read them identically (see [[EnrichmentAttempt]]'s
 * `Failed` vs `Unchanged`).
 */
enum AttemptOutcome:
  /** The fetch moved the row's displayed badge to `value`. */
  case Changed(value: String)
  /** The fetch ran and completed, but the displayed value didn't move. This is the
   *  ordinary steady state — it is NOT an error, and it's what the adaptive cadence
   *  legitimately backs off on. */
  case Unchanged
  /** The fetch threw. `message` is the exception's class + message, trimmed to
   *  something a debug page can render on one line. */
  case Failed(message: String)

object AttemptOutcome:
  /** How long a rendered failure message may be — long enough to carry a useful
   *  HTTP status / host / cause, short enough not to blow up a table cell. */
  private val MaxMessageLength = 300

  /** Build a `Failed` from a throwable, naming the exception type (a bare
   *  `getMessage` is frequently null or an opaque URL, which reads as "no error"
   *  on the page). Truncated to [[MaxMessageLength]]. */
  def failed(exception: Throwable): Failed =
    val detail = Option(exception.getMessage).map(_.trim).filter(_.nonEmpty)
    val text   = detail.fold(exception.getClass.getSimpleName)(m => s"${exception.getClass.getSimpleName}: $m")
    Failed(if (text.length <= MaxMessageLength) text else text.take(MaxMessageLength - 1) + "…")

/**
 * The last attempt a given source made against a given film: when it started, how
 * long it took, and how it ended.
 *
 * Only the LAST attempt per (source, film) is kept. A full history would be the
 * more useful artifact, but it grows without bound across ~300 cinemas' worth of
 * corpus × 4 sources × a refresh every few hours; the cadence store already keeps
 * the last two *changes*, which is the history that answers "is this value
 * moving". This answers the different question the cadence can't: "did the most
 * recent attempt actually work".
 */
case class EnrichmentAttempt(
  at:         Instant,
  durationMs: Long,
  outcome:    AttemptOutcome
):
  def failed: Boolean = outcome match
    case AttemptOutcome.Failed(_) => true
    case _                        => false
