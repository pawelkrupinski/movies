package tools

/**
 * Whether a whole-collection read saw every row — what a keyset scan
 * (`services.movies.KeysetScan`) and every repository method built on one answer, instead
 * of a `Boolean`.
 *
 * The bug this exists to make unwritable: a scan that failed part-way still handed the
 * rows it had read to its consumer, and answered `false` — a value Scala lets a caller drop
 * without a word. Callers did drop it: a reader returned the rows it had as the whole
 * collection, a hydrate reported the corpus loaded, and a prune's inputs came from a read
 * that never finished. A `ScanOutcome` cannot be dropped: the build turns an unused value
 * of this type into an error (`-Wnonunit-statement`, filtered to the outcome types in
 * `build.sbt`), so every caller branches on [[isComplete]] or matches the two cases — and a
 * destructive step driven by a scan says, in code, that it saw the whole collection.
 */
sealed trait ScanOutcome {
  import ScanOutcome._

  def isComplete: Boolean = this == Complete

  /** This scan, then `next` — complete only when both are. `next` runs only after a complete
   *  scan: once one read is short, the whole answer already is. */
  def andThen(next: => ScanOutcome): ScanOutcome = this match {
    case Complete      => next
    case short: Incomplete => short
  }

  /** The rows a collecting scan gathered, as the read they are: answered only when the scan
   *  saw everything — a partial collection is never handed out as the whole. */
  def collected[A](rows: => A): ReadOutcome[A] = this match {
    case Complete          => ReadOutcome.Answered(rows)
    case Incomplete(cause) => ReadOutcome.Failed(ReadFailure.Thrown(cause))
  }

  /** One line for a log. */
  def explain: String = this match {
    case Complete          => "complete"
    case Incomplete(cause) => s"incomplete: ${cause.getClass.getSimpleName}: ${cause.getMessage}"
  }
}

object ScanOutcome {
  case object Complete extends ScanOutcome
  /** The scan stopped before the last row; `cause` is the read failure that stopped it. */
  final case class Incomplete(cause: Throwable) extends ScanOutcome

  /** A store whose scan cannot fail part-way — an in-memory one — answers with this. */
  def complete: ScanOutcome = Complete

  /** For a store that tracks completeness itself: [[Complete]] when `whole`, else
   *  [[Incomplete]] with an exception saying `why`. */
  def of(whole: Boolean, why: => String): ScanOutcome =
    if (whole) Complete else Incomplete(new IncompleteScanException(why))
}

/** Why a scan reported [[ScanOutcome.Incomplete]] when it has no read failure of its own to name. */
final class IncompleteScanException(message: String) extends RuntimeException(message)
