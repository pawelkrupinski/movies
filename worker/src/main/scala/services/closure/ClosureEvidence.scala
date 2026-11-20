package services.closure

import java.time.{Instant, LocalDate, ZoneOffset}

import services.scrapes.VenueClosure.{FallbackEvidence, SourceDrop, VenueSignal}

/** A closure's evidence in one line, for the page, the ledger and the PR body. */
object ClosureEvidence {

  /** What the aggregators answered, then what was seen of the venue itself, each as observed. */
  def line(drop: SourceDrop, signals: Seq[VenueSignal]): String = {
    val fallback = drop.fallback match {
      case FallbackEvidence.ListedEmpty(since, lastSeen)    => s"its fallback has listed no screenings since ${day(since)} (last checked ${day(lastSeen)})"
      case FallbackEvidence.Dropped(source, since, _, _)    => s"its fallback $source has answered gone since ${day(since)}"
      case FallbackEvidence.NoFallback                      => "it has no fallback feed"
    }
    val lastListing = drop.lastContentAt.fold("no listing on record")(at => s"last listing ${day(at)}")
    val venue =
      if (signals.isEmpty) "Venue itself: no own website or second listing on record to check."
      else signals.map {
        case VenueSignal.Closure(observed) => s"CLOSED: $observed"
        case VenueSignal.Life(observed)    => s"OPEN: $observed"
        case VenueSignal.Silent(observed)  => s"no answer: $observed"
      }.mkString("Venue itself: ", "; ", ".")
    s"Aggregator page gone since ${day(drop.goneSince)} (${drop.primaryError}, last probed ${day(drop.lastAttemptAt)}); $fallback; $lastListing. $venue"
  }

  /** The UTC day of `instant`, as the pages name it. */
  private[closure] def day(instant: Instant): LocalDate = LocalDate.ofInstant(instant, ZoneOffset.UTC)
}
