package services.scrapes

import services.fallback.FallbackState

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

/**
 * Whether a venue has CLOSED, beyond the doubt that [[GoneUpstream]] leaves.
 *
 * Gone is a day of 404s, and only slows the scraping down. Closed is what may take
 * a venue off the roster, so it asks for independent sources to agree over weeks:
 *
 *  - the primary's page has answered 404/410 ([[GoneUpstream]]) for
 *    [[ConfirmAfter]], and is still being probed ([[FreshWithin]]); and
 *  - the venue's fallback feed, when it has one, has ANSWERED with no screenings
 *    (its page exists and lists nothing) for [[ConfirmAfter]], confirmed again
 *    within [[FreshWithin]]. A fallback that only errored has said nothing; or
 *  - with no fallback to corroborate it, the primary alone has held for
 *    [[ConfirmAfterUncorroborated]].
 *
 * The rule is the same for every country: it reads per-venue evidence only.
 */
object VenueClosure {

  val ConfirmAfter: FiniteDuration               = 14.days
  val ConfirmAfterUncorroborated: FiniteDuration = 28.days
  val FreshWithin: FiniteDuration                = 3.days

  enum FallbackEvidence {
    case ListedEmpty(since: Instant, lastSeen: Instant)
    case NoFallback
  }

  /** What a closure rests on — enough for a human to check it in a minute. */
  case class ClosureEvidence(goneSince: Instant, primaryError: String, lastAttemptAt: Instant,
                             lastContentAt: Option[Instant], fallback: FallbackEvidence)

  /** The evidence that `row`'s venue has closed, or why it does not yet show it. */
  def judge(row: ArchivedScrape, fallback: Option[FallbackState], hasFallback: Boolean, now: Instant): Either[String, ClosureEvidence] = {
    def daysSince(instant: Instant): Long = JDuration.between(instant, now).toDays
    def heldFor(instant: Instant, duration: FiniteDuration): Boolean = JDuration.between(instant, now).toMillis >= duration.toMillis
    def fresh(instant: Instant): Boolean = !heldFor(instant, FreshWithin)
    val needed = if (hasFallback) ConfirmAfter else ConfirmAfterUncorroborated

    for {
      barren    <- row.lastBarren.toRight("serving")
      goneSince <- GoneUpstream.goneSince(row, now).toRight(s"not gone upstream (${barren.outcome.label}: ${barren.error.getOrElse("no error")})")
      _         <- Either.cond(heldFor(goneSince, needed), (), s"gone ${daysSince(goneSince)} days, needs ${needed.toDays}")
      _         <- Either.cond(fresh(barren.at), (), s"primary last probed ${daysSince(barren.at)} days ago")
      evidence  <-
        if (!hasFallback) Right(FallbackEvidence.NoFallback)
        else for {
          spell <- fallback.flatMap(_.emptyFallback).toRight("fallback has not answered empty")
          _     <- Either.cond(heldFor(spell.since, ConfirmAfter), (), s"fallback empty ${daysSince(spell.since)} days, needs ${ConfirmAfter.toDays}")
          _     <- Either.cond(fresh(spell.lastSeen), (), s"fallback last confirmed empty ${daysSince(spell.lastSeen)} days ago")
        } yield FallbackEvidence.ListedEmpty(spell.since, spell.lastSeen)
    } yield ClosureEvidence(goneSince, barren.error.getOrElse(""), barren.at, row.contentAt, evidence)
  }
}
