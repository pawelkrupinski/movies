package services.scrapes

import models.Opening
import services.fallback.FallbackState

import java.time.{Duration => JDuration, Instant}
import scala.concurrent.duration._

/**
 * Whether a venue has CLOSED, beyond the doubt that [[GoneUpstream]] leaves — in two
 * steps, because an aggregator dropping a venue is not the venue closing.
 *
 * '''Source dropped''' ([[sourceDropped]]) is what the listings show, held over weeks:
 *
 *  - the primary's page has answered 404/410 ([[GoneUpstream]]) for
 *    [[ConfirmAfter]], and is still being probed ([[FreshWithin]]); and
 *  - the venue's fallback feed, when it has one, has ANSWERED without screenings — its
 *    page lists nothing, or is gone too — for [[ConfirmAfter]], confirmed again within
 *    [[FreshWithin]]. A fallback that only errored has said nothing; or
 *  - with no fallback to corroborate it, the primary alone has held for
 *    [[ConfirmAfterUncorroborated]].
 *
 * That proves only that we have lost the venue's programme. ACME Screening Room in
 * Lambertville was open, with an October festival on its own site, when flicks.us
 * 404'd it, and an aggregator-only rule retired it as "closed".
 *
 * '''Closed''' ([[judge]]) needs, on top, a [[VenueSignal.Closure]] about the venue
 * ITSELF — its own site gone or announcing the closure ([[OwnSiteEvidence]]), or a
 * second listing that has positively dropped it ([[listingSignal]]) — and no
 * [[VenueSignal.Life]] against it. A dropped venue with no such evidence stays
 * [[Verdict.SourceDropped]]: a coverage gap that needs a new source, never a closure.
 *
 * A seasonal venue is never judged off-season, and its spells count from the day its
 * season opened. The rule is the same for every country: it reads per-venue evidence only.
 */
object VenueClosure {

  val ConfirmAfter: FiniteDuration               = 14.days
  val ConfirmAfterUncorroborated: FiniteDuration = 28.days
  val FreshWithin: FiniteDuration                = 3.days

  enum FallbackEvidence {
    case ListedEmpty(since: Instant, lastSeen: Instant)
    /** The fallback's own page for the venue answered `error` (a 404/410) throughout. */
    case Dropped(source: String, since: Instant, lastSeen: Instant, error: String)
    case NoFallback
  }

  /** What a source drop rests on — enough for a human to check it in a minute. */
  case class SourceDrop(goneSince: Instant, primaryError: String, lastAttemptAt: Instant,
                        lastContentAt: Option[Instant], fallback: FallbackEvidence)

  /** One observation about the venue itself, worded as what was seen ("own site
   *  https://… answered HTTP 404"), so a reader can tell it from an aggregator's 404. */
  enum VenueSignal {
    /** The venue shows it has closed. */
    case Closure(observed: String)
    /** The venue shows it is still there: a reachable site with no closure notice. */
    case Life(observed: String)
    /** Asked, but the answer says nothing either way (blocked, timed out, 5xx). */
    case Silent(observed: String)

    def observed: String
  }

  enum Verdict {
    /** Not even dropped by its sources: `why` names what is still missing. */
    case Listed(why: String)
    /** The sources dropped it, but nothing shows the venue closed: a coverage gap. */
    case SourceDropped(drop: SourceDrop, signals: Seq[VenueSignal])
    /** The sources dropped it AND the venue itself shows it has closed. */
    case Closed(drop: SourceDrop, signals: Seq[VenueSignal])
  }

  /** The verdict on a venue whose drop is `drop`, asking `signals` about the venue
   *  itself only once its sources have dropped it — the checks cost a request each.
   *  Any sign of life outweighs every sign of closure: retiring an open venue hides
   *  it from everyone, keeping a closed one costs a page. */
  def judge(drop: Either[String, SourceDrop], signals: SourceDrop => Seq[VenueSignal]): Verdict = drop match {
    case Left(why) => Verdict.Listed(why)
    case Right(dropped) =>
      val seen = signals(dropped)
      val life = seen.exists { case VenueSignal.Life(_) => true; case _ => false }
      val shut = seen.exists { case VenueSignal.Closure(_) => true; case _ => false }
      if (shut && !life) Verdict.Closed(dropped, seen) else Verdict.SourceDropped(dropped, seen)
  }

  /** What the drop's own fallback says about the venue: a second listing that has
   *  dropped its page is a closure signal; one that lists nothing has said nothing. */
  def listingSignal(drop: SourceDrop): Option[VenueSignal] = drop.fallback match {
    case FallbackEvidence.Dropped(source, since, lastSeen, error) =>
      Some(VenueSignal.Closure(s"second listing $source dropped the venue: $error, since ${since.toString.take(10)} (last checked ${lastSeen.toString.take(10)})"))
    case _ => None
  }

  /** Whether `row`'s venue has been dropped by its sources, or why it does not yet show it. */
  def sourceDropped(row: ArchivedScrape, fallback: Option[FallbackState], hasFallback: Boolean, opening: Opening,
                    now: Instant): Either[String, SourceDrop] = {
    def daysSince(instant: Instant): Long = JDuration.between(instant, now).toDays
    def heldFor(instant: Instant, duration: FiniteDuration): Boolean = JDuration.between(instant, now).toMillis >= duration.toMillis
    def fresh(instant: Instant): Boolean = !heldFor(instant, FreshWithin)
    // A spell counts from when this season opened at the earliest: before it, a seasonal venue is dark anyway.
    val seasonOpened = opening.seasonOpenedAt(now)
    def thisSeason(since: Instant): Instant = seasonOpened.filter(_.isAfter(since)).getOrElse(since)
    val needed = if (hasFallback) ConfirmAfter else ConfirmAfterUncorroborated
    def held(what: String, since: Instant, lastSeen: Instant): Either[String, Unit] = for {
      _ <- Either.cond(heldFor(thisSeason(since), ConfirmAfter), (), s"fallback $what ${daysSince(thisSeason(since))} days, needs ${ConfirmAfter.toDays}")
      _ <- Either.cond(fresh(lastSeen), (), s"fallback last confirmed $what ${daysSince(lastSeen)} days ago")
    } yield ()

    for {
      _         <- Either.cond(!opening.offSeasonAt(now), (), s"seasonal ($opening), out of season")
      barren    <- row.lastBarren.toRight("serving")
      goneSince <- GoneUpstream.goneSince(row, now).toRight(s"not gone upstream (${barren.outcome.label}: ${barren.error.getOrElse("no error")})")
      _         <- Either.cond(heldFor(thisSeason(goneSince), needed), (), s"gone ${daysSince(thisSeason(goneSince))} days, needs ${needed.toDays}")
      _         <- Either.cond(fresh(barren.at), (), s"primary last probed ${daysSince(barren.at)} days ago")
      evidence  <-
        if (!hasFallback) Right(FallbackEvidence.NoFallback)
        else fallback.flatMap(state => state.droppedFallback.map(state.fallbackSource -> _)) match {
          case Some((source, spell)) =>
            held("gone", spell.since, spell.lastSeen).map(_ => FallbackEvidence.Dropped(source, spell.since, spell.lastSeen, spell.error))
          case None =>
            fallback.flatMap(_.emptyFallback).toRight("fallback has not answered empty or gone")
              .flatMap(spell => held("empty", spell.since, spell.lastSeen).map(_ => FallbackEvidence.ListedEmpty(spell.since, spell.lastSeen)))
        }
    } yield SourceDrop(goneSince, barren.error.getOrElse(""), barren.at, row.contentAt, evidence)
  }
}
