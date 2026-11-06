package services.identity

import services.movies.ListingKey

import java.time.Clock
import scala.concurrent.duration.*

/**
 * Take changes that UNDO the listing's last one: a listing moved from film A to B (A or B possibly no film) and now from
 * B back to A, within [[TakeReversals.Horizon]] of the change it undoes. Exactly the reversal of the flip backtest
 * (docs/design/identity-no-drift.md, "Predicting flips (backtest, 2026-11-05)": two consecutive changes of a listing's TMDB
 * film in its take history, the second the first reversed — 215 in the week to 2026-11-05, outside deploys), counted as
 * the projection writes them, so every identity deploy gets a before/after number with no reader of the history.
 *
 * Two readings of one reversal ([[TakeReversals.Reversal]]):
 *  - an OSCILLATION ([[TakeReversals.Reversal.oscillation]]): the undone change at most [[TakeReversals.OscillationWindow]]
 *    written projections earlier, deploys included — two rules undoing each other. UK Chichester's "The Ties That Bind
 *    Us" flipped between the 2008 and the 2025 film on every pod start of 2026-10-13/14 (18 reversals in 41 h): the
 *    periodic projection's exact-top-hit own match took the 2008 film, and the changed-scope projection's
 *    families-and-poster correction took the 2025 one back 17–20 s later. What `IdentityTakeOscillating` reads;
 *  - a FLIP ([[TakeReversals.Reversal.flip]]): neither change set off by a deploy (the trigger as the take's line reports
 *    it: `review` when a pin decided either side), however many projections apart — the backtest's count. A take lost one day and
 *    regained the next (DE "Der mit dem Wolf tanzt (35-jähriges Jubiläum)", lost to a gap, regained by a fix) is one, and
 *    no oscillation.
 * Each is classified by `scripts/flip-timeline`'s classes ([[TakeReversals.Reversal.cause]]), which reads one listing's
 * reversals off the history with the same definition, so the panel's counts and the timeline's agree.
 *
 * Memory: the LAST film change of each listing changed within the horizon, at most `maxHeld` of them (a projection logs
 * at most 2,000 changes; the backtest's week moved a few thousand listings a country), the least recently changed
 * forgotten first: a few MB at most per country, the listing keys the model's own. A worker restart (an OOM, a SIGKILL,
 * a deploy) starts from `seed`: the last film changes the take history kept within the horizon (`TakeHistory.reversalsHeld`,
 * read at boot), the latest `maxHeld`, so a reversal whose two changes straddle a restart is counted. A seeded change is
 * never an oscillation's (its projections apart are unknown, and taken as more than [[TakeReversals.OscillationWindow]]),
 * and one a deploy set off keeps its mark: a reversal of it, or one the boot's own projection makes, is no flip.
 */
final class TakeReversals(clock: Clock, horizon: FiniteDuration = TakeReversals.Horizon, maxHeld: Int = 20000,
                          seed: Seq[(ListingKey, TakeReversals.Held)] = Nil) {
  import TakeReversals.*
  private var projection = 0L
  // insertion-ordered, a listing re-inserted at each change: the eldest entry is the least recently changed listing
  private val held = new java.util.LinkedHashMap[ListingKey, Held](16, 0.75f, false)
  locally {
    val now = clock.millis()
    seed.filter { case (_, h) => now - h.at <= horizon.toMillis }.sortBy(_._2.at).takeRight(maxHeld).foreach { case (listing, h) =>
      held.remove(listing)
      held.put(listing, h.copy(projection = Seeded))
    }
  }

  /** Notes one written projection's changes (none when it moved no take) and returns those that reverse the listing's last
   *  film change. Only a match change ([[TakeChange.moved]]) that moves the TMDB film is one. `decision`: the decision now
   *  holding a listing, if any — its stage ([[DecidedBy.stage]]) and its listings. */
  def note(changes: Seq[TakeChange], decision: ListingKey => Option[ResolverDecision]): Seq[Reversal] = synchronized {
    projection += 1
    val now = clock.millis()
    forget(now)
    changes.filter(c => c.moved && c.before != c.after).flatMap { change =>
      val last = Option(held.remove(change.listing))
      val holding  = decision(change.listing)
      val stage    = holding.map(DecidedBy.stage(_, change.listing))
      val together = holding.fold(Set.empty[ListingKey])(_.members.toSet).hashCode
      held.put(change.listing, Held(change.before, change.after, stage, deployed(change), together, now, projection))
      if (held.size > maxHeld) forget(now)
      last.filter(h => h.before == change.after && h.after == change.before)
        .map(h => Reversal(change, stage, h, (now - h.at) / 1000.0, projection - h.projection, together != h.members))
    }
  }

  /** Drops the least recently changed listings past the capacity, and every one changed longer than the horizon ago. */
  private def forget(now: Long): Unit = {
    val eldest = held.values.iterator
    var stale  = true
    while (stale && eldest.hasNext) {
      val h = eldest.next()
      stale = held.size > maxHeld || now - h.at > horizon.toMillis
      if (stale) eldest.remove()
    }
  }
}

object TakeReversals {
  /** How long a listing's last change is held: the backtest's widest bucket. */
  val Horizon: FiniteDuration = 24.hours
  /** The most written projections apart an oscillation's two changes are. */
  val OscillationWindow = 3
  /** The projection a seeded change is held at: before the boot's first, by more than [[OscillationWindow]]. */
  private val Seeded: Long = -OscillationWindow.toLong
  /** The stage of a change no decision names (a listing no cluster holds). */
  val NoStage = "none"
  /** Every [[Reversal.cause]]: `scripts/flip-timeline`'s classes. */
  val Causes: Seq[String] = Seq("DEPLOY/RESTART", "LATE-ANSWER", "SIBLING/POPULATION", "RULE")
  /** Every [[Reversal.pattern]]. */
  val Patterns: Seq[String] = Seq("film-none-film", "none-film-none", "film-film-film")

  /** A listing's last film change: the film it left and the one it took, the stage that decided it ([[DecidedBy.stage]]),
   *  whether a deploy set it off, the listings its decision held (a hash of the set), when (epoch millis) and in which
   *  written projection. */
  final case class Held(before: Option[Int], after: Option[Int], stage: Option[String], deploy: Boolean, members: Int,
                        at: Long, projection: Long)

  /** `change`, decided at `stage`, undoing `undone`, `seconds` and `projections` written projections after it;
   *  `regrouped`: the decision now holding the listing holds other listings than the one that took it off. */
  final case class Reversal(change: TakeChange, stage: Option[String], undone: Held, seconds: Double, projections: Long,
                            regrouped: Boolean) {
    def oscillation: Boolean = projections <= OscillationWindow
    def flip: Boolean        = !undone.deploy && !deployed(change)
    /** Why it reversed, by `scripts/flip-timeline`'s classes and in its order, first match wins, as far as the worker can
     *  tell at the change: `DEPLOY/RESTART` (a deploy's projection on either side: no [[flip]]), `LATE-ANSWER` (the
     *  restoring projection was set off by an answer; the timeline also requires an ask of the cluster filed after the
     *  dropping projection gave up waiting, which it reads off the history, so this is its upper bound),
     *  `SIBLING/POPULATION` (the two decisions hold other listings; the timeline also counts a member re-published
     *  between them, which here is a `RULE`) or `RULE` (the same inputs, decided otherwise). */
    def cause: String =
      if (!flip) "DEPLOY/RESTART"
      else if (reportedTrigger(change).exists(_.split('+').contains("answer"))) "LATE-ANSWER"
      else if (regrouped) "SIBLING/POPULATION"
      else "RULE"
    /** `film-none-film` (a take lost and regained), `none-film-none` (gained and lost again) or `film-film-film` (moved to
     *  another film and back). */
    def pattern: String = (undone.before, undone.after) match {
      case (Some(_), None) => "film-none-film"
      case (None, _)       => "none-film-none"
      case _               => "film-film-film"
    }
    /** The stage that moved the listing off its take, and the one that put it back. */
    def droppedBy: String  = undone.stage.getOrElse(NoStage)
    def restoredBy: String = stage.getOrElse(NoStage)
    /** How long the undone change stood, as a bucket: `1m` (under a minute), `1h` (a minute to an hour), `24h` (longer). */
    def lasted: String = if (seconds < 60) "1m" else if (seconds < 3600) "1h" else "24h"
  }

  /** What set `change` off as its take-change line and the history report it ([[TakeChangeLines]]): `review` when a pin
   *  decided either side. */
  private def reportedTrigger(change: TakeChange): Option[String] =
    if (change.basisAfter.contains(ResolverDecision.Basis.Pinned) || change.basisBefore.contains(ResolverDecision.Basis.Pinned)) Some("review")
    else change.trigger
  private def deployed(change: TakeChange): Boolean = reportedTrigger(change).contains("deploy")
}
