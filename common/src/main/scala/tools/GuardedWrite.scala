package tools

import scala.annotation.tailrec

/**
 * Read, decide, and write only over what was read — the one shape a read-modify-write takes,
 * so that a writer that read a row before another writer changed it can never write its older
 * decision back over the newer row (a LOST UPDATE).
 *
 * The bug this exists to make unwritable was fixed ad hoc, site by site: staging rows re-stamped
 * over a resolve that landed while the step waited on TMDB, a searched IMDb id written over the
 * id TMDB's resolution had just set, a blank scrape's marker written over a listing that landed
 * between its read and its write. Each grew its own compare-and-set and its own retry. Here the
 * loop is written once:
 *
 *  1. `read` the stored state (throwing when it cannot be read — an unread row is not an absent one);
 *  2. `decide` what to write over it, or nothing;
 *  3. `writeOver` it, guarded on the state as read (see `services.MongoGuard` for Mongo), answering
 *     whether the guard still matched;
 *  4. on a mismatch, read and decide again — the decision is re-made on the new state, never
 *     replayed — up to `attempts` times, then answer [[GuardedWrite.ChangedUnderYou]].
 *
 * The answer cannot be dropped unread (the build's `-Wnonunit-statement` filter names this type,
 * like `ScanOutcome` and `ReadOutcome`): a caller says what "another writer kept changing it"
 * means for it. A read or write that throws propagates — that is a failure, not a lost race.
 *
 * How to use (Mongo; `MongoScrapeArchiveRepository.recordBarren` is a live example):
 * {{{
 * GuardedWrite(3)(() => readProjected(id, Fields)) { asRead =>
 *   decide(asRead)                                  // Option[Bson]: the update, or None for nothing to do
 * } { (asRead, update) =>
 *   MongoGuard.updateIfUnchanged(c, MongoGuard.unchanged(id, asRead, Fields), update, timeout, insert = false)
 * } match {
 *   case GuardedWrite.Landed(_)            => …
 *   case GuardedWrite.Unneeded             => …
 *   case GuardedWrite.ChangedUnderYou(n)   => logger.warn(…)   // say what losing the race means here
 * }
 * }}}
 * `Fields` are the fields the decision read; use `MongoGuard.wholeUnchanged` when no field set proves
 * the row unchanged. `NoUnguardedReadModifyWriteSpec` fails the build on a read-then-write without it.
 */
sealed trait GuardedWrite[+A] {
  def landed: Boolean = this.isInstanceOf[GuardedWrite.Landed[?]]
}

object GuardedWrite {
  /** The write landed over the state it was decided on. */
  final case class Landed[+A](value: A) extends GuardedWrite[A]
  /** `decide` answered nothing to write over the state as last read. */
  case object Unneeded extends GuardedWrite[Nothing]
  /** Another writer changed the row between every read and its write, `attempts` times in a row. */
  final case class ChangedUnderYou(attempts: Int) extends GuardedWrite[Nothing]

  def apply[S, A](attempts: Int)(read: () => S)(decide: S => Option[A])(writeOver: (S, A) => Boolean): GuardedWrite[A] = {
    require(attempts >= 1, s"a guarded write needs at least one attempt, not $attempts")
    @tailrec def attempt(n: Int): GuardedWrite[A] = {
      val stored = read()
      decide(stored) match {
        case None                                         => Unneeded
        case Some(next) if writeOver(stored, next)        => Landed(next)
        case Some(_) if n >= attempts                     => ChangedUnderYou(n)
        case Some(_)                                      => attempt(n + 1)
      }
    }
    attempt(1)
  }
}
