package services.identity

import services.movies.ListingKey

import scala.collection.mutable

/**
 * A REPLAY'S FLIPS: every listing's served take, recorded at each step of an incremental replay (each projection, each
 * apply), and the listings whose take flip-flops — what prod's regression watch counts as a listing flip
 * (`kinowo_identity_listing_flips`, infra's identity-regressions.py) and an end-state comparison cannot see, the replay
 * ending where it began (UK "The Stranger", 1429348 <-> 848791; PL "Ktoś całkiem obcy": a take written from one path,
 * overridden by a correction seconds later, then back). A listing FLIPS when its take
 *
 *  - goes back to a film it left (A -> B -> A),
 *  - moves to another film more than once (A -> B -> C), or
 *  - changes at a step where none of its deciding inputs changed: an interim take a pending correction overrides.
 *
 * The films a listing moves between are counted as prod's watch counts them, between DECIDED takes. A take served while an
 * answer it reads was PENDING ([[Served.pending]], [[TakeOutcome.Pending]]: a search unanswered, a record awaited, a page
 * unread) is provisional -- the decision on the inputs as they stand, its film or none -- and no take: the owner,
 * 2026-10-31 (identity-flips-structural step 6), "decide from the inputs as they stand, unanswered questions treated as
 * unknown", never a previous take substituted. How often a provisional take differs from the final one is measured
 * (IdentityDrift's pending tally), not failed. A first take of none is no film left, so it counts only where no input
 * moved it. A `none` with nothing pending is a take dropped and given back: A -> none -> A leaves A and goes back to it
 * (the review of 58ea51d22, 2026-10-30: counting every `none` alike left the check blind to a take dropping out and back
 * at any step where some answer was filed). A take going from provisional to decided on the same film is recorded too,
 * so A -> B (provisional) -> B (decided) -> A is the flip it is.
 *
 * Only changes are kept, so recording a step costs one look-up a listing.
 */
final class TakeFlips {
  import TakeFlips._

  private val steps   = mutable.ArrayBuffer.empty[String]
  private val last    = mutable.HashMap.empty[ListingKey, Seen]
  private val changes = mutable.HashMap.empty[ListingKey, Vector[Seen]]

  /** One step's served takes. `moved`: whether a listing's deciding inputs — its own scrape, its cluster, the answers
   *  filed, the rules — changed since the step before. A listing's first take is no change. */
  def record(step: String, served: IterableOnce[(ListingKey, Served)], moved: ListingKey => Boolean): Unit = {
    val at = steps.size
    steps += step
    served.iterator.foreach { case (key, now) =>
      last.get(key) match {
        case None                                           => last(key) = Seen(at, now, moved = true)
        case Some(before) if before.served.film != now.film || before.served.pending != now.pending =>
          val seen = Seen(at, now, moved(key))
          changes(key) = changes.getOrElse(key, Vector(before)) :+ seen
          last(key) = seen
        case Some(_)                                        => ()
      }
    }
  }

  /** How many steps were recorded. */
  def recorded: Int = steps.size

  /** Every listing that flipped, its takes in the order served. */
  def flips: Seq[Flip] =
    changes.iterator.flatMap { case (key, seen) =>
      val takes = seen.map(s => Taken(steps(s.step), s.served, s.moved))
      Option.when(Flip.reasons(takes).nonEmpty)(Flip(key, takes))
    }.toSeq.sortBy(_.key)(using ListingKey.ordering)
}

object TakeFlips {

  /** No film served: the take of a listing that holds none. */
  val NoFilm = "none"

  /** A listing's served take: its film (`tmdb:<id>`, `<source>:<id>` or `none`) and the path that decided it; `pending`,
   *  whether an answer its decision reads was still pending then: a provisional take, film or none, and no take. */
  final case class Served(film: String, path: String, pending: Boolean = false)

  private final case class Seen(step: Int, served: Served, moved: Boolean)

  /** One take a listing was served from `step` on; `moved` — whether its deciding inputs changed at that step. */
  final case class Taken(step: String, served: Served, moved: Boolean) {
    def line: String = s"${served.film} [$step${if (moved) "" else ", inputs unchanged"}: ${served.path}]"
  }

  /** A listing whose take flip-flopped: every take it was served, the first included. */
  final case class Flip(key: ListingKey, takes: Vector[Taken]) {
    def reasons: Seq[String] = Flip.reasons(takes)
    def films: String        = takes.map(_.served.film).mkString(" -> ")
    def line: String         = s"${key.venue} | ${key.rawTitle}: ${takes.map(_.line).mkString(" -> ")} (${reasons.mkString("; ")})"
  }

  object Flip {
    def reasons(takes: Vector[Taken]): Seq[String] = {
      // a take an answer was pending for -- a film or none -- is provisional, no take; a none with nothing pending is a take
      // given up, kept in the sequence
      val sequence  = takes.map(_.served).filterNot(_.pending).map(_.film)
        .foldLeft(Vector.empty[String])((seen, film) => if (seen.lastOption.contains(film)) seen else seen :+ film)
      val back      = sequence.indices.exists(i => sequence(i) != NoFilm && sequence.indexOf(sequence(i)) < i)
      val films     = sequence.filter(_ != NoFilm).foldLeft(Vector.empty[String])((seen, film) => if (seen.lastOption.contains(film)) seen else seen :+ film)
      // a step served Pending is skipped (a provisional take is no take): neither a take left nor a change with inputs
      // unchanged; nor is a step that only decided the film it served provisionally
      val unchanged = takes.indices.drop(1).filter { i =>
        val t = takes(i)
        !t.moved && !t.served.pending && t.served.film != takes(i - 1).served.film
      }.map(takes(_).step)
      Seq(
        Option.when(back)("back to a film it left"),
        Option.when(!back && films.size > 2)(s"moved between films ${films.size - 1} times"),
        Option.when(unchanged.nonEmpty)(s"changed with its inputs unchanged at ${unchanged.mkString(", ")}")).flatten
    }
  }
}
