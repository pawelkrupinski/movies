package services.movies

/**
 * Keeps a change-stream re-read from rolling back a write this process made after it.
 *
 * The change stream fans a film out by RE-READING it ([[MovieChangeStream]]), and the
 * re-read is a snapshot: taken before a local write lands in Mongo and delivered after the
 * cache already holds that write, it hands the cache the film as it was. `MovieCache`
 * stored it blindly, so its own write was undone until the write's own event re-read the
 * film again — a window in which every reader of the cache saw the older row. The
 * 2026-09-25 `RetryResolveServingIntegrationSpec` flake was exactly that: a retry concluded
 * a fresh no-match and, a few milliseconds later, the cache was back on the old miss.
 *
 * Two halves, one per side of the race:
 *  - a LOCAL write runs inside [[writing]] — from before the resident row changes until
 *    the store write behind it has returned;
 *  - the stream takes a [[mark]] BEFORE each re-read, and the receiving cache applies that
 *    read only through [[ifUndisturbed]], which refuses it when a local write of the film was
 *    in flight at the mark or began after it.
 *
 * Refusing is safe because the disturbing write rings its OWN event, whose re-read comes
 * after it: the film still reaches the cache, just fresh. (A write that changes nothing in
 * Mongo rings nothing — but then the cache already holds what Mongo does.)
 *
 * The check and the apply happen under one lock, and so does a write's start: a write can
 * begin either before the check (and the read is refused) or after the apply (and the write
 * overwrites it) — never in between. Striped rather than per film, so it costs a fixed array
 * however many films a long-lived worker writes; two films sharing a stripe only ever cost
 * a refused read that the next event re-delivers.
 */
final class FilmWriteFence(stripes: Int = FilmWriteFence.DefaultStripes) {
  // Guarded by the stripe's own monitor. `started == finished` ⇔ no write in flight.
  private final class Stripe { var started = 0L; var finished = 0L }
  private val table = Array.fill(stripes)(new Stripe)
  private def stripeOf(id: String): Stripe = table(Math.floorMod(id.hashCode, stripes))

  /** Run a local write of `id` — the resident row's change AND the store write behind it. */
  def writing[A](id: FilmId)(write: => A): A = {
    val stripe = stripeOf(id.value)
    stripe.synchronized { stripe.started += 1 }
    try write finally stripe.synchronized { stripe.finished += 1 }
  }

  /** Taken BEFORE re-reading `id`: what [[ifUndisturbed]] later compares against.
   *  [[FilmWriteFence.InFlight]] when a local write of it is under way right now. */
  def mark(id: String): Long = {
    val stripe = stripeOf(id)
    stripe.synchronized { if (stripe.started == stripe.finished) stripe.started else FilmWriteFence.InFlight }
  }

  /** Run `apply` — atomically with any local write's start — only if no local write of `id`
   *  was in flight at `mark` or has begun since. [[FilmWriteFence.Unfenced]] always applies.
   *  True when `apply` ran. */
  def ifUndisturbed(id: String, mark: Long)(apply: => Unit): Boolean =
    if (mark == FilmWriteFence.Unfenced) { apply; true }
    else {
      val stripe = stripeOf(id)
      stripe.synchronized {
        val undisturbed = mark != FilmWriteFence.InFlight && stripe.started == mark && stripe.finished == mark
        if (undisturbed) apply
        undisturbed
      }
    }
}

object FilmWriteFence {
  val DefaultStripes = 1024
  /** The mark of a read taken while a local write of the film was in flight: never applied. */
  val InFlight: Long = -1L
  /** The mark of a delivery that is not a snapshot racing a write — an in-memory store that
   *  notifies synchronously with the write itself. Always applied. */
  val Unfenced: Long = -2L
}
