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
 * The cache's backstop `rehydrate` is the same race on a whole-corpus read: it takes
 * [[markAll]] before its `findAll`, and neither stores nor evicts a film written since.
 *
 * Refusing is safe because the disturbing write rings its OWN event, whose re-read comes
 * after it: the film still reaches the cache, just fresh. (A write that changes nothing in
 * Mongo rings nothing — but then the cache already holds what Mongo does.)
 *
 * The check and the apply happen under one lock, and so does a write's start: a write can
 * begin either before the check (and the read is refused) or after the apply (and the write
 * overwrites it) — never in between. Striped rather than per film, so it costs a fixed array
 * however many films a long-lived worker writes — but each stripe remembers WHICH films its
 * in-flight and last [[FilmWriteFence.RememberedWrites]] writes were of, so another film's write
 * on the same stripe never refuses this one's read. It must not: that write rings only its own
 * film's event, so a read refused for it is never re-delivered, and the cache would sit on the
 * older row until the backstop rehydrate, hours later. Only when more writes landed on the stripe
 * during one read than it remembers is the read refused without knowing (which needs ~that many
 * writes to 1 of 1,024 stripes inside one read).
 */
final class FilmWriteFence(stripes: Int = FilmWriteFence.DefaultStripes) {
  // Guarded by the stripe's own monitor. `started` numbers the stripe's writes (the n-th write
  // to start is write n); `recent` holds the film of write n at `n % RememberedWrites`, and
  // `inFlight` the films of the writes started and not yet finished (a handful at most).
  private final class Stripe {
    var started  = 0L
    val recent   = new Array[String](FilmWriteFence.RememberedWrites)
    val inFlight = scala.collection.mutable.ArrayBuffer.empty[String]

    def markOf(id: String): Long = if (inFlight.contains(id)) FilmWriteFence.InFlight else started

    /** No write of `id` in flight, none begun since `mark` — false too when more writes began
     *  since than `recent` remembers, so whose they were is unknown. */
    def undisturbed(id: String, mark: Long): Boolean =
      mark != FilmWriteFence.InFlight && !inFlight.contains(id) && started - mark <= FilmWriteFence.RememberedWrites &&
        !(mark + 1 to started).exists(n => recent((n % FilmWriteFence.RememberedWrites).toInt) == id)
  }
  private val table = Array.fill(stripes)(new Stripe)
  private def stripeOf(id: String): Stripe = table(Math.floorMod(id.hashCode, stripes))

  /** Run a local write of `id` — the resident row's change AND the store write behind it. */
  def writing[A](id: FilmId)(write: => A): A = {
    val stripe = stripeOf(id.value)
    stripe.synchronized {
      stripe.started += 1
      stripe.recent((stripe.started % FilmWriteFence.RememberedWrites).toInt) = id.value
      stripe.inFlight += id.value
    }
    try write finally stripe.synchronized { stripe.inFlight -= id.value; () }
  }

  /** Taken BEFORE re-reading `id`: what [[ifUndisturbed]] later compares against.
   *  [[FilmWriteFence.InFlight]] when a local write of it is under way right now. */
  def mark(id: String): Long = {
    val stripe = stripeOf(id)
    stripe.synchronized(stripe.markOf(id))
  }

  /** [[mark]] for EVERY film at once — taken before a whole-corpus read (the cache's backstop
   *  `findAll`), whose films are not known until it returns. `marks.of(id)` is that film's mark. */
  def markAll(): FilmWriteFence.Marks = {
    val taken = table.map(stripe => stripe.synchronized((stripe.started, stripe.inFlight.toSet)))
    new FilmWriteFence.Marks(id => {
      val (started, inFlight) = taken(Math.floorMod(id.hashCode, stripes))
      if (inFlight.contains(id)) FilmWriteFence.InFlight else started
    })
  }

  /** Run `apply` — atomically with any local write's start — only if no local write of `id`
   *  was in flight at `mark` or has begun since. [[FilmWriteFence.Unfenced]] always applies.
   *  True when `apply` ran. */
  def ifUndisturbed(id: String, mark: Long)(apply: => Unit): Boolean =
    if (mark == FilmWriteFence.Unfenced) { apply; true }
    else {
      val stripe = stripeOf(id)
      stripe.synchronized {
        val undisturbed = stripe.undisturbed(id, mark)
        if (undisturbed) apply
        undisturbed
      }
    }
}

object FilmWriteFence {
  val DefaultStripes = 1024
  /** How many of its latest writes a stripe remembers the film of. */
  val RememberedWrites = 8
  /** The mark of a read taken while a local write of the film was in flight: never applied. */
  val InFlight: Long = -1L
  /** The mark of a delivery that is not a snapshot racing a write — an in-memory store that
   *  notifies synchronously with the write itself. Always applied. */
  val Unfenced: Long = -2L

  /** Every film's mark as of one instant — see [[FilmWriteFence.markAll]]. */
  final class Marks private[FilmWriteFence] (markOf: String => Long) {
    def of(id: String): Long = markOf(id)
  }
}
