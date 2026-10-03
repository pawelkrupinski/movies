package tools

import services.readmodel.ReadModelProjector

/** A harness's read-model seed: `reconcile` again until a sweep completes its source scan, projects
 *  every row it read and prunes every orphan it found. Under a loaded local Mongo a page's side read
 *  can time out (the scan skips that page) and a read-model write can time out after the server
 *  applied it (a card stands without its screenings) or before (an orphan stays served); production's
 *  next sweep repairs each, and nothing in a harness would. Bounded, and failing as what it is rather than as a missing showtime later. */
object WholeReconcile {
  val DefaultAttempts = 5

  def apply(projector: ReadModelProjector, attempts: Int = DefaultAttempts): Unit =
    if (!Iterator.continually(projector.reconcile()).take(attempts).contains(true))
      throw new IllegalStateException(s"read-model reconcile: no complete, wholly projected sweep in $attempts attempts — " +
        "Mongo reads or read-model writes kept failing, so the projected read model is not the corpus's")
}
