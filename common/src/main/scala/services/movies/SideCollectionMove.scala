package services.movies

/**
 * Carrying one film's rows in a side collection (`screenings`, `movie_slots`) from an
 * OLD document id to a NEW one — the re-key / fold move, as one rule shared by
 * [[MongoMovieRepository]] and the in-memory fake.
 *
 * It lives above the trait seam on purpose. The move is business logic, not storage: it
 * decides when a copy is safe to follow with a delete, and a fake that re-implemented it
 * (or, as the fake did, skipped the verification entirely) would let every re-key spec
 * pass against rules production does not follow.
 *
 * The rule, in the order the steps have to happen:
 *
 *  1. Read the SOURCE. A failed read is not "nothing to move" — the rows are there and we
 *     cannot see them, and the caller is about to delete `oldId`. Refuse.
 *  2. Nothing to move ⇒ done, and the caller may proceed: there is nothing to lose.
 *  3. Read the DESTINATION, because the write below is a `replaceFilm`, i.e. upsert PLUS a
 *     delete of every key the payload does not name. A failed read here reads as "the
 *     destination is empty" and the rows already at `newId` are deleted by that `$nin`.
 *     This is the case a bare `findForFilm` could not express and the one that made the
 *     move destructive in exactly the situation it exists to protect.
 *  4. Write, then VERIFY at the destination before deleting the source. `replaceFilm`
 *     swallows its own failures, so a copy can silently not happen while the delete
 *     proceeds; a Mongo transaction would not help, because there is no exception and no
 *     rollback to trigger. Verify BOTH directions — the moved keys arrived AND the
 *     destination's own keys survived — so a partial write can't pass by naming only what
 *     it added.
 *
 * Returns whether the source id is now safe to delete. `false` means the caller must NOT
 * proceed with the rename: leaving a duplicate at the old id is recoverable
 * (`scripts.ReapOrphanedFilmRows` clears it), losing the film's only copy is not.
 */
object SideCollectionMove {

  def move[A](
    oldId:      String,
    newId:      String,
    read:       String => (Map[String, A], Boolean),
    replace:    (String, Map[String, A]) => Boolean,
    deleteFilm: String => Unit,
    onSkip:     String => Unit = _ => (),
    onMoved:    Int => Unit    = _ => ()
  ): Boolean = {
    val (moving, movingRead) = read(oldId)
    if (!movingRead) {
      onSkip(s"could not READ the rows at $oldId, so they cannot be carried to $newId — " +
        "deferring the rename rather than deleting rows we never saw")
      false
    } else if (moving.isEmpty) true   // nothing filed under the old id; the rename is free
    else {
      val (destination, destinationRead) = read(newId)
      if (!destinationRead) {
        onSkip(s"could not READ the rows already at $newId — merging onto an unread destination " +
          "would delete them, so the rename waits for a readable one")
        false
      } else {
        val landed = replace(newId, destination ++ moving)
        val (after, afterRead) = read(newId)
        val complete = landed && afterRead &&
          moving.keySet.subsetOf(after.keySet) && destination.keySet.subsetOf(after.keySet)
        if (complete) { deleteFilm(oldId); onMoved(moving.size); true }
        else {
          onSkip(s"the copy from $oldId to $newId did not land — keeping the old rows rather " +
            "than deleting the film's only copy")
          false
        }
      }
    }
  }
}
