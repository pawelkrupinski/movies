package services.movies

import models.{MovieRecord, Source, SourceData}

import java.time.Instant
import scala.util.Try

/**
 * What `MovieRepository.upsert` writes to `movies` for one film, and whether it has to —
 * the decision, apart from the reads that feed it and the write that acts on it.
 *
 * `MongoMovieRepository.upsert` re-stitches the record, lands its slots, reads the stored
 * document, calls [[plan]], and writes only when the plan says the stored row differs.
 * Nothing here touches Mongo, so the four cases the decision turns on — no stored
 * document, a stored one that already matches, one that differs, and a read that FAILED —
 * are pinned by `MoviesUpsertSpec` without a replica set. The Mongo half is covered by
 * `MoviesWriteSkippedWhenUnchangedIntegrationSpec`.
 */
object MoviesUpsert {

  /** `document` is what `upsert` would write. `unchanged` says the stored row already equals
   *  it, so the write — and its oplog entry, and that entry's change-stream fanout — can be
   *  skipped. */
  final case class Plan(document: StoredMovieDto, unchanged: Boolean)

  /**
   * @param restitched      the record's slots after the screenings re-stitch
   * @param slotsLanded     whether `movie_slots` now holds those slots (or already did)
   * @param slotsForStorage the repository's [[MovieRepository.slotsForStorage]] — the slot
   *                        map `movies` may carry when the slots have NOT landed
   * @param stored          the `movies` row as read: `Success(None)` is "no such document",
   *                        `Failure` is "could not look" — and the two are NOT the same thing
   * @param now             the `updatedAt` to stamp
   */
  def plan(id: String, key: String, record: MovieRecord, restitched: Map[Source, SourceData], slotsLanded: Boolean,
           slotsForStorage: Map[Source, SourceData] => Map[Source, SourceData],
           stored: Try[Option[StoredMovieDto]], now: Instant): Plan = {
    // Under the read-split `movies` carries no showtimes (they go to `screenings`), and
    // once the slots have landed it carries no sourceData either — which is what shrinks
    // the document the change stream re-decodes on every write.
    val dataForMovies = if (slotsLanded) Map.empty[Source, SourceData] else slotsForStorage(restitched)
    val document      = StoredMovieDto.fromDomain(id, key, record.copy(data = dataForMovies), now)
    // The timestamp is normalised away before comparing: `updatedAt` is stamped
    // `Instant.now()` on every call, so comparing it would make every document differ and
    // the guard dead on arrival. (A retired `slotsUpdatedAt` marker on a legacy document is
    // not decoded at all, so it cannot make one differ either.)
    //
    // A read that FAILED reads as "changed" and writes. A failed read is not evidence that
    // the stored document matches — and an absent document is not evidence either.
    val unchanged = stored.toOption.flatten.exists(existing =>
      existing.copy(updatedAt = document.updatedAt) == document)
    Plan(document, unchanged)
  }
}
