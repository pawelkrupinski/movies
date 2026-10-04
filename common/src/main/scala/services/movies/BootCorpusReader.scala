package services.movies

/** A boot-time reader of the whole corpus that takes the cache's boot hydrate read instead of
 *  making its own, so that a boot reads the corpus once.
 *
 *  Before this, a worker boot read every film with its `movie_slots` and `screenings` three
 *  times in its first two minutes (2026-10-04, worker-us, 2,142 films): the hydrate (9.4 s), the
 *  read-model projector's missing-card check (4.0 s, slots only) and the corpus census's first
 *  pass (12.5 s). Workers restart on nearly every deploy, so those reads were the largest cost
 *  left in a boot.
 *
 *  [[CaffeineMovieCache]] calls [[bootCorpus]] exactly once, from its boot hydrate:
 *   - `Some(rows)` when the read was COMPLETE. The rows are fully stitched, with slots and
 *     showtimes, as `MovieRepository.foreachRecord` delivers them.
 *   - `None` when no complete read was made. A partial read is never handed on: a reader that
 *     takes it for the corpus would publish or heal from a corpus that only looks smaller. On
 *     `None`, the reader reads for itself as it did before.
 *
 *  The rows are the hydrate's whole stitched corpus, the largest object a worker holds. A reader
 *  derives what it needs and lets them go. It must never keep them past its own pass. */
trait BootCorpusReader {
  def bootCorpus(read: Option[Seq[StoredMovieRecord]]): Unit
}
