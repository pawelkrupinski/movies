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
 *  [[CaffeineMovieCache]] hands each page of its boot hydrate's read to [[bootPage]] as the page
 *  is read, fully stitched, with slots and showtimes, as `MovieRepository.foreachPage` delivers
 *  it; then it ends the read with [[bootReadEnded]]:
 *   - [[BootReadEnd.Whole]] when the read was COMPLETE and not empty: the pages are the corpus.
 *   - [[BootReadEnd.Retrying]] when it failed or came back empty and the hydrate reads again:
 *     forget its pages; the next read's pages follow.
 *   - [[BootReadEnd.GaveUp]] when no complete read was made and none will be. A partial read is
 *     never the corpus: a reader that takes it for one would publish or heal from a corpus that
 *     only looks smaller. The reader reads for itself as it did before.
 *  A backstop rehydrate later in the process's life is not the boot's, and offers nothing.
 *
 *  A page is a slice of the largest object a worker reads. A reader derives what it needs and
 *  lets the page go: the hydrate keeps each film without its showtimes, so a page dropped once
 *  it is read is all that stops a boot from holding — and, at boot's allocation rate, promoting
 *  into the old generation — every showtime of the corpus at once (2026-10-05, a worker-us boot
 *  replayed locally: 1.4M showtimes; streaming them cut its first 150 s of promotion ~800 → ~710 MB,
 *  and the old generation's allocation-failure collection with it). */
trait BootCorpusReader {
  def bootPage(rows: Seq[StoredMovieRecord]): Unit
  def bootReadEnded(end: BootReadEnd): Unit
}

/** How one of the boot hydrate's reads ended, for a [[BootCorpusReader]]. */
enum BootReadEnd {
  case Whole, Retrying, GaveUp
}
