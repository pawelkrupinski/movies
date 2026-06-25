package services.cinemas

import models.CinemaMovie

/**
 * A cinema whose scrape fans out over many independent chunks (per-day pages,
 * per-event pages). Production runs each chunk as its own queued `ScrapeChunk`
 * task and aggregates them with a final `ScrapeChunkReduce` task (see
 * `ChunkScrapeStore` / `ScrapeChunkHandler` / `ScrapeChunkReduceHandler`); the
 * synchronous `fetch()` below composes the SAME three functions in-process, so
 * any non-task caller (the deterministic fixture harness, a client unit test)
 * gets identical output.
 *
 * A conversion is therefore behaviour-preserving iff
 * `reduceChunks ∘ fetchChunk ∘ planChunks` equals the old monolithic `fetch()`.
 */
trait ChunkedCinemaScraper extends CinemaScraper {

  /** Enumerate the chunk keys for one scrape, known upfront. May fetch a nav /
   *  index page (whose failure fails the whole scrape, recorded as a normal
   *  outcome). Each key must map to an independently-fetchable unit. */
  def planChunks(): Seq[String]

  /** Fetch + parse ONE chunk into its slice of the listing. Must be independent
   *  of the other chunks — any cross-chunk merge belongs in `reduceChunks`. A
   *  throw reschedules just this chunk's task (the per-chunk retry), so don't
   *  swallow a failure you'd want retried. */
  def fetchChunk(key: String): Seq[CinemaMovie]

  /** Aggregate every chunk's slice into the cinema's full listing. The default
   *  merges films by identity (`filmUrl`, else title) and unions their
   *  showtimes — the shape every per-day client's final step already uses.
   *  Override for a bespoke grouping key. */
  def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    ChunkedCinemaScraper.mergeByIdentity(chunks.toSeq.sortBy(_._1).flatMap(_._2))

  final def fetch(): Seq[CinemaMovie] =
    reduceChunks(planChunks().map(k => k -> fetchChunk(k)).toMap)
}

object ChunkedCinemaScraper {
  /** Group films by `filmUrl` (falling back to title), union + dedupe + sort
   *  their showtimes, keep the first occurrence's film metadata. Deterministic
   *  (sorted by the grouping key) so the in-process and task paths agree. */
  def mergeByIdentity(movies: Seq[CinemaMovie]): Seq[CinemaMovie] =
    movies.groupBy(m => m.filmUrl.getOrElse(m.movie.title)).toSeq
      .sortBy(_._1)
      .map { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes)
          .distinctBy(s => (s.dateTime, s.bookingUrl, s.room, s.format))
          .sortBy(_.dateTime)
        group.head.copy(showtimes = showtimes)
      }
}
