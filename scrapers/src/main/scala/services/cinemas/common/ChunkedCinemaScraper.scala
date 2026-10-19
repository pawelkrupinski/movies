package services.cinemas.common

import models.{CinemaMovie, Showtime}

/** One chunked scrape's plan: the chunk keys, and — only when there are none — whether the
 *  source said so itself. Built through [[ChunkPlan.of]] or [[ChunkPlan.NoScheduleListed]], so
 *  a plan with keys can never claim the venue has nothing on. */
final case class ChunkPlan private (keys: Seq[String], noScheduleListed: Boolean)

object ChunkPlan {
  /** Keys, empty or not, that vouch for nothing beyond themselves. */
  def of(keys: Seq[String]): ChunkPlan = ChunkPlan(keys, noScheduleListed = false)

  /** The venue's page parsed and says it has no schedule. */
  val NoScheduleListed: ChunkPlan = ChunkPlan(Seq.empty, noScheduleListed = true)
}

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

  /** [[planChunks]], and whether an empty plan is the source affirmatively listing no
   *  schedule (see `CinemaScraper.noScheduleListed`). What the production planner calls.
   *  The default vouches for nothing; override only where the page itself says it has
   *  nothing on, so a drifted page cannot pass for a closed venue. */
  def planSchedule(): ChunkPlan = ChunkPlan.of(planChunks())

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

/** A chunked scraper whose chunk is ONE fetched page, so fetching and parsing come apart: a page
 *  identical to the one it parsed last time need not be parsed again (`ScrapeChunkHandler`, through
 *  [[services.tasks.ChunkPageMemo]]). `pageParser` names what [[parseChunkPage]] makes of a page — it
 *  must change with any change to that, or a remembered parse outlives the code that made it. */
trait PagedChunkScraper extends ChunkedCinemaScraper {
  /** The chunk's page. Throws as [[fetchChunk]] does. */
  def fetchChunkPage(key: String): String
  /** What a page of chunk `key` holds. */
  def parseChunkPage(key: String, page: String): Seq[CinemaMovie]
  def pageParser: String
  final def fetchChunk(key: String): Seq[CinemaMovie] = parseChunkPage(key, fetchChunkPage(key))
}

object ChunkedCinemaScraper {
  /** Group films by `filmUrl` (falling back to title), union + dedupe + sort
   *  their showtimes, keep the first occurrence's film metadata. Deterministic
   *  (sorted by the grouping key) so the in-process and task paths agree.
   *  `sameShowtime` is what makes two showtimes one; by default everything a
   *  showtime carries. */
  def mergeByIdentity(movies: Seq[CinemaMovie],
                      sameShowtime: Showtime => Any = s => (s.dateTime, s.bookingUrl, s.room, s.format)): Seq[CinemaMovie] =
    movies.groupBy(m => m.filmUrl.getOrElse(m.movie.title)).toSeq
      .sortBy(_._1)
      .map { case (_, group) =>
        group.head.copy(showtimes = group.flatMap(_.showtimes).distinctBy(sameShowtime).sortBy(_.dateTime))
      }
}
