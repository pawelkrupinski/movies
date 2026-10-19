package services.cinemas.common

import java.time.LocalDate

/**
 * The chunk-key encoding for a [[ChunkedCinemaScraper]] that fetches its programme one
 * day at a time: consecutive days grouped into runs of [[PerChunk]], each run one chunk
 * key of comma-joined ISO dates.
 *
 * Grouping keeps a wide day list from multiplying chunk TASKS day for day (see
 * [[ScrapeHorizon.liveDays]]); one shared encoding keeps `planChunks` and `fetchChunk`
 * from drifting apart per client.
 */
object DayChunks {

  /** Days one chunk task covers. 7 ≈ a week: enough to cut the chunk/write count ~7×
   *  vs per-day, small enough that a chunk's slot time and its retry blast-radius stay
   *  bounded. */
  val PerChunk: Int = 7

  /** `days` → chunk keys, in order. */
  def keys(days: Seq[LocalDate]): Seq[String] =
    days.map(_.toString).grouped(PerChunk).map(_.mkString(",")).toSeq

  /** A key [[keys]] produced → its days. */
  def days(key: String): Seq[LocalDate] =
    key.split(",").toSeq.map(LocalDate.parse)
}
