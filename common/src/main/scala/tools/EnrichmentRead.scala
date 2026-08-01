package tools

/**
 * The rule every enrichment client reads its upstream through: **an absent
 * answer is data; a failed read is not.**
 *
 * The rating clients (IMDb, Rotten Tomatoes, Metacritic, Filmweb) all resolve to
 * `Option` — `None` meaning "this film has no score". They used to reach that
 * `None` with a bare `Try(...).toOption`, which also swallowed every 403, 429,
 * 5xx and timeout. That makes a total upstream outage byte-for-byte identical to
 * a film nobody has rated, and the consequences are not cosmetic: `RatingHandler`
 * is written to treat a THROWN fetch as a failure (it records the attempt for the
 * /debug page, skips `freshness.markFresh`, skips the cadence report, and lets the
 * queue's retry/backoff see it) — but it only ever saw `Success(None)`, so a dead
 * source was booked as a healthy "checked, unchanged" refresh.
 *
 * That is exactly how IMDb's CDN block ran ~47 hours from 2026-07-30 with every
 * worker logging the entirely ordinary-looking `→ rating none`.
 *
 * Only 404 and 410 are an answer: the RT and Metacritic resolvers probe candidate
 * slugs and read a 404 as "that slug isn't a film", which is information the
 * ladder depends on. Everything else — a block, a throttle, a server error, a
 * timeout, a connection reset, or an unexpected bug — means we learned nothing,
 * and the caller must be told rather than handed a confident `None`.
 *
 * Kept in `common/tools` beside [[HttpStatusException]] so both the worker's
 * clients and any future reader share ONE definition of "the read failed" —
 * the same line [[HttpOutcome]] draws for the metrics and
 * [[MonitoringHttpFetch]] draws for /uptime.
 */
object EnrichmentRead {

  /** The HTTP codes that are a genuine "there is nothing here" answer. Everything
   *  outside this set is a failed read, including the rest of the 4xx range. */
  private val AbsentCodes: Set[Int] = Set(404, 410)

  /** A status carried in the message rather than the type. [[HttpStatusException]]
   *  deliberately keeps the `HTTP <code> for <method> <url>` message shape the code
   *  threw before it existed, and both [[MonitoringHttpFetch]]'s failure classifier
   *  and the client fakes rely on that shape — so a bare
   *  `RuntimeException("HTTP 404")` has to classify identically to the typed one.
   *  Matching only the type would treat those as failed reads and break the
   *  slug-probe ladders that read a 404 as "no such page". */
  private val StatusInMessage = """^HTTP (\d{3})\b""".r.unanchored

  /** The HTTP status a failure carries, however it carries it. */
  private def statusOf(failure: Throwable): Option[Int] = failure match {
    case e: HttpStatusException => Some(e.code)
    case e: RuntimeException if e.getMessage != null =>
      StatusInMessage.findFirstMatchIn(e.getMessage).map(_.group(1).toInt)
    case _ => None
  }

  /** Run `read`, turning only an upstream "not found" into `None`. Every other
   *  failure propagates unchanged, so the caller's own error handling — task
   *  retry/backoff, attempt recording, freshness — sees the failure it needs.
   *
   *  By-name so the read itself happens inside the guard. */
  def absentOnNotFound[A](read: => A): Option[A] =
    try Some(read)
    catch {
      case failure: Throwable if statusOf(failure).exists(AbsentCodes.contains) => None
    }
}
