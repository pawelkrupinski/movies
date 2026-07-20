package tools

/**
 * Thrown by [[RateLimitedHttpFetch]] when a paced host's outbound gate is
 * already backed up beyond that host's `maxGateWait` — i.e. so many requests
 * are queued on the pace that the next one would have to block for longer than
 * the cap. Rather than park a worker on that whole backlog (which is what pushed
 * the UK ScrapeChunk task-duration to the histogram ceiling in waves), the fetch
 * fails fast with this so a QUEUED caller ([[services.tasks.ScrapeChunkHandler]])
 * reschedules the unit of work with backoff — moving the wait from a blocked
 * worker into queue-time, where the reaper's exponential backoff naturally
 * spreads the overflow past the burst.
 *
 * It is deliberately NOT an [[HttpStatusException]]: nothing reached the wire, so
 * the circuit breaker must not count it as a host failure and `MonitoringHttpFetch`
 * must not read it as a 5xx/403/429 outage. A distinct type also lets the handler
 * log it as routine backpressure instead of a scary scrape failure.
 */
class PaceGateBackpressureException(host: String, backlogMillis: Long, capMillis: Long)
    extends RuntimeException(
      s"pace gate for $host backed up ${backlogMillis}ms > ${capMillis}ms cap — deferring"
    )
