package services.tasks

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

/** One active chunked-scrape run for a cinema (keyed by cinema displayName). */
case class ChunkRun(cinema: String, runId: String, expectedKeys: Seq[String], createdAt: Instant) {
  def isStale(now: Instant, staleAfter: FiniteDuration): Boolean =
    createdAt.isBefore(now.minusMillis(staleAfter.toMillis))
}

/**
 * Coordination + partial-data store for chunked scrapes. Two jobs:
 *
 *  - A per-cinema MUTEX + generation token. `startRun` atomically refuses to
 *    start a second run while one is active and not stale, stamping a unique
 *    `runId`. Every chunk/reduce task carries that runId and is dropped when it
 *    no longer matches `activeRun`, so an overlapping or superseding re-scrape
 *    can never mix data or double-publish — the conflict-prevention requirement.
 *  - Durable partial results. Each chunk's slice is stored under
 *    `(cinema, runId, key)`, so a retried/redelivered chunk is idempotent and the
 *    reduce reads whatever has landed (incl. a partial set on timeout).
 *
 * Two implementations share this contract: [[MongoChunkScrapeStore]] (durable,
 * multi-instance-safe) and [[InMemoryChunkScrapeStore]] (tests / Mongo-less dev
 * + the deterministic fixture harness).
 *
 * MULTIPLE WORKER INSTANCES + RESTARTS. All run/chunk state lives HERE (shared
 * Mongo in prod), never in instance memory — so neither "duplicate the data on
 * each instance" nor a designated single-manager instance is needed; any
 * instance can fetch any chunk and write its slice, and any instance can reduce.
 * The guarantees:
 *   - One logical run per cinema: `startRun`'s atomic `_id = cinema` upsert means
 *     only one instance's run wins, even if several plan at once.
 *   - One processor per chunk: the [[TaskQueue]] hands each `ScrapeChunk` task to
 *     exactly one instance (atomic claim); a crashed holder's lease is reaped and
 *     the task re-claimed elsewhere — the already-stored slices persist here, so
 *     no work is redone beyond the in-flight chunk.
 *   - One publish per run: the reduce task is dedup'd in the queue, and whichever
 *     instance finishes the last chunk (reading this shared store) enqueues it; a
 *     completion seen by no instance (split across instances, or lost to a
 *     restart) is recovered by the cluster-claimed `ChunkScrapeReaper` backstop.
 *   - Restart-safe: an instance dying mid-run loses nothing — the run doc + landed
 *     slices + queued chunk tasks all survive here; a surviving (or rebooted)
 *     instance's coordinator/reaper drives the run to its reduce, or partial-reduces
 *     it once abandoned. Freshness is marked only at reduce, so a fully-lost run is
 *     simply re-scraped after the scrape TTL.
 */
trait ChunkScrapeStore {
  /** Start a run for `cinema` over `expectedKeys`, returning its fresh runId —
   *  UNLESS an active, non-stale run already exists (then None: one run at a
   *  time). A stale (abandoned) active run is superseded by the new one. */
  def startRun(cinema: String, expectedKeys: Seq[String], now: Instant, staleAfter: FiniteDuration): Option[String]

  /** The cinema's current active run, if any. */
  def activeRun(cinema: String): Option[ChunkRun]

  /** Store (idempotently) one chunk's serialised slice. A no-op if the run is no
   *  longer the cinema's active run. */
  def storeChunk(cinema: String, runId: String, key: String, valueJson: String, now: Instant): Unit

  /** The chunk keys stored so far for the run. */
  def storedKeys(cinema: String, runId: String): Set[String]

  /** key -> serialised slice for every stored chunk of the run. */
  def loadChunks(cinema: String, runId: String): Map[String, String]

  /** Every active run — for the backstop reaper to check completeness/staleness. */
  def activeRuns(): Seq[ChunkRun]

  /** Finish the run: drop its chunk slices and clear the active marker. A no-op
   *  unless `runId` is still the cinema's active run. */
  def completeRun(cinema: String, runId: String): Unit
}

/**
 * In-memory `ChunkScrapeStore` for tests, Mongo-less dev, and the deterministic
 * fixture harness. Mirrors [[MongoChunkScrapeStore]]'s semantics; one monitor
 * guards all state. runIds are a per-store counter (deterministic for tests);
 * the Mongo store uses UUIDs.
 */
class InMemoryChunkScrapeStore extends ChunkScrapeStore {
  private case class State(run: ChunkRun, chunks: Map[String, String])
  private val byCinema = scala.collection.mutable.Map.empty[String, State]
  private val lock     = new Object
  private var counter  = 0L

  def startRun(cinema: String, expectedKeys: Seq[String], now: Instant, staleAfter: FiniteDuration): Option[String] =
    lock.synchronized {
      byCinema.get(cinema) match {
        case Some(State(r, _)) if !r.isStale(now, staleAfter) => None
        case _ =>
          counter += 1
          val runId = s"$cinema#$counter"
          byCinema.put(cinema, State(ChunkRun(cinema, runId, expectedKeys, now), Map.empty))
          Some(runId)
      }
    }

  def activeRun(cinema: String): Option[ChunkRun] = lock.synchronized(byCinema.get(cinema).map(_.run))

  def storeChunk(cinema: String, runId: String, key: String, valueJson: String, now: Instant): Unit =
    lock.synchronized {
      byCinema.get(cinema).foreach { st =>
        if (st.run.runId == runId) byCinema.put(cinema, st.copy(chunks = st.chunks.updated(key, valueJson)))
      }
    }

  def storedKeys(cinema: String, runId: String): Set[String] = lock.synchronized {
    byCinema.get(cinema).filter(_.run.runId == runId).map(_.chunks.keySet.toSet).getOrElse(Set.empty)
  }

  def loadChunks(cinema: String, runId: String): Map[String, String] = lock.synchronized {
    byCinema.get(cinema).filter(_.run.runId == runId).map(_.chunks).getOrElse(Map.empty)
  }

  def activeRuns(): Seq[ChunkRun] = lock.synchronized(byCinema.values.map(_.run).toVector)

  def completeRun(cinema: String, runId: String): Unit = lock.synchronized {
    if (byCinema.get(cinema).exists(_.run.runId == runId)) byCinema.remove(cinema)
  }
}
