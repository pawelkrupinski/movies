package tools

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * Proves a client fans its requests out concurrently without timing anything.
 *
 * Every GET holds its slot until `parties` requests have arrived (or `patience`
 * runs out), then answers 404 — the "no such page" a slug probe moves past. Run
 * concurrently, the first `parties` calls meet and `peakInFlight` reaches
 * `parties`; run one at a time, each call waits out its patience alone and the
 * peak stays at 1. The verdict is the peak, not a wall-clock bound, so a slow CI
 * runner cannot turn a sequential client green or a concurrent one red.
 */
final class RendezvousHttpFetch(parties: Int = 2, patience: Long = 2000L) extends GetOnlyHttpFetch {
  private val arrived  = new CountDownLatch(parties)
  private val inFlight = new AtomicInteger(0)
  private val peak     = new AtomicInteger(0)

  /** The most requests this fetch ever held at once. */
  def peakInFlight: Int = peak.get()

  override def get(url: String): String = {
    val now = inFlight.incrementAndGet()
    peak.updateAndGet(math.max(_, now))
    arrived.countDown()
    try { arrived.await(patience, TimeUnit.MILLISECONDS); UpstreamNotFound(url) }
    finally { inFlight.decrementAndGet(); () }
  }
}
