package tools

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/**
 * Fetches, within a time budget, the requests a hermetic leg's recorded tree could not answer
 * ([[MissingFixtures.Refetch]]), into a fixture tree of their own — the half of "every convergence
 * build publishes the missing data it found" that does the fetching (`FillMissingFixtures`, the
 * leg's rows run before their suites, and the hand-dispatched `Convergence fill`).
 *
 * The policy lives here, above the two seams a run hands in, so a spec drives it with an in-memory
 * fetch: `held` says whether an earlier fill already holds a request (never asked twice), and
 * `fetch` is the RECORDING chain the recorder itself writes through — the per-host pace, 429 gate and
 * breaker production asks every host with (`HttpWiring.pacedWire`), behind `RecordingHttpFetch` and the
 * remembered-verdict cache — so what it writes is byte for byte what a recording would have written.
 *
 * Interleaved by host, so one slowly paced host (Flicks, 200 ms a request) never holds the others
 * back, and stopped at the budget: a request not started by then is left for the next leg's fill.
 */
final class MissingFixtureFill(held: MissingFixtures.Refetch => Boolean, fetch: HttpFetch, threads: Int,
                               nanoTime: () => Long = () => System.nanoTime(),
                               sign: String => Option[(String, Map[String, String])] = FillCredentials(None).sign) {
  import MissingFixtureFill.Outcome

  def fill(gaps: Seq[MissingFixtures.Refetch], budget: FiniteDuration): Outcome = {
    val order    = MissingFixtureFill.interleavedByHost(gaps).toIndexedSeq
    val deadline = nanoTime() + budget.toNanos
    val next     = new AtomicInteger(0)
    val fetched, failed, alreadyHeld, unsigned = new AtomicInteger(0)
    val failures = new java.util.concurrent.ConcurrentHashMap[String, AtomicInteger]()

    def work(): Unit = {
      var index = next.getAndIncrement()
      while (index < order.size && nanoTime() < deadline) {
        val gap = order(index)
        if (held(gap)) alreadyHeld.incrementAndGet()
        else sign(gap.url) match {
          // masks a credential this fill holds no key for: never asked with the mask in it
          case None => unsigned.incrementAndGet()
          case Some((url, headers)) => try {
            gap.body match {
              case Some(body)                => fetch.post(url, body.text, body.contentType)
              case None if gap.verb == "BYTES" => fetch.getBytes(url)
              case None if headers.isEmpty   => fetch.get(url)
              case None                      => fetch.get(url, headers)
            }
            fetched.incrementAndGet()
          } catch {
          case NonFatal(e) =>
            failed.incrementAndGet()
              failures.computeIfAbsent(e.getClass.getSimpleName, _ => new AtomicInteger(0)).incrementAndGet()
          }
        }
        index = next.getAndIncrement()
      }
    }

    val pool = DaemonExecutors.boundedEC("missing-fixture-fill", threads.max(1))
    try {
      (1 to threads.max(1)).foreach(_ => pool.submit(new Runnable { def run(): Unit = work() }))
      pool.shutdown()
      // Past the budget, a worker finishes only the request it is in — its pace slot and its host's timeout.
      pool.awaitTermination(budget.toMillis + MissingFixtureFill.Grace.toMillis, TimeUnit.MILLISECONDS)
    } finally pool.shutdownNow()

    val asked = fetched.get + failed.get + alreadyHeld.get + unsigned.get
    Outcome(gaps.size, fetched.get, failed.get, alreadyHeld.get, unsigned.get, (order.size - asked).max(0),
      failures.entrySet().toArray(Array.empty[java.util.Map.Entry[String, AtomicInteger]]).map(e => e.getKey -> e.getValue.get).toMap)
  }
}

object MissingFixtureFill {

  /** How long a request still in flight at the budget may take to finish. */
  val Grace: FiniteDuration = FiniteDuration(60, TimeUnit.SECONDS)

  /** What one fill did: of `listed` requests, `fetched` answered (and were recorded), `failed` did not
   *  (a durable verdict — a 404 — is remembered all the same), `alreadyHeld` an earlier fill had, `unsigned`
   *  masked a credential this fill holds no key for ([[FillCredentials]]), and `unreached` the budget ran out before. */
  final case class Outcome(listed: Int, fetched: Int, failed: Int, alreadyHeld: Int, unsigned: Int, unreached: Int, failures: Map[String, Int]) {
    def describe: String =
      s"$listed listed: $fetched fetched, $failed failed${if (failures.isEmpty) "" else failures.toSeq.sorted.map { case (k, n) => s"$k $n" }.mkString(" (", ", ", ")")}, " +
        s"$alreadyHeld already held, $unsigned needing a key this fill lacks, $unreached left for the next leg"
  }

  /** The chain a fill writes through: `ArchiveReplayWiring.recordedChain` into `tree` under `root` — the
   *  recorder's own, so a fetched page lands where a hermetic replay looks for it, and a durable verdict
   *  (a 404) is remembered beside it as a recording remembers one. A transient failure is not: the next
   *  leg's fill asks again. */
  def recordingInto(root: settings.FixtureRoot, tree: String, live: HttpFetch): HttpFetch = {
    val cache = new EnrichmentCache(new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(root, tree)),
      persistSuccesses = false, transients = EnrichmentCache.Transients.Forgotten)
    ArchiveReplayWiring.recordedChain(tree, root, Some(cache), live, "fill-fixtures", "fill-live")
  }

  /** Whether `tree` under `root` — the fills earlier legs published — already answers a request: a recorded
   *  response, or a remembered verdict. */
  def heldIn(root: settings.FixtureRoot, tree: String): MissingFixtures.Refetch => Boolean = {
    val recorded = new clients.tools.FakeHttpFetch(tree, strict = true, foldYear = false, root = root)
    val verdicts = new EnrichmentCache(new FileEnrichmentCacheStore(FileEnrichmentCacheStore.beside(root, tree),
      FileEnrichmentCacheStore.NeverExpires), transients = EnrichmentCache.Transients.Replayed)
    verdicts.preload()
    gap => gap.body match {
      case Some(body) =>
        verdicts.lookup(CachingEnrichmentFetch.keyOf("POST", gap.url, Some(body.text))).isDefined ||
          scala.util.Try(recorded.post(gap.url, body.text, body.contentType).length).isSuccess
      case None =>
        Seq("BYTES", "GET").exists(verb => verdicts.lookup(CachingEnrichmentFetch.keyOf(verb, gap.url)).isDefined) ||
          scala.util.Try(if (gap.verb == "BYTES") recorded.getBytes(gap.url).length else recorded.get(gap.url).length).isSuccess
    }
  }

  /** Round-robin over the hosts, each host's requests in their listed order. */
  def interleavedByHost(gaps: Seq[MissingFixtures.Refetch]): Seq[MissingFixtures.Refetch] = {
    val byHost = gaps.groupBy(g => scala.util.Try(java.net.URI.create(g.url).getHost).toOption.flatMap(Option(_)).getOrElse(""))
      .toSeq.sortBy(_._1).map(_._2)
    val longest = if (byHost.isEmpty) 0 else byHost.map(_.size).max
    (0 until longest).flatMap(i => byHost.flatMap(_.lift(i)))
  }
}
