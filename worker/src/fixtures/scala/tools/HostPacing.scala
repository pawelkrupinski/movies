package tools

import java.util.concurrent.ConcurrentHashMap
import scala.util.{Failure, Success, Try}

/**
 * The pacing of a capture's LIVE reads (`integration.IdentityShadow.LiveGapLeaf`, `integration.ExperimentCacheFetch`):
 * at most a host's limit at once — `budget` to start with — and on a 429 or a 503 the host's limit HALVED (never below
 * one) and the read retried after a growing back-off (`backOffMillis` × attempt), `retries` times at most. The host told
 * us to slow down; a halved limit stays halved for the rest of the run. A read whose connection was never made
 * ([[HostPacing.neverConnected]]) is retried the same way, its limit kept: the network blinked, the host said nothing.
 *
 * Runs side by side (`scripts/identity-capture.sh`'s countries) reach the same hosts from one machine: each is handed
 * its [[HostPacing.share]] of one budget (`KINOWO_IDENTITY_LIVE_PER_HOST`), so together they ask no more at once than
 * one run did.
 */
final class HostPacing(budget: Int, retries: Int = 6, backOffMillis: Long = 5000L, sleep: Long => Unit = Thread.sleep) {

  private final class Host {
    var limit: Int    = math.max(1, budget)
    var inFlight: Int = 0
  }
  private val hosts = new ConcurrentHashMap[String, Host]()

  private def host(name: String): Host = hosts.computeIfAbsent(name, _ => new Host)

  /** The host's limit now. */
  def limitOf(name: String): Int = { val h = host(name); h.synchronized(h.limit) }

  def apply[A](url: String)(read: => A): A = {
    val h = host(Option(java.net.URI.create(url).getHost).getOrElse(url))
    @scala.annotation.tailrec def attempt(n: Int): A = {
      h.synchronized { while (h.inFlight >= h.limit) h.wait(); h.inFlight += 1 }
      val result = try Try(read) finally h.synchronized { h.inFlight -= 1; h.notifyAll() }
      result match {
        case Success(answer) => answer
        case Failure(e: HttpStatusException) if HostPacing.Throttled(e.code) && n < retries =>
          h.synchronized { h.limit = math.max(1, h.limit / 2) }
          sleep(backOffMillis * (n + 1))
          attempt(n + 1)
        case Failure(e) if HostPacing.neverConnected(e) && n < retries =>
          sleep(backOffMillis * (n + 1))
          attempt(n + 1)
        case Failure(e) => throw e
      }
    }
    attempt(0)
  }
}

object HostPacing {
  /** The statuses a host slows us down with. */
  val Throttled: Set[Int] = Set(429, 503)

  /** A read that never reached the host — its connection refused or dropped before a request went out
   *  (`ConnectException`, however wrapped): safe to ask again, and a blip of the network, not the host's answer. A read
   *  that timed out is not one: the host may still be working on it. */
  def neverConnected(e: Throwable): Boolean =
    Iterator.iterate(e)(_.getCause).takeWhile(_ != null).take(8).exists(_.isInstanceOf[java.net.ConnectException])

  /** One run's share of a host budget `total` split between `runs` side by side: at least one each. */
  def share(total: Int, runs: Int): Int = math.max(1, total / math.max(1, runs))
}
