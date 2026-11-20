package tools

import java.net.InetAddress
import java.net.spi.InetAddressResolver.LookupPolicy
import java.time.{Clock, Duration, Instant}
import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*

/** WHICH OF A HOST'S ADDRESSES THE JVM CONNECTS TO, and the next one when that one will not connect.
 *
 *  The JDK's HttpClient connects to the FIRST address a host resolves to and fails when that one does not answer; it
 *  never tries the others (`PlainHttpConnection`: one `InetSocketAddress`, resolved once). ES "Ocine Gavarres"
 *  (tickets.ocinegavarres.es, 2026-11-10..15) resolves to two addresses, one of which drops every connection: the
 *  worker's scrapes of it failed for 10 h at a time (WorkerHttpHostCircuitOpen) while the other address answered.
 *
 *  So every lookup the JVM makes goes through here ([[FailoverAddressResolverProvider]], the JDK's resolver SPI): a
 *  host's addresses in the order its resolver gave, each one that failed to connect within [[remember]] moved behind
 *  those that did not. [[RealHttpFetch]] reports a connect failure ([[connectFailed]]) and asks the same request again,
 *  [[AddressFailover.MostAddressesTried]] times at most, and the JVM's next connect resolves the next address.
 *
 *  That needs the JVM to ask on every connect: its own lookup cache (`networkaddress.cache.ttl`, 30 s by default)
 *  would hand the failed address out again until it expired. The image sets it to 0 (Dockerfile, the linked runtime's
 *  `java.security`) and this class keeps the lookups instead, for [[cacheFor]]. A JVM that still caches gets the order
 *  but no failover ([[active]] false): which address it tried is then unknown, and blaming the wrong one would move a
 *  live address behind a dead one. Through a proxy the proxy resolves, and nothing here applies. */
final class AddressFailover(clock: Clock, val active: Boolean, remember: Duration = AddressFailover.Remember,
                            cacheFor: Duration = AddressFailover.CacheFor) {
  private final case class Resolved(addresses: Seq[InetAddress], until: Instant)
  /** Each lookup, by (host, policy): handed out for [[cacheFor]], and kept [[remember]] past its write for
   *  [[connectFailed]], which reads the addresses a connect began from however long ago it resolved them. Every host the
   *  JVM ever connected to was held for its life, the worker's poster and venue hosts by the thousand; now a host not
   *  asked for within [[remember]] is let go, and at most [[AddressFailover.MostHostsHeld]] are held at once. */
  private val lookups     = held[(String, Int), Resolved]
  private val resolved    = lookups.asMap()
  /** The addresses whose connect failed, and when (the latest failure). */
  private val unreachable = new ConcurrentHashMap[InetAddress, Instant]()
  /** When an address was last found unreachable, by host — what [[connectFailed]] reads to tell a stale failure apart:
   *  a connect older than [[remember]] is none, so the entry is let go after it, as a lookup is. */
  private val lastMoved   = held[String, Instant].asMap()

  private def held[K, V]: com.github.benmanes.caffeine.cache.Cache[K, V] =
    BoundedCache.ofSize(AddressFailover.MostHostsHeld)
      .expireAfterWrite(if (remember.compareTo(cacheFor) >= 0) remember else cacheFor)
      .ticker(() => clock.millis() * 1000000L).build[K, V]()

  /** How many lookups are held, the expired let go first: what bounds the failover's memory. */
  private[tools] def heldLookups: Long = { lookups.cleanUp(); lookups.estimatedSize() }

  def now(): Instant = clock.instant()

  /** `host`'s addresses, reachable first: `resolve`'s answer, kept for [[cacheFor]]. */
  def lookup(host: String, policy: LookupPolicy, resolve: String => Seq[InetAddress]): Seq[InetAddress] = {
    val key  = (host.toLowerCase(Locale.ROOT), policy.characteristics())
    val at   = now()
    val held = Option(resolved.get(key)).filter(_.until.isAfter(at)).getOrElse {
      val fresh = Resolved(resolve(host), at.plus(cacheFor))
      resolved.put(key, fresh)
      fresh
    }
    ordered(held.addresses, at)
  }

  /** A connect to `host`, begun at `startedAt`, failed: the address it went to — the first of the host's order, unless
   *  the order moved since it began (another connect's failure already moved that address back) — is unreachable for
   *  [[remember]]. Whether another of the host's addresses is still to be tried. */
  def connectFailed(host: String, startedAt: Instant): Boolean = active && {
    val name      = host.toLowerCase(Locale.ROOT)
    val addresses = resolved.asScala.collect { case ((h, _), r) if h == name => r.addresses }.flatten.toSeq.distinct
    val at        = now()
    if (addresses.nonEmpty && !Option(lastMoved.get(name)).exists(_.isAfter(startedAt))) {
      ordered(addresses, at).headOption.foreach { tried =>
        unreachable.put(tried, at)
        lastMoved.put(name, at)
      }
      unreachable.entrySet.removeIf(e => !failedRecently(e.getValue, at))
    }
    addresses.exists(a => !Option(unreachable.get(a)).exists(failedRecently(_, at)))
  }

  private def failedRecently(failedAt: Instant, at: Instant): Boolean = failedAt.plus(remember).isAfter(at)

  /** Reachable first, each group in the resolver's order; of the unreachable, the longest-failed first. */
  private def ordered(addresses: Seq[InetAddress], at: Instant): Seq[InetAddress] = {
    val (down, up) = addresses.partition(a => Option(unreachable.get(a)).exists(failedRecently(_, at)))
    up ++ down.sortBy(unreachable.get)
  }
}

object AddressFailover {
  /** How long an address that would not connect stays behind its host's others. */
  val Remember: Duration = Duration.ofMinutes(30)
  /** How long a lookup is kept: the JDK's own default positive TTL, which the image turns off in its favour. */
  val CacheFor: Duration = Duration.ofSeconds(30)
  /** The most addresses one request is sent to (the first and two more): a host down at every address fails after
   *  its own, and a round-robin set of dozens costs no more than three connect budgets. */
  val MostAddressesTried: Int = 3
  /** The most hosts whose lookups (and last failure) are held at once: well past the hosts a worker connects to within
   *  [[Remember]] (every venue of its country and their poster hosts, some hundreds), so only a burst beyond it evicts. */
  val MostHostsHeld: Long = 10000

  /** Whether the JVM keeps lookups itself: `networkaddress.cache.ttl` (else `sun.net.inetaddr.ttl`) anything but 0,
   *  its default 30 s included — read as `sun.net.InetAddressCachePolicy` reads them. */
  def jvmCachesLookups(security: String => Option[String] = name => Option(java.security.Security.getProperty(name)),
                       system: String => Option[String] = name => Option(System.getProperty(name))): Boolean =
    security("networkaddress.cache.ttl").orElse(system("sun.net.inetaddr.ttl")).flatMap(_.trim.toIntOption).forall(_ != 0)

}

/** A resolver answering `base`'s lookups through `failover`: the seam is `base` — the JDK's built-in resolver in
 *  production, a fixed table in AddressFailoverSpec's own JVM. */
final class FailoverAddressResolver(failover: AddressFailover, base: java.net.spi.InetAddressResolver)
    extends java.net.spi.InetAddressResolver {
  override def lookupByName(host: String, policy: LookupPolicy): java.util.stream.Stream[InetAddress] =
    failover.lookup(host, policy, name => base.lookupByName(name, policy).iterator.asScala.toSeq).asJava.stream()
  override def lookupByAddress(address: Array[Byte]): String = base.lookupByAddress(address)
}

/** The JDK resolver SPI entry (META-INF/services/java.net.spi.InetAddressResolverProvider): the built-in resolver's
 *  answers, through [[FailoverAddressResolverProvider.failover]]. */
final class FailoverAddressResolverProvider extends java.net.spi.InetAddressResolverProvider {
  override def get(configuration: java.net.spi.InetAddressResolverProvider.Configuration): java.net.spi.InetAddressResolver =
    new FailoverAddressResolver(FailoverAddressResolverProvider.failover, configuration.builtinResolver())
  override def name(): String = "kinowo-address-failover"
}

object FailoverAddressResolverProvider {
  /** The JVM's one failover state. The JDK's resolver is the process's by its own design (one provider, loaded once by
   *  `ServiceLoader`, no composition root to be handed anything), so the state it consults is the process's too, and
   *  this is its composition root: its one clock. [[RealHttpFetch]] reports to the same instance. */
  lazy val failover: AddressFailover =
    new AddressFailover(java.time.Clock.systemUTC(), active = !AddressFailover.jvmCachesLookups())
}
