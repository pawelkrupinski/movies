package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetAddress
import java.net.spi.InetAddressResolver.LookupPolicy
import java.time.Instant

/** ES "Ocine Gavarres" (tickets.ocinegavarres.es, 2026-11-10..15): two addresses, one dropping every connection, and the
 *  JDK's client tried only the first — 10 h of failed scrapes at a time while the other answered. A host's addresses are
 *  handed out reachable first, and a request whose connect fails goes again to the next (end to end, in a JVM of its
 *  own: AddressFailoverIntegrationSpec). */
class AddressFailoverSpec extends AnyFlatSpec with Matchers {
  private val t0      = Instant.parse("2026-11-15T10:00:00Z")
  private val policy  = LookupPolicy.of(LookupPolicy.IPV4)
  // built from their bytes: no lookup, no connection
  private val dead    = InetAddress.getByAddress(Array[Byte](91, 126, 240.toByte, 89))
  private val live    = InetAddress.getByAddress(Array[Byte](213.toByte, 97, 37, 173.toByte))
  private val both    = (_: String) => Seq(dead, live)

  "lookup" should "hand a host's addresses out in the resolver's order until one fails to connect, then behind the others" in {
    val clock    = new MutableClock(t0)
    val failover = new AddressFailover(clock, active = true)
    failover.lookup("tickets.ocinegavarres.es", policy, both) shouldBe Seq(dead, live)
    failover.connectFailed("tickets.ocinegavarres.es", t0) shouldBe true
    failover.lookup("Tickets.Ocinegavarres.es", policy, both) shouldBe Seq(live, dead)
    clock.setTo(t0.plus(AddressFailover.Remember).plusSeconds(1))
    failover.lookup("tickets.ocinegavarres.es", policy, both) shouldBe Seq(dead, live)
  }

  it should "keep a lookup for its cache time, and ask the resolver again after" in {
    val clock    = new MutableClock(t0)
    val failover = new AddressFailover(clock, active = true)
    var asked    = 0
    val counting = (_: String) => { asked += 1; Seq(dead, live) }
    failover.lookup("a.example", policy, counting)
    failover.lookup("a.example", policy, counting)
    asked shouldBe 1
    clock.setTo(t0.plus(AddressFailover.CacheFor))
    failover.lookup("a.example", policy, counting)
    asked shouldBe 2
  }

  "connectFailed" should "say no address is left once every one has failed, so a host down everywhere is not asked again" in {
    val failover = new AddressFailover(new MutableClock(t0), active = true)
    failover.lookup("tickets.ocinegavarres.es", policy, both)
    failover.connectFailed("tickets.ocinegavarres.es", t0) shouldBe true
    failover.connectFailed("tickets.ocinegavarres.es", t0) shouldBe false
    failover.connectFailed("one.example", t0) shouldBe false // never resolved here: nothing known to try next
  }

  it should "not blame the next address for a connect begun before the order moved" in {
    val clock    = new MutableClock(t0)
    val failover = new AddressFailover(clock, active = true)
    failover.lookup("tickets.ocinegavarres.es", policy, both)
    clock.setTo(t0.plusSeconds(2))
    failover.connectFailed("tickets.ocinegavarres.es", t0.plusSeconds(1)) shouldBe true // a connect begun at +1 s
    failover.connectFailed("tickets.ocinegavarres.es", t0) shouldBe true                 // one begun at 0, to the same dead address
    failover.lookup("tickets.ocinegavarres.es", policy, both) shouldBe Seq(live, dead)
  }

  it should "move nothing in a JVM that keeps lookups itself, where which address a connect went to is unknown" in {
    val failover = new AddressFailover(new MutableClock(t0), active = false)
    failover.lookup("tickets.ocinegavarres.es", policy, both)
    failover.connectFailed("tickets.ocinegavarres.es", t0) shouldBe false
    failover.lookup("tickets.ocinegavarres.es", policy, both) shouldBe Seq(dead, live)
  }

  "jvmCachesLookups" should "read the JDK's cache TTL as the JDK does: the security property, else the system one, else 30 s" in {
    AddressFailover.jvmCachesLookups(_ => None, _ => None) shouldBe true
    AddressFailover.jvmCachesLookups(Map("networkaddress.cache.ttl" -> "0").get, _ => None) shouldBe false
    AddressFailover.jvmCachesLookups(_ => None, Map("sun.net.inetaddr.ttl" -> "0").get) shouldBe false
    AddressFailover.jvmCachesLookups(Map("networkaddress.cache.ttl" -> "30").get, Map("sun.net.inetaddr.ttl" -> "0").get) shouldBe true
  }
}
