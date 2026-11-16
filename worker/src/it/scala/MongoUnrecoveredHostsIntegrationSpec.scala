package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{IntegrationMongoSuite, IsolatedMongoDatabase, MongoUnrecoveredHosts, UnrecoveredHosts}

import java.time.Instant

/** The breaker's unrecovered hosts in Mongo, per chain: what `InMemoryUnrecoveredHosts` keeps in one process, here kept
 *  for the worker's next boot. */
class MongoUnrecoveredHostsIntegrationSpec extends AnyFlatSpec with Matchers with IntegrationMongoSuite with BeforeAndAfterAll {
  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "http_unrecovered_hosts")
  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  "the unrecovered hosts" should "be kept per chain, refreshed in place, and forgotten when the host answers" in {
    val (scrape, enrich) = (new MongoUnrecoveredHosts(isolated.database, "scrape"), new MongoUnrecoveredHosts(isolated.database, "enrich"))
    val at = Instant.parse("2026-11-12T13:50:00Z")
    scrape.failed(UnrecoveredHosts.Host("kinoikar.mok-jar.pl", Some("SSLHandshakeException: PKIX"), at))
    scrape.failed(UnrecoveredHosts.Host("kinoikar.mok-jar.pl", Some("SSLHandshakeException: PKIX"), at.plusSeconds(300)))
    scrape.failed(UnrecoveredHosts.Host("bilety.kinoszczytno.pl", None, at))
    enrich.failed(UnrecoveredHosts.Host("restapi.helios.pl", Some("HttpTimeoutException"), at))

    // a new process's store over the same database reads what the last one left
    new MongoUnrecoveredHosts(isolated.database, "scrape").load() shouldBe Seq(
      UnrecoveredHosts.Host("bilety.kinoszczytno.pl", None, at),
      UnrecoveredHosts.Host("kinoikar.mok-jar.pl", Some("SSLHandshakeException: PKIX"), at.plusSeconds(300)))
    enrich.load().map(_.host) shouldBe Seq("restapi.helios.pl")

    scrape.recovered("kinoikar.mok-jar.pl")
    scrape.load().map(_.host) shouldBe Seq("bilety.kinoszczytno.pl")
    enrich.load().map(_.host) shouldBe Seq("restapi.helios.pl")
  }
}
