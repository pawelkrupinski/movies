package services.cinemas

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.GetOnlyHttpFetch

import java.net.URI
import java.time.LocalDate
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The diagnostic catalogue (`FilmwebDiff`) reaches the venues that refuse a datacenter runner —
 * Multikino, biletyna, Kino Kryterium — through the residential proxy it is handed, as the worker
 * does, and without one goes straight to `http`: never Zyte on its own.
 */
class DiagnosticCatalogEgressSpec extends AnyFlatSpec with Matchers {

  /** Records the host of every request and fails it like a dead tunnel, so the chain moves on. */
  private final class HostLog extends GetOnlyHttpFetch {
    private val seen = new ConcurrentLinkedQueue[String]
    def hosts: Set[String] = seen.asScala.toSet
    override def get(url: String): String = {
      seen.add(URI.create(url).getHost)
      throw new java.io.IOException("proxy: Tunnel failed, got: 503")
    }
  }

  private val Refusing = Seq(MultikinoKoszalin, KinoKameralne, KinoKryterium)
  private val RefusingHosts = Set("www.multikino.pl", "biletyna.pl", "bilety.ck105.koszalin.pl")

  private def scrapeRefusing(http: HostLog, proxy: Option[HostLog]): Unit = {
    val catalog = new CinemaScraperCatalog(http, LocalDate.of(2026, 10, 2),
      titles = services.movies.TitleNormalizer.forCountry(Country.Poland), proxyShards = proxy.map(IndexedSeq(_)))
    catalog.all.filter(s => Refusing.contains(s.cinema)).foreach(s => Try(s.fetch()))
  }

  "The diagnostic catalogue" should "ask the residential proxy first for the venues that refuse a runner" in {
    val proxy = new HostLog
    scrapeRefusing(new HostLog, Some(proxy))
    proxy.hosts should contain allElementsOf RefusingHosts
  }

  it should "reach those venues directly when it has no proxy" in {
    val http = new HostLog
    scrapeRefusing(http, proxy = None)
    http.hosts should contain allElementsOf RefusingHosts
  }
}
