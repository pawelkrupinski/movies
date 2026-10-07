package integration

import models.CineworldGreenwich
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{CatalogueId, Listing, ResolverDecision}
import services.movies.ListingKey
import tools.{HostPacing, HttpFetch, HttpStatusException}

import java.nio.file.Files
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger

/** The capture's read-ahead of the venue pages its recorded tree lacks: paced like every other live read of the capture
 *  — one per-host budget shared between the countries run side by side, a 429 or 503 retried — and kept for the next
 *  run's `LiveGapLeaf` to answer; and which of the gaps it met it reads beside a page. */
class LiveGapLeafReadPagesSpec extends AnyFlatSpec with Matchers {

  import IdentityShadow.LiveGapLeaf

  private def pages(n: Int) = {
    val host = s"read-pages-${java.util.UUID.randomUUID()}.example"
    (1 to n).map(i => s"https://$host/film/$i")
  }
  /** A GET-only host: the read-ahead posts nothing. */
  private abstract class Pages extends HttpFetch {
    def post(url: String, body: String, contentType: String): String = throw new UnsupportedOperationException(url)
  }
  private def forget(urls: Seq[String]): Unit = urls.foreach(url => Files.deleteIfExists(LiveGapLeaf.fileOf(s"GET $url")))

  "the venue-page read-ahead" should "retry a page the host throttled, and keep it" in {
    val urls    = pages(1)
    val refused = new AtomicInteger
    val fetch = new Pages {
      def get(url: String): String =
        if (refused.getAndIncrement() == 0) throw new HttpStatusException(429, "GET", url, None) else s"<html>$url</html>"
    }
    try {
      LiveGapLeaf.readPages(urls, new HostPacing(budget = 4, sleep = _ => ()), fetch) shouldBe 1
      Files.readString(LiveGapLeaf.fileOf(s"GET ${urls.head}")) shouldBe s"<html>${urls.head}</html>"
    } finally forget(urls)
  }

  it should "read no more pages of a host at once than the run's share of the budget" in {
    val urls   = pages(6)
    val inside = new AtomicInteger
    val peak   = new AtomicInteger
    val seen   = ConcurrentHashMap.newKeySet[String]()
    val fetch = new Pages {
      def get(url: String): String = {
        peak.accumulateAndGet(inside.incrementAndGet(), math.max)
        try { seen.add(url); url } finally inside.decrementAndGet()
      }
    }
    try {
      LiveGapLeaf.readPages(urls, new HostPacing(budget = 1, sleep = _ => ()), fetch) shouldBe 6
      seen.size shouldBe 6
      peak.get shouldBe 1
    } finally forget(urls)
  }

  private def listing(page: String, catalogueIds: CatalogueId*) = {
    val title = page.stripSuffix("/").split('/').last
    Listing(CineworldGreenwich, ListingKey.Published(CineworldGreenwich.displayName, title, None, Nil), title, title, title, None, Nil, None,
      Some(page), None, catalogueIds = catalogueIds)
  }

  "the venue-page gaps a capture reads" should "be those ending in a listing's page slug" in {
    val alamo = listing("https://drafthouse.com/austin/show/the-shining-45th")
    LiveGapLeaf.gapsOf(Seq(alamo), Seq(
      "https://drafthouse.com/s/mother/v2/core/presentation/the-shining-45th",
      "https://drafthouse.com/s/mother/v2/core/presentation/another-film")) shouldBe
      Seq("https://drafthouse.com/s/mother/v2/core/presentation/the-shining-45th")
  }

  it should "include those naming one of its catalogue ids in the API's ids= form (Cineworld's detail, off its page)" in {
    val macbeth = listing("https://www.cineworld.co.uk/films/1000043020-rbo-cinema-season-2026-27-macbeth",
      CatalogueId("boxoffice", "1000043020"))
    val api = "https://www.cineworld.co.uk/api/gatsby-source-boxofficeapi/movies?basic=false&castingLimit=10&ids="
    LiveGapLeaf.gapsOf(Seq(macbeth), Seq(s"${api}1000043020", s"${api}1000052295", s"${api}10000430201")) shouldBe
      Seq(s"${api}1000043020")
  }

  "the venue pages a capture reads ahead" should "be the unread ones of its captured clusters and of the model's POOLED takes — " +
    "where a venue's facts can still veto the take — never of a member's own match" in {
    val page = "https://www.flicks.co.uk/movie/"
    val captured = listing(s"${page}rbo-cinema-season-2026-27-la-fanciulla-del-west/")
    val pooled   = listing(s"${page}cbeebies-panto-2026-treasure-island/")
    val own      = listing(s"${page}the-shining/")
    val read     = listing(s"${page}cbeebies-panto-2026-treasure-island-relaxed/")
    def decision(basis: ResolverDecision.Basis, film: Option[Int], members: Listing*) =
      ResolverDecision(members.map(_.key), film, 0.9, basis, Nil)()
    val decisions = Seq(decision(ResolverDecision.Basis.BelowThreshold, None, captured),
      decision(ResolverDecision.Basis.PooledMatch, Some(6646), pooled, read), decision(ResolverDecision.Basis.OwnMatch, Some(694), own))
    LiveGapLeaf.readAhead(decisions, Set(captured.key), Seq(captured, pooled, own, read), _ != read) shouldBe Seq(captured, pooled)
  }
}
