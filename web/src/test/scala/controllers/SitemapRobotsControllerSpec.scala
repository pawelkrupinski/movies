package controllers

import models.{CityScreening, Helios, Multikino, MultikinoPasazGrunwaldzki, MovieRecord, ResolvedMovie, ResolvedRatings, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.readmodel.{InMemoryReadModelRepository, WebReadModel}

import java.time.{Instant, LocalDateTime}

/** End-to-end checks on the two crawl-control endpoints: robots.txt advertises
 *  the sitemap + fences off the operational noise (while keeping `Allow: /` for
 *  Facebook's scraper), and sitemap.xml enumerates the live corpus. */
class SitemapRobotsControllerSpec extends AnyFlatSpec with Matchers {

  private def controller(): MovieController = {
    val now = LocalDateTime.now()
    val rec = MovieRecord(
      imdbId = Some("tt1"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Testowy Film"),
          releaseYear = Some(2024),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil)),
        )
      )
    )
    TestMovieController.build(Seq(("Testowy Film", Some(2024), rec)))._1
  }

  // X-Forwarded-* mirror the Fly edge so PageMeta.origin yields the prod host.
  private def req(path: String) =
    FakeRequest(GET, path)
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "kinowo.net")

  "robots.txt" should "stay crawlable, advertise the sitemap, and fence off noise" in {
    val res  = controller().robotsTxt(req("/robots.txt"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("text/plain")
    val body = contentAsString(res)
    body should include("User-agent: *")
    body should include("Allow: /")
    body should include("Sitemap: https://kinowo.net/sitemap.xml")
    body should include("Disallow: /debug")
    body should include("Disallow: /admin")
    body should include("Disallow: /*/api/")
    // og:image PNGs must stay crawlable — Facebook honours robots.txt for them.
    body should not include "og-image"
  }

  it should "fence off the browse facets, which the sitemap deliberately omits" in {
    val body = contentAsString(controller().robotsTxt(req("/robots.txt")))
    body should include("Disallow: /*/movies")
    // The pre-rename address 301s onto that one rather than 404ing, so a
    // crawler that already has it in its frontier would spend the same budget
    // walking the redirects. Both spellings stay fenced.
    body should include("Disallow: /*/filmy")
  }

  it should "keep the film deep-links crawlable — they carry the long tail" in {
    val body = contentAsString(controller().robotsTxt(req("/robots.txt")))
    body should not include "Disallow: /*/movie\n"
    body should not include "Disallow: /*/movie?"
  }

  // SemrushBot's crawl only feeds its own SEO product, not us, and (unlike
  // meta-externalagent) it actually honours robots.txt — see
  // project_us_movie_route_applebot_semrushbot_spikes memory.
  it should "block SemrushBot entirely, while leaving the wildcard rule for everyone else" in {
    val body = contentAsString(controller().robotsTxt(req("/robots.txt")))
    body should include("User-agent: SemrushBot\nDisallow: /")
    // The general allowlist stays untouched for other crawlers (e.g. Applebot).
    body should include("User-agent: *")
    body should include("Allow: /")
  }

  "sitemap.xml" should "enumerate the landing, the city, its plan, and live films" in {
    val res = controller().sitemap(req("/sitemap.xml"))
    status(res)      shouldBe OK
    contentType(res) shouldBe Some("application/xml")
    header("Cache-Control", res) shouldBe Some("public, max-age=3600")
    val body = contentAsString(res)
    body should include("<urlset")
    body should include("<loc>https://kinowo.net/</loc>")
    body should include("<loc>https://kinowo.net/poznan/</loc>")
    body should include("<loc>https://kinowo.net/poznan/movie/testowy-film</loc>")
  }

  /** The US crawl map is its metros — the state is not an address, so naming
   *  `/california/` would sitemap a 404 while the metros people search for went
   *  unadvertised. */
  it should "advertise every US metro, and no state" in {
    val us   = TestMovieController.build(Seq.empty, servingCountry = models.Country.UnitedStates)._1
    val body = contentAsString(us.sitemap(req("/sitemap.xml")))
    body should include("<loc>https://kinowo.net/us/los-angeles/</loc>")
    body should include("<loc>https://kinowo.net/us/san-francisco-bay-area/</loc>")
    // The state is how a metro is FOUND, never a page of its own.
    body should not include "/us/california/"
    // A district is a filter inside a metro, never a URL of its own.
    body should not include "/us/los-angeles/santa-monica/"
    // A state small enough AND compact enough to be one city is advertised as
    // that city; one whose metros are a flight apart is advertised as its metros.
    body should include("<loc>https://kinowo.net/us/vermont/</loc>")
    body should include("<loc>https://kinowo.net/us/anchorage/</loc>")
    body should not include "/us/alaska/"
  }

  /** A country that shares `showtimes.cc` is served one segment down, so every
   *  `<loc>` has to carry that segment — a sitemap advertising `/kent/` on a
   *  deployment reachable at `/uk/kent/` is a file of 404s, and it is the one
   *  file a crawler trusts to enumerate the site. */
  it should "hang every URL off the mount point on a country sharing the brand domain" in {
    val uk = TestMovieController.build(Seq.empty, servingCountry = models.Country.UnitedKingdom)._1
    val body = contentAsString(uk.sitemap(
      FakeRequest(GET, "/sitemap.xml")
        .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")))
    body should include("<loc>https://showtimes.cc/uk/</loc>")
    body should include("<loc>https://showtimes.cc/uk/kent/</loc>")
    body should not include "<loc>https://showtimes.cc/kent/</loc>"
  }

  /** The brand front door owns the apex ROOT, which is the only `robots.txt` and
   *  `sitemap.xml` a crawler will ever fetch for `showtimes.cc` — the countries
   *  mounted beneath it have no host root of their own. Answered by the
   *  deployment mounted at `/`, which is the one on its own domain. */
  private def apexReq(path: String) =
    FakeRequest(GET, path)
      .withHeaders("X-Forwarded-Proto" -> "https", "X-Forwarded-Host" -> "showtimes.cc")

  "the front door's sitemap.xml" should "be an index of the countries mounted under the apex" in {
    val body = contentAsString(controller().sitemap(apexReq("/sitemap.xml")))
    body should include("<sitemapindex")
    body should include("<loc>https://showtimes.cc/uk/sitemap.xml</loc>")
    body should include("<loc>https://showtimes.cc/de/sitemap.xml</loc>")
    body should include("<loc>https://showtimes.cc/us/sitemap.xml</loc>")
    // Poland is a different host with a root sitemap of its own, and this
    // deployment's own cities have no business being crawled off the apex.
    body should not include "kinowo.net"
    body should not include "/poznan/"
  }

  "the front door's robots.txt" should "point at each mounted country's sitemap and fence off its noise" in {
    val body = contentAsString(controller().robotsTxt(apexReq("/robots.txt")))
    body should include("Sitemap: https://showtimes.cc/uk/sitemap.xml")
    body should include("Sitemap: https://showtimes.cc/de/sitemap.xml")
    body should include("Disallow: /uk/debug")
    body should include("Disallow: /us/*/movies")
    // The apex is not Poland's front page, so it must not advertise Poland's.
    body should not include "kinowo.net"
  }

  it should "scope to this deployment's country, not the global City.all" in {
    // KINOWO_COUNTRY is unset in tests → Poland. A Poland host must NOT advertise
    // the UK/Germany cities that also live in City.all (they render empty here).
    val body = contentAsString(controller().sitemap(req("/sitemap.xml")))
    body should include("/warszawa/")          // a Polish city stays
    body should not include "/london/"          // UK city — different deployment
    body should not include "/kent/"            // UK region added in the Flicks roster
    body should not include "/berlin/"          // German city
  }

  // ── Per-city lastmod ─────────────────────────────────────────────────────────
  //
  // Before this fix, every URL in the file — landing aside — carried
  // `readModel.lastModified`, the MODEL-WIDE stamp that moves on ANY city's
  // change. That claimed a Wrocław showtime edit as a change to Poznań's URLs
  // too (and vice versa) — exactly the over-invalidation
  // `WebReadModel.lastModifiedFor(citySlug)` already exists to avoid for the
  // conditional-GET validator (see `WebReadModelSpec`). `StubReadModel`
  // overrides only that one seam, to deterministic values, so the two cities'
  // dates are pinned rather than racing the wall clock (which would round both
  // onto today's date regardless of the bug, on any run finishing inside a day).

  private def ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private def resolvedMovie(id: String, title: String) =
    ResolvedMovie(id, title, None, None, Nil, None, Some(2026), Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def cityScreening(id: String, filmId: String, city: String, cinema: models.Cinema) =
    CityScreening(id, filmId, city, cinema.displayName, None, Seq(models.Showtime(LocalDateTime.now().plusDays(1), None)))

  private class StubReadModel(repository: InMemoryReadModelRepository, stamps: Map[String, Instant])
      extends WebReadModel(repository) {
    override def lastModifiedFor(citySlug: String): Instant = stamps.getOrElse(citySlug, Instant.EPOCH)
  }

  private val PoznanStamp  = Instant.parse("2026-01-01T00:00:00Z")
  private val WroclawStamp = Instant.parse("2026-06-15T00:00:00Z")

  private def twoCityController(): MovieController = {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(resolvedMovie("belle|2026", "Belle"))
    repository.upsertMovie(resolvedMovie("diuna|2026", "Diuna"))
    repository.upsertScreening(cityScreening("s-poznan", "belle|2026", "poznan", Multikino))
    repository.upsertScreening(cityScreening("s-wroclaw", "diuna|2026", "wroclaw", MultikinoPasazGrunwaldzki))
    val readModel = new StubReadModel(repository, Map("poznan" -> PoznanStamp, "wroclaw" -> WroclawStamp))
    readModel.reload()
    TestMovieController.build(Nil, readModel = Some(readModel))._1
  }

  private def lastmodOf(xml: String, locSuffix: String): String = {
    val line = xml.linesIterator.find(_.contains(s"$locSuffix</loc>"))
      .getOrElse(fail(s"no <url> ending in $locSuffix in:\n$xml"))
    val start = line.indexOf("<lastmod>") + "<lastmod>".length
    line.substring(start, line.indexOf("</lastmod>"))
  }

  "sitemap.xml" should "stamp each city's URLs with THAT city's own lastmod, not a shared one" in {
    val body = contentAsString(twoCityController().sitemap(req("/sitemap.xml")))

    lastmodOf(body, "/poznan/")             shouldBe "2026-01-01"
    lastmodOf(body, "/wroclaw/")            shouldBe "2026-06-15"
    lastmodOf(body, "/poznan/movie/belle")  shouldBe "2026-01-01"
    lastmodOf(body, "/wroclaw/movie/diuna") shouldBe "2026-06-15"
    // The whole point of the fix: two cities that changed on different days do
    // not collapse onto one shared date.
    lastmodOf(body, "/poznan/") should not be lastmodOf(body, "/wroclaw/")
  }

  "/{city}/sitemap.xml" should "carry that one city's own lastmod and omit the landing URL" in {
    val body = contentAsString(twoCityController().citySitemap("wroclaw")(req("/wroclaw/sitemap.xml")))

    lastmodOf(body, "/wroclaw/")            shouldBe "2026-06-15"
    lastmodOf(body, "/wroclaw/movie/diuna") shouldBe "2026-06-15"
    body should not include "<loc>https://kinowo.net/</loc>"
    body should not include "/poznan/"
  }

  // ── Partitioning an oversized country into a sitemap index ──────────────────
  //
  // Google caps a single sitemap at 50,000 URLs; `SitemapBuilder.CityPartitionThreshold`
  // sits well under that so a corpus is split long before it becomes a real risk.
  // The threshold is a URL COUNT, not a hardcoded country, so whichever corpus
  // grows past it next gets partitioned — pinned here by crossing it with a
  // synthetic Polish corpus rather than depending on the real US roster.

  private def manyFilmsController(filmCount: Int): MovieController = {
    val repository = new InMemoryReadModelRepository
    val now = LocalDateTime.now().plusDays(1)
    (1 to filmCount).foreach { i =>
      val id = s"film-$i|2026"
      repository.upsertMovie(resolvedMovie(id, s"Film $i"))
      repository.upsertScreening(
        CityScreening(s"s-$i", id, "poznan", Multikino.displayName, None, Seq(models.Showtime(now, None))))
    }
    val readModel = new WebReadModel(repository)
    readModel.reload()
    TestMovieController.build(Nil, readModel = Some(readModel))._1
  }

  "sitemap.xml" should "stay a flat file below the partition threshold" in {
    val body = contentAsString(manyFilmsController(5).sitemap(req("/sitemap.xml")))
    body should include("<urlset")
    body should not include "<sitemapindex"
  }

  it should "partition into a per-city sitemap index once the corpus crosses the threshold" in {
    val body = contentAsString(
      manyFilmsController(SitemapBuilder.CityPartitionThreshold).sitemap(req("/sitemap.xml")))

    body should include("<sitemapindex")
    body should not include "<urlset"
    body should include("<loc>https://kinowo.net/sitemap-root.xml</loc>")
    body should include("<loc>https://kinowo.net/poznan/sitemap.xml</loc>")
  }

  "sitemap-root.xml" should "carry only the landing URL" in {
    val body = contentAsString(
      manyFilmsController(SitemapBuilder.CityPartitionThreshold).sitemapRoot(req("/sitemap-root.xml")))
    body should include("<loc>https://kinowo.net/</loc>")
    body should not include "/poznan/"
  }

  "{city}/sitemap.xml" should "carry that city's full film list once the country is partitioned" in {
    val body = contentAsString(
      manyFilmsController(SitemapBuilder.CityPartitionThreshold).citySitemap("poznan")(req("/poznan/sitemap.xml")))
    body should include("<urlset")
    body should include(s"<loc>https://kinowo.net/poznan/movie/film-1</loc>")
    body should include(s"<loc>https://kinowo.net/poznan/movie/film-${SitemapBuilder.CityPartitionThreshold}</loc>")
    body should not include "<loc>https://kinowo.net/</loc>"
  }
}
