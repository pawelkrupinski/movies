package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.readmodel.{TestReadModel, WebReadModel}

/**
 * The film page's `og:image` is the worker-rendered share card, served from disk by Caddy at
 * `/share-cards/<country>/<file>` — the web only names it, from `web_movies.shareCard` — and the
 * city's static card while a film has none. Nothing on the web decodes an image.
 */
class ShareCardOgImageSpec extends AnyFlatSpec with Matchers {

  private val card = "f0123456789abcd.jpg?v=0123456789abcdef"

  private def film(title: String) =
    TestMovieController.showing(title, posterUrl = Some(s"https://cinema.example/${title.toLowerCase(java.util.Locale.ROOT)}.jpg"))

  private val records = Seq(film("Diuna"), film("Belle"))

  /** The controller over a read model where Diuna has a share card and Belle has none. */
  private def controller(): MovieController = {
    val store = TestReadModel.store(records)
    store.findAllMovies().filter(_.title == "Diuna").foreach(m => store.upsertMovie(m.copy(shareCard = Some(card))))
    val readModel = new WebReadModel(store, clock = _root_.tools.SpecClock.Pinned)
    readModel.reload()
    TestMovieController.build(records, readModel = Some(readModel))._1
  }

  private val request = FakeRequest().withHeaders("Host" -> "kinowo.net", "X-Forwarded-Proto" -> "https")
  private val cityCard = "https://kinowo.net/share-cards/pl/city-poznan.jpg"

  private def ogImage(slug: String): Option[String] =
    """<meta property="og:image"\s+content="([^"]+)">""".r
      .findFirstMatchIn(contentAsString(controller().filmBySlug("poznan", slug).apply(request))).map(_.group(1))

  "A film page" should "point og:image at its share card on its country's host" in {
    ogImage("diuna") shouldBe Some(s"https://kinowo.net/share-cards/pl/$card")
  }

  // The request's host is whatever vhost the proxy matched (the Polish pods answer both
  // kinowo.net and the showtimes.cc front door) and, off the proxy, whatever a client sent.
  // A preview card's address is the country's, like the city card it falls back to.
  it should "name its share card on the country's host whatever host the request arrived on" in {
    val html = contentAsString(controller().filmBySlug("poznan", "diuna").apply(
      request.withHeaders("X-Forwarded-Host" -> "attacker.example")))
    """<meta property="og:image"\s+content="([^"]+)">""".r.findFirstMatchIn(html).map(_.group(1)) shouldBe
      Some(s"https://kinowo.net/share-cards/pl/$card")
  }

  it should "fall back to the city's card while the film has none" in {
    ogImage("belle") shouldBe Some(cityCard)
  }

  it should "always declare the image's type and size, so a first share renders without fetching it" in {
    val html = contentAsString(controller().filmBySlug("poznan", "belle").apply(request))
    html should include ("""<meta property="og:image:type"   content="image/jpeg">""")
    html should include ("""<meta property="og:image:width"  content="1200">""")
    html should include ("""<meta property="og:image:height" content="630">""")
  }

  "A city's card" should "be the worker's file on disk, on its country's host" in {
    ShareCardUrl.city(models.Poznan) shouldBe cityCard
    ShareCardUrl.city(models.London) shouldBe "https://showtimes.cc/share-cards/uk/city-london.jpg"
  }

  // Every city slug must pass Caddy's share-card matcher, `[A-Za-z0-9_-][A-Za-z0-9._-]*\.jpg`, or
  // its card is a 404 however well the worker drew it.
  it should "have a file name Caddy serves, for every city in every country" in {
    models.Country.all.flatMap(_.cities).map(_.shareImage).filterNot(_.matches("city-[a-z0-9-]+\\.jpg")) shouldBe empty
  }

  "The committed-asset city card URL" should "redirect permanently to the worker's card" in {
    val result = controller().legacyCityCard("og-poznan.jpg").apply(request)
    status(result) shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some(cityCard)
  }

  // The route that sends it there, read off the routes file: above the assets route (Play matches
  // top-down), taking every old city card and never a home card, which is still an asset.
  it should "be routed ahead of the assets, and never take a home card" in {
    val lines   = scala.io.Source.fromResource("routes").getLines().map(_.trim).filterNot(l => l.isEmpty || l.startsWith("#")).toVector
    val legacy  = lines.indexWhere(_.contains("MovieController.legacyCityCard"))
    val assets  = lines.indexWhere(_.contains("controllers.Assets.versioned"))
    legacy should (be >= 0 and be < assets)
    val pattern = "\\$file<(.+)>".r.findFirstMatchIn(lines(legacy)).get.group(1)
    Seq("og-poznan.jpg", "og-zielona-gora.jpg", "og-homestead.jpg").filterNot(_.matches(pattern)) shouldBe empty
    Seq("og-home.jpg", "og-home-uk.jpg", "og-home-us.jpg", "logo.png", "og-poznan.png").filter(_.matches(pattern)) shouldBe empty
  }

  it should "send a slug no city answers to any more to the home card" in {
    val result = controller().legacyCityCard("og-atlantis.jpg").apply(request)
    status(result) shouldBe FOUND
    redirectLocation(result) shouldBe Some("https://kinowo.net/assets/img/og-home.jpg")
  }

  "A city card the worker has not drawn yet" should "fall back to its country's home card, never a 404" in {
    redirectLocation(controller().cityCardFallback("uk").apply(request)) shouldBe Some("https://showtimes.cc/uk/assets/img/og-home-uk.jpg")
    redirectLocation(controller().cityCardFallback("pl").apply(request)) shouldBe Some("https://kinowo.net/assets/img/og-home.jpg")
    redirectLocation(controller().cityCardFallback("zz").apply(request)) shouldBe Some("https://kinowo.net/assets/img/og-home.jpg")
  }
}
