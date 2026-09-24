package controllers

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.readmodel.{TestReadModel, WebReadModel}

/**
 * The film page's `og:image` is the worker-rendered share card, served from disk by Caddy at
 * `/share-cards/<country>/<file>` — the web only names it, from `web_movies.shareCard` — and the
 * city's static card while a film has none. The old render endpoints answer with a redirect to
 * the same images, for the URLs preview scrapers already cached; nothing on the web decodes an
 * image any more.
 */
class ShareCardOgImageSpec extends AnyFlatSpec with Matchers {

  private val card = "f0123456789abcd.jpg?v=0123456789abcdef"

  private def record(title: String) = MovieRecord(
    imdbId = None,
    data = Map[Source, SourceData](Helios -> SourceData(
      title     = Some(title),
      posterUrl = Some(s"https://cinema.example/${title.toLowerCase}.jpg"),
      showtimes = Seq(models.Showtime(TestMovieController.now.plusHours(2), None, None, Nil))
    )))

  private val records = Seq(("Diuna", Option.empty[Int], record("Diuna")), ("Belle", Option.empty[Int], record("Belle")))

  /** The controller over a read model where Diuna has a share card and Belle has none. */
  private def controller(): MovieController = {
    val store = TestReadModel.store(records)
    store.findAllMovies().filter(_.title == "Diuna").foreach(m => store.upsertMovie(m.copy(shareCard = Some(card))))
    val readModel = new WebReadModel(store)
    readModel.reload()
    TestMovieController.build(records, readModel = Some(readModel))._1
  }

  private val request = FakeRequest().withHeaders("Host" -> "kinowo.net", "X-Forwarded-Proto" -> "https")
  private val cityCard = "https://kinowo.net/assets/img/og-poznan.jpg"

  private def ogImage(slug: String): Option[String] =
    """<meta property="og:image"\s+content="([^"]+)">""".r
      .findFirstMatchIn(contentAsString(controller().filmBySlug("poznan", slug).apply(request))).map(_.group(1))

  "A film page" should "point og:image at its share card on the request's host" in {
    ogImage("diuna") shouldBe Some(s"https://kinowo.net/share-cards/pl/$card")
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

  "The old film card URL" should "redirect to the film's share card, or to the city card" in {
    val ctrl = controller()
    val withCard = ctrl.ogImage("poznan", "Diuna").apply(request)
    status(withCard) shouldBe MOVED_PERMANENTLY
    redirectLocation(withCard) shouldBe Some(s"https://kinowo.net/share-cards/pl/$card")
    redirectLocation(ctrl.ogImage("poznan", "Belle").apply(request)) shouldBe Some(cityCard)
    redirectLocation(ctrl.ogImage("poznan", "No Such Film").apply(request)) shouldBe Some(cityCard)
  }

  "The old city card URL" should "redirect to the city's static card" in {
    val result = controller().cityOgImage("poznan").apply(request)
    status(result) shouldBe MOVED_PERMANENTLY
    redirectLocation(result) shouldBe Some(cityCard)
  }
}
