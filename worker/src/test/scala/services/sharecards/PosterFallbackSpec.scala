package services.sharecards

import com.sun.net.httpserver.HttpServer
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.net.InetSocketAddress

/** Films whose posters all fail still get a card (without a poster, replaced once one works), and
 *  every poster failure says why. */
class PosterFallbackSpec extends AnyFlatSpec with Matchers {

  private final class MetricsRig(download: CountingDownload) extends Rig(download = download) {
    val series = new ShareCardMetrics.Series(Seq("pl"), new PrometheusRegistry)
    override val metrics = series.forCountry("pl")
  }
  private def metricsRig(download: CountingDownload) = new MetricsRig(download)

  "A film none of whose posters work" should "get a card without a poster, which counts as its card" in {
    val rig = metricsRig(new CountingDownload(failing = Set("https://cdn.example/poster-a.jpg")))
    val inputs = rig.service.inputs(film())
    rig.service.render(inputs, Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.RenderedNoPoster
    rig.service.existing(inputs) shouldBe Some(inputs.version(None))
    rig.service.readyToPublish(film()) shouldBe true
    javax.imageio.ImageIO.read(rig.store.cardPath(film()._id).toFile).getWidth shouldBe 1200
    rig.series.renderCount("pl", ShareCardMetrics.Outcome.RenderedNoPoster, ShareCardReason.NewFilm) shouldBe 1
    // A projection does not re-render it every time: it IS the film's card for now.
    rig.service.onProjected(film(), screened = true)
    drain(rig.queue) shouldBe empty
  }

  it should "have the poster retried later, and the card replaced once one works" in {
    val store = tempStore()
    val broken = new Rig(store = store, download = new CountingDownload(failing = Set("https://cdn.example/poster-a.jpg")))
    broken.service.render(broken.service.inputs(film()), Seq(ShareCardReason.NewFilm))
    val noPoster = broken.service.current(film()).get

    val fixed = new Rig(store = store)
    fixed.readModel.upsertMovie(film().copy(shareCard = Some(noPoster)))
    fixed.readModel.upsertScreening(screening(film()._id))
    val backfill = new ShareCardBackfill(fixed.service, fixed.readModel, fixed.queue, fixed.metrics, fixed.clock)
    backfill.tick() shouldBe 1
    val task = drain(fixed.queue).head
    new RenderShareCardHandler(fixed.service).handle(task) shouldBe services.tasks.HandlerOutcome.Done
    fixed.service.existing(fixed.service.inputs(film())) shouldBe Some(fixed.service.inputs(film()).version(Some(film().posterUrl.get)))
    fixed.service.current(film()) should not be Some(noPoster)
  }

  "A poster fetch failure" should "be counted with its reason" in {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    def route(path: String, status: Int, body: Array[Byte]): Unit =
      server.createContext(path, exchange => { exchange.sendResponseHeaders(status, body.length.toLong); exchange.getResponseBody.write(body); exchange.close() })
    route("/missing.jpg", 404, "nope".getBytes)
    route("/broken.jpg", 503, "down".getBytes)
    route("/huge.jpg", 200, new Array[Byte](4096))
    route("/garbage.jpg", 200, "not an image at all".getBytes)
    server.start()
    try {
      val base = s"http://127.0.0.1:${server.getAddress.getPort}"
      val series = new ShareCardMetrics.Series(Seq("pl"), new PrometheusRegistry)
      val posters = new ShareCardPosters(tempStore(), new HttpPosterDownload(maxBytes = 1024), new VipsPosterShrinker(binary = None), series.forCountry("pl"))
      Seq("missing", "broken", "huge", "garbage").foreach(name => posters.load(s"f$name", Seq(s"$base/$name.jpg")) shouldBe None)
      Seq(PosterFailure.Http4xx, PosterFailure.Http5xx, PosterFailure.TooLarge, PosterFailure.DecodeError)
        .foreach(reason => withClue(reason) { series.posterFetchCount("pl", reason) shouldBe 1 })
    } finally server.stop(0)
  }

  it should "name a progressive poster the header refused" in {
    val giant = PosterMemoryCapSpec.jpegHeader(8000, 12000, progressive = true)
    new VipsPosterShrinker(binary = None).coverSlot(giant) shouldBe Left(PosterFailure.ProgressiveEstimate)
  }
}
