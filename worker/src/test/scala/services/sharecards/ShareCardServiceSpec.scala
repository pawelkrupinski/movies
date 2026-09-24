package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{HandlerOutcome, Task, TaskType}
import ShareCardTestKit.*

import java.nio.file.Files
import javax.imageio.ImageIO
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class ShareCardServiceSpec extends AnyFlatSpec with Matchers {

  private def renderTask(rig: Rig, inputs: ShareCardInputs, attempts: Int = 1): HandlerOutcome =
    new RenderShareCardHandler(rig.service).handle(Task("t", TaskType.RenderShareCard, "k", inputs.toPayload, attempts))

  "A RenderShareCard task" should "write the film's card, stamped with its version, atomically" in {
    val rig    = new Rig
    val inputs = rig.service.inputs(film())
    renderTask(rig, inputs) shouldBe HandlerOutcome.Done

    val version = inputs.version(Some("https://cdn.example/poster-a.jpg"))
    rig.service.existing(inputs) shouldBe Some(version)
    rig.service.current(film()) shouldBe Some(s"f0123456789abcd.jpg?v=$version")
    val image = ImageIO.read(rig.store.cardPath(film()._id).toFile)
    (image.getWidth, image.getHeight) shouldBe ((1200, 630))
    // No temp file survives a completed write.
    Files.list(rig.store.root).iterator.asScala.map(_.getFileName.toString).filter(_.endsWith(".tmp")).toSeq shouldBe empty
  }

  it should "name the card by the fallback poster it was drawn from when the primary fails" in {
    val rig    = new Rig(download = new CountingDownload(failing = Set("https://multikino.example/403.jpg")))
    val movie  = film(poster = "https://multikino.example/403.jpg").copy(fallbackPosterUrls = Seq("https://cdn.example/b.jpg"))
    val inputs = rig.service.inputs(movie)
    rig.service.render(inputs, Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.Rendered
    rig.service.existing(inputs) shouldBe Some(inputs.version(Some("https://cdn.example/b.jpg")))
  }

  it should "not render again for inputs whose card exists" in {
    val rig    = new Rig
    val inputs = rig.service.inputs(film())
    rig.service.render(inputs, Seq(ShareCardReason.Backfill)) shouldBe ShareCardMetrics.Outcome.Rendered
    val before = Files.getLastModifiedTime(rig.store.cardPath(film()._id))
    rig.service.render(inputs, Seq(ShareCardReason.Backfill)) shouldBe ShareCardMetrics.Outcome.Existing
    Files.getLastModifiedTime(rig.store.cardPath(film()._id)) shouldBe before
    rig.download.total shouldBe 1
  }

  it should "finish with a card without a poster when every poster fails, rather than retry the task" in {
    val rig    = new Rig(download = new CountingDownload(failing = Set("https://cdn.example/poster-a.jpg")))
    val inputs = rig.service.inputs(film())
    renderTask(rig, inputs, attempts = 1) shouldBe HandlerOutcome.Done
    rig.service.existing(inputs) shouldBe Some(inputs.version(None))
  }

  it should "count a posterless card's re-try apart from a film's first poster load" in {
    val series = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val rig    = new Rig(download = new CountingDownload(failing = Set("https://cdn.example/poster-a.jpg"))) {
      override val metrics = series.forCountry("pl")
    }
    val inputs = rig.service.inputs(film())
    rig.service.render(inputs, Seq(ShareCardReason.NewFilm)) shouldBe ShareCardMetrics.Outcome.RenderedNoPoster
    rig.service.render(inputs, Seq(ShareCardReason.Backfill), retryPoster = true) shouldBe ShareCardMetrics.Outcome.Existing
    (series.posterLoadCount("pl", ok = false, retry = false), series.posterLoadCount("pl", ok = false, retry = true)) shouldBe ((1.0, 1.0))
  }

  "A projection" should "enqueue a render only when something the card draws changed" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.onProjected(movie, screened = true)
    // The fallback list churning is not a change: the card was drawn from the primary.
    rig.service.onProjected(movie.copy(fallbackPosterUrls = Seq("https://cinema.example/x.jpg")), screened = true)
    // Nor is a rating moving below the precision the badge shows.
    rig.service.onProjected(movie.copy(ratings = movie.ratings.copy(imdb = Some(7.84))), screened = true)
    drain(rig.queue) shouldBe empty

    val rerated = movie.copy(ratings = movie.ratings.copy(imdb = Some(8.1)))
    rig.service.onProjected(rerated, screened = true)
    val queued = drain(rig.queue)
    queued.map(_.taskType) shouldBe Seq(TaskType.RenderShareCard)
    queued.head.payload("reasons") shouldBe ShareCardReason.Ratings
  }

  it should "re-render a ratings change at once, however recent the card" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.current(movie)
    Seq(8.0, 8.3).foreach(imdb => rig.service.onProjected(movie.copy(ratings = movie.ratings.copy(imdb = Some(imdb))), screened = true))
    // Both claimable now: nothing is held back for a day.
    val now = Iterator.continually(rig.queue.claim("spec", 1.minute, T0)).takeWhile(_.isDefined).flatten.toSeq
    now.map(t => ShareCardInputs.fromPayload(t.payload).flatMap(_.imdb)) shouldBe Seq(Some(8.0), Some(8.3))
    now.map(_.payload("reasons")).distinct shouldBe Seq(ShareCardReason.Ratings)
  }

  it should "re-render with reason poster when the poster the card was drawn from is no longer a candidate" in {
    val rig = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    rig.service.current(movie)
    val moved = rig.service.inputs(movie.copy(posterUrl = Some("https://cdn.example/new.jpg")))
    rig.service.reasonsFor(moved) shouldBe Seq(ShareCardReason.Poster)
    rig.service.reasonsFor(rig.service.inputs(movie.copy(title = "Diuna 2", posterUrl = Some("https://cdn.example/new.jpg")))) shouldBe
      Seq(ShareCardReason.Poster, ShareCardReason.Title)
    rig.service.reasonsFor(rig.service.inputs(film(id = "fnew"))) shouldBe Seq(ShareCardReason.NewFilm)
  }

  it should "not render a card for a film with no screenings" in {
    val rig = new Rig
    rig.service.onProjected(film(), screened = false)
    drain(rig.queue) shouldBe empty
  }

  "current" should "keep the card a film had while the card for its new inputs renders, then switch" in {
    val rig   = new Rig
    val movie = film()
    rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    val old = rig.service.current(movie)
    old shouldBe defined

    val rerated = movie.copy(ratings = movie.ratings.copy(imdb = Some(8.1)))
    rig.service.current(rerated) shouldBe old                       // new card not written yet
    rig.service.readyToPublish(rerated) shouldBe false
    rig.service.render(rig.service.inputs(rerated), Seq(ShareCardReason.Ratings))
    rig.service.current(rerated) should (be (defined) and not be old)
    rig.service.readyToPublish(rerated) shouldBe true
  }

  "The first-publish request" should "queue the render ahead of the backlog and the end of the hold" in {
    val rig = new Rig
    rig.service.enqueueRender(rig.service.inputs(film(id = "fbacklog")), Seq(ShareCardReason.Backfill))
    rig.service.requestFirstCard(film(), T0.plusSeconds(120))
    val queued = drain(rig.queue)
    queued.head.taskType shouldBe TaskType.RenderShareCard
    queued.head.payload("filmId") shouldBe "f0123456789abcd"
    queued.head.payload(ShareCardService.FirstKey) shouldBe "true"
    queued.map(_.taskType) should contain (TaskType.ReleaseShareCardHold)
  }

  "Two replicas" should "render one card once, however many of them ask" in {
    val first  = new Rig
    val second = new Rig(store = first.store)
    val inputs = first.service.inputs(film())
    val secondOnSharedQueue = new ShareCardService(models.Country.default, first.store, second.posters, first.queue, second.metrics, second.clock)
    first.service.enqueueRender(inputs, Seq(ShareCardReason.NewFilm)) shouldBe services.tasks.EnqueueResult.Added
    secondOnSharedQueue.enqueueRender(inputs, Seq(ShareCardReason.NewFilm)) shouldBe services.tasks.EnqueueResult.Duplicate
    drain(first.queue).map(_.taskType) shouldBe Seq(TaskType.RenderShareCard)
  }
}
