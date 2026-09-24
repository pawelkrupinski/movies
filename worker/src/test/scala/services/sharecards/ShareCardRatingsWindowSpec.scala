package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{HandlerOutcome, Task}
import ShareCardTestKit.*

import java.nio.file.Files
import java.nio.file.attribute.FileTime
import java.time.Instant
import scala.concurrent.duration.*

/** A ratings-only change re-renders a film's card at most once a day; anything else at once. */
class ShareCardRatingsWindowSpec extends AnyFlatSpec with Matchers {

  private val hour = 3600L

  /** A rig whose film has a card written at T0, and whose clock reads `now`. */
  private def rigAt(now: Instant): (Rig, models.ResolvedMovie) = {
    val store = tempStore()
    val first = new Rig(store = store)
    val movie = film()
    first.service.render(first.service.inputs(movie), Seq(ShareCardReason.NewFilm))
    first.service.existing(first.service.inputs(movie)).foreach(name =>
      Files.setLastModifiedTime(store.cardPath(name), FileTime.from(T0)))
    val later = new Rig(store = store, clock = clockAt(now))
    later.readModel.upsertMovie(movie.copy(shareCard = first.service.existing(first.service.inputs(movie))))
    (later, movie)
  }

  private def rated(movie: models.ResolvedMovie, imdb: Double) = movie.copy(ratings = movie.ratings.copy(imdb = Some(imdb)))

  private def claimAt(rig: Rig, at: Instant): Option[Task] = rig.queue.claim("spec", 1.minute, at)

  "Three rating changes in a day" should "make one render, a day after the card, with the final ratings" in {
    val (rig, movie) = rigAt(T0.plusSeconds(1 * hour))
    rig.service.onProjected(rated(movie, 8.0), screened = true)
    rig.service.onProjected(rated(movie, 8.3), screened = true)
    rig.service.onProjected(rated(movie, 8.5), screened = true)

    claimAt(rig, T0.plusSeconds(23 * hour)) shouldBe None               // nothing due inside the day
    val task = claimAt(rig, T0.plusSeconds(24 * hour + 1)).get
    claimAt(rig, T0.plusSeconds(48 * hour)) shouldBe None               // and only the one
    ShareCardInputs.fromPayload(task.payload).flatMap(_.imdb) shouldBe Some(8.5)

    new RenderShareCardHandler(rig.service).handle(task) shouldBe HandlerOutcome.Done
    rig.service.existing(rig.service.inputs(rated(movie, 8.5))) shouldBe defined
    rig.service.existing(rig.service.inputs(rated(movie, 8.3))) shouldBe None
  }

  "A title change inside the window" should "render at once, with the ratings as they are then" in {
    val (rig, movie) = rigAt(T0.plusSeconds(2 * hour))
    rig.service.onProjected(rated(movie, 8.3), screened = true)         // deferred
    val retitled = rated(movie, 8.3).copy(title = "Diuna: Część pierwsza")
    rig.service.onProjected(retitled, screened = true)

    val now = claimAt(rig, T0.plusSeconds(2 * hour)).get
    val drawn = ShareCardInputs.fromPayload(now.payload).get
    (drawn.title, drawn.imdb) shouldBe (("Diuna: Część pierwsza", Some(8.3)))
    now.payload.get(ShareCardService.AnchorKey) shouldBe None           // not the deferred one

    // Once that render lands, the deferred one has nothing left to do.
    new RenderShareCardHandler(rig.service).handle(now) shouldBe HandlerOutcome.Done
    val deferred = claimAt(rig, T0.plusSeconds(25 * hour)).get
    new RenderShareCardHandler(rig.service).handle(deferred) shouldBe HandlerOutcome.Skipped
  }

  "A first render" should "never wait" in {
    val rig = new Rig(clock = clockAt(T0.plusSeconds(hour)))
    rig.service.onProjected(film(id = "fbrandnew"), screened = true)
    claimAt(rig, T0.plusSeconds(hour)).map(_.payload("filmId")) shouldBe Some("fbrandnew")
  }

  "A rating change a day after the card" should "render at once" in {
    val (rig, movie) = rigAt(T0.plusSeconds(25 * hour))
    rig.service.onProjected(rated(movie, 8.4), screened = true)
    claimAt(rig, T0.plusSeconds(25 * hour)).map(_.payload("reasons")) shouldBe Some(ShareCardReason.Ratings)
  }

  "Ratings that come back to the card's own" should "leave the waiting render nothing to draw" in {
    val (rig, movie) = rigAt(T0.plusSeconds(hour))
    rig.service.onProjected(rated(movie, 8.3), screened = true)
    rig.service.onProjected(movie, screened = true)                     // back to 7.8
    val task = claimAt(rig, T0.plusSeconds(25 * hour)).get
    ShareCardInputs.fromPayload(task.payload).flatMap(_.imdb) shouldBe Some(7.8)
    rig.service.render(ShareCardInputs.fromPayload(task.payload).get, Seq(ShareCardReason.Ratings)) shouldBe ShareCardMetrics.Outcome.Existing
  }
}
