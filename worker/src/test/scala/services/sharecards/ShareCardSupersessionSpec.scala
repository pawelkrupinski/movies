package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.TaskFinished
import services.tasks.{HandlerOutcome, TaskType}
import ShareCardTestKit.*

/**
 * A film's card URL names its version, and the file under it is overwritten by every render. A
 * render of inputs a newer request has replaced must therefore never land: finishing after the
 * newer render, it would put the older picture under the URL `web_movies` names for the newer one,
 * which a preview cache then keeps for a year.
 */
class ShareCardSupersessionSpec extends AnyFlatSpec with Matchers {

  private val older = film()
  private val newer = film(ratings = ratings.copy(imdb = Some(8.4)))

  "A render of inputs a newer request replaced" should "not overwrite the newer card, whichever finishes last" in {
    val rig = new Rig
    rig.service.request(rig.service.inputs(older))
    rig.service.request(rig.service.inputs(newer))
    val tasks   = drain(rig.queue)
    val handler = new RenderShareCardHandler(rig.service)
    tasks.map(_.payload("imdb")) shouldBe Seq("7.8", "8.4")

    handler.handle(tasks(1)) shouldBe HandlerOutcome.Done
    handler.handle(tasks(0)) shouldBe HandlerOutcome.Skipped
    rig.service.existing(rig.service.inputs(newer)) shouldBe defined
  }

  it should "still render when the only newer ask is older than it (a backfill's day-old sweep)" in {
    val rig = new Rig
    rig.service.request(rig.service.inputs(newer))
    rig.service.request(rig.service.inputs(older), askedAt = T0.minusSeconds(3600))
    val tasks   = drain(rig.queue)
    val handler = new RenderShareCardHandler(rig.service)
    tasks.map(handler.handle) shouldBe Seq(HandlerOutcome.Done, HandlerOutcome.Skipped)
    rig.service.existing(rig.service.inputs(newer)) shouldBe defined
  }

  "A superseded first card" should "not end the film's first-publish hold: the newer render will" in {
    val rig = new Rig
    val released = collection.mutable.Buffer.empty[String]
    val followUp = new ShareCardFollowUp(rig.store, rig.service.superseded, _ => (), released += _)
    rig.service.enqueueRender(rig.service.inputs(older), Seq(ShareCardReason.NewFilm), first = true)
    rig.service.enqueueRender(rig.service.inputs(newer), Seq(ShareCardReason.NewFilm), first = true)
    val tasks = drain(rig.queue)

    followUp.onTaskFinished(TaskFinished(TaskType.RenderShareCard, tasks(0).dedupKey, tasks(0).payload))
    released shouldBe empty
    followUp.onTaskFinished(TaskFinished(TaskType.RenderShareCard, tasks(1).dedupKey, tasks(1).payload))
    released.toSeq shouldBe Seq(older._id)                  // the newest one drew nothing: the hold ends
  }
}
