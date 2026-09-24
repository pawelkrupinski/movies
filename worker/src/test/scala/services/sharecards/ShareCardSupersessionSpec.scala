package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.TaskFinished
import services.tasks.{HandlerOutcome, TaskType}
import ShareCardTestKit.*
import java.nio.file.Files

/**
 * A film's card URL names its version, and the file under it is overwritten by every render. A
 * render of inputs a newer request has replaced must therefore never land: finishing after the
 * newer render, it would put the older picture under the URL `web_movies` names for the newer one,
 * which a preview cache then keeps for a year.
 */
class ShareCardSupersessionSpec extends AnyFlatSpec with Matchers with org.scalatest.LoneElement {

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

  // The supersession map above is per PROCESS: a replica that never saw the newer ask — the other
  // pod of a rolling deploy, or one freshly booted — renders the older task as if it were current,
  // and its rename lands the older picture under the newer card's URL. What each card was asked
  // for travels in its file, beside its version, and a write never replaces a newer ask's card.
  it should "not overwrite a newer ask's card that ANOTHER replica wrote to the shared directory" in {
    val store  = tempStore()
    val podA   = new Rig(store = store, clock = clockAt(T0.plusSeconds(60)))
    val podB   = new Rig(store = store, clock = clockAt(T0))
    podB.service.request(podB.service.inputs(older))               // asked first, on pod B
    podA.service.request(podA.service.inputs(newer))               // asked a minute later, on pod A
    new RenderShareCardHandler(podA.service).handle(drain(podA.queue).loneElement) shouldBe HandlerOutcome.Done
    new RenderShareCardHandler(podB.service).handle(drain(podB.queue).loneElement)   // pod B finishes last
    podA.service.existing(podA.service.inputs(newer)) shouldBe defined
    podB.service.existing(podB.service.inputs(older)) shouldBe empty
  }

  // The ask-order check and the rename are two steps, and a check run while another replica sits
  // between its own check and rename used to pass both. Each card write now holds the film's lock
  // file (a POSIX record lock, which another PROCESS honours) across check and rename. Held here by
  // a separate process, as a replica's would be, the write must wait for it.
  it should "hold a card write until another process's lock on the film is released" in {
    val rig   = new Rig
    val movie = film()
    val lock  = rig.store.lockFor(rig.store.cardPath(movie._id))
    Files.createDirectories(lock.getParent)
    val holder = new ProcessBuilder("python3", "-c",
      "import fcntl,sys,time; f=open(sys.argv[1],'a'); fcntl.lockf(f, fcntl.LOCK_EX); print('locked', flush=True); time.sleep(2)",
      lock.toString).redirectErrorStream(true).start()
    new java.io.BufferedReader(new java.io.InputStreamReader(holder.getInputStream)).readLine() shouldBe "locked"

    val written = new java.util.concurrent.CountDownLatch(1)
    val writer  = new Thread(() => { rig.service.render(rig.service.inputs(movie), Seq(ShareCardReason.NewFilm), askedAt = Some(T0)); written.countDown() })
    writer.start()
    written.await(1, java.util.concurrent.TimeUnit.SECONDS) shouldBe false     // waiting on the other process
    holder.waitFor()
    written.await(10, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
    rig.service.existing(rig.service.inputs(movie)) shouldBe defined
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
