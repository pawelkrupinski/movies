package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.TaskFinished
import services.tasks.TaskType
import ShareCardTestKit.*

class ShareCardBackfillSpec extends AnyFlatSpec with Matchers {

  private def seed(rig: Rig, count: Int): Seq[models.ResolvedMovie] =
    (1 to count).map { i =>
      val movie = film(id = f"fback$i%03d")
      rig.readModel.upsertMovie(movie); rig.readModel.upsertScreening(screening(movie._id))
      movie
    }

  private def newSeries() = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)

  private def backfillOn(rig: Rig, series: ShareCardMetrics.Series = newSeries(), maxBacklog: Int = 30): ShareCardBackfill =
    new ShareCardBackfill(rig.service, rig.readModel, rig.queue, series.forCountry("pl"), rig.clock,
      maxBacklog = settings.ShareCardBackfillMaxBacklog(maxBacklog))

  /** A `PruneShareCards` pass finished, as the task framework announces it. */
  private def prunePass(backfill: ShareCardBackfill, mode: String = PruneShareCardsHandler.Budget): Unit =
    ShareCardBackfill.onTaskFinished(backfill)(TaskFinished(TaskType.PruneShareCards, s"share-card-$mode", Map(PruneShareCardsHandler.ModeKey -> mode)))

  /** Render every waiting card, announcing each finished render as the task framework does.
   *  Returns the films rendered. */
  private def renderWaiting(rig: Rig, backfill: ShareCardBackfill): Seq[String] =
    drain(rig.queue).map { task =>
      new RenderShareCardHandler(rig.service).handle(task)
      ShareCardBackfill.onTaskFinished(backfill)(TaskFinished(task.taskType, task.dedupKey, task.payload))
      task.payload("filmId")
    }

  private def waiting(rig: Rig): Int = rig.queue.waitingCount(TaskType.RenderShareCard)

  "The backfill" should "sweep at the first prune pass after boot, and let a missing card in as each render finishes, never past the backlog cap" in {
    val rig = new Rig
    val films = seed(rig, 12)
    rig.readModel.upsertMovie(film(id = "foffscreen"))                     // no screenings: no card
    val series   = newSeries()
    val backfill = backfillOn(rig, series, maxBacklog = 5)
    prunePass(backfill)
    waiting(rig) shouldBe 5
    prunePass(backfill)                                                    // no second sweep; the cap holds
    waiting(rig) shouldBe 5
    val queued = drain(rig.queue)
    queued.map(_.payload("reasons")).distinct shouldBe Seq(ShareCardReason.NewFilm)
    queued.foreach(task => rig.queue.enqueue(task.taskType, task.dedupKey, task.payload, submittedAt = rig.clock.instant()))

    val rendered = Iterator.continually(renderWaiting(rig, backfill)).takeWhile(_.nonEmpty).toSeq
    rendered.map(_.size) shouldBe Seq(5, 5, 2)                             // each finished render made room for the next
    rendered.flatten should contain theSameElementsAs films.map(_._id)
    series.coverageFor("pl") shouldBe 1.0
  }

  it should "not end its sweep on an unread web_movies — the next prune pass sweeps again" in {
    val readModel = new services.readmodel.UnreadableReadModelRepository
    readModel.failingReads = false
    val rig = new Rig(readModel = readModel)
    seed(rig, 3)
    readModel.failingReads = true
    readModel.screeningsReadable = true               // only web_movies is unreadable
    val backfill = backfillOn(rig)
    prunePass(backfill)
    waiting(rig) shouldBe 0

    readModel.healReads()
    prunePass(backfill)
    waiting(rig) shouldBe 3                           // swept at the next pass, not a day later
  }

  it should "sweep again after the daily prune, not after every budget pass" in {
    val rig = new Rig
    seed(rig, 2).foreach(m => rig.service.render(rig.service.inputs(m), Seq(ShareCardReason.NewFilm)))
    val series   = newSeries()
    val backfill = backfillOn(rig, series)
    prunePass(backfill)
    series.coverageFor("pl") shouldBe 1.0

    // On screen, but no projection told the backfill: only a sweep finds it.
    val unseen = film(id = "funseen")
    rig.readModel.upsertMovie(unseen); rig.readModel.upsertScreening(screening(unseen._id))
    prunePass(backfill)
    waiting(rig) shouldBe 0
    prunePass(backfill, PruneShareCardsHandler.Daily)
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(unseen._id)
    series.coverageFor("pl") shouldBe 2.0 / 3
  }

  it should "count a newly projected film as missing its card until that card's render finishes" in {
    val rig = new Rig
    seed(rig, 2).foreach(m => rig.service.render(rig.service.inputs(m), Seq(ShareCardReason.NewFilm)))
    val series   = newSeries()
    val backfill = backfillOn(rig, series)
    val ledger   = new BackfilledShareCardLedger(rig.service, backfill)
    prunePass(backfill)

    val arrived = film(id = "farrived")
    ledger.onProjected(arrived, screened = true)                          // the projection asks for its render
    series.coverageFor("pl") shouldBe 2.0 / 3
    renderWaiting(rig, backfill) shouldBe Seq(arrived._id)
    series.coverageFor("pl") shouldBe 1.0
  }

  it should "leave a film whose card was re-rendered since the sweep to the projection, and count it covered" in {
    val rig = new Rig
    val films    = seed(rig, 3)
    val series   = newSeries()
    val backfill = backfillOn(rig, series, maxBacklog = 1)
    val ledger   = new BackfilledShareCardLedger(rig.service, backfill)
    prunePass(backfill)                                                   // the sweep: all three missing
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(films(0)._id)
    // The second film's rating moved after the sweep, and the projection asked for its card for the
    // new inputs. The sweep's inputs for it are stale: rendering them would overwrite that card
    // with an older picture, under a URL whose version names the newer one.
    val moved = films(1).copy(ratings = films(1).ratings.copy(imdb = Some(8.4)))
    ledger.onProjected(moved, screened = true)
    renderWaiting(rig, backfill) shouldBe Seq(moved._id)
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(films(2)._id)
    series.coverageFor("pl") shouldBe 1.0 / 3
  }

  // The sweep's film list is a day old by its end. A film that left the screens meanwhile has its
  // card deleted at once (the projection's retirement), so it read as a film on screen without a
  // card: every country's gauge sagged overnight by exactly the films whose run ended since the sweep.
  it should "stop counting a film the moment it leaves the screens" in {
    val rig = new Rig
    val films = seed(rig, 4)
    films.take(3).foreach(m => rig.service.render(rig.service.inputs(m), Seq(ShareCardReason.NewFilm)))
    val series   = newSeries()
    val backfill = backfillOn(rig, series, maxBacklog = 0)
    val ledger   = new BackfilledShareCardLedger(rig.service, backfill)
    prunePass(backfill)
    series.coverageFor("pl") shouldBe 3.0 / 4

    ledger.onRetired(films(0)._id)                                        // left the read model
    series.coverageFor("pl") shouldBe 2.0 / 3
    ledger.onProjected(films(1), screened = false)                        // its last screening went
    series.coverageFor("pl") shouldBe 1.0 / 2
  }

  // Nothing tracked used to publish nothing, so the gauge froze at whatever the last film left it at —
  // a country whose last film without a card left the screens read 0% covered until the next film came.
  // Nor may it read as full coverage: a wiped or failed web_screenings read tracks nothing too, and 1.0
  // would silence ShareCardCoverageLow and ShareCardCoverageAbsent alike. The sample is withdrawn, so
  // Absent fires if nothing comes back.
  it should "withdraw its coverage once no film on screen expects a card, not freeze at the last ratio" in {
    val rig      = new Rig
    val only     = seed(rig, 1).head                                      // on screen, card missing
    val series   = newSeries()
    val backfill = backfillOn(rig, series, maxBacklog = 0)
    prunePass(backfill)
    series.coverageSample("pl") shouldBe Some(0.0)

    backfill.onRetired(only._id)
    series.coverageSample("pl") shouldBe None
  }

  it should "withdraw its coverage when its sweep finds no film on screen" in {
    val rig      = new Rig
    val series   = newSeries()
    series.forCountry("pl").coverage(0.25)                                // what a previous sweep left
    prunePass(backfillOn(rig, series))
    series.coverageSample("pl") shouldBe None
  }

  it should "not bring back a film that left the screens while its sweep was reading" in {
    var duringRead: () => Unit = () => ()
    val readModel = new services.readmodel.InMemoryReadModelRepository {
      override def findAllMoviesChecked(): tools.ReadOutcome[Seq[models.ResolvedMovie]] = { duringRead(); super.findAllMoviesChecked() }
    }
    val rig      = new Rig(readModel = readModel)
    val films    = seed(rig, 2)
    val backfill = backfillOn(rig)
    duringRead = () => backfill.onRetired(films(0)._id)                   // retired after the read began
    prunePass(backfill)
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(films(1)._id)
  }

  "A finished render" should "re-project its film, or end a first card's hold when no card could be made" in {
    val rig = new Rig
    val refreshed, released = collection.mutable.Buffer.empty[String]
    val followUp = new ShareCardFollowUp(rig.store, rig.service.superseded, refreshed += _, released += _)
    val made   = rig.service.inputs(film())
    val failed = rig.service.inputs(film(id = "fnoposter", poster = "https://gone.example/p.jpg"))
    rig.service.render(made, Seq(ShareCardReason.NewFilm))
    followUp.onTaskFinished(TaskFinished(TaskType.RenderShareCard, "k", made.toPayload))
    followUp.onTaskFinished(TaskFinished(TaskType.RenderShareCard, "k", failed.toPayload + (ShareCardService.FirstKey -> "true")))
    refreshed.toSeq shouldBe Seq(made.filmId)
    released.toSeq shouldBe Seq("fnoposter")
  }
}
