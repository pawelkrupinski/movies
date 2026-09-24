package services.sharecards

import models.Country
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

  "The backfill" should "not end its sweep on an unread web_movies — the next tick sweeps again" in {
    val readModel = new services.readmodel.UnreadableReadModelRepository
    readModel.failingReads = false
    val rig = new Rig(readModel = readModel)
    seed(rig, 3)
    readModel.failingReads = true
    readModel.screeningsReadable = true               // only web_movies is unreadable
    val backfill = new ShareCardBackfill(rig.service, rig.readModel, rig.queue, rig.metrics, rig.clock, batch = 20, maxBacklog = 30)
    backfill.tick() shouldBe 0

    readModel.healReads()
    backfill.tick() shouldBe 3                        // swept on the next tick, not a day later
  }

  it should "feed missing cards into the queue in bounded batches, never past the backlog cap" in {
    val rig = new Rig
    seed(rig, 50)
    rig.readModel.upsertMovie(film(id = "foffscreen"))                     // no screenings: no card
    val backfill = new ShareCardBackfill(rig.service, rig.readModel, rig.queue, rig.metrics, rig.clock, batch = 20, maxBacklog = 30)
    backfill.tick() shouldBe 20
    backfill.tick() shouldBe 10                                             // 30 waiting: the cap
    backfill.tick() shouldBe 0
    val queued = drain(rig.queue)
    queued.map(_.taskType).distinct shouldBe Seq(TaskType.RenderShareCard)
    queued.map(_.payload("filmId")) should not contain "foffscreen"
    queued.map(_.payload("reasons")).distinct shouldBe Seq(ShareCardReason.NewFilm)
    backfill.tick() shouldBe 20                                             // queue drained: the next batch
  }

  it should "skip films whose card landed meanwhile, and report coverage" in {
    val rig = new Rig
    val films = seed(rig, 4)
    val series = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val metrics = series.forCountry("pl")
    val backfill = new ShareCardBackfill(rig.service, rig.readModel, rig.queue, metrics, rig.clock, batch = 1, maxBacklog = 10)
    backfill.tick() shouldBe 1
    films.take(2).foreach(m => rig.service.render(rig.service.inputs(m), Seq(ShareCardReason.Backfill)))
    backfill.tick() shouldBe 1
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(films(0)._id, films(2)._id)
    val snapshot = series.coverageFor("pl")
    snapshot shouldBe 0.5
  }

  it should "leave a film whose card was re-rendered since the sweep to the projection, and count it covered" in {
    val rig = new Rig
    val films = seed(rig, 3)
    val series = new ShareCardMetrics.Series(Seq("pl"), new io.prometheus.metrics.model.registry.PrometheusRegistry)
    val backfill = new ShareCardBackfill(rig.service, rig.readModel, rig.queue, series.forCountry("pl"), rig.clock, batch = 1, maxBacklog = 10)
    backfill.tick() shouldBe 1                                               // the sweep: all three missing
    drain(rig.queue)
    // The second film's rating moved after the sweep; the projection rendered its card for the
    // new inputs. The sweep's inputs for it are stale: rendering them would overwrite that card
    // with an older picture, under a URL whose version names the newer one.
    val moved = films(1).copy(ratings = films(1).ratings.copy(imdb = Some(8.4)))
    rig.readModel.upsertMovie(moved)
    rig.service.render(rig.service.inputs(moved), Seq(ShareCardReason.Ratings))
    backfill.tick() shouldBe 1
    backfill.tick() shouldBe 0
    drain(rig.queue).map(_.payload("filmId")) shouldBe Seq(films(2)._id)
    series.coverageFor("pl") shouldBe 1.0 / 3
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

class ShareCardRescraperSpec extends AnyFlatSpec with Matchers {

  private final class CountingGraph(failing: Set[String] = Set.empty) extends FacebookGraph {
    val urls = collection.mutable.Buffer.empty[String]
    def scrape(url: String): Either[String, Unit] = { urls += url; if (failing(url)) Left("HTTP 500") else Right(()) }
  }

  "A pending film's landed card" should "have Facebook re-scrape each of its city pages" in {
    val rig = new Rig
    val movie = film()
    rig.readModel.upsertMovie(movie)
    rig.readModel.upsertScreening(screening(movie._id, "poznan"))
    rig.readModel.upsertScreening(screening(movie._id, "wroclaw"))
    val graph = new CountingGraph
    new ShareCardRescraper(Some(graph), rig.readModel, Country.default, rig.metrics, rig.clock).rescrape(movie._id) shouldBe true
    graph.urls.toSeq shouldBe Seq("https://kinowo.net/poznan/movie/diuna", "https://kinowo.net/wroclaw/movie/diuna")
  }

  it should "ask to be retried when Facebook refuses, and be a no-op without app credentials" in {
    val rig = new Rig
    val movie = film()
    rig.readModel.upsertMovie(movie); rig.readModel.upsertScreening(screening(movie._id))
    new ShareCardRescraper(Some(new CountingGraph(failing = Set("https://kinowo.net/poznan/movie/diuna"))), rig.readModel,
      Country.default, rig.metrics, rig.clock).rescrape(movie._id) shouldBe false
    new ShareCardRescraper(None, rig.readModel, Country.default, rig.metrics, rig.clock).rescrape(movie._id) shouldBe true
  }

  // The page list comes from the read model. A read that failed produced no pages, so "no
  // request failed" held and a re-scrape that never ran was reported done.
  it should "ask to be retried, not report done, when the read model could not be read" in {
    Seq(false, true).foreach { screeningsReadable =>
      val readModel = new services.readmodel.UnreadableReadModelRepository
      readModel.failingReads = false
      val rig   = new Rig(readModel = readModel)
      val movie = film()
      readModel.upsertMovie(movie); readModel.upsertScreening(screening(movie._id))
      val graph = new CountingGraph
      val rescraper = new ShareCardRescraper(Some(graph), readModel, Country.default, rig.metrics, rig.clock)
      readModel.failingReads = true
      readModel.screeningsReadable = screeningsReadable   // true: only web_movies (the slugs) is unreadable
      withClue(s"screeningsReadable=$screeningsReadable: ") {
        rescraper.rescrape(movie._id) shouldBe false
        graph.urls shouldBe empty
      }
    }
  }

  // A burst of re-scrapes (a template change re-draws every recent film) runs one task per film,
  // 10s apart. Each read the page list off a scan of the WHOLE of web_screenings — hundreds of
  // thousands of ids in the US — to keep the few rows filed under one film.
  it should "read only the film's own screenings for its pages, never the whole of web_screenings" in {
    val readModel = new services.readmodel.InMemoryReadModelRepository {
      override def findAllScreeningRefsChecked(): (Seq[services.readmodel.ScreeningRef], Boolean) =
        fail("a re-scrape scanned every screening in the read model for one film's pages")
    }
    val rig   = new Rig(readModel = readModel)
    val movie = film()
    readModel.upsertMovie(movie)
    readModel.upsertScreening(screening(movie._id, "poznan"))
    readModel.upsertScreening(screening(movie._id, "wroclaw"))
    val graph = new CountingGraph
    new ShareCardRescraper(Some(graph), readModel, Country.default, rig.metrics, rig.clock).rescrape(movie._id) shouldBe true
    graph.urls.toSeq shouldBe Seq("https://kinowo.net/poznan/movie/diuna", "https://kinowo.net/wroclaw/movie/diuna")
  }

  it should "read the film slugs once for a burst of re-scrapes, and again for a film they lack" in {
    val rig = new Rig
    val first  = film()
    val second = film(id = "fsecond", title = "Oppenheimer")
    rig.readModel.upsertMovie(first); rig.readModel.upsertScreening(screening(first._id))
    val graph     = new CountingGraph
    val rescraper = new ShareCardRescraper(Some(graph), rig.readModel, Country.default, rig.metrics, rig.clock)
    val before    = rig.readModel.findAllMoviesCalls.get
    rescraper.rescrape(first._id); rescraper.rescrape(first._id)
    rig.readModel.findAllMoviesCalls.get - before shouldBe 1

    rig.readModel.upsertMovie(second); rig.readModel.upsertScreening(screening(second._id))
    rescraper.rescrape(second._id)                                          // new since: read again
    rig.readModel.findAllMoviesCalls.get - before shouldBe 2
    graph.urls.last shouldBe "https://kinowo.net/poznan/movie/oppenheimer"
  }

  "The landing of a pending film's card" should "queue one spaced-out re-scrape task" in {
    val rig = new Rig
    rig.service.onPendingCardLanded("fa"); rig.service.onPendingCardLanded("fb")
    val tasks = drain(rig.queue)
    tasks.map(t => (t.taskType, t.payload("filmId"))) shouldBe Seq(TaskType.RescrapeShareCard -> "fa", TaskType.RescrapeShareCard -> "fb")
  }
}
