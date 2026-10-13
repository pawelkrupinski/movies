package services.sharecards

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{City, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.InMemoryReadModelRepository
import services.tasks.{HandlerOutcome, InMemoryTaskQueue, Task, TaskType}
import ShareCardTestKit.*
import tools.{CityCardRenderer, MutableClock}

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.nio.file.Files
import java.time.{Duration, Instant}
import javax.imageio.ImageIO

class CityCardServiceSpec extends AnyFlatSpec with Matchers {
  import CityCardServiceSpec.*

  "A city card" should "draw the city's most widely shown film first, from the cached posters, and be a 1200×630 JPEG" in new Setup {
    // fwide plays at two of Poznań's cinemas, fnarrow at one; fnoposter has nothing cached.
    showAt("fnarrow", "poznan", "Kino Muza")
    showAt("fwide", "poznan", "Kino Muza"); showAt("fwide", "poznan", "Rialto")
    showAt("fnoposter", "poznan", "Rialto")
    cachePoster("fwide", Color.RED); cachePoster("fnarrow", Color.GREEN)
    service.render(Poznan) shouldBe ShareCardMetrics.CityOutcome.Rendered
    val card = ImageIO.read(store.cityCardPath(Poznan).toFile)
    (card.getWidth, card.getHeight) shouldBe (1200, 630)
    val (x0, y0) = CityCardRenderer.tileAt(0)
    val (x1, y1) = CityCardRenderer.tileAt(1)
    near(new Color(card.getRGB(x0 + 180, y0 + 150)), Color.RED) shouldBe true
    near(new Color(card.getRGB(x1 + 180, y1 + 150)), Color.GREEN) shouldBe true
    store.version(store.cityCardPath(Poznan)) shouldBe defined
    store.written(store.cityCardPath(Poznan)) shouldBe Some(clock.instant())
  }

  it should "only move its date when a re-render would draw the same picture" in new Setup {
    cachePoster("fwide", Color.RED); showAt("fwide", "poznan", "Kino Muza")
    service.render(Poznan) shouldBe ShareCardMetrics.CityOutcome.Rendered
    val bytes = Files.readAllBytes(store.cityCardPath(Poznan))
    clock.advance(Duration.ofDays(8))
    service.render(Poznan) shouldBe ShareCardMetrics.CityOutcome.Unchanged
    Files.readAllBytes(store.cityCardPath(Poznan)) shouldBe bytes
    store.written(store.cityCardPath(Poznan)) shouldBe Some(clock.instant())
    series.cityRenderCount("pl", ShareCardMetrics.CityOutcome.Unchanged) shouldBe 1.0
  }

  it should "be drawn without posters for a city showing nothing yet" in new Setup {
    service.render(Poznan) shouldBe ShareCardMetrics.CityOutcome.Rendered
    Files.exists(store.cityCardPath(Poznan)) shouldBe true
  }

  it should "be refused, not written, when the directory has no room for a new file" in new Setup(roomFull = true) {
    service.render(Poznan) shouldBe ShareCardMetrics.CityOutcome.Refused
    Files.exists(store.cityCardPath(Poznan)) shouldBe false
    series.refusedCount("pl", ShareCardStore.Kind.City) shouldBe 1.0
    handler.handle(task(Poznan)) shouldBe HandlerOutcome.Done
  }

  "The reaper" should "enqueue the cities with no card first, at most the cap a tick, and never twice" in new Setup(cap = 5) {
    val withCard = Country.Poland.cities.take(3)
    withCard.foreach(city => card(city, clock.instant()))
    service.enqueueDue() shouldBe 5
    val asked = waitingKeys
    asked should have size 5
    asked.intersect(withCard.map(CityCardService.dedupKey).toSet) shouldBe empty
    service.enqueueDue() shouldBe 0 // the same five still wait: duplicates
  }

  it should "give every city in the catalog a card, renders following the queue" in new Setup(cap = 1000) {
    service.enqueueDue() shouldBe Country.Poland.cities.size
    drain(queue).foreach(handler.handle)
    Country.Poland.cities.filterNot(city => Files.exists(store.cityCardPath(city))) shouldBe empty
    series.cityCoverageFor("pl") shouldBe 0.0 // published at the tick, before the renders
    service.enqueueDue() shouldBe 0
    series.cityCoverageFor("pl") shouldBe 1.0
  }

  it should "count a card not rendered for two weeks as missing in the coverage" in new Setup {
    Country.Poland.cities.foreach(city => card(city, clock.instant()))
    card(Poznan, clock.instant().minus(Duration.ofDays(15)))
    service.enqueueDue()
    series.cityCoverageFor("pl") shouldBe (Country.Poland.cities.size - 1).toDouble / Country.Poland.cities.size
  }

  "The handler" should "skip a city whose card is not due, and render one that is" in new Setup {
    service.render(Poznan)
    handler.handle(task(Poznan)) shouldBe HandlerOutcome.Skipped // just rendered: its phase has not come round
    clock.advance(Duration.ofDays(8))
    handler.handle(task(Poznan)) shouldBe HandlerOutcome.Done
    handler.handle(Task("t", TaskType.RenderCityCard, "city-card|atlantis", Map(CityCardService.CityKey -> "atlantis"), 0)) shouldBe HandlerOutcome.Skipped
  }

  "The weekly spread" should "re-render every city exactly once a week, evenly across the days" in {
    // The US catalog, ~2,000 cities, every card rendered at the same instant (a cold start just
    // done): a burst it must NOT repeat. Hourly ticks over the next week; a claimed render stamps
    // the card's mtime, as `render` does.
    val clock   = new MutableClock(T0)
    val store   = tempStore()
    val queue   = new InMemoryTaskQueue
    val country = Country.UnitedStates
    val service = new CityCardService(country, store, new InMemoryReadModelRepository, queue, ShareCardMetrics.noop, clock)
    country.cities.foreach(city => card(store, city, T0))
    val renders = collection.mutable.Buffer.empty[(String, Instant)]
    (1 to 7 * 24).foreach { _ =>
      clock.advance(Duration.ofHours(1))
      service.enqueueDue() should be <= CityCardService.MaxEnqueuePerTick
      drain(queue).foreach { t =>
        val slug = t.payload(CityCardService.CityKey)
        renders += slug -> clock.instant()
        store.touch(store.cityCardPath(country.bySlug(slug)), clock.instant())
      }
    }
    renders.map(_._1).groupBy(identity).values.map(_.size).toSet shouldBe Set(1)
    renders.map(_._1).toSet shouldBe country.cities.map(_.slug).toSet
    val perDay = renders.groupBy { case (_, at) => Duration.between(T0, at).minusHours(1).toDays }.values.map(_.size)
    val mean   = country.cities.size / 7.0
    perDay.foreach(n => n.toDouble shouldBe mean +- mean * 0.2)
  }
}

object CityCardServiceSpec {
  val Poznan: City = Country.Poland.bySlug("poznan")

  def near(c: Color, target: Color, tol: Int = 40): Boolean =
    math.abs(c.getRed - target.getRed) <= tol && math.abs(c.getGreen - target.getGreen) <= tol && math.abs(c.getBlue - target.getBlue) <= tol

  /** An existing city card, last rendered at `at`. */
  def card(store: ShareCardStore, city: City, at: Instant): Unit = {
    Files.write(store.cityCardPath(city), Array[Byte](1))
    store.touch(store.cityCardPath(city), at)
  }

  class Setup(cap: Int = CityCardService.MaxEnqueuePerTick, roomFull: Boolean = false) {
    val series    = new ShareCardMetrics.Series(Seq("pl"), new PrometheusRegistry)
    val metrics   = series.forCountry("pl")
    val store     = {
      val dir = Files.createTempDirectory("share-cards-")
      val s = new ShareCardStore(dir, if (roomFull) new ShareCardRoom(Long.MaxValue, () => 0L, metrics.refused) else ShareCardRoom.Unlimited)
      assert(s.usable); s
    }
    val clock     = new MutableClock(T0)
    val queue     = new InMemoryTaskQueue
    val readModel = new InMemoryReadModelRepository
    val service   = new CityCardService(Country.Poland, store, readModel, queue, metrics, clock, maxEnqueuePerTick = cap)
    val handler   = new RenderCityCardHandler(service)

    def card(city: City, at: Instant): Unit = CityCardServiceSpec.card(store, city, at)
    def task(city: City): Task = Task("t", TaskType.RenderCityCard, CityCardService.dedupKey(city), Map(CityCardService.CityKey -> city.slug), 0)

    def showAt(filmId: String, city: String, cinema: String): Unit =
      readModel.upsertScreening(screening(filmId, city).copy(_id = s"$filmId|$city|$cinema", cinema = cinema))

    /** A poster in the film cards' poster cache, as the film card's render leaves it. */
    def cachePoster(filmId: String, colour: Color): Unit = {
      val image = new BufferedImage(420, 630, BufferedImage.TYPE_INT_RGB)
      val g = image.createGraphics(); g.setColor(colour); g.fillRect(0, 0, 420, 630); g.dispose()
      val out = new ByteArrayOutputStream(); ImageIO.write(image, "jpg", out)
      store.writeAtomically(store.posterPath(filmId), out.toByteArray, s"poster-$filmId")
    }

    /** The keys of the waiting tasks, left waiting. */
    def waitingKeys: Set[String] = {
      val tasks = drain(queue)
      tasks.foreach(t => queue.enqueue(t.taskType, t.dedupKey, t.payload, submittedAt = T0))
      tasks.map(_.dedupKey).toSet
    }
  }
}
