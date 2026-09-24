package services.readmodel

import models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.InMemoryMovieRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.{Clock, Instant, LocalDateTime, ZoneId, ZoneOffset}
import scala.concurrent.duration.*

/** The projection's share-card seam: `shareCard` on `web_movies`, and the first-publish gate. */
class ReadModelProjectorShareCardSpec extends AnyFlatSpec with Matchers {

  private val T0 = Instant.parse("2026-06-01T10:00:00Z")

  /** A clock the spec moves by hand. */
  private final class StepClock(var now: Instant) extends Clock {
    def getZone: ZoneId = ZoneOffset.UTC
    override def withZone(zone: ZoneId): Clock = this
    def instant(): Instant = now
  }

  /** The ledger as the projection sees it: which cards exist is set by the spec. */
  private final class ScriptedLedger extends ShareCardLedger {
    var cards     = Map.empty[String, String]
    val requested = scala.collection.mutable.Buffer.empty[(String, Instant)]
    val projected = scala.collection.mutable.Buffer.empty[String]
    val landed    = scala.collection.mutable.Buffer.empty[String]
    def current(movie: ResolvedMovie): Option[String]      = cards.get(movie._id)
    def readyToPublish(movie: ResolvedMovie): Boolean      = cards.contains(movie._id)
    def requestFirstCard(movie: ResolvedMovie, until: Instant): Unit = requested += (movie._id -> until)
    def onPendingCardLanded(filmId: String): Unit          = landed += filmId
    def onProjected(movie: ResolvedMovie, screened: Boolean): Unit = projected += movie._id
    val retired = scala.collection.mutable.Buffer.empty[String]
    def onRetired(filmId: String): Unit                    = retired += filmId
  }

  private def slot(screened: Boolean = true) = SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"),
    showtimes = if (screened) Seq(Showtime(LocalDateTime.parse("2026-06-02T18:00"), None)) else Nil)
  private def record(rating: Double, screened: Boolean = true) =
    MovieRecord(imdbRating = Some(rating), tmdbId = Some(1), data = Map[Source, SourceData](Multikino -> slot(screened)))

  private class Setup {
    val clock      = new StepClock(T0)
    val ledger     = new ScriptedLedger
    val repository = new InMemoryMovieRepository()
    val readModel  = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, readModel, readModel, shareCards = ledger, firstCardHold = 2.minutes, clock = clock)
    def upsert(rating: Double, screened: Boolean = true): String = {
      repository.upsert("Foo", Some(2024), record(rating, screened))
      val row = repository.findAll().head
      projector.onMovieUpsert(row)
      ReadModelProjection.filmId(row, titleNormalizer)
    }
    def published(id: String): Option[ResolvedMovie] = readModel.findAllMovies().find(_._id == id)
  }

  "A new film" should "not be published before its share card exists" in new Setup {
    val id = upsert(7.5)
    published(id) shouldBe None
    readModel.findAllScreenings() shouldBe empty
    ledger.requested.toSeq shouldBe Seq(id -> T0.plusSeconds(120))

    ledger.cards = Map(id -> "card-a.jpg")       // the render task finished
    projector.refreshShareCard(id)
    published(id).map(_.shareCard) shouldBe Some(Some("card-a.jpg"))
    published(id).map(_.shareCardPending) shouldBe Some(false)
    readModel.findAllScreenings() should not be empty
  }

  it should "be published anyway, with the fallback and marked pending, once its hold runs out" in new Setup {
    val id = upsert(7.5)
    clock.now = T0.plusSeconds(119)
    projector.releaseExpiredHolds()
    published(id) shouldBe None

    clock.now = T0.plusSeconds(120)
    projector.releaseExpiredHolds()
    published(id).map(m => (m.shareCard, m.shareCardPending)) shouldBe Some((None, true))
  }

  it should "lose its pending mark, and have the scrapers told, when its card finally lands" in new Setup {
    val id = upsert(7.5)
    clock.now = T0.plusSeconds(121)
    projector.releaseExpiredHolds()
    upsert(7.6)                                              // still no card: stays pending
    published(id).map(_.shareCardPending) shouldBe Some(true)
    ledger.landed shouldBe empty

    ledger.cards = Map(id -> "card-b.jpg")
    projector.refreshShareCard(id)
    published(id).map(m => (m.shareCard, m.shareCardPending)) shouldBe Some((Some("card-b.jpg"), false))
    ledger.landed.toSeq shouldBe Seq(id)
  }

  "A published film whose card inputs change" should "not be gated: it keeps its current card and is written at once" in new Setup {
    val id = upsert(7.5)
    ledger.cards = Map(id -> "card-old.jpg")
    projector.refreshShareCard(id)
    published(id).map(_.shareCard) shouldBe Some(Some("card-old.jpg"))
    ledger.projected.clear()
    ledger.requested.clear()

    // A rating moves; the ledger has no card for the new inputs yet, so it answers the old one.
    upsert(8.2)
    published(id).map(m => (m.ratings.imdb, m.shareCard)) shouldBe Some((Some(8.2), Some("card-old.jpg")))
    ledger.projected.toSeq shouldBe Seq(id)
    ledger.requested shouldBe empty
    projector.heldCards shouldBe empty
  }

  "A film whose document exists but that is on no screen" should "be held like a new film when it comes back on screen" in new Setup {
    // Published once, then off the screens: the daily prune has retired its card since.
    val id = upsert(7.5)
    ledger.cards = Map(id -> "card.jpg?v=0")
    projector.refreshShareCard(id)
    upsert(7.5, screened = false)
    readModel.findAllScreenings() shouldBe empty
    ledger.cards = Map.empty
    ledger.requested.clear()
    ledger.projected.clear()

    // Back on screen with an unchanged document: nothing may serve it before its card exists.
    upsert(7.5)
    readModel.findAllScreenings() shouldBe empty
    ledger.requested.map(_._1).toSeq shouldBe Seq(id)
    ledger.retired shouldBe empty
    published(id) should not be empty                 // the unserved document is left alone

    ledger.cards = Map(id -> "card.jpg?v=1")
    projector.refreshShareCard(id)
    published(id).map(_.shareCard) shouldBe Some(Some("card.jpg?v=1"))
    readModel.findAllScreenings() should not be empty
  }

  "A film first published with no screenings" should "be held when its first screenings arrive" in new Setup {
    val id = upsert(7.5, screened = false)
    published(id) should not be empty
    ledger.requested shouldBe empty

    upsert(7.5)
    readModel.findAllScreenings() shouldBe empty
    ledger.requested.map(_._1).toSeq shouldBe Seq(id)
  }

  "A film dropped from the read model" should "have its share-card files retired at once" in new Setup {
    val id = upsert(7.5)
    ledger.cards = Map(id -> "card.jpg?v=0")
    projector.refreshShareCard(id)
    projector.onMovieDelete(repository.findAll().head.id)
    published(id) shouldBe None
    ledger.retired.toSeq shouldBe Seq(id)
  }

  "With no share cards at all" should "publish at once, carrying no card" in {
    val repository = new InMemoryMovieRepository()
    val readModel  = new InMemoryReadModelRepository()
    val projector  = new ReadModelProjector(repository, readModel, readModel)
    repository.upsert("Foo", Some(2024), record(7.5))
    projector.onMovieUpsert(repository.findAll().head)
    readModel.findAllMovies().map(_.shareCard) shouldBe Seq(None)
  }
}
