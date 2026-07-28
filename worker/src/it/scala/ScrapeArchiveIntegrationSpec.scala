package integration

import models.{Cinema, CinemaMovie, Movie, Multikino, Showtime}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository, ScrapeAttempt, ScrapeOutcome}
import tools.Env

import java.time.{Instant, LocalDateTime}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The macro-derived BSON codecs for `cinema_scrapes` can only fail at RUNTIME —
 * a missing provider for a nested type compiles fine and blows up on the first
 * write, and an `IgnoreNone` field with no default decodes as a missing-field
 * error on read. So the round-trip has to run against a real Mongo, with a film
 * that populates every optional and every collection field.
 *
 * The "keep the last good listing" rule is equally storage-shaped here: it is
 * enforced by a conditional update, not by the in-memory branch the unit spec
 * exercises, so it is re-asserted against the real collection.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ScrapeArchiveIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))

  private val Morning = Instant.parse("2026-07-28T09:00:00Z")
  private val Noon    = Instant.parse("2026-07-28T12:00:00Z")
  private val Evening = Instant.parse("2026-07-28T18:00:00Z")

  private def purge(): Unit =
    Await.result(db.getCollection(ScrapeArchiveRepository.Collection)
      .deleteOne(Filters.eq("_id", Multikino.displayName)).toFuture(), 10.seconds)

  private def rowCount(): Long =
    Await.result(db.getCollection(ScrapeArchiveRepository.Collection)
      .countDocuments(Filters.eq("_id", Multikino.displayName)).toFuture(), 10.seconds)

  private val fullyPopulated = CinemaMovie(
    movie       = Movie("Diuna", Some(155), Some(2026), Seq("USA", "Kanada"), Seq("Sci-Fi", "Przygodowy"),
                        Some("Dune"), Some("DIUNA (2026) — NAPISY")),
    cinema      = Multikino,
    posterUrl   = Some("https://example.test/poster.jpg"),
    filmUrl     = Some("https://example.test/film/diuna"),
    synopsis    = Some("Paul Atryda wyrusza na Arrakis."),
    cast        = Seq("Timothée Chalamet", "Zendaya"),
    director    = Seq("Denis Villeneuve"),
    showtimes   = Seq(
      Showtime(LocalDateTime.of(2026, 8, 1, 18, 30), Some("https://example.test/book/1"), Some("Sala 4"), List("2D", "NAP")),
      // Every optional absent — the IgnoreNone path that decodes back to None/Nil.
      Showtime(LocalDateTime.of(2026, 8, 1, 21, 0), None)
    ),
    externalIds = Map("filmweb" -> "123456"),
    trailerUrl  = Some("https://youtube.test/watch?v=abc"),
    ageRating   = Some("15")
  )

  private val minimal = CinemaMovie(Movie("Anora"), Multikino, None, None, None, Seq.empty, Seq.empty,
    Seq(Showtime(LocalDateTime.of(2026, 8, 2, 12, 0), None)))

  private def scraped(at: Instant, films: Seq[CinemaMovie]) =
    ScrapeAttempt(Multikino, Cinema.cityOf(Multikino), at, listingComplete = true, films)

  private def blank(at: Instant) =
    ScrapeAttempt(Multikino, Cinema.cityOf(Multikino), at, listingComplete = true, films = Seq.empty)

  private def threw(at: Instant, error: String) =
    ScrapeAttempt(Multikino, Cinema.cityOf(Multikino), at, listingComplete = true, Seq.empty, error = Some(error))

  "MongoScrapeArchiveRepository" should "round-trip a full scrape through real BSON" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    repository.enabled shouldBe true
    try {
      repository.record(scraped(Noon, Seq(fullyPopulated, minimal)))

      val stored = repository.find(Multikino).getOrElse(fail("nothing archived"))
      stored.cinema    shouldBe Multikino
      stored.city      shouldBe Cinema.cityOf(Multikino)
      stored.contentAt shouldBe Some(Noon)
      stored.outcome   shouldBe ScrapeOutcome.Ok
      stored.lastSuccess.map(_.listingComplete) shouldBe Some(true)
      stored.films should have size 2

      val diuna = stored.films.find(_.movie.title == "Diuna").getOrElse(fail("Diuna missing"))
      diuna shouldBe fullyPopulated
      // Spelled out, because each of these is a field a replay would silently
      // lose if its codec were wrong rather than absent.
      diuna.movie.originalTitle     shouldBe Some("Dune")
      diuna.movie.countries         shouldBe Seq("USA", "Kanada")
      diuna.externalIds             shouldBe Map("filmweb" -> "123456")
      diuna.ageRating               shouldBe Some("15")
      diuna.showtimes.head.room     shouldBe Some("Sala 4")
      diuna.showtimes.head.format   shouldBe List("2D", "NAP")
      diuna.showtimes(1).bookingUrl shouldBe None
      diuna.showtimes(1).room       shouldBe None
      diuna.showtimes(1).format     shouldBe Nil

      stored.films.find(_.movie.title == "Anora").getOrElse(fail("Anora missing")) shouldBe minimal
    } finally purge()
  }

  it should "replace the row on re-scrape rather than accumulate" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    try {
      repository.record(scraped(Morning, Seq(fullyPopulated, minimal)))
      repository.record(scraped(Noon, Seq(minimal)))

      val stored = repository.find(Multikino).getOrElse(fail("nothing archived"))
      stored.films.map(_.movie.title) shouldBe Seq("Anora")
      stored.contentAt                shouldBe Some(Noon)
      rowCount()                      shouldBe 1L
    } finally purge()
  }

  it should "keep the stored listing when a later scrape comes back empty or throws" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    try {
      repository.record(scraped(Morning, Seq(fullyPopulated)))
      repository.record(blank(Noon))

      val afterBlank = repository.find(Multikino).getOrElse(fail("nothing archived"))
      afterBlank.films.map(_.movie.title) shouldBe Seq("Diuna")
      afterBlank.contentAt                shouldBe Some(Morning)
      afterBlank.outcome                  shouldBe ScrapeOutcome.Empty
      afterBlank.lastBarren.map(_.at)     shouldBe Some(Noon)
      afterBlank.current                  shouldBe false

      repository.record(threw(Evening, "503 from multikino.pl"))
      val afterThrow = repository.find(Multikino).getOrElse(fail("nothing archived"))
      afterThrow.films.map(_.movie.title)    shouldBe Seq("Diuna")
      afterThrow.outcome                     shouldBe ScrapeOutcome.Failed
      afterThrow.lastBarren.flatMap(_.error) shouldBe Some("503 from multikino.pl")
      afterThrow.lastBarren.map(_.at)        shouldBe Some(Evening)
      rowCount()                             shouldBe 1L
    } finally purge()
  }

  it should "clear the barren marker when the cinema recovers" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    try {
      repository.record(scraped(Morning, Seq(fullyPopulated)))
      repository.record(threw(Noon, "503"))
      repository.record(scraped(Evening, Seq(minimal)))

      val stored = repository.find(Multikino).getOrElse(fail("nothing archived"))
      stored.lastBarren               shouldBe None
      stored.outcome                  shouldBe ScrapeOutcome.Ok
      stored.current                  shouldBe true
      stored.films.map(_.movie.title) shouldBe Seq("Anora")
    } finally purge()
  }

  it should "ignore a barren attempt older than the stored listing" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    try {
      repository.record(scraped(Noon, Seq(fullyPopulated)))
      repository.record(blank(Morning))

      val stored = repository.find(Multikino).getOrElse(fail("nothing archived"))
      stored.lastBarren shouldBe None
      stored.current    shouldBe true
    } finally purge()
  }

  it should "record a cinema that has only ever failed as a listing-less row" in {
    val repository = new MongoScrapeArchiveRepository(Some(db))
    try {
      repository.record(threw(Noon, "DNS failure"))

      val stored = repository.find(Multikino).getOrElse(fail("nothing archived"))
      stored.lastSuccess                     shouldBe None
      stored.films                           shouldBe empty
      stored.outcome                         shouldBe ScrapeOutcome.Failed
      stored.lastBarren.flatMap(_.error)     shouldBe Some("DNS failure")
      // …and a later real scrape fills it in without leaving a second row.
      repository.record(scraped(Evening, Seq(minimal)))
      repository.find(Multikino).flatMap(_.contentAt) shouldBe Some(Evening)
      rowCount()                                      shouldBe 1L
    } finally purge()
  }

  it should "no-op without a database rather than fail the scrape that fed it" in {
    val repository = new MongoScrapeArchiveRepository(None)
    repository.enabled shouldBe false
    repository.record(scraped(Noon, Seq(minimal)))
    repository.find(Multikino) shouldBe None
    repository.findAll()       shouldBe empty
  }
}
