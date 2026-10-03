package services.movies

import models.{CinemaShowing, KinoApollo, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A film's slots at some venues read alone — `readVenues`, what a change confined to those venues'
 * showtimes is applied from — must be EXACTLY the slots the whole-film read stitches at those
 * venues, on a real split store: its per-venue `_id` ranges are Mongo queries the in-memory store
 * only imitates.
 */
class VenueReadIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private def at(hour: Int) = Showtime(java.time.LocalDateTime.of(2031, 6, 12, hour, 0), Some(s"https://book/$hour"))

  // `SourceData`'s equality is showtime-blind by design: every field, arrays by content.
  private def everyField(slots: Iterable[(Source, SourceData)]) = slots.toSeq.map { case (source, slot) =>
    source.displayName -> slot.productIterator.map { case a: Array[?] => a.toSeq; case other => other }.toList }.sortBy(_._1)

  "readVenues" should "read the venues' slots exactly as the whole-film read stitches them" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "venue-read") { db =>
      val repository = new MongoMovieRepository(Some(db), screenings = Some(new MongoScreeningsRepository(Some(db))),
        slots = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)
      repository.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1064213), data = Map[Source, SourceData](
        CinemaShowing(Multikino, "anora")           -> SourceData(title = Some("Anora"), filmUrl = Some("https://mk/anora"), showtimes = Seq(at(18), at(21))),
        CinemaShowing(Multikino, "anora35mm")       -> SourceData(title = Some("Anora (35mm)"), showtimes = Seq.empty),
        CinemaShowing(KinoApollo, "anora")          -> SourceData(title = Some("Anora"), filmUrl = Some("https://apollo/anora"), showtimes = Seq(at(20))))))
      val id    = repository.findAll().head.id
      val whole = repository.findByIdChecked(id).answered.get.record.data

      val venues = repository.readVenues(id.value, Set(Multikino)).get
      venues.atCinemas.keySet shouldBe Set(Multikino)
      everyField(venues.atCinemas(Multikino)) shouldBe everyField(whole.collect { case (s @ CinemaShowing(Multikino, _), slot) => s -> slot })
      withClue("only the asked venue's rows: ") { venues.atCinemas.values.flatten.map(_._1.cinema).toSet shouldBe Set(Multikino) }
    }
  }

  it should "decline when a venue's showtimes row has no slot row beside it" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "venue-read-orphan") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val repository = new MongoMovieRepository(Some(db), screenings = Some(screenings),
        slots = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)
      repository.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1064213), data = Map[Source, SourceData](
        CinemaShowing(KinoApollo, "anora") -> SourceData(title = Some("Anora"), showtimes = Seq(at(20))))))
      val id = repository.findAll().head.id
      screenings.upsertSlot(id.value, CinemaShowing(KinoApollo, "orphan").displayName, ListedShowtimes(Seq(at(9)), None))
      repository.readVenues(id.value, Set(KinoApollo)) shouldBe None
    }
  }
}
