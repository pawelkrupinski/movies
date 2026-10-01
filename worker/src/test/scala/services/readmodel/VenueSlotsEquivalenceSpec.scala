package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, FilmId, FilmWriteFence, InMemoryMovieRepository, InMemoryScreeningsRepository, StoredMovieRecord, VenueSlots}
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime
import scala.util.Random

/**
 * A change confined to some venues' showtimes, applied from those venues alone
 * ([[ReadModelProjector.onVenueSlots]]), must leave the read model EXACTLY as projecting the whole
 * film again leaves it — or decline and leave it to the whole-film projection. Random films (several
 * cinemas, display-title variants, venues holding two slots of the film) take random showtime changes
 * (some venues gaining their first showtimes or losing their last) one way on one projector and the
 * other way on another, and the two read models must agree after every change.
 */
class VenueSlotsEquivalenceSpec extends AnyFlatSpec with Matchers {

  private val clock   = java.time.Clock.fixed(java.time.Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val cinemas = Cinema.all.filter(City.forCinema(_).isDefined).take(8)
  private val titles  = Seq(Some("Foo"), Some("Foo"), Some("Foo"), None, Some("Foo (napisy)"), Some("Фу"))
  private val times   = (10 to 22 by 2).map(h => Showtime(LocalDateTime.of(2026, 6, 7, h, 0), Some(s"https://book/$h")))

  private def showtimes(rng: Random): Seq[Showtime] = if (rng.nextInt(5) == 0) Nil else rng.shuffle(times).take(1 + rng.nextInt(4))

  private def film(rng: Random): MovieRecord = {
    val slots = cinemas.flatMap { cinema =>
      (0 until rng.nextInt(3)).map { i =>
        val title = titles(rng.nextInt(titles.size))
        CinemaShowing(cinema, s"foo$i") -> SourceData(title = title, filmUrl = Some(s"https://${cinema.displayName}/foo$i"),
          showtimes = showtimes(rng))
      }
    }
    MovieRecord(tmdbId = Some(1), imdbRating = Some(7.5), data = (slots :+ (Tmdb -> SourceData(title = Some("Foo")))).toMap[Source, SourceData])
  }

  // `SourceData`'s equality is showtime-blind by design (and blind to its cache-only digests), so a
  // comparison of records proves nothing here: every field of every slot, arrays by content.
  private def everyField(record: MovieRecord) =
    (record, record.data.toSeq.map { case (source, slot) => source.displayName -> slot.productIterator.map {
      case array: Array[?]       => array.toSeq
      case Some(array: Array[?]) => Some(array.toSeq)
      case other                 => other
    }.toList }.sortBy(_._1))

  private def stored(record: MovieRecord) = StoredMovieRecord.synthesised("Foo", Some(2024), record, titleNormalizer)

  private def projector(rm: InMemoryReadModelRepository) =
    new ReadModelProjector(new InMemoryMovieRepository(normalizer = titleNormalizer), rm, rm, clock = clock)

  "A change at some venues" should "write from those venues alone exactly what projecting the whole film writes" in {
    var accepted, declined = 0
    val reasons = scala.collection.mutable.Set.empty[String]
    (0 until 300).foreach { round =>
      val rng = new Random(round)
      val (wholeRm, venueRm) = (new InMemoryReadModelRepository(), new InMemoryReadModelRepository())
      val (whole, venue)     = (projector(wholeRm), projector(venueRm))
      var record = film(rng)
      whole.onMovieUpsert(stored(record)); venue.onMovieUpsert(stored(record))
      (0 until 6).foreach { step =>
        val touched = rng.shuffle(cinemas).take(1 + rng.nextInt(2)).toSet
        record = record.copy(data = record.data.map {
          case (s @ CinemaShowing(cinema, _), slot) if touched(cinema) => s -> slot.copy(showtimes = showtimes(rng))
          case other => other
        })
        val now = stored(record)
        whole.onMovieUpsert(now)
        val atCinemas = touched.map(cinema => cinema -> record.data.toSeq.collect {
          case (s @ CinemaShowing(`cinema`, _), slot) => s -> slot }).toMap
        venue.onVenueSlots(VenueSlots(FilmId(now.id.value), atCinemas)) match {
          case services.movies.VenueVerdict.Applied          => accepted += 1
          case services.movies.VenueVerdict.Declined(reason) => declined += 1; reasons += reason; venue.onMovieUpsert(now)
          case services.movies.VenueVerdict.NotYet           => fail("a row projected above cannot be one still to learn")
        }
        withClue(s"round $round step $step: ") {
          venueRm.findAllMovies().toSet shouldBe wholeRm.findAllMovies().toSet
          venueRm.findAllScreenings().toSet shouldBe wholeRm.findAllScreenings().toSet
        }
      }
      whole.stop(); venue.stop()
    }
    // Both paths must actually have run, or the agreement above proves nothing.
    accepted should be > 300
    declined should be > 100
    // Every decline names why, and the random changes reach the reasons that are about presence.
    import services.movies.ChangeStreamMetrics.VenueDecline as Why
    reasons.toSet should contain allOf (Why.ProjectorVenueAppears, Why.ProjectorVenueVanishes)
    reasons.toSet.subsetOf(Why.All.toSet) shouldBe true
  }

  "The cache" should "hold after a change at some venues, applied from them alone, exactly what the whole film's re-read leaves" in {
    var accepted, declined = 0
    (0 until 200).foreach { round =>
      val rng = new Random(10000 + round)
      // Showtimes in their own collection, as in production: what the cache strips its slots for.
      def cache() = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer,
        screenings = Some(new InMemoryScreeningsRepository)), new services.events.InProcessEventBus(), normalizer = titleNormalizer, clock = clock)
      val (whole, venue) = (cache(), cache())
      var record = film(rng)
      whole.applyUpsert(stored(record), FilmWriteFence.Unfenced); venue.applyUpsert(stored(record), FilmWriteFence.Unfenced)
      (0 until 6).foreach { step =>
        val touched = rng.shuffle(cinemas).take(1 + rng.nextInt(2)).toSet
        record = record.copy(data = record.data.map {
          case (s @ CinemaShowing(cinema, _), slot) if touched(cinema) => s -> slot.copy(showtimes = showtimes(rng))
          case other => other
        })
        val now = stored(record)
        whole.applyUpsert(now, FilmWriteFence.Unfenced)
        val atCinemas = touched.map(cinema => cinema -> record.data.toSeq.collect {
          case (s @ CinemaShowing(`cinema`, _), slot) => s -> slot }).toMap
        if (venue.applyVenueSlots(VenueSlots(FilmId(now.id.value), atCinemas), FilmWriteFence.Unfenced) == services.movies.VenueVerdict.Applied) accepted += 1
        else { declined += 1; venue.applyUpsert(now, FilmWriteFence.Unfenced) }
        val key = now.cacheKey(titleNormalizer)
        withClue(s"round $round step $step: ")(venue.get(key).map(everyField) shouldBe whole.get(key).map(everyField))
      }
    }
    accepted shouldBe 1200
    declined shouldBe 0
  }

  // After a boot the projector knows only the read model's contents, not the rows they came from: it
  // learns them from the corpus census's read, and a change at a film's venues waits until it has.
  private final class Restart(seed: Long) {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val (beforeRm, afterRm) = (new InMemoryReadModelRepository(), new InMemoryReadModelRepository())
    val rng    = new Random(seed)
    var record = film(rng)
    while (!record.data.exists { case (CinemaShowing(_, _), slot) => slot.showtimes.nonEmpty; case _ => false }) record = film(rng)
    repository.upsert("Foo", Some(2024), record)
    def row = repository.findAll().head
    // the process before the restart projected it, into both read models
    Seq(beforeRm, afterRm).foreach { rm => val p = projector(rm); p.onMovieUpsert(row); p.stop() }
    val whole = projector(beforeRm)                          // a projector that keeps projecting whole
    whole.onMovieUpsert(row)
    val booted = projector(afterRm)                          // the restarted worker's
    def cinema = record.data.collectFirst { case (CinemaShowing(c, _), slot) if slot.showtimes.nonEmpty => c }.get
    def change(at: models.Cinema): VenueSlots = {
      record = record.copy(data = record.data.map {
        case (s @ CinemaShowing(`at`, _), slot) => s -> slot.copy(showtimes = slot.showtimes.take(1) :+ times.last)
        case other => other
      })
      repository.upsert("Foo", Some(2024), record)
      whole.onMovieUpsert(row)
      VenueSlots(row.id, Map(at -> record.data.toSeq.collect { case (s @ CinemaShowing(`at`, _), slot) => s -> slot }))
    }
  }

  "The projector" should "learn a row from a census read, wait for it until then, and apply its venues alone after" in {
    val world = new Restart(7)
    import world.*
    booted.learn(ReadModelProjection.partition(row, titleNormalizer)) shouldBe false   // before its seed: refused
    booted.seedFromReadModel()
    val venues = change(cinema)
    booted.onVenueSlots(venues) shouldBe services.movies.VenueVerdict.NotYet           // not learned yet: wait
    // The census reads the source — already changed at this venue — so that venue's stored row stays
    // unvouched; it is the one the change rewrites, so the apply needs nothing from it.
    booted.learn(ReadModelProjection.partition(row, titleNormalizer)) shouldBe true
    booted.learnedAll()
    booted.onVenueSlots(venues) shouldBe services.movies.VenueVerdict.Applied
    afterRm.findAllMovies().toSet shouldBe beforeRm.findAllMovies().toSet
    afterRm.findAllScreenings().toSet shouldBe beforeRm.findAllScreenings().toSet
    booted.onVenueSlots(VenueSlots(FilmId("absent|2024"), venues.atCinemas)) shouldBe
      services.movies.VenueVerdict.Declined(services.movies.ChangeStreamMetrics.VenueDecline.ProjectorRowUnprojected)
    whole.stop(); booted.stop()
  }

  it should "apply a change at one venue of a card whose other row drifted, leaving that row to the content check" in {
    val world = new Restart(11)
    import world.*
    val drifted = afterRm.findAllScreenings().head
    val stale   = drifted.copy(showtimes = Seq(times.head))
    afterRm.upsertScreening(stale)                                                       // the read model drifted
    booted.seedFromReadModel()
    booted.learn(ReadModelProjection.partition(row, titleNormalizer))
    booted.learnedAll()
    record.data.collectFirst { case (CinemaShowing(c, _), slot) if slot.showtimes.nonEmpty && c.displayName != drifted.cinema => c }
      .foreach { at =>
        booted.onVenueSlots(change(at)) shouldBe services.movies.VenueVerdict.Applied
        afterRm.findAllScreenings().find(_._id == drifted._id) shouldBe Some(stale)        // untouched: the content check's
        afterRm.findAllScreenings().filter(_.cinema == at.displayName).toSet shouldBe
          beforeRm.findAllScreenings().filter(_.cinema == at.displayName).toSet
      }
    whole.stop(); booted.stop()
  }
}
